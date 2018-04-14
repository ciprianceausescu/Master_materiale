%   Package: aggregate
%   Author : Richard A. O'Keefe
%   Updated: 28 Nov 1989
%   Defines: an aggregation operator for data-base-style queries

%   Copyright (C) 1989, Quintus Computer Systems, Inc.  All rights reserved.

/*  In earlier releases of the Dec-10 Prolog, C Prolog, and Quintus
    Prolog libraries, the predicate free_variables/4 was defined
    elsewhere (typically not.pl), and used freely by any library
    file which included that one.  However, I don't really want to
    make free_variables/4 a public predicate of library(not), so
    this predicate has been split out into its own file.

    This is support for not.pl, setof.pl, and foreach.pl.  It is
    not meant for general use.

    In August 1987, term_variables/3 was added to support the new
    grouped_{bag,set}_of/4 predicates.

    In April 1989, all of the code for free_variables/4 was scrapped.
    It had always been our intention to have free_variables/4 check
    for quantifiers ONLY in "code" rather than "data".  Note that it
    should really look inside the meta-arguments of meta_predicates.
    The built-in setof/3 and bagof/3 use a version of free_variables/4
    which does this.
*/

:- module(aggregate, [
	forall/2,
	foreach/2,
	aggregate/3,
	aggregate/4,
	aggregate_all/3,
	aggregate_all/4,
	free_variables/4,
	term_variables/3
   ]).
:- meta_predicate
	forall(0, 0),
	foreach(0, 0),
	aggregate(+, 0, ?),
	aggregate(+, +, 0, ?),
	aggregate_all(+, 0, ?),
	aggregate_all(+, +, 0, ?),
	save_instances(+, 0),
	save_instances(+, +, 0).

:- use_module(library(types), [
	illarg/3
	]).
:- mode
	free_variables(+, +, +, -),
	    free_variables_1(+, +, +, -),
	    data_variables(+, +, +, -),
		data_variables(+, +, +, +, -),
	list_is_free_of(+, +),
	term_is_free_of(+, +),
	    term_is_free_of(+, +, +),
	term_variables(+, +, -),
	    term_variables(+, +, +, -),
	list_instances(+, -),
	list_instances(+, +, -).

/*----------------------------------------------------------------------
%@  @cindex aggregation
%@  Data base query languages usually provide so-called "aggregation"
%@  operations.  Given a relation, aggregation specifies
%@  @itemize @bullet
%@  @item a column of the relation
%@  @item an operation, one of @{sum,max,min,ave,var@} or more
%@  @end itemize
%@  
%@  One might, for example, ask
%@  
%@  @example
%@      PRINT DEPT,SUM(AREA) WHERE OFFICE(_ID,DEPT,AREA,_OCCUPANT)
%@  @end example
%@  
%@  and get a table of <@var{Department},@var{TotalArea}> pairs.  The Prolog
%@  equivalent of this might be
%@  
%@  @example
%@      dept_office_area(Dept, TotalArea) :-
%@              aggregate(sum(Area),
%@                  I^O^office(I,Dept,Area,O), TotalArea).
%@  @end example
%@  
%@  where @var{Area} is the column and @code{sum(_)} is the aggregation operator.
%@  We can also ask who has the smallest office in each department:
%@  
%@  @example
%@      smallest_office(Dept, Occupant) :-
%@              aggregate(min(Area),
%@                      I^O^office(I,Dept,Area,O), MinArea),
%@              office(_, Dept, MinArea, Occupant).
%@  @end example
%@  
%@  This module provides an aggregation operator in Prolog:
%@  
%@  @example
%@          aggregate(Template, Generator, Results)
%@  @end example
%@  
%@  where:
%@  @itemize @bullet
%@  @item
%@  @var{Template} is @var{operator}(@var{expression})
%@  or  @var{constructor}(@var{arg},...,@var{arg})
%@  @item
%@  each @var{arg} is @var{operator}(@var{expression})
%@  @item
%@  @var{operator} is @code{sum | min | max}  @{for now@}
%@  @item
%@  @var{expression} is an arithmetic expression
%@  @end itemize
%@  
%@  @var{Results} is unified with a form of the same structure as @var{Template}.
%@  
%@  Things like mean and standard deviation can be calculated from
%@  sums, e.g. to find the average population of countries (defined
%@  as "if you sampled people at random, what would be the mean
%@  size of their answers to the question 'what is the population
%@  of your country?'?") we could do
%@  
%@  @example
%@  ?-  aggregate(x(sum(Pop),sum(Pop*Pop)),
%@                Country^population(Country,Pop),
%@                x(People,PeopleTimesPops)),
%@      AveragePop is PeopleTimesPops/People.
%@  @end example
%@  
%@  Note that according to this definition, @code{aggregate/3} FAILS if
%@  there are no solutions.  For @code{max(_)}, @code{min(_)}, and many other
%@  operations (such as @code{mean(_)}) this is the only sensible
%@  definition (which is why @code{bagof/3} works that way).  Even if
%@  @var{bagof/3} yielded an empty list, @var{aggregate/3} would still fail.
%@  
%@  Concerning the minimum and maximum, it is convenient at times to
%@  know Which term had the minimum or maximum value.  So we write
%@  
%@  @example
%@      min(Expression, Term)
%@      max(Expression, Term)
%@  @end example
%@  
%@  and in the constructed term we will have
%@  
%@  @example
%@      min(MinimumValue, TermForThatValue)
%@      max(MaximumValue, TermForThatValue)
%@  @end example
%@  
%@  So another way of asking who has the smallest office is
%@  
%@  @example
%@      smallest_office(Dept, Occupant) :-
%@              aggregate(min(Area,O),
%@                      I^office(I,Dept,Area,O), min(_,Occupant)).
%@  @end example
%@  
%@  Consider queries like
%@  
%@  @example
%@      aggregate(sum(Pay), Person^pay(Person,Pay), TotalPay)
%@  @end example
%@  
%@  where for some reason @code{pay/2} might have multiple solutions.
%@  (For example, someone might be listed in two departments.)
%@  We need a way of saying "treat identical instances of the
%@  Template as a single instance, UNLESS they correspond to
%@  different instances of a Discriminator."  That is what
%@  
%@  @example
%@      aggregate(Template, Discriminator, Generator, Results)
%@  @end example
%@  
%@  does.  
%@  @c Thus
%@  @c 
%@  @c     NU Prolog               Quintus Prolog
%@  @c     count(D, Goal, C)       aggregate(count, D, Goal, C)
%@  @c     max(X, Goal, M)         aggregate(max(X), Goal, M)
%@  @c     min(X, Goal, M)         aggregate(min(X), Goal, M)
%@  @c     sum(X, D, Goal, S)      aggregate(sum(X), D, Goal, S)
%@  
%@  Operations available:
%@  @table @code
%@  @item count
%@  @code{sum(1)}
%@  @item sum(@var{E})
%@  sum of values of @var{E}
%@  @item min(@var{E})
%@  minimum of values of @var{E}
%@  @item min(@var{E},@var{X})
%@  @code{min(@var{E})} with corresponding instance of @var{X}
%@  @item max(@var{E})
%@  maximum of values of @var{E}
%@  @item max(@var{E},@var{X})
%@  @code{max(@var{E})} with corresponding instance of @var{X}
%@  @item set(@var{X})
%@  ordered set of instances of @var{X}
%@  @item bag(@var{X})
%@  list of instances of @var{X} in generated order.
%@  @end table
%@  
%@  @example
%@  bagof(X, G, B) :- aggregate(bag(X),    G, L).
%@  setof(X, G, B) :- aggregate(set(X), X, G, L).
%@  @end example
%@  
%@  @c In 1989, two new operations were added:
%@  @c     aggregate_all(Template, Generator, Results),
%@  @c     aggregate_all(Template, Discriminator, Generator, Results).
%@  @c They are hybrids between the aggregate/[3,4] operations and the
%@  @c {set,bag}_of_all/3 operations found in library(setof).  They insist
%@  @c that all the variables in Generator should be captured by the
%@  @c Template, the Discriminator, or existential quantifiers.  This means
%@  @c that it makes sense for them to return zero counts, zero sums, empty
%@  @c sets, and empty bags.
%@  
%@  Exported predicates:
%@  
%@  @table @code
----------------------------------------------------------------------*/





%@  @item forall(@var{:Generator}, @var{:Goal})
%@  @PLXindex {forall/2 (aggregate)}
%@  succeeds when @var{Goal} is provable for each true instance of @var{Generator}.
%@  Note that there is a sort of double negation going on in here (it
%@  is in effect a nested pair of failure-driven loops), so it will
%@  never bind any of the variables which occur in it.

forall(Generator, Goal) :-
	call(( Generator, ( Goal -> fail ; true ) -> fail ; true )).


%@  @item foreach(@var{:Generator}, @var{:Goal})
%@  @PLXindex {foreach/2 (aggregate)}
%@  for each proof of Generator in turn, we make a copy of @var{Goal} with
%@  the appropriate substitution, then we execute these copies in
%@  sequence.  For example, @code{foreach(between(1,3,I), p(I))} is
%@  equivalent to @code{p(1), p(2), p(3)}.
%@  
%@  Note that this is not the same as @code{forall/2}.  For example,
%@  @code{forall(between(1,3,I), p(I))} is equivalent to
%@  @code{\+ \+ p(1), \+ \+ p(2), \+ \+ p(3)}.
%@  
%@  The trick in @code{foreach/2} is to ensure that the variables of @var{Goal} which
%@  do not occur in @var{Generator} are restored properly.  (If there are no
%@  such variables, you might as well use @code{forall/2}.)
%@  
%@  Like @code{forall/2}, this predicate does a failure-driven loop over the
%@  @var{Generator}.  Unlike @code{forall/2}, the @var{Goals} are executed as an ordinary
%@  conjunction, and may succeed in more than one way.

foreach(Generator, Goal) :-
	free_variables(Goal, Generator, [], FreeVariables),	
	save_instances(FreeVariables, Goal, Generator),
	conj_instances(FreeVariables, Conjunction),
	call(user:Conjunction).



%@  @item aggregate(@var{+Template}, @var{+Discriminator}, @var{:Generator}, @var{-Result})
%@  @PLXindex {aggregate/4 (aggregate)}
%@  is a generalisation of @code{setof/3} which lets you compute sums,
%@  minima, maxima, and so on.  

aggregate(Template, Discriminator, Generator, Result) :-
	parse_template(Template, Pattern),
	setof(Discriminator-Pattern, Generator, Set),
	strip_discriminator(Set, List),
	process_results(List, Pattern, Result).

strip_discriminator([], []).
strip_discriminator([_-T|S], [T|R]) :-
	strip_discriminator(S, R).


%@  @item aggregate(@var{+Template}, @var{:Generator}, @var{-Result})
%@  @PLXindex {aggregate/3 (aggregate)}
%@  is a generalisation of @code{findall/3} which lets you compute sums,
%@  minima, maxima, and so on.  

aggregate(Template, Generator, Result) :-
	parse_template(Template, Pattern),
	bagof(Pattern, Generator, List),
	process_results(List, Pattern, Result).


/*  A general note:
    The simplest way of coding these predicates would be to copy the
    definitions of aggregate/[3,4] replacing setof/3 by set_of_all/3
    and bagof/3 by bag_of_all/3.  I have chosen not to do that for
    two reasons:
    (a) calling free_variables/4 here means that I can report the
	actual call in any error message, not the internal *_of_all/3.
    (b) at the moment I call setof/3 and bagof/3 because they are quite
	a bit faster than the library *_of_all/3 predicates.  That is
	going to change:  in the 2.5 final release findall/3 will be
	available to users, which means that set_of_all/3 and bag_of_all/3
	can be about as fast as setof/3 and bagof/3.
    Reason (a) will still hold, so in the 2.5 final release, make the
    changes suggested by the comments.
*/

%@  @item aggregate_all(+Template, +Discriminator, :Generator, -Result)
%@  @PLXindex {aggregate_all/4 (aggregate)}
%@  is like @code{aggregate/4} except that it will find at most one solution,
%@  and does not bind free variables in the @var{Generator}.  
%@  @c It is a hybrid
%@  @c between aggregate/4 and set_of_all/3.

aggregate_all(Template, Discriminator, Generator, Result) :-
        free_variables(Generator, Discriminator-Template, [], Vars),
        Vars \== [],            % Note that there is no cut here!
	illarg(var, aggregate_all(Template,Discriminator,Generator,Result), 0).
aggregate_all(Template, Discriminator, Generator, Result) :-
	parse_template(Template, Pattern),
	findall(Discriminator-Pattern, Generator, Bag),
	sort(Bag, Set),
	strip_discriminator(Set, List),
	process_results(List, Pattern, Result).


%@  @item aggregate_all(+Template, :Generator, -Result)
%@  @PLXindex {aggregate_all/3 (aggregate)}
%@  is like @code{aggregate/3} except that it will find at most one solution,
%@  and does not bind free variables in the @var{Generator}.  
%@  @c It is a hybrid
%@  @c between aggregate/3 and bag_of_all/3.

aggregate_all(Template, Generator, Result) :-
        free_variables(Generator, Template, [], Vars),
        Vars \== [],            % Note that there is no cut here!
	illarg(var, aggregate_all(Template,Generator,Result), 0).
aggregate_all(Template, Generator, Result) :-
	parse_template(Template, Pattern),
	findall(Pattern, Generator, List),
	process_results(List, Pattern, Result).


%   parse_template(+Template, -Pattern)
%   takes a template as written by the programmer and turns it into
%   a Pattern which has a disinct constructor function for each case.
%   The cases for Pattern are

%	any(V)		was a source variable.  This should be reported
%			as an error, but isn't for the moment.
%	set(Term)	was set(Term).  The final result will be a list
%			in standard order with no duplicates.  It will
%			NOT have set(_) wrapped around it.
%	bag(Term)	was bag(Term).  The final result will be a list
%			in the order generated, with duplicates if they
%			were generated.  It will NOT have bag(_) around it.
%	sum(Expr)	was sum(Expr).  The final result will be
%			the sum of the values of the Exprs.  It will NOT
%			have sum(_) wrapped around it.
%	count		was count.  We could use sum(1), but this is
%			more compact.
%	min(Expr)	was min(Expr).  The final result will be the
%			minimum of the values of the Exprs.  It will NOT
%			have min(_) wrapped around it.
%	max(Expr)	was max(Expr).  The final result will be the
%			maximum of the values of the Exprs.  It will NOT
%			have max(_) wrapped around it.
%	min(Expr,Term)	was min(Expr,Term).  The final result will be the
%			pair min(N,T) where N is the minimum of the values
%			of the Exprs and T is the corresponding Term.  This
%			DOES have the min(_,_) wrapper.
%	max(Expr,Term)	was max(Expr,Term).  The final result will be the
%			pair max(N,T) where N is the maximum of the values
%			of the Exprs and T is the corresponding Term.  This
%			DOES have the max(_,_) wrapper.
%	pat(F,List)	F is an atom and List is a list of patterns.  The
%			final result is F(List') where List' is the sequence
%			of values of elements of List.  For example,
%			sum(X)/count in the Template would be represented
%			as pat(/,[sum(X),sum(1)]) in the Pattern, and the
%			final result might be 273/9.
%	[H|T]		an optimisation of pat(.,[H,T]).
%	con(Term)	An optimisation of pat/2 where the Term is ground.

parse_template(Var,      any(Var)) :- var(Var), !.
parse_template(set(X),   set(X))   :- !.
parse_template(bag(X),   bag(X))   :- !.
parse_template(count,    count)    :- !.
parse_template(sum(E),   sum(E))   :- !.
parse_template(min(E),   min(E))   :- !.
parse_template(max(E),   max(E))   :- !.
parse_template(min(E,T), min(E,T)) :- !.
parse_template(max(E,T), max(E,T)) :- !.
parse_template([],       [])       :- !.
parse_template(Term,     con(Term)):-
	numbervars(Term, 0, 0),
	!.
parse_template([E|Es],	 [X|Xs])   :- !,
	parse_template(E, X),
	parse_template(Es, Xs).
parse_template(Template, pat(F,L)) :-
	functor(Template, F, N),
	parse_template(N, Template, [], L).

parse_template(N, Template, L0, L) :-
    (	N =:= 0 -> L = L0
    ;	M is N-1,
	arg(N, Template, Arg),
	parse_template(Arg, Pat),
	parse_template(M, Template, [Pat|L0], L)
    ).


%   process_results(+ListInstances, +Pattern, ?Result)
%   takes a list of instances of the Pattern as calculated by
%   bagof/3 or setof/3, combines the results according to the
%   pattern, and unifies Result with the combination.

%   If ListOfPatternInstances is empty, the Result is defaulted from
%   the Pattern.  For aggregate_all/[3,4] this makes sense, but for
%   aggregate/[3,4] it doesn't, as those two are supposed to succeed
%   only when the List is nonempty.  However, bagof/3 and setof/3 do
%   not yield an empty list, so we've nothing to worry about.

process_results([], Pattern, Result) :-
	process_default(Pattern, Result).
process_results([First|Rest], Pattern, Result) :-
	process_first(First, X0),
	process_rest(Rest, X0, X1),
	process_result(Pattern, X1, Result).

/*  When there are no solutions,
	any(_), min(_), min(_,_), max(_), and max(_,_)
    can be assigned no meaning, so process_default/2 fails.
*/
process_default(con(X),  X).
process_default(set(_), []).
process_default(bag(_), []).
process_default(count,   0).
process_default(sum(_),  0).
process_default([],     []).
process_default([E|Es], [X|Xs]) :-
	process_default(E, X),
	process_default(Es, Xs).
process_default(pat(F,Es), T) :-
	process_default(Es, Xs),
	T =.. [F|Xs].

process_first(any(X), X).
process_first(con(X), X).
process_first(set(X), [X]).
process_first(bag(X), [X]).
process_first(count,  1).
process_first(sum(E), X) :- X is E.
process_first(min(E), X) :- X is E.
process_first(max(E), X) :- X is E.
process_first(min(E,T), min(X,T)) :- X is E.
process_first(max(E,T), max(X,T)) :- X is E.
process_first([],     []).
process_first([E|Es], [X|Xs]) :-
	process_first(E, X),
	process_first(Es, Xs).
process_first(pat(_,Es), Xs) :-
	process_first(Es, Xs).


process_rest([], X, X).
process_rest([E|Es], X0, X) :-
	process_next(E, X0, X1),
	process_rest(Es, X1, X).

process_next(any(_), V, V).
process_next(con(_), V, V).
process_next(set(X), V, [X|V]).
process_next(bag(X), V, [X|V]).
process_next(count,  V, V1) :-
	V1 is V+1.
process_next(sum(E), V, V1) :-
	X is E,
	V1 is V+X.
process_next(min(E), V, V1) :-
	X is E,
	( X < V -> V1 = X ; V1 = V ).
process_next(max(E), V, V1) :-
	X is E,
	( X > V -> V1 = X ; V1 = V ).
process_next(min(E,T), min(V,C), min(V1,C1)) :-
	X is E,
	( X < V -> V1 = X, C1 = T ; V1 = V, C1 = C ).
process_next(max(E,T), max(V,C), max(V1,C1)) :-
	X is E,
	( X > V -> V1 = X, C1 = T ; V1 = V, C1 = C ).
process_next([], [], []).
process_next([E|Es], [V|Vs], [W|Ws]) :-
	process_next(E, V, W),
	process_next(Es, Vs, Ws).
process_next(pat(_,L), V, V1) :-
	process_next(L, V, V1).



process_result(any(_), X, X).
process_result(con(_), X, X).
process_result(set(_), V, X) :-
	sort(V, X).
process_result(bag(_), V, X) :-
	rev(V, [], X).
process_result(count,  X, X).
process_result(sum(_), X, X).
process_result(min(_), X, X).
process_result(max(_), X, X).
process_result(min(_,_), X, X).
process_result(max(_,_), X, X).
process_result([], [], []).
process_result([E|Es], [V|Vs], [X|Xs]) :-
	process_result(E, V, X),
	process_result(Es, Vs, Xs).
process_result(pat(F,L), V, X) :-
	length(L, N),
	functor(X, F, N),
	process_result_s(L, V, X, 0).

process_result_s([], [], _, _).
process_result_s([E|Es], [X|Xs], T, M) :-
	N is M+1,
	arg(N, T, V),
	process_result(E, X, V),
	process_result_s(Es, Xs, T, N).


rev([], L, L).
rev([H|T], L, R) :-
	rev(T, [H|L], R).


%@  @item free_variables(+Goal, +Bound, +Vars0, -Vars)
%@  @PLXindex {free_variables/4 (aggregate)}
%@  binds @var{Vars} to the union of @var{Vars0} with the set of @emph{free} variables
%@  in @var{Goal}, that is the set of variables which are captured neither
%@  by @var{Bound} nor by any internal quantifiers or templates in @var{Goal}.
%@  We have to watch out for @code{setof/3} and @code{bagof/3} themselves, for the
%@  explicit existential quantifier @code{@var{Vars}^@var{Goal}}, and for things like
%@  @code{\+(_)} which might look as though they bind variables but can't.

free_variables(Term, Bound, Vars0, Vars) :-
	(   nonvar(Term) ->
	    free_variables_1(Term, Bound, Vars0, Vars)
	;   term_is_free_of(Bound, Term),
	    list_is_free_of(Vars0, Term)
	->  Vars = [Term|Vars0]
	;   Vars = Vars0
	).

free_variables_1(:(_Module,Goal), Bound) --> !,
	free_variables_1(Goal, Bound).
free_variables_1((Conjunct,Conjuncts), Bound) --> !,
	free_variables(Conjunct,  Bound),
	free_variables(Conjuncts, Bound).
free_variables_1((Disjunct ; Disjuncts), Bound) --> !,
	free_variables(Disjunct,  Bound),
	free_variables(Disjuncts, Bound).
free_variables_1((If -> Then), Bound) --> !,
	free_variables(If,   Bound),
	free_variables(Then, Bound).
free_variables_1(call(Goal), Bound) --> !,
	free_variables(Goal, Bound).
free_variables_1(\+(_), _) --> !.
free_variables_1(Vars^Goal, Bound) --> !,
	free_variables(Goal, Vars^Bound).
free_variables_1(setof(Template,Generator,Set), Bound) --> !,
	free_variables(Generator, Template^Bound),
	data_variables(Set, Bound).
free_variables_1(bagof(Template,Generator,Bag), Bound) --> !,
	free_variables(Generator, Template^Bound),
	data_variables(Bag, Bound).

/*  The following two clauses are included because you will have to
    use set_of/3 and bag_of/3 throughout your program instead of
    setof/3 and bagof/3 to work around the bug.
*/
free_variables_1(set_of(Template,Generator,Set), Bound) --> !,
	free_variables(Generator, Template^Bound),
	data_variables(Set, Bound).
free_variables_1(bag_of(Template,Generator,Bag), Bound) --> !,
	free_variables(Generator, Template^Bound),
	data_variables(Bag, Bound).

/*  If you intend to use any of the library predicates
	set_of/3, bag_of/3, set_of_all/3, bag_of_all/3,
	findall/3, aggregate/3, grouped_bag_of/3,
	grouped_set_of/3, grouped_aggregate/3, forall/2
    then move the corresponding clauses above this comment.
    This version of library(freevars) was prepared for the
    Philips laboratory in Brussels.  The next line is not a mistake!
/*
free_variables_1(set_of_all(Template,Generator,Set), Bound) --> !,
	free_variables(Generator, Template^Bound),
	data_variables(Set, Bound).
free_variables_1(bag_of_all(Template,Generator,Bag), Bound) --> !,
	free_variables(Generator, Template^Bound),
	data_variables(Bag, Bound).
free_variables_1(findall(_,_,List), Bound) --> !,
	% The Generator is ignored, just like \+(Generator)
	data_variables(List, Bound).
free_variables_1(aggregate(Template,Generator,Aggregate), Bound) --> !,
	free_variables(Generator, Template^Bound),
	data_variables(Aggregate, Bound).
free_variables_1(grouped_set_of(GroupVars,_,_,Set), Bound) --> !,
	data_variables(GroupVars, Bound),
	data_variables(Set, Bound).
free_variables_1(grouped_bag_of(GroupVars,_,_,Bag), Bound) --> !,
	data_variables(GroupVars, Bound),
	data_variables(Bag, Bound).
free_variables_1(grouped_aggregate(GroupVars,_,_,Aggregate), Bound) --> !,
	data_variables(GroupVars, Bound),
	data_variables(Aggregate, Bound).
free_variables_1(forall(_,_), _) --> !.

/*  End of "optional" clauses.
*/
free_variables_1(NormalGoal, Bound) -->
	data_variables(NormalGoal, Bound).

%   data_variables(+Term, +Bound, +Vars0, -Vars)
%   binds Vars to the union of Vars0 with the set of variables in Term
%   which do not occur in (are not "captured by") Bound.  When the Bound
%   contains no variables, it delivers the same results as
%   term_variables(Term, Vars0, Vars).

data_variables(Term, Bound, Vars0, Vars) :-
	(   nonvar(Term) ->
	    functor(Term, _, N),
	    data_variables(N, Term, Bound, Vars0, Vars)
	;   term_is_free_of(Bound, Term),
	    list_is_free_of(Vars0, Term)
	->  Vars = [Term|Vars0]
	;   Vars = Vars0
	).

data_variables(N, Term, Bound) -->
    (	{ N =:= 0 } -> []
    ;	{ arg(N, Term, Arg), M is N-1 },
	data_variables(Arg, Bound),
	data_variables(M, Term, Bound)
    ).



%   term_is_free_of(+Term, +Var)
%   is a meta-logical predicate which is true when the variable Var
%   does not occur anywhere in the term Term.  It is used when the
%   Term is a tree built from all the existential quantifiers and
%   Templates dominating (the goal containing) this variable.

term_is_free_of(Term, Var) :-
	(   var(Term) ->
	    Term \== Var
	;   functor(Term, _, N),
	    term_is_free_of(N, Term, Var)
	).

term_is_free_of(N, Term, Var) :-
	(   N =:= 0 -> true
	;   arg(N, Term, Arg),
	    term_is_free_of(Arg, Var),
	    M is N-1,
	    term_is_free_of(M, Term, Var)
	).


%   list_is_free_of(+Vars0, +Var)
%   is a meta-logical predicate which is true when the variable Var
%   is not an element of Vars0, which is known to be a list of variables.
%   It is used when Vars0 is the set of free variables which have been
%   built up so far, and we are considering whether Var should be
%   added to this set.

list_is_free_of([], _).
list_is_free_of([Head|Tail], Var) :-
	Head \== Var,
	list_is_free_of(Tail, Var).


%@  @item term_variables(@var{+Term}, @var{+Vars0}, @var{-Vars})
%@  @PLXindex {term_variables/3 (aggregate)}
%@  binds @var{Vars} to a union of @var{Vars0} and the variables which occur in @var{Term}.
%@  This doesn't take quantifiers into account at all.

term_variables(Term, Vars0, Vars) :-
	nonvar(Term),
	!,
	functor(Term, _, N),
	term_variables(N, Term, Vars0, Vars).
term_variables(Term, Vars0, [Term|Vars0]) :-
	list_is_free_of(Vars0, Term),
	!.
term_variables(_, Vars, Vars).


term_variables(N, Term) -->
    (	{ N =:= 0 } -> []
    ;	{ arg(N, Term, Arg), M is N-1 },
	term_variables(Arg),
	term_variables(M, Term)
    ).



:- dynamic
	setof_stack/2.



/*  In the first draft of this file, the intention was to use
	setof_stack(Term)
    where Term is [] or found(Instance) or Key-Instance.
    However, this involves indexing on the functor, which is rather
    pointless here.  So the current version uses
	setof_stack(_, Term)
    to avoid indexing.  The low-level operation which supports the
    built-in operations setof/3 and bagof/3 is more than 3 times
    faster than the version of findall/3 in this library.
*/

%   save_instances(+Template, +Generator)
%   enumerates all provable instances of the Generator and stores the
%   associated Template instances.  Neither argument ends up changed.

save_instances(Template, Generator) :-
	asserta(setof_stack(_, [])),
	call(Generator),
	asserta(setof_stack(_, found(Template))),
	fail.
save_instances(_, _).



%   list_instances(?List)
%   pulls all the Template instances out of the data base until it
%   hits the marker, and unifies List with the result.

list_instances(List) :-
	list_instances([], List).


%   list_instances(+SoFar, ?Total)
%   pulls all the Template instances out of the data base until it
%   hits the marker, and puts them on the front of the accumulator
%   SoFar.  This routine is used by findall/3-4 and by bag_of when
%   the Generator has no free variables.

list_instances(SoFar, Total) :-
	retract(setof_stack(_, Term)),
	!,				%   must not backtrack
	'list instances'(Term, SoFar, Total).


'list instances'([], Total, Total).
'list instances'(found(Template), SoFar, Total) :-
	list_instances([Template|SoFar], Total).



/*  save_instances/3 and list_instances/3 are used by bag_of/3 and
    set_of/3 when there are free variables (which are collected as
    the Key).  They are not used by either version of findall.
    They are in this module so that they can see the setof_stack/1
    predicate.  Using such a predicate was a new feature in the
    2.3 release, which has the effect that the 'recorded' data base
    is now entirely free to customers.
*/

%   save_instances(+Key, +Template, +Generator)
%   enumerates all provable instances of the Generator and stores
%   the associated Key-Template instances.  None of the arguments
%   ends up changed.

save_instances(Key, Template, Generator) :-
	asserta(setof_stack(_, [])),
	call(Generator),
	asserta(setof_stack(_, -(Key,Template))),
	fail.
save_instances(_, _, _).



%   make_key(+Vars, -Length, -Key)
%   is given a list of variables Vars, and constructs a term which
%   holds all those variables.  If there are up to 250 variables,
%   the result is a one-level term.  If there are between 250 and
%   250*250=62500 variables, the result is a two-level term.  The
%   outer functor, if any, is * /N, and the inner functor is . /M.
%   If there are more than 250*250 variables, this predicate fails.
%   It is up to the caller to report the error.

make_key(Vars, Length, Key) :-
	length(Vars, Length),
	(   Length =<     3 -> make_key_1(Length, Vars, Key)
	;   Length =<   250 -> make_one_level_key(Length, Vars, Key, [])
	;   Length =< 62500 -> make_two_level_key(Length, Vars, Key)
	).

make_key_1(1, [X1],	.(X1)).
make_key_1(2, [X1,X2],	.(X1,X2)).
make_key_1(3, [X1,X2,X3],	.(X1,X2,X3)).

make_one_level_key(Length, Vars, Key, Vars1) :-
	functor(Key, ., Length),
	make_one_level_key(Length, Vars, 1, Key, Vars1).

make_one_level_key(Length, [Var|Vars], N, Key, Vars1) :-
	(   Length > 1 ->
	    arg(N, Key, Var),
	    Length1 is Length-1,
	    N1 is N+1,
	    make_one_level_key(Length1, Vars, N1, Key, Vars1)
	;   arg(N, Key, Var),
	    Vars1 = Vars
	).

make_two_level_key(Length, Vars, Key) :-
	Arity is (Length+249) // 250,
	functor(Key, *, Arity),
	make_two_level_key(Length, Vars, 1, Key).

make_two_level_key(Length, Vars, N, Key) :-
	(   Length > 250 ->
	    make_one_level_key(250, Vars, Arg, Vars1),
	    arg(N, Key, Arg),
	    Length1 is Length-250,
	    N1 is N+1,
	    make_two_level_key(Length1, Vars1, N1, Key)
	;   make_one_level_key(Length, Vars, Arg, []),
	    arg(N, Key, Arg)
	).



%   list_instances(+Key, +Length, -List)
%   pulls all the Key-Template instances out of the data base until
%   it hits the marker, and unifies the result with List.
%   Note that asserting something into the data base and pulling it out
%   again renames all the variables; to counteract this we use replace_
%   key_variables to put the old variables back.  Fortunately if we
%   bind X=Y, the newer variable will be bound to the older, and the
%   original key variables are guaranteed to be older than the new ones.
%   This replacement must be done @i<before> the keysort.

%   The make_variable_global({List}) hack gives us a way to tell
%   whether a variable in a Key-Template instance is new or has
%   already been bound to one of the variables in the original
%   Key.  (This fixes a bug where binding two variables in an
%   instance could force them to be bound in all instances!)  The
%   Length is passed in so that we know whether we have a one-level
%   or a two-level key.

list_instances(Key, Length, List) :-
	make_variable_global({List}),
	(   Length =< 250 ->
	    list_instances_one(Key, Length, [], List)
	;   Arity is (Length+249) // 250,
	    list_instances_two(Key, Arity, [], List)
	).


make_variable_global(_).



%   list_instances_one(+Key, +N, +OldBag, -NewBag)
%   is used when the number of variables in Key is N and is at most 250.
%   That is, when we have a one-level key.  We build the list up in
%   reverse order.  We rely on NewBag being a global unbound variable,
%   and use it as a magic marker to decide which variables to replace.

list_instances_one(Key, N, OldBag, NewBag) :-
	retract(setof_stack(_, Term)),
	!,				%  must not backtrack!
	list_instances_one(Term, Key, N, OldBag, NewBag).

list_instances_one([], _, _, AnsBag, AnsBag).
list_instances_one(NewKey-Term, Key, N, OldBag, NewBag) :-
	replace_key_variables_one(N, Key, NewBag, NewKey),
	list_instances_one(Key, N, [NewKey-Term|OldBag], NewBag).

replace_key_variables_one(N, OldKey, MagicMarker, NewKey) :-
	(   N =:= 0 -> true
	;   M is N-1,
	    arg(N, NewKey, Arg),
	    (   var(Arg), Arg @> MagicMarker ->
		arg(N, OldKey, Arg)
	    ;	true
	    ),
	    replace_key_variables_one(M, OldKey, MagicMarker, NewKey)
	).


%   list_instances_two(+Key, +N, +OldBag, -NewBag)
%   is like list_instances_one/4 except that N is the arity of the
%   Key, and the Key is a two-level key.  This should almost never
%   be used.  Note that NewBag plays two roles!

list_instances_two(Key, N, OldBag, NewBag) :-
	retract(setof_stack(_, Term)),
	!,				%  must not backtrack!
	list_instances_two(Term, Key, N, OldBag, NewBag).

list_instances_two([], _, _, AnsBag, AnsBag).
list_instances_two(NewKey-Term, Key, N, OldBag, NewBag) :-
	replace_key_variables_two(N, Key, NewBag, NewKey),
	list_instances_two(Key, N, [NewKey-Term|OldBag], NewBag).

replace_key_variables_two(N, OldKey, MagicMarker, NewKey) :-
	arg(N, OldKey, OldArg),
	arg(N, NewKey, NewArg),
	functor(NewArg, ., Arity),
	replace_key_variables_one(Arity, OldArg, MagicMarker, NewArg),
	(   N =:= 1 -> true
	;   M is N-1,
	    replace_key_variables_two(M, OldKey, MagicMarker, NewKey)
	).



%   conj_instances(+Key, -Goals)
%   picks up the items stored in the data base and returns them
%   as a conjunction (G1, ..., Gn, true).
%   Note that the free variables are mapped back to themselves.

conj_instances(Key, Goals) :-
	conj_instances(Key, true, Goals).

conj_instances(Key, Goals0, Goals) :-
	retract(setof_stack(_, Term)),
	!,
	conj_instances(Term, Key, Goals0, Goals).

conj_instances([], _, Goals, Goals).
conj_instances(Key-Goal, Key, Goals0, Goals) :-
	conj_instances(Key, (Goal,Goals0), Goals).


%@  @end table

