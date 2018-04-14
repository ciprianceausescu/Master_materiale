/* Copyright (C) 1995, Swedish Institute of Computer Science. */

%   File       : terms.pl
%   Author     : Mats Carlsson
%   Updated    : 25 July 2000
%   Purpose    : Miscellaneous operations on terms

:- module(terms, [
	subsumeschk/2, 
	subsumes/2, 
	variant/2, 
	term_subsumer/3, 
	term_hash/2,
	term_hash/4,
	term_variables/2,
	term_variables_bag/2,
	acyclic_term/1,
	cyclic_term/1,
	% library(order)
	term_order/3,
	% library(occurs)
	contains_term/2,	%   T2 contains term T1
	contains_var/2,		%   T2 contains variable V1
	free_of_term/2,		%   T2 is free of term T1
	free_of_var/2,		%   T2 is free of variable V1
	occurrences_of_term/3,	%   T2 contains N3 instances of term T1
	occurrences_of_var/3,	%   T2 contains N3 instances of var V1
	sub_term/2,		%   T1 is a sub-term of T2 (enumerate T1)
	% library(term_depth)
	depth_bound/2,
	length_bound/2,
	size_bound/2,
	term_depth/2,
	term_size/2,
	% library(same_functor)
	same_functor/2,
	same_functor/3,
	same_functor/4
	]).

:- use_module(library(types), [
	must_be/4,
	illarg/3
	]).
:- use_module(library(avl), [
	empty_avl/1,
	avl_fetch/3,
	avl_store/4
	]).

%@  This library module provides miscellaneous operations on terms.
%@  Exported predicates:
%@  
%@  @table @code


%@  @item subsumeschk(@var{+General}, @var{+Specific})
%@  @PLXindex {subsumeschk/2 (terms)}
%@  is true when @var{Specific} is an instance of @var{General}.  It
%@  does not bind any variables.

subsumeschk(General, _Specific) :-
	var(General), !.
subsumeschk(General, Specific) :-
	nonvar(Specific),
	\+ \+ prolog:'$subsumes'(General, Specific).


%@  @item subsumes(@var{+General}, @var{+Specific})
%@  @PLXindex {subsumes/2 (terms)}
%@  is true when @var{Specific} is an instance of @var{General}.  It will bind
%@  variables in @var{General} (but not those in @var{Specific}) so that @var{General}
%@  becomes identical to @var{Specific}.

subsumes(General, Specific) :-
        subsumeschk(General, Specific),
        General = Specific.


%@  @item variant(@var{+Term}, @var{+Variant})
%@  @PLXindex {variant/2 (terms)}
%@  is true when @var{Term} and @var{Variant} are identical modulo renaming of variables,
%@  provided @var{Term} and @var{Variant} have no variables in common.


variant(Term, Variant) :-
        subsumeschk(Term, Variant),
        subsumeschk(Variant, Term).



%@  @item term_subsumer(@var{+Term1}, @var{+Term2}, @var{-Term})
%@  @PLXindex {term_subsumer/3 (terms)}
%@  binds @var{Term} to a most specific generalisation of @var{Term1} and @var{Term2}.
%@  Using Plotkin's algorithm [Machine Intelligence 5, 1970], extended
%@  by Dan Sahlin to handle cyclic structures.

term_subsumer(Term1, Term2, Subsumer) :-
	cyclic_term(Term1),
	cyclic_term(Term2), !,
	empty_avl(S),
	cyclic_subsumer(Term1, Term2, S, _, S, Subsumer).
term_subsumer(Term1, Term2, Subsumer) :-
	empty_avl(S),
	subsumer(Term1, Term2, S, _, Subsumer).

subsumer(Term1, Term2, S0, S, Term) :-
	(   compound(Term1), compound(Term2),
	    functor(Term1, F, N), functor(Term2, F, N)
	->  functor(Term, F, N),
	    subsumer(N, Term1, Term2, S0, S, Term)
	;   Term1 == Term2 -> S = S0, Term = Term1
	;   avl_fetch(Term1-Term2, S0, V) -> S = S0, Term = V
	;   avl_store(Term1-Term2, S0, Term, S)
	).

subsumer(0, _, _, S, S, _) :- !.
subsumer(N, T1, T2, S0, S, T3) :-
	arg(N, T1, T1x),
	arg(N, T2, T2x),
	arg(N, T3, T3x),
	subsumer(T1x, T2x, S0, S1, T3x),
	M is N-1,
	subsumer(M, T1, T2, S1, S, T3).


cyclic_subsumer(Term1, Term2, S0, S, U, Term) :-
	(   compound(Term1), compound(Term2),
	    functor(Term1, F, N), functor(Term2, F, N) ->
	    (   avl_fetch(Term1-Term2, U, V) -> S = S0, Term = V
	    ;   functor(Term, F, N),
		avl_store(Term1-Term2, U, Term, U1),
	        cyclic_subsumer(N, Term1, Term2, S0, S, U1, Term)
	    )
	;   Term1 == Term2 -> S = S0, Term = Term1
	;   avl_fetch(Term1-Term2, S0, V) -> S = S0, Term = V
	;   avl_store(Term1-Term2, S0, Term, S)
	).

cyclic_subsumer(0, _, _, S, S, _, _) :- !.
cyclic_subsumer(N, T1, T2, S0, S, U, T3) :-
	arg(N, T1, T1x),
	arg(N, T2, T2x),
	arg(N, T3, T3x),
	cyclic_subsumer(T1x, T2x, S0, S1, U, T3x),
	M is N-1,
	cyclic_subsumer(M, T1, T2, S1, S, U, T3).



%@  @item term_hash(@var{+Term}, @var{-Hash})
%@  @PLXindex {term_hash/[2,4] (terms)}
%@  If @var{Term} is ground, an integer hash value corresponding to @var{Term} 
%@  is unified with @var{Hash}.  Otherwise, the goal just succeeds.

term_hash(Term, Value) :-
	(   Modulus = 0x10000000		% 1<<28 for 32-bit arch
    /** ;   Modulus = 0x10000000000000000000	% 1<<56 for 64-bit arch **/
	),
    /** prolog:'$large_data'(0, Modulus, _), !, **/
	prolog:'$term_hash'(Term, -1, Modulus, Value).

%@  @item term_hash(@var{+Term}, @var{+Depth}, @var{+Range}, @var{-Hash})
%@  If @var{Term} is instantiated to the given @var{Depth}, an integer hash value in
%@  the range @var{[0,Range)} corresponding to @var{Term} is unified with @var{Hash}.
%@  Otherwise, the goal just succeeds.

term_hash(Term, Depth, Range, Value) :-
	Goal = term_hash(Term,Depth,Range,Value),
	Type = integer(between(-1,UB)),
	UB is (1<<31)-1,	% also on 64-bit machines
	must_be(Depth, Type, Goal, 2),
	must_be(Range, Type, Goal, 3),
	prolog:'$term_hash'(Term, Depth, Range, Value).



%@  @item term_variables(@var{+Term}, @var{-Variables})
%@  @PLXindex {term_variables/2 (terms)}
%@  True if @var{Variables} is the set of variables occurring in @var{Term}.

term_variables(Term, Variables) :-
	prolog:term_variables(Term, Variables).

%@  @item term_variables_bag(@var{+Term}, @var{-Variables})
%@  @PLXindex {term_variables_bag/2 (terms)}
%@  True if @var{Variables} is the list of variables occurring in @var{Term},
%@  in first occurrence order.

term_variables_bag(Term, Variables) :-
	prolog:'$term_variables'(Term, Variables).


%@  @item acyclic_term(@var{+X})
%@  @PLXindex {acyclic_term/1 (terms)}
%@  True if @var{X} is finite (acyclic).  Runs in linear time.

acyclic_term(X) :-
	prolog:'$acyclic'(X).



%@  @item cyclic_term(@var{+X})
%@  @PLXindex {cyclic_term/1 (terms)}
%@  True if @var{X} is infinite (cyclic).  Runs in linear time.

cyclic_term(X) :-
	\+prolog:'$acyclic'(X).

%@  @item term_order(@var{+X}, @var{+Y}, @var{-R})
%@  @PLXindex {term_order/3 (terms)}
%@  is true when @var{X} and @var{Y} are arbitrary terms, and @var{R} is @code{<}, @code{=}, or @code{>} according
%@  as @var{X @@< Y}, @var{X == Y}, or @var{X @@> Y}.  This is the same as @code{compare/3}, except
%@  for the argument order.

term_order(X, Y, R) :-
	compare(R, X, Y).



%   Module : occurs
%   Author : Richard A. O'Keefe
%   Updated: 10 Apr 1990
%   Purpose: checking whether a term does/does not contain a term/variable

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- mode
	contains_term(+, +),		%   Kernel x Term ->
	contains_var(+, +),		%   Kernel x Term ->
	free_of_term(+, +),		%   Kernel x Term ->
	    free_of_term(+, +, +),	%   Tally x Term x Kernel ->
	free_of_var(+, +),		%   Kernel x Term ->
	    free_of_var(+, +, +),	%   Tally x Term x Kernel ->
	occurrences_of_term(+, +, ?),	%   Kernel x Term -> Tally
	    occurrences_of_term(+, +, +, -),
		occurrences_of_term(+, +, +, +, -),
	occurrences_of_var(+, +, ?),	%   Kernel x Term -> Tally
	    occurrences_of_var(+, +, +, -),
		occurrences_of_var(+, +, +, +, -),
	sub_term(?, +),			%   Kernel x Term
	    sub_term(+, +, ?).		%   Tally x Term x Kernel




%   These relations were available in the public domain library with
%   different names and inconsistent argument orders.  The names are
%   different so that "contains", "freeof" and so forth can be used
%   by your own code.  free_of_var is used by the "unify" package.
%   The _term predicates check for a sub-term that unifies with the
%   Kernel argument, the _var predicates for one identical to it.
%   In release 2.0, the *_var predicates were modified to continue
%   using the identity (==) rather than unifiability (=) test, but
%   to cease insisting that the Kernel should be a variable.  When
%   the Kernel is a variable, they continue to work as before.



%@  @item contains_term(@var{+Kernel}, @var{+Expression})
%@  @PLXindex {contains_term/2 (terms)}
%@  is true when the given @var{Kernel} occurs somewhere in the @var{Expression}.
%@  It can only be used as a test; to generate sub-terms use @code{sub_term/2}.

contains_term(Kernel, Expression) :-
	\+ free_of_term(Kernel, Expression).


%@  @item free_of_term(@var{+Kernel}, @var{+Expression})
%@  @PLXindex {free_of_term/2 (terms)}
%@  is true when the given @var{Kernel} does not occur anywhere in the
%@  @var{Expression}.  NB: if the @var{Expression} contains an unbound variable,
%@  this must fail, as the @var{Kernel} might occur there.  Since there are
%@  infinitely many @var{Kernels} not contained in any @var{Expression}, and also
%@  infinitely many @var{Expressions} not containing any @var{Kernel}, it doesn't
%@  make sense to use this except as a test.

free_of_term(Kernel, Kernel) :- !,
	fail.
free_of_term(Kernel, Expression) :-
	nonvar(Expression),
	functor(Expression, _, Arity),
	free_of_term(Arity, Expression, Kernel).

free_of_term(0, _, _) :- !.
free_of_term(N, Expression, Kernel) :-
	arg(N, Expression, Argument),
	free_of_term(Kernel, Argument),
	M is N-1,
	free_of_term(M, Expression, Kernel).



%@  @item occurrences_of_term(@var{+Kernel}, @var{+Expression}, @var{-Tally})
%@  @PLXindex {occurrences_of_term/3 (terms)}
%@  is true when the given @var{Kernel} occurs exactly @var{Tally} times in
%@  @var{Expression}.  It can only be used to calculate or test @var{Tally};
%@  to enumerate @var{Kernels} you'll have to use @code{sub_term/2} and then
%@  test them with this routine.  If you just want to find out
%@  whether @var{Kernel} occurs in @var{Expression} or not, use @code{contains_term/2}
%@  or @code{free_of_term/2}.

occurrences_of_term(Kernel, Expression, Occurrences) :-
	occurrences_of_term(Expression, Kernel, 0, Tally),
	Occurrences = Tally.

occurrences_of_term(Kernel, Kernel, SoFar, Tally) :- !,
	Tally is SoFar+1.
occurrences_of_term(Expression, Kernel, SoFar, Tally) :-
	nonvar(Expression),
	functor(Expression, _, Arity),
	occurrences_of_term(Arity, Expression, Kernel, SoFar, Tally).

occurrences_of_term(0, _, _, Tally, Tally) :- !.
occurrences_of_term(N, Expression, Kernel, SoFar, Tally) :-
	arg(N, Expression, Argument),
	occurrences_of_term(Argument, Kernel, SoFar, Accum),
	M is N-1,
	occurrences_of_term(M, Expression, Kernel, Accum, Tally).



%@  @item contains_var(@var{+Variable}, @var{+Term})
%@  @PLXindex {contains_var/2 (terms)}
%@  is true when the given @var{Term} contains at least one sub-term which
%@  is identical to the given @var{Variable}.  We use @code{==} to check for
%@  the variable (@code{contains_term/2} uses @code{=}) so it can be used to check
%@  for arbitrary terms, not just variables.

contains_var(Variable, Term) :-
	\+ free_of_var(Variable, Term).


%@  @item free_of_var(@var{+Variable}, @var{+Term})
%@  @PLXindex {free_of_var/2 (terms)}
%@  is true when the given @var{Term} contains no sub-term identical to the
%@  given @var{Variable} (which may actually be any term, not just a var).
%@  For variables, this is precisely the "occurs check" which is
%@  needed for sound unification.

free_of_var(Variable, Term) :-
	Term == Variable,
	!,
	fail.
free_of_var(Variable, Term) :-
	compound(Term),
	!,
	functor(Term, _, Arity),
	free_of_var(Arity, Term, Variable).
free_of_var(_, _).

free_of_var(1, Term, Variable) :- !,
	arg(1, Term, Argument),
	free_of_var(Variable, Argument).
free_of_var(N, Term, Variable) :-
	arg(N, Term, Argument),
	free_of_var(Variable, Argument),
	M is N-1, !,
	free_of_var(M, Term, Variable).



%@  @item occurrences_of_var(@var{+Term}, @var{+Variable}, @var{-Tally})
%@  @PLXindex {occurrences_of_var/3 (terms)}
%@  is true when the given @var{Variable} occurs exactly @var{Tally} times in
%@  @var{Term}.  It can only be used to calculate or test @var{Tally};
%@  to enumerate Variables you'll have to use @code{sub_term/2} and then
%@  test them with this routine.  If you just want to find out
%@  whether @var{Variable} occurs in @var{Term} or not, use @code{contains_var/2}
%@  or @code{free_of_var/2}.

occurrences_of_var(Variable, Term, Occurrences) :-
	occurrences_of_var(Term, Variable, 0, Tally),
	Occurrences = Tally.

occurrences_of_var(Term, Variable, SoFar, Tally) :-
	Term == Variable,
	!,
	Tally is SoFar+1.
occurrences_of_var(Term, Variable, SoFar, Tally) :-
	compound(Term),
	!,
	functor(Term, _, Arity),
	occurrences_of_var(Arity, Term, Variable, SoFar, Tally).
occurrences_of_var(_, _, Tally, Tally).


occurrences_of_var(1, Term, Variable, SoFar, Tally) :- !,
	arg(1, Term, Argument),
	occurrences_of_var(Argument, Variable, SoFar, Tally).
occurrences_of_var(N, Term, Variable, SoFar, Tally) :-
	arg(N, Term, Argument),
	occurrences_of_var(Argument, Variable, SoFar, Accum),
	M is N-1,
	occurrences_of_var(M, Term, Variable, Accum, Tally).



%@  @item sub_term(@var{?Kernel}, @var{+Term})
%@  @PLXindex {sub_term/2 (terms)}
%@  is true when @var{Kernel} is a sub-term of @var{Term}.  It enumerates the
%@  sub-terms of @var{Term} in an arbitrary order.  Well, it is defined
%@  that a sub-term of @var{Term} will be enumerated before its own
%@  sub-terms are (but of course some of those sub-terms might be
%@  elsewhere in @var{Term} as well).

sub_term(Term, Term).
sub_term(SubTerm, Term) :-
	nonvar(Term),
	functor(Term, _, N),
	sub_term(N, Term, SubTerm).

sub_term(N, Term, SubTerm) :-
	arg(N, Term, Arg),
	sub_term(SubTerm, Arg).
sub_term(N, Term, SubTerm) :-
	N > 1,
	M is N-1,
	sub_term(M, Term, SubTerm).



%   Package: term_depth
%   Author : Richard A. O'Keefe
%   Updated: 04/15/99
%   Purpose: Find or check the depth of a term.

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

/*  Many resolution-based theorem provers impose a Depth Bound on the
    terms they create.  Not the least of the reasons for this is to
    stop infinite loops.  This module exports five predicates:

	term_depth(Term, Depth)
	depth_bound(Term, Bound)

	term_size(Term, Size)
	size_bound(Term, Bound)

	length_bound(List, Bound)

    term_depth calculates the depth of the term, using the definition
	term_depth(Var) = 0
	term_depth(Const) = 0
	term_depth(F(T1,...,Tn)) = 1+max(term_depth(T1),...,term_depth(Tn))

    Mostly, we couldn't care less what the depth of a term is, provided
    it is below some fixed bound.  depth_bound checks that the depth of
    the given term is below the bound (which is assumed to be an integer
    >= 1), without ever finding out what the depth actually is.

    term_size calculates the size of the term, defined to be the number
    of constant and function symbols in it.  (Note that this is a lower
    bound on the size of any term instantiated from it, and that
    instantiating any variable to a non-variable must increase the size.
    This latter property is why we don't count variables as 1.)

	term_size(Var) = 0
	term_size(Const) = 1
	term_size(F(T1,...,Tn)) = 1+term_size(T1)+...+term_size(Tn).

    size_bound(Term, Bound) is true if the size of Term is less than
    or equal to the Bound (assumed to be an integer >= 0).  Note that
    size_bound/2 and depth_bound/2 will always terminate.

    length_bound(List, Bound) is true when List is a list having at
    most Bound elements.  Bound must be instantiated.  If List ends
    with a variable, it will be instantiated to successively longer
    proper lists, up to the length permitted by the Bound.  This was
    added when I noticed that the depth of a list of constants is
    its length, and we already have a length/2, but did not have a
    length_bound.

    In the DEC-10 Prolog library, this was depth.pl, defining
    depth_of_term/2 and depth_bound/2.
*/

:- mode
	depth_bound(+, +),
	    depth_bound(+, +, +),
	length_bound/2,
	size_bound(+, +),
	    size_bound(+, +, -),
		size_bound(+, +, -),
	term_depth(+, ?),
	    term_depth(+, +, +, -),
	term_size(+, ?),
	    term_size(+, +, +, ?).



%@  @item depth_bound(@var{+Term}, @var{+Bound})
%@  @PLXindex {depth_bound/2 (terms)}
%@  is true when the term depth of @var{Term} is no greater than @var{Bound},
%@  that is, when constructor functions are nested no more than @var{Bound} deep.
%@  Later variable bindings may invalidate this bound.  To find the
%@  (current) depth, use @code{term_depth/2}.

depth_bound(Compound, Bound) :-
	nonvar(Compound),
	functor(Compound, _, Arity),
	Arity > 0,
	!,
	Bound > 0,		% this is the test!
	Limit is Bound-1,
	depth_bound(Arity, Compound, Limit).
depth_bound(_, _).


depth_bound(N, Compound, Limit) :-
    (	N =:= 0 -> true
    ;	arg(N, Compound, Arg),
	depth_bound(Arg, Limit),
	M is N-1,
	depth_bound(M, Compound, Limit)
    ).



%@  @item length_bound(@var{?List}, @var{+Bound})
%@  @PLXindex {length_bound/2 (terms)}
%@  is true when the length of @var{List} is no greater than @var{Bound}.  It can be
%@  used to enumerate Lists up to the bound.  
%   See also bnd/[2,3,4] in
%   library(more_lists).  Its companion is length/2, which is built in.

length_bound([], Bound) :-
	Bound >= 0.
length_bound([_|List], Bound) :-
	Bound > 0,
	Limit is Bound-1,
	length_bound(List, Limit).



%@  @item size_bound(@var{+Term}, @var{+Bound})
%@  @PLXindex {size_bound/2 (terms)}
%@  is true when the number of constant and function symbols in @var{Term} is
%@  (currently) at most @var{Bound}.  If @var{Term} is non-ground, later variable
%@  bindings may invalidate this bound.  To find the (current) size, use
%@  @code{term_size/2}.

size_bound(Term, Bound) :-
	size_bound(Term, Bound, _).


size_bound(Term, Bound, Left) :-
    (	var(Term) -> Left = Bound
    ;/* nonvar(Term) */
	functor(Term, _, Arity),
	Bound > 0,		% this is the test!
	Limit is Bound-1,
	size_bound(Arity, Term, Limit, Left)
    ).


size_bound(N, Term, Limit, Left) :-
    (	N =:= 0 ->
	Left is Limit
    ;	arg(N, Term, Arg),
	size_bound(Arg, Limit, Limit1),
	M is N-1,
	size_bound(M, Term, Limit1, Left)
    ).



%@  @item term_depth(@var{+Term}, @var{-Depth})
%@  @PLXindex {term_depth/2 (terms)}
%@  @example
%@  calculates the Depth of a Term, using the definition
%@      term_depth(Var) = 0
%@      term_depth(Const) = 0
%@      term_depth(F(T1,...,Tn)) = 1+max(term_depth(T1),...,term_depth(Tn))
%@  @end example

term_depth(Compound, Depth) :-
	nonvar(Compound),
	functor(Compound, _, Arity),
	Arity > 0,
	!,
	term_depth(Arity, Compound, 0, ArgDepth),
	Depth is ArgDepth+1.
term_depth(_, 0).


term_depth(N, Compound, SoFar, Depth) :-
    (	N =:= 0 ->
	Depth is SoFar
    ;   arg(N, Compound, Arg),
	term_depth(Arg, ArgDepth),
	(   ArgDepth > SoFar -> Accum is ArgDepth
	;   /* otherwise */     Accum is SoFar
	),
	M is N-1,
	term_depth(M, Compound, Accum, Depth)
    ).



%@  @item term_size(@var{+Term}, @var{-Size})
%@  @PLXindex {term_size/2 (terms)}
%@  calculates the @var{Size} of a @var{Term}, defined to be the number of constant and
%@  function symbol occurrences in it.

term_size(Term, Size) :-
    (	var(Term) -> Size = 0
    ;/* nonvar(Term) */
	functor(Term, _, Arity),
	term_size(Arity, Term, 1, Size)
    ).


term_size(N, NonVar, SoFar, Size) :-
    (	N =:= 0 ->
	Size is SoFar
    ;   arg(N, NonVar, Arg),
	term_size(Arg, ArgSize),
	Accum is SoFar+ArgSize,
	M is N-1,
	term_size(M, NonVar, Accum, Size)
    ).

%   Module : same_functor
%   Author : Richard A. O'Keefe
%   Updated: 29 Aug 1989
%   Defines: same_functor/[2,3,4]

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- mode
	same_functor(?, ?),
	same_functor(?, ?, ?),
	same_functor(?, ?, ?, ?).



%@  @item same_functor(@var{?T1}, @var{?T2})
%@  @PLXindex {same_functor/[2,3,4] (terms)}
%@  is true when @var{T1} and @var{T2} have the same principal functor.  If one of
%@  the terms is a variable, it will be instantiated to a new term
%@  with the same principal functor as the other term (which should be
%@  instantiated) and with arguments being new distinct variables.  If
%@  both terms are variables, an error is reported.

same_functor(T1, T2) :-
	(   nonvar(T1) ->
	    functor(T1, F, N),
	    functor(T2, F, N)
	;   nonvar(T2) ->
	    functor(T2, F, N),
	    functor(T1, F, N)
	;   % var(T1), var(T2)
	    illarg(var, same_functor(T1,T2), 0)
	).


%@  @item same_functor(@var{?T1}, @var{?T2}, @var{?N})
%@  is true when @var{T1} and @var{T2} have the same principal functor, and their
%@  common arity is @var{N}. Like @code{same_functor/3}, at least one of @var{T1} and @var{T2}
%@  must be bound, or an error will be reported.  

same_functor(T1, T2, N) :-
	(   nonvar(T1) ->
	    functor(T1, F, N),
	    functor(T2, F, N)
	;   nonvar(T2) ->
	    functor(T2, F, N),
	    functor(T1, F, N)
	;   % var(T1), var(T2)
	    illarg(var, same_functor(T1,T2,N), 0)
	).



%@  @item same_functor(@var{?T1}, @var{?T2}, @var{?F}, @var{?N})
%@  is true when @var{T1} and @var{T2} have the same principal functor, and their
%@  common functor is @var{F/N}. Given @var{T1} (or @var{T2}) the remaining arguments
%@  can be computed.  Given @var{F} and @var{N}, the remaining arguments can be
%@  computed.  If too many arguments are unbound, an error is reported.

same_functor(T1, T2, F, N) :-
	(   nonvar(T1) ->
	    functor(T1, F, N),
	    functor(T2, F, N)
	;   nonvar(T2) ->
	    functor(T2, F, N),
	    functor(T1, F, N)
	;   number(F) ->
	    N = 0, T1 = F, T2 = F
	;   nonvar(F), nonvar(N) ->
	    functor(T1, F, N),
	    functor(T2, F, N)
	;   %  var(T1), var(T2), (var(F) ; var(N)), \+number(F).
	    illarg(var, same_functor(T1,T2,F,N), 0)
	).

%@  @end table
