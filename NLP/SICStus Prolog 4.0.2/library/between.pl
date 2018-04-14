%   Package: between
%   Author : Richard A. O'Keefe
%   Updated: 02 Nov 1988
%   Purpose: Generate integers.

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(between, [
	between/3,		%   Lower x Upper x Bounded
	gen_int/1,		%   Integer
	gen_nat/1,		%   Natural
	numlist/3,		%   Lower x Upper -> List
	repeat/1		%   Natural
   ]).
:- use_module(library(types), [
	must_be/4
   ]).
:- mode
	between(?, ?, ?),
	    between1(+, +, -),
	gen_int(?),
	gen_nat(?),
	gen_nat(+, -),
	numlist(?, ?, ?),
	    anchor(+, +, -, -),
		anchor(+, +, -),
	    numlist1(+, +, ?),
		numlist2(+, +, ?),
	repeat(+).

%@  This library module provides some means of generating integers.
%@  Exported predicates:
%@  
%@  @table @code

%@  @item between(@var{+Lower}, @var{+Upper}, @var{-Number})
%@  @PLXindex {between/3 (between)}
%@  is true when @var{Lower}, @var{Upper}, and @var{Number} are integers,
%@  and @var{Lower =< Number =< Upper}.  If @var{Lower} and @var{Upper} are given,
%@  @var{Number} can be tested or enumerated.  If either @var{Lower} or @var{Upper}
%@  is absent, there is not enough information to find it, and an
%@  error will be reported.

between(Lower, Upper, Point) :-
	integer(Lower),
	integer(Upper),
	(   integer(Point), !,		%  These cuts must be cuts;
	    Lower =< Point, Point =< Upper
	;   var(Point), !,		%  they can't be arrows.
	    Lower =< Upper,
	    between1(Lower, Upper, Point)
	).
between(Lower, Upper, Point) :-
	Goal = between(Lower,Upper,Point),
	must_be(Lower, integer, Goal, 1),
	must_be(Upper, integer, Goal, 2),
	must_be(Point, integer, Goal, 3).



%%  between1(Lower, Upper, Point)
%   enumerates values of Point satisfying Lower =< Point =< Upper,
%   where it is already known that Lower =< Upper and Point was a
%   variable.  A purer version of this is left as a comment.

between1(L, L, L) :- !.
between1(L, _, L).		% between1(L, U, L) :- L =< U.
between1(L, U, N) :-		% between1(L, U, N) :- L < U,
	M is L+1,		%	M is L+1,
	between1(M, U, N).	%	between1(M, U, N).



%@  @item gen_nat(@var{?N})
%@  @PLXindex {gen_nat/1 (between)}
%@  is true when @var{N} is a natural number.  If @var{N} is a variable, it will
%@  enumerate the natural numbers 0,1,2,... and of course not terminate.
%@  It is not meant to be applied to anything but integers and variables.

gen_nat(N) :-			% gen-erate nat-ural
	(   integer(N) ->	% if we aren't to generate it
	    N >= 0		% test that it is not negative
	;   var(N) ->		% if we are to generate it,
	    gen_nat(0, N)	% do so counting up from 0.
	;   must_be(N, integer, gen_nat(N), 1)
	).
 
 
gen_nat(L, L).
gen_nat(L, N) :-		% generate natural > L
	M is L+1,
	gen_nat(M, N).		% generate natural >= M
 
 

%@  @item gen_int(@var{?I})
%@  @PLXindex {gen_int/1 (between)}
%@  is true when I is an integer.  If I is a variable, it will
%@  enumerate the integers in the order 0, 1, -1, 2, -2, 3, -3, &c.
%@  Of course this sequence has no end.
%@  It is not meant to be applied to anything but integers and variables.

gen_int(I) :-			% gen-erate int-eger
	(   integer(I) ->	% if we aren't to generate it
	    true		% just succeed.
	;   var(I) ->		% if we are to generate it,
	    gen_int(0, I)	% do so starting from 0.
	;   must_be(I, integer, gen_int(I), 1)
	).

gen_int(L, L).
gen_int(L, N) :-
	(   L > 0 -> M is -L	% 1-> -1, 2-> -2, &c
	;   M is 1-L		% 0-> 1, -1-> 2, &c
	),
	gen_int(M, N).


%@  @item repeat(@var{+N})
%@  @PLXindex {repeat/1 (between)}
%@  (where @var{N} is a non-negative integer) succeeds exactly @var{N} times.
%@  You can only understand it procedurally, and really it is only
%@  included for compatibility with some other Prologs.

repeat(N) :-
	(   integer(N) ->
	    N >= 1,
	    repeat1(N)
	;   must_be(N, integer, repeat(N), 1)
	).

repeat1(1) :- !.		% the structure of this
repeat1(_).			% predicate is parallel to
repeat1(N) :-			% the structure of
	M is N-1,		% between1/3 above.
	repeat1(M).


%@  @item numlist(@var{?Lower}, @var{?Upper}, @var{?List})
%@  @PLXindex {numlist/3 (between)}
%@  is true when @var{List} is @var{[Lower, ..., Upper]}, @var{Lower} and @var{Upper} integers.
%@  For example, @code{numlist(1, 3, L)} binds @code{L = [1,2,3]}.
%@  This is not yet as general as it ought to be: if @var{Lower} and @var{Upper} are
%@  not both integers, @var{List} must be proper, and if @var{Lower} and @var{Upper} are
%@  both variables, at least one element of List must be an integer.
%@  If @var{Lower = Upper+1}, @code{numlist(@var{Lower}, @var{Upper}, [])} is true.

numlist(Lower, Upper, List) :-
	integer(Upper),
	integer(Lower),
	!,
	numlist1(Lower, Upper, List).
numlist(Lower, Upper, List) :-
	var(Upper),			% when Lower and Upper are variables,
	var(Lower),			% all information must come from List.
	anchor(List, 0, L, U),		% List contains an integer
	!,
	Lower = L,
	Upper = U,
	numlist1(L, U, List).
numlist(Lower, Upper, List) :-		% if Lower or Upper known, and
	anchor(List, 0, Length),	% List is proper, that's enough.
	(   integer(Lower), !, Upper is Lower+Length-1
	;   integer(Upper), !, Lower is Upper-Length+1
	),
	numlist1(Lower, Upper, List).
numlist(Lower, Upper, List) :-
	Goal = numlist(Lower,Upper,List),
	must_be(Lower, integer, Goal, 1),
	must_be(Upper, integer, Goal, 2),
	fail.			% no suitable type name for List.


%%  anchor(+List, +Offset, -Lower, ?Upper)
%   succeeds when List is a proper list of length Upper-Lower+1 all of
%   whose elements are integers or variables, containing at least one
%   integer.  The Lower value is obtained from the first integer element,
%   and the Upper value from the length.

anchor(-, _, _, _) :- !, fail.		% reject variables
anchor([Integer|Rest], Offset, Lower, Upper) :-
	integer(Integer),
	!,
	Lower is Integer-Offset,
	anchor(Rest, Integer, Upper).
anchor([_|Rest], Offset, Lower, Upper) :-
	Next is Offset+1,
	anchor(Rest, Next, Lower, Upper).

anchor(-, _, _) :- !, fail.		% reject variables
anchor([], Upper, Upper).
anchor([_|Rest], M, Upper) :-
	N is M+1,
	anchor(Rest, N, Upper).


%%  numlist1(Lower, Upper, List)
%   is called when Lower and Upper are known to be integers, and we want
%   to unify List with [Lower,...,Upper], but we do not know whether the
%   Lower and Upper values are in order or not.  Note that if Upper is
%   less than Lower-1 there is no solution for List.

numlist1(Lower, Upper, List) :-
	(   Lower =< Upper -> numlist2(Lower, Upper, List)
	;   Lower =:= Upper+1 -> List = []
	).


%%  numlist2(Lower, Upper, List)
%   is called when Lower and Upper are known to be integers
%   with Lower =< Upper, and we want to unify List with [Lower,...,Upper].

numlist2(Lower, Lower, List) :- !,
	List = [Lower].
numlist2(Lower, Upper, [Lower|Rest]) :-
	Next is Lower+1,
	numlist2(Next, Upper, Rest).

%@  @end table
 
