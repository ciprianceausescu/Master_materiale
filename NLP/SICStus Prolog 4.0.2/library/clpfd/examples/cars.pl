/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Car Sequencing Problem
 * Author    : Mats Carlsson

 TODO: In any column of OptionsVars, the longest stretch of zeros is:

		n - (k + (q-p)*max(0,ceiling(k/p)-2)) =
		n - (k + (q-p)*max(0,(k-p-1)//p))

 where:

 n = #cars
 k = demand for option
 p/q = capacity of option
 
 */

:- module(cars, [cars/2]).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(avl)).
:- use_module(library(clpfd)).
% ?- bench([cars(minslack,tiny),cars(minslack,'4/72'),cars(minslack,'16/81'),cars(minslack,'41/66'),cars(minslack,'26/82')]).

:- discontiguous
	problem/4,
	capacities/2,
	sizes/2,
	class/4.
:- dynamic
	problem/4,
	capacities/2,
	sizes/2,
	class/4.

%---------------------------------
% Problem 4/72  (Regin & Puget #1)
%---------------------------------

cars(ff, Key) :-
	system(Key, Vars, _, _),
	labeling([ff], Vars),
	writeq(Vars),
	nl.
cars(minslack, Key) :-
	system(Key, Vars, OptionsVarsT, Ks),
	problem(Key, NbCars, _, _),
	capacities(Key, Ps),
	sizes(Key, Qs),
	options_by_slack(Ps, Qs, Ks, NbCars, 0, Options1),
	keysort(Options1, Options2),
	keys_and_values(Options2, _, Options3),
	label_options(Options3, OptionsVarsT),
	writeq(Vars),
	nl.

options_by_slack([], [], [], _, _, []).
options_by_slack([P|Ps], [Q|Qs], [K|Ks], NbCars, I, [Slack-J|Options]) :-
	J is I+1,
	slack_length(P, Q, K, NbCars, Slack0),
	Slack is Slack0 - Q,
	options_by_slack(Ps, Qs, Ks, NbCars, J, Options).

slack_length(P, Q, K, N, L) :-
	L is N - (K + (Q-P)*max((K-P-1)//P,0)) + 1.
	
label_options([], _).
label_options([Option|Options], OVT) :-
	nth1(Option, OVT, L1),
	labeling([down], L1),
	label_options(Options, OVT).

system(Key, ClassVars, OptionVarsT, DemandsPerOption) :-
	problem(Key, NbCars, NbOptions, NbClasses),
	capacities(Key, Capacities),
	sizes(Key, Sizes),
	NbCl1 is NbClasses-1,
	length(ClassVars, NbCars),
	domain(ClassVars, 0, NbCl1),
	findall(Demand, class(Key,_,Demand,_), Demands1),
	findall(Row, class(Key,_,_,Row), Rows),
	sumlist(Demands1, NbCars), % sanity check
	demand_keys(Demands1, 0, Demands2, []),
	global_cardinality(ClassVars, Demands2),
	transpose(Rows, Columns),
	scalar_products(Columns, DemandsPerOption, Demands1),
	findall([Class|Row], class(Key,Class,_,Row), ClassesRows),
	option_vars(ClassVars, ClassOptionVars, OptionVars, NbOptions),
	table(ClassOptionVars, ClassesRows),
	transpose(OptionVars, OptionVarsT),
	capacity_constraints(OptionVarsT, Capacities, Sizes, DemandsPerOption).

scalar_products([], [], _).
scalar_products([Col|Cols], [D|Ds], Vec) :-
	scalar_product(Vec, Col, #=, D),
	scalar_products(Cols, Ds, Vec).

demand_keys([], _) --> [].
demand_keys([D|Ds], I) --> [I-D],
	{J is I+1},
	demand_keys(Ds, J).

option_vars([], [], [], _).
option_vars([C|Cs], [[C|O]|COs], [O|Os], NbOptions) :-
	length(O, NbOptions),
	domain(O, 0, 1),
	option_vars(Cs, COs, Os, NbOptions).

capacity_constraints([], [], [], []).
capacity_constraints([Use|Uses], [Cap|Caps], [Size|Sizes], [Demand|Demands]) :-
	capacity(Use, Cap, Size, Demand),
	capacity_constraints(Uses, Caps, Sizes, Demands).


% Capacity constraints in terms of finite automata.
% capacity(Vars, P, Q, K).
capacity(Vars, 1, 2, K) :- !,
	ac_automaton(Vars, K, s0,
		     [arc(s0,0,s0),
		      arc(s0,1,s1),
				%
		      arc(s1,0,s0)
		     ]).
capacity(Vars, 1, 3, K) :- !,
	ac_automaton(Vars, K, s0,
		  [arc(s0,0,s0),
		   arc(s0,1,s1),
		   %
		   arc(s1,0,s0),
		   arc(s1,1,s2),
		   %
		   arc(s2,0,s1)
		  ]).
capacity(Vars, 2, 3, K) :- !,
	ac_automaton(Vars, K, s0,
		  [arc(s0,0,s0),
		   arc(s0,1,s1),
		   %
		   arc(s1,0,s0),
		   arc(s1,1,s2),
		   %
		   arc(s2,0,s0/*here*/)
		  ]).
capacity(Vars, 1, 5, K) :- !,
	ac_automaton(Vars, K, s0,
		  [arc(s0,0,s0),
		   arc(s0,1,s1),
		   %
		   arc(s1,0,s0),
		   arc(s1,1,s2),
		   %
		   arc(s2,0,s1),
		   arc(s2,1,s3),
		   %
		   arc(s3,0,s2),
		   arc(s3,1,s4),
		   %
		   arc(s4,0,s3)
		  ]).
capacity(Vars, 2, 5, K) :- !,
	ac_automaton(Vars, K, s0000,
		  [arc(s0000,0,s0000),
		   arc(s0000,1,s0001),
		   %
		   arc(s0001,0,s0010),
		   arc(s0001,1,s0011),
		   % 
		   arc(s0010,0,s0100),
		   arc(s0010,1,s0101),
		   % 
		   arc(s0011,0,s0110),
		   % 
		   arc(s0100,0,s0000),
		   arc(s0100,1,s1001),
		   % 
		   arc(s0101,0,s1010),
		   % 
		   arc(s0110,0,s1100),
		   % 
		   arc(s1001,0,s0010),
		   % 
		   arc(s1010,0,s0100),
		   % 
		   arc(s1100,0,s0000)
		  ]).

ac_automaton(Vars, K, Start, Arcs1) :-
	Start0 =.. [Start,0],
	ac_closure([Start0], [Start0], Closure, K, Arcs1, Arcs2, []),
	ac_map(Closure, 0, L1, K, EndList),
	ord_list_to_avl(L1, Map),
	avl_fetch(Start0, Map, StartInt),
	list_to_fdset(EndList, EndSet),
	EndInt in_set EndSet,
	ac_thread(Vars, StartInt, EndInt, Tuples, []),
	ac_map_arcs(Arcs2, Table, Map),
	table(Tuples, Table).

ac_closure([], Closure, Closure, _, _) --> [].
ac_closure([FromI|L1], Sofar1, Closure, K, Arcs) -->
	ac_arcs(Arcs, FromI, K, Incr),
	{sort(Incr, Incr1)},
	{ord_union(Sofar1, Incr1, Sofar2, L2)},
	{append(L1, L2, L3)},
	ac_closure(L3, Sofar2, Closure, K, Arcs).

ac_arcs([], _, _, []) --> [].
ac_arcs([arc(From,Via,To)|Arcs], FromI, K, [ToJ|Incr]) -->
	{FromI =.. [From,I]},
	{J is I+Via},
	{J =< K},
	{ToJ =.. [To,J]}, !,
						[arc(FromI,Via,ToJ)],
	ac_arcs(Arcs, FromI, K, Incr).
ac_arcs([_|Arcs], FromI, K, Incr) -->
	ac_arcs(Arcs, FromI, K, Incr).
	
ac_thread([], S1, S1) --> [].
ac_thread([Sig|Sigs], S1, S3) --> [[S1,Sig,S2]],
	ac_thread(Sigs, S2, S3).
	
ac_map([], _, [], _, []).
ac_map([X|Xs], I, [X-J|XJs], K, Set1) :-
	J is I+1,
	(   arg(1, X, K)
	->  Set1 = [J|Set2]
	;   Set1 = Set2
	),
	ac_map(Xs, J, XJs, K, Set2).

ac_map_arcs([], [], _).
ac_map_arcs([arc(From1,Via,To1)|Arcs1], [[From2,Via,To2]|Arcs2], Map) :-
	avl_fetch(From1, Map, From2),
	avl_fetch(To1, Map, To2),
	ac_map_arcs(Arcs1, Arcs2, Map).


% problem(ID, #cars, #options, #classes).
% capacities(ID, [for each option, the maximum number of cars with that option in a block]).
% sizes(ID, [for each option, the block size to which the maximum number refers]).
% class(ID, class, #cars, [for each option, whether or not this class requires it (1 or 0)]).

problem(tiny, 10, 5, 6).
capacities(tiny, [1,2,1,2,1]).
sizes(tiny, [2,3,3,5,5]).
class(tiny, 0, 1, [1,0,1,1,0]).
class(tiny, 1, 1, [0,0,0,1,0]).
class(tiny, 2, 2, [0,1,0,0,1]).
class(tiny, 3, 2, [0,1,0,1,0]).
class(tiny, 4, 2, [1,0,1,0,0]).
class(tiny, 5, 2, [1,1,0,0,0]).

problem('4/72', 100, 5, 22).
capacities('4/72', [1,2,1,2,1]).
sizes('4/72', [2,3,3,5,5]).
class('4/72', 0, 6, [1,0,0,1,0]).
class('4/72', 1, 10,[1,1,1,0,0]).
class('4/72', 2, 2, [1,1,0,0,1]).
class('4/72', 3, 2, [0,1,1,0,0]).
class('4/72', 4, 8, [0,0,0,1,0]).
class('4/72', 5, 15,[0,1,0,0,0]).
class('4/72', 6, 1, [0,1,1,1,0]).
class('4/72', 7, 5, [0,0,1,1,0]).
class('4/72', 8, 2, [1,0,1,1,0]).
class('4/72', 9, 3, [0,0,1,0,0]).
class('4/72', 10, 2, [1,0,1,0,0]).
class('4/72', 11, 1, [1,1,1,0,1]).
class('4/72', 12, 8, [0,1,0,1,0]).
class('4/72', 13, 3, [1,0,0,1,1]).
class('4/72', 14, 10,[1,0,0,0,0]).
class('4/72', 15, 4, [0,1,0,0,1]).
class('4/72', 16, 4, [0,0,0,0,1]).
class('4/72', 17, 2, [1,0,0,0,1]).
class('4/72', 18, 4, [1,1,0,0,0]).
class('4/72', 19, 6, [1,1,0,1,0]).
class('4/72', 20, 1, [1,0,1,0,1]).
class('4/72', 21, 1, [1,1,1,1,1]).


%--------------
% Problem 16/81
%--------------

problem('16/81', 100,5,26).
capacities('16/81', [1,2,1,2,1]).
sizes('16/81', [2,3,3,5,5]).
class('16/81', 0, 10, [1,0,0,0,0]).
class('16/81', 1, 2, [0,0,0,0,1]).
class('16/81', 2, 8, [0,1,0,1,0]).
class('16/81', 3, 8, [0,0,0,1,0]).
class('16/81', 4, 6, [0,1,1,0,0]).
class('16/81', 5, 11, [0,1,0,0,0]).
class('16/81', 6, 3, [0,0,1,0,0]).
class('16/81', 7, 2, [0,0,1,1,0]).
class('16/81', 8, 7, [1,1,0,0,0]).
class('16/81', 9, 2, [1,0,0,1,1]).
class('16/81', 10, 4, [1,0,1,0,0]).
class('16/81', 11, 7, [1,0,0,1,0]).
class('16/81', 12, 1, [1,1,1,0,1]).
class('16/81', 13, 3, [0,1,1,1,0]).
class('16/81', 14, 4, [0,1,0,0,1]).
class('16/81', 15, 5, [1,1,1,0,0]).
class('16/81', 16, 2, [1,1,0,0,1]).
class('16/81', 17, 1, [1,0,1,1,1]).
class('16/81', 18, 2, [1,0,1,1,0]).
class('16/81', 19, 3, [1,0,0,0,1]).
class('16/81', 20, 2, [0,1,1,0,1]).
class('16/81', 21, 1, [0,1,0,1,1]).
class('16/81', 22, 3, [1,1,0,1,0]).
class('16/81', 23, 1, [0,0,1,1,1]).
class('16/81', 24, 1, [1,1,1,1,1]).
class('16/81', 25, 1, [1,1,1,1,0]).


%--------------
% Problem 41/66
%--------------
problem('41/66', 100,5,19).
capacities('41/66', [1,2,1,2,1]).
sizes('41/66', [2,3,3,5,5]).
class('41/66', 0, 7, [1,0,0,0,0]).
class('41/66', 1, 9, [0,1,1,0,0]).
class('41/66', 2, 4, [0,0,0,1,0]).
class('41/66', 3, 2, [0,1,0,1,1]).
class('41/66', 4, 6, [0,0,1,0,0]).
class('41/66', 5, 18, [0,1,0,0,0]).
class('41/66', 6, 6, [0,1,0,0,1]).
class('41/66', 7, 6, [0,0,0,0,1]).
class('41/66', 8, 1, [1,1,0,1,1]).
class('41/66', 9, 10, [1,1,0,0,0]).
class('41/66', 10, 2, [1,0,0,0,1]).
class('41/66', 11, 11, [0,1,0,1,0]).
class('41/66', 12, 5, [0,0,1,1,0]).
class('41/66', 13, 1, [0,1,1,1,0]).
class('41/66', 14, 1, [0,1,1,0,1]).
class('41/66', 15, 3, [1,0,1,0,0]).
class('41/66', 16, 3, [1,1,1,0,0]).
class('41/66', 17, 3, [1,1,0,1,0]).
class('41/66', 18, 2, [1,1,1,1,0]).

%-------------
%Problem 26/82
%-------------
problem('26/82', 100,5,24).
capacities('26/82', [1,2,1,2,1]).
sizes('26/82', [2,3,3,5,5]).
class('26/82', 0, 2, [1,1,0,1,0]).
class('26/82', 1, 13, [0,1,0,0,0]).
class('26/82', 2, 10, [0,1,0,1,0]).
class('26/82', 3, 14, [1,1,0,0,0]).
class('26/82', 4, 5, [0,0,0,1,0]).
class('26/82', 5, 2, [0,1,0,1,1]).
class('26/82', 6, 2, [0,1,1,0,0]).
class('26/82', 7, 8, [1,0,0,1,0]).
class('26/82', 8, 5, [0,0,1,1,0]).
class('26/82', 9, 3, [1,1,1,0,0]).
class('26/82', 10, 9, [1,0,0,0,0]).
class('26/82', 11, 6, [1,1,0,0,1]).
class('26/82', 12, 2, [1,1,1,1,0]).
class('26/82', 13, 2, [0,0,0,0,1]).
class('26/82', 14, 1, [1,1,1,0,1]).
class('26/82', 15, 2, [0,1,1,1,0]).
class('26/82', 16, 2, [1,0,1,0,0]).
class('26/82', 17, 1, [1,0,0,0,1]).
class('26/82', 18, 1, [1,0,1,1,0]).
class('26/82', 19, 6, [0,0,1,0,0]).
class('26/82', 20, 1, [1,1,1,1,1]).
class('26/82', 21, 1, [0,0,1,1,1]).
class('26/82', 22, 1, [0,1,1,0,1]).
class('26/82', 23, 1, [0,0,1,0,1]).
