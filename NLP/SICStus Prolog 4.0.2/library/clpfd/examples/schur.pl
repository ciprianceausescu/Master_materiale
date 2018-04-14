:- module(schur, [schur/2]).

% ?- bench([schur(122,5)]).

:- use_module(library(trees)).
:- use_module(library(clpfd)).

schur(N, P) :-
	length(Integers, N),
	length(Binaries, N),
	functors(Binaries, P),
	list_to_tree(Binaries, Tree),
	domain(Integers, 1, P),
	make_relation(0, P, Relation, []),
	make_table(Integers, Binaries, Table),
	table(Table, Relation),
	schur_constraints(0, N, Tree),
	symmetry_labeling(Integers, 1, P),
	writeq(Integers),
	nl.

symmetry_labeling(Vars, K, K) :- !,
	labeling([enum], Vars).
symmetry_labeling([], _, _).
symmetry_labeling([V|Vars], Lim, K) :-
	V #=< Lim,
	indomain(V),
	(V<Lim -> Lim1=Lim ; Lim1 is Lim+1),
	symmetry_labeling(Vars, Lim1, K).

functors([], _).
functors([F|Fs], A) :-
	functor(F, f, A),
	functors(Fs, A).

make_relation(P, P) --> !.
make_relation(I, P) --> [[J|Row]],
	{J is I+1},
	{row(0, P, J, Row, [])},
	make_relation(J, P).

row(P, P, _) --> !.
row(I, P, K) -->
	{J is I+1},
	({J=:=K} -> [1] ; [0]),
	row(J, P, K).

make_table([], [], []).
make_table([Int|Ints], [Bin|Bins], [[Int|ZOs]|Table]) :-
	Bin =.. [_|ZOs],
	domain(ZOs, 0, 1),
	make_table(Ints, Bins, Table).

schur_constraints(I, N, _) :-
	I >= N>>1, !.
schur_constraints(I, N, Tree) :-
	I1 is I+1,
	M1 is N-I1,
	get_label(I1, Tree, IL),
	schur_constraints(I, M1, I1, IL, Tree),
	schur_constraints(I1, N, Tree).

schur_constraints(N, N, _, _, _) :- !.
schur_constraints(J, N, I, IL, Tree) :-
	J1 is J+1,
	get_label(J1, Tree, JL),
	K is J1+I,
	get_label(K, Tree, KL),
	functor(IL, _, A),
	schur_triple(0, A, IL, JL, KL),
	schur_constraints(J1, N, I, IL, Tree).

schur_triple(A, A, _, _, _) :- !.
schur_triple(I, A, IL, JL, KL) :-
	J is I+1,
	arg(J, IL, IA),
	arg(J, JL, JA),
	arg(J, KL, KA),
	at_most_two(IA, JA, KA),
	schur_triple(J, A, IL, JL, KL).

at_most_two(IA, JA, KA) +:
	IA + JA + KA #=< 2.


