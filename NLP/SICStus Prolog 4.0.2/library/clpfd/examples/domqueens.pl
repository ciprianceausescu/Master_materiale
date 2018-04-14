% Dominating Queens
% Place a minimal number of queens so that all positions are under attack.
% See N. Beldiceanu et al., Global Constraint Catalog.
% Author    : Mats Carlsson

:- module(domqueens, [
        domqueens/1,
        domqueens/2
        ]).

:- use_module(library(clpfd)).

domqueens(N) :-
	create_mutable([], Mut),
	domqueens([ff,value(valsel(Mut))], N).

domqueens(Lab, N) :-
	NN is N*N,
	length(Xs, NN),
	domains(Xs, 0, N, NN),
	C in 1..N,
	nvalue(C, Xs),
	indomain(C),
	labeling(Lab, Xs),
	sort(Xs, Ys),
	draw(Ys, N, NN).

draw(Ys, N, NN) :-
	format('+~*c+\n', [N,0'-]),
	draw_lines(0, NN, N, Ys),
	format('+~*c+\n', [N,0'-]).

draw_lines(NN, NN, _, _) :- !.
draw_lines(I, NN, N, Ys) :-
	for(I, 1, I+N-1, L, []),
	asciify(L, Ys, String, "|\n"),
	format([0'||String], []),
	J is I+N,
	draw_lines(J, NN, N, Ys).

asciify([], _) --> [].
asciify([X|Xs], Ys) --> "Q",
	{member(X, Ys)}, !, 
	asciify(Xs, Ys).
asciify([_|Xs], Ys) --> " ",
	asciify(Xs, Ys).


domains([], _, _, _).
domains([X|Xs], Pos, N, NN) :-
	Col is Pos mod N,
	Row is Pos //  N,
	NUL is min(Col,Row),	% # up left
	NUR is min(N-Col-1,Row), % # up right
	NDL is min(Col,N-Row-1), % # down left
	NDR is min(N-Col-1,N-Row-1), % # down right
	for(-Row*N+Pos, N, (N-Row-1)*N+Pos, S1, S2),
	for(Pos-Col, 1, Pos+(N-Col-1), S2, S3),
	for(NUL*(N+1)+Pos, N+1, NDR*(N+1)+Pos, S3, S4),
	for(NUR*(N-1)+Pos, N-1, NDL*(N-1)+Pos, S4, []),
	sort(S1, L),
	list_to_fdset(L, Set),
	X in_set Set,
	Pos1 is Pos+1,
	domains(Xs, Pos1, N, NN).

for(LB, Step, UB) -->
	{LB1 is LB},
	{Step1 is Step},
	{UB1 is UB},
	for1(LB1, Step1, UB1).

for1(L, _, U) --> {L>U}, !.
for1(L, S, U) --> [L],
	{L1 is L+S},
	for1(L1, S, U).

valsel(Sofar, X, _, BB0, BB) :-
	get_mutable(SofarSet, Sofar),
	fd_set(X, XSet),
	fdset_intersection(XSet, SofarSet, OldSet),
	valsel(Sofar, X, SofarSet, OldSet, BB0, BB).

valsel(_, X, _, Old, BB0, BB) :-
	fdset_min(Old, Y),
	X #= Y,
	first_bound(BB0, BB).
valsel(Mut, X, Sofar, Old, BB0, BB) :-
	fdset_complement(Old, NotOld),
	X in_set NotOld,
	labeling([bisect], [X]),
	fdset_add_element(Sofar, X, Sofar1),
	update_mutable(Sofar1, Mut),
	later_bound(BB0, BB).

