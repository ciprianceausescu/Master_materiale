dame(N, Solutie) :- initializeaza(N, Tabla), permutari(Tabla, Solutie), verifica(Solutie).

initializeaza(0, []) :- !.
initializeaza(N, List) :- N1 is N - 1, initializeaza(N1, List1), append(List1, [N], List).

permutari([H], [H]).
permutari([H|T], L) :- permutari(T, L1), insert(H, L1, L).

insert(H, T, [H|T]).
insert(H, [X|T1], [X|T2]) :- insert(H, T1, T2).

verifica([_]).
verifica([H|T]) :- ok(H, T, 1), verifica(T).

ok(_, [], _).
ok(X, [H|T], Dist) :- X - H =\= Dist, H - X =\= Dist, Dist1 is Dist + 1, ok(X, T, Dist1).