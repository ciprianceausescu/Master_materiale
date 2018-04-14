% sa concateneze doua liste
concat([], L, L).
concat([H|T], L, [H|L2]) :- concat(T, L, L2).

% aux
isMember(H, [H|_]).
isMember(X, [_|T]) :- isMember(X, T).

% sa verifive daca o lista este multime
isSet([]).
isSet([H|T]) :- \+isMember(H, T), isSet(T).

% ...
listToSet([], []).
listToSet([H|T], S) :- listToSet(T,L), (isMember(H,L) -> concat([], L, S); concat([H], L, S)).

% intersectia a doua multimi
intersectie([], _, []) :- !.
intersectie(_, [], []) :- !.
intersectie([H|T], L, Li) :- intersectie(T, L, LAux), (isMember(H, L) -> concat([H], LAux, Li); concat([], LAux, Li)).

% reuniunea a doua multimi
reuniune(A, B, R) :- concat(A, B, C), listToSet(C, R).

% diferenta a doua multimi
diferenta([], _, []).
diferenta([H|T], L, D) :- diferenta(T, L, Aux), (isMember(H, L) -> concat([], Aux, D); concat([H], Aux, D)).

% inserare daca nu exista
insertIfNotExist(L, E, L1) :- isMember(E, L) -> concat([], L, L1); concat([E], L, L1).

% sterge un element din lista
sterge(_, [], []).
sterge(X, [X|T], L) :- sterge(X, T, L), !.
sterge(X, [H|T], [H|T1]) :- sterge(X, T, T1).

% factorial(n)
factorial(0, 1) :- !.
factorial(N, F) :- N1 is N - 1, factorial(N1, F1), F is F1 * N.

% fib(n)
fib(0, 0) :- !.
fib(1, 1) :- !.
fib(N, F) :- N1 is N - 1, N2 is N - 2, fib(N1, F1), fib(N2, F2), F is F1 + F2.

% cmmdc(m, n)
cmmdc(M, 0, M) :- !.
cmmdc(0, M, M) :- !.
cmmdc(_, 1, 1) :- !.
cmmdc(1, _, 1) :- !.
cmmdc(M, N, C) :- M > N -> Aux is M - N, cmmdc(Aux, N, C); Aux is N - M, cmmdc(M, Aux, C).

% cate elemente are o lista
len([], 0).
len([_|T], L) :- len(T, N), L is N + 1.

% suma elementelor pozitive dintr-o listă de întregi
sumOfPos([], 0).
sumOfPos([H|T], S) :- sumOfPos(T, SAux), (H > 0 -> S is SAux + H; S is SAux).

% inlocuire o aparitie
inl([], _, _, []).
inl([X|T], X, Y, [Y|T]) :- !.
inl([H|T], X, Y, [H|T2]) :- inl(T, X, Y, T2).

% inlocuire toate aparitiile
inlocuire([], _, _, []).
inlocuire([H|T], E1, E2, L) :- inlocuire(T, E1, E2, Aux), (H == E1 -> concat([E2], Aux, L); concat([H], Aux, L)).

% inversul unei liste
invers([], []).
invers([H|T], L) :- invers(T, Aux), concat(Aux, [H], L).

% minimul unei liste
min([M], M).
min([H|T], Min) :- min(T, M), (H < M -> Min is H; Min is M).

% aflarea unui element de pe o pozitie, incepand de la 0
elementPos([H|_], 0, H).
elementPos([_|T], P, X) :- P1 is P - 1, elementPos(T, P1, X).

% inserarea unui element pe o pozitie, incepand de la 0
insertPos(L, E, 0, [E|L]).
insertPos([H|T], E, P, [H|L]) :- P1 is P - 1, insertPos(T, E, P1, L).

% interclasare doua liste
interclasare([], L, L) :- !.
interclasare(L, [], L) :- !.
interclasare([H1|T1], [H2|T2], [H1|T]) :- H1 =< H2, interclasare(T1, [H2|T2], T).
interclasare([H1|T1], [H2|T2], [H2|T]) :- H1 > H2, interclasare([H1|T1], T2, T).

% impartire in subliste
subliste([], _, [], []).
subliste([H|T], X, [H|L1], L2) :- H < X, subliste(T, X, L1, L2).
subliste([H|T], X, L1, [H|L2]) :- H > X, subliste(T, X, L1, L2).
subliste([X|T], X, L1, L2) :- subliste(T, X, L1, L2).