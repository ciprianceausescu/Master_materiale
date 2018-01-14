% Laboratory 3

% Auxiliary functions.
% ----------
% Check if a element is member in a list.
% X - element to check
% L - list for check
isMemberInList(X, [X|_]).
isMemberInList(X, [_|L]) :- isMemberInList(X, L).

% Concat 2 lists
% L1 - list number 1
% L2 - list number 2
% RL - result from L1 and L2
% H - head of list
concat2Lists([], L2, L2).
concat2Lists([H|L1], L2, [H|RL]) :- concat2Lists(L1, L2, RL).

% Minimum from 2 elements.
% A - first element
% B - second element
minimum(A, B, A) :- A =< B.
minimum(A, B, B) :- B =< A.

% Delete an element from a list.
% E - element
% L - list
% E1, E2 - heads of list
% RL - result list
deleteFromList(E, [E|L], L).
deleteFromList(E, [E1, E2|L], [E1|RL]) :- deleteFromList(E, [E2|L], RL).

% Main functions.
% ----------
% Replace an element into a list.
% X - element to replace
% Y - element for replace
% Z - other element from list
% L - list of elements
% RL - result list
replaceFirst(_, _, [], []).
replaceFirst(X, Y, [X|L], [Y|L]) :- !.
replaceFirst(X, Y, [Z|L], [Z|RL]) :- replaceFirst(X, Y, L, RL).

replaceAll(_, _, [], []).
replaceAll(X, Y, [X|L], [Y|RL]) :- replaceAll(X, Y, L, RL), !.
replaceAll(X, Y, [Z|L], [Z|RL]) :- replaceAll(X, Y, L, RL).

% Invert a list.
% L - list of elements
% RL - result list
% TRL - temp result list
% H - head of list
invertList([],[]).
invertList([H|L],RL) :- invertList(L, TRL), concat2Lists(TRL, [H], RL).

% Permutation in a list.
% E - element
% L - list of elements
% TP - temp permutation
% RP - result permutation
permutation([E|L], RP) :- permutation(L, TP), deleteFromList(E, RP, TP).
permutation([], []).

% Get minimum element from a list.
% E - element
% TE - temp element
% H - head of list
% L - list
getMinim([E], E).
getMinim([H|L], E) :- getMinim(L, TE), minimum(H, TE, E).

% Get element by position.
% L - list of elements
% X - found element
% E - an element
% P - position
% TP - temp position
getByPosition([X|_], 1, X).
getByPosition([_|L], P, E) :- TP is P - 1, getByPosition(L, TP, E).

% Insert element on a position
% E - element to insert
% P - position for insert
% H - head of list
% L - list of elements
% TP - temp position
% RL - result list
insertAt(E, 1, L, [E|L]).
insertAt(E, P, [H|L], [H|RL]) :- TP is P - 1, insertAt(E, TP, L, RL).

% Merge 2 list of sorted elements.
% RS - right sorted list
% LS - left sorted list
% L - head of left sorted list
% R - head of right sorted list
% FS - final sorted list
merge([], RS, RS).
merge(LS, [], LS).
merge([L|LS], [R|RS], [L|FS]) :- L =< R, merge(LS, [R|RS], FS).
merge([L|LS], [R|RS], [R|FS]) :- L > R, merge([L|LS], RS, FS).

% Divide a list by a value.
% L - list of elements
% H - head of list
% E - element for divide
% SN - smaller numbers list
% GN - greater numbers list
divideBy([], _, [], []).
divideBy([H], E, [H], []) :- H =< E, !.
divideBy([H], E, [], [H]) :- H > E, !.
divideBy([H|L], E, [H|SN], GN) :- H =< E, divideBy(L, E, SN, GN).
divideBy([H|L], E, SN, [H|GN]) :- H > E, divideBy(L, E, SN, GN).
