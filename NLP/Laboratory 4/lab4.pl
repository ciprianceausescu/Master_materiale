% Laboratory 4

% Auxiliary functions.
% ----------
% Check if a element is member in a list.
% X - element to check
% L - list for check
isMemberInList(X, [X|_]).
isMemberInList(X, [_|L]) :- isMemberInList(X, L).

% Invert a list.
% L - list of elements
% RL - result list
% TRL - temp result list
% H - head of list
invertList([],[]).
invertList([H|L],RL) :- invertList(L, TRL), concat2Lists(TRL, [H], RL).

% Define table of game.
% line/column
% N - number of elements
% S - template for completate solutions
% TN - temp number of elements
% TS - temp template

tableOfGameDynamic(1, [1/_]) :- !.
tableOfGameDynamic(N, [N/_ | S]) :- TN is N - 1, tableOfGameDynamic(TN, S).
tableOfGame(N, S) :- tableOfGameDynamic(N, TS), invertList(TS, S).

% Define absolute difference between two numbers.
% N1 - first number
% N2 - second number
% ABS - absolute difference
absDifference(N1, N2, ABS) :- ABS is N1 - N2, N1 - N2 >= 0.
absDifference(N1, N2, ABS) :- ABS is -(N1 - N2), N1 - N2 < 0.

% Concat 2 lists.
% L1 - list number 1
% L2 - list number 2
% RL - result from L1 and L2
% H - head of list
concat2Lists([], L2, L2).
concat2Lists([H|L1], L2, [H|RL]) :- concat2Lists(L1, L2, RL).

% Make a list of N elements.
% N - number of elements
% L - list of elements
% TN - temp number of elements
% TL - temp list
makeListDynamic(0, []) :- !.
makeListDynamic(N, [N | L]) :- TN is N - 1, makeListDynamic(TN, L).
makeList(N, L) :- makeListDynamic(N, TL), invertList(TL, L).

% Main functions.
% ----------
% Check if a space on the table is free
% L - line
% C - column
% S - previous solutions
% TL - temp line
% TC - temp column
freeSpace(_, []).
freeSpace(L/C, [TL/TC|S]) :- C =\= TC,
                             L =\= TL,
                             absDifference(TC, C, ABS1),
                             absDifference(TL, L, ABS2),
                             ABS1 =\= ABS2,
                             freeSpace(L/C, S).

% Find solutions on the table.
% L - line
% C - column
% S - solution on the table
findSolutions([], _).
findSolutions([L/C|S], LL) :- findSolutions(S, LL), isMemberInList(C, LL), freeSpace(L/C, S).

% Resolv problem for a given number N.
% N - dimension of table
% S - solutions
% L - list of numbers in range 1 to N
resolve(N, S) :- makeList(N, L), tableOfGame(N, S), findSolutions(S, L).
