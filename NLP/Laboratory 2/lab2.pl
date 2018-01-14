% Laboratory 2
% TEMA PROBLEMA CELOR 8 DAME

% Auxiliary functions.
% ----------
% Check if a element is member in a list.
% X - element to check
% L - list for check
isMemberInList(X, [X|_]).
isMemberInList(X, [_|L]) :- isMemberInList(X, L).

% Main functions.
% ----------
% Concat 2 lists
% L1 - list number 1
% L2 - list number 2
% RL - result from L1 and L2
% H - head of list
concat2Lists([], L2, L2).
concat2Lists([H|L1], L2, [H|RL]) :- concat2Lists(L1, L2, RL).

% Check if a list is a set.
% L - list for check
% H - head of list
isSet([]).
isSet([H|L]) :- (\+isMemberInList(H,L)), isSet(L).

% Transform list to set.
% L - initial list
% S - result set from list
% H - head of list
listToSet([], []).
listToSet([H|L], [H|S]) :- \+isMemberInList(H, L), listToSet(L, S).
listToSet([H|L], S) :- isMemberInList(H, L), listToSet(L, S).

% Intersection of sets.
% S1 - set 1
% S2 - set 2
% RS - result set
intersectionOfSets([], [], []).
intersectionOfSets(S1, S2, RS) :- setof(E, (isMemberInList(E, S1), isMemberInList(E, S2)), RS).

% Union of sets.
% S1 - set 1
% S2 - set 2
% RS - result set
unionOfSets([], [], []).
unionOfSets(S1, S2, RS) :- setof(E, (isMemberInList(E, S1); isMemberInList(E, S2)), RS).

% Difference of sets.
% S1 - set 1
% S2 - set 2
% RS - result set
differenceOfSets([], [], []).
differenceOfSets(S1, S2, RS) :- setof(E, (isMemberInList(E, S1), \+isMemberInList(E, S2)), RS).

% Insert an element in top of list.
% E - element
% L - list of elements
insertTop(E, [], [E]) :- !.
insertTop(E, L, [E|L]) :- \+isMemberInList(E,L).

% Delete an element from a list.
% E - element
% L - list
% E1, E2 - heads of list
% RL - result list
deleteFromList(E, [E|L], L) :- !.
deleteFromList(E, [E1, E2|L], [E1|RL]) :- deleteFromList(E, [E2|L], RL).

% Factorial for a number.
% N - number for factorial
% R - result
% TN - temp number
% TR - temp result
factorial(0, R) :- R is 1,!.
factorial(N, R) :- TN is N - 1, factorial(TN, TR), R is TR * N.

% Fibonacci for a number.
% N - number for fibonacci
% R - result
% TN1, TN2 - temp number
% TR1, TR2 - temp result
fibonacci(0, R) :- R is 0, !.
fibonacci(1, R) :- R is 1, !.
fibonacci(N, R) :- TN1 is N - 1, TN2 is N - 2, fibonacci(TN1, TR1), fibonacci(TN2, TR2), R is TR1 + TR2.

% CMMDC for 2 numbers.
% X, Y - numbers
% X1, Y1 - temp numbers
% R - result
cmmdc(X,Y,R):- X =:= Y, R is X.
cmmdc(X,Y,R):- X > Y, X1 is X - Y, cmmdc(X1, Y, R).
cmmdc(X,Y,R):- Y > X, Y1 is Y - X, cmmdc(X, Y1, R).

% Number of elements from a list.
% L - list
% R - result
% TR - temp result
numberOfElements([], 0).
numberOfElements([_|L], R) :- numberOfElements(L, TR), R is TR + 1.

% Sum of positive elemets from a list.
% L - list
% R - result
% H - head of list
% TR - temp result
sumOfPositiveElements([], 0).
sumOfPositiveElements([H|L], R) :- H =< 0, sumOfPositiveElements(L, R).
sumOfPositiveElements([H|L], R) :- H > 0, sumOfPositiveElements(L, TR), R is TR + H.
