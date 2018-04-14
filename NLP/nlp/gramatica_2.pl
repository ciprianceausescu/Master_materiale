% Gramatica 2
% S -> VP NP
% VP -> V NP
% NP -> Det N

s(X, Z) :- np(X, Y), vp(Y, Z).
vp(X, Z) :- v(X, Y), np(Y, Z).
np(X, Z) :- det(X, Y), n(Y, Z).

det([un|X], X).
det([niste|X], X).
det([o|X], X).
n([elev|X], X).
n([elevi|X], X).
n([carte|X], X).
v([iubeste|X], X).
v([iubesc|X], X).

% Generare propozitii
% s(X, []).

% Recunoastere propozitii
% s([un, elev, iubeste, o, carte], []).