% Gramatica 1
% S -> NP VP
% VP -> V
% VP -> V NP

s(X, Z) :- np(X, Y), vp(Y, Z).
vp(X, Z) :- v(X, Z).
vp(X, Z) :- v(X, Y), np(Y, Z).

np(['Dr. Popescu'|X], X).
np(['Colentina'|X], X).
np(['surori'|X], X).
np(['pacienti'|X], X).
v([omoara|X], X).
v([angajeaza|X], X).

% Generare propozitii
% s(X, []).

% Recunoastere propozitii
% s(['Colentina',angajeaza, surori], []).