% Laboratory 5

parse(C, S1, S) :- cuvant(W, S1, S2),P=..[W, C, S2, S],call(P).

% Reguli PS si clauze de terminare
s(s, X, X).
vp(vp, X, X).
n(n, X, X).
pp(pp, X, X).
conj(conj, X, X).

% S --> NP VP
np(C, S1, S) :- parse(vp, S1, S2), s(C, S2, S).

% NP --> NP Conj NP
np(C, S1, S) :- parse(conj, S1, S2), parse(np, S2, S3), np(C, S3, S).
np(np, X, X).

% NP --> Det N
det(C, S1, S) :- parse(n, S1, S2), np(C, S2, S).
det(det, X, X).

% VP --> V NP
v(C, S1, S) :- parse(np, S1, S2), vp(C, S2, S).

% VP --> V NP PP
v(C, S1, S) :- parse(np, S1, S2), parse(pp, S2, S3), vp(C, S3, S).
v(v, X, X).

% PP --> P NP
p(C, S1, S) :- parse(np, S1, S2), pp(C, S2, S).
p(p, X, X).

% Lexicon
cuvant(det, [the|X], X).
cuvant(det, [all|X], X).
cuvant(det, [every|X], X).
cuvant(p, [near|X], X).
cuvant(conj, [and|X], X).
cuvant(n, [dog|X], X).
cuvant(n, [dogs|X], X).
cuvant(n, [cat|X], X).
cuvant(n, [cats|X], X).
cuvant(n, [elephant|X], X).
cuvant(n, [elephants|X], X).
cuvant(v, [chase|X], X).
cuvant(v, [chases|X], X).
cuvant(v, [see|X], X).
cuvant(v, [sees|X], X).
cuvant(v, [amuse|X], X).
cuvant(v, [amuses|X], X).

% Exemplu apel: parse_sentence([the, dog, see, the, cat]). - propozitia
% trebuie sa fie conforma cu gramatica pentru un raspuns yes.
parse_sentence(S) :- parse(s, S, []).
