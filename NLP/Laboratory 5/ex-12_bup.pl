% Laboratory 5

parse(C, [Cuvant | S1], S, Numar, Arbore) :- cuvant(W, Numar, [Cuvant | S1], S2),P=..[W, C, S2, S, Numar, [W, [Cuvant]], Arbore],call(P).

% Reguli PS si clauze de terminare
s(s, X, X, _, Arbore, Arbore).
vp(vp, X, X, _, Arbore, Arbore).
n(n, X, X, _, Arbore, Arbore).
pp(pp, X, X, _, Arbore, Arbore).
conj(conj, X, X, _, Arbore, Arbore).

% S --> NP VP
np(C, S1, S, Numar, Arbore1, Arbore) :- parse(vp, S1, S2, Numar, Arbore2), s(C, S2, S, Numar, [s, Arbore1, Arbore2], Arbore).

% NP --> NP Conj NP
np(C, S1, S, Numar, Arbore1, Arbore) :- parse(conj, S1, S2, Numar, Arbore2), parse(np, S2, S3, Numar, Arbore3), np(C, S3, S, plural, [np, Arbore1, Arbore2, Arbore3], Arbore).
np(np, X, X, _, Arbore, Arbore).

% NP --> Det N
det(C, S1, S, Numar, Arbore1, Arbore) :- parse(n, S1, S2, Numar, Arbore2), np(C, S2, S, Numar, [np, Arbore1, Arbore2], Arbore).
det(det, X, X, _, Arbore, Arbore).

% VP --> V NP
v(C, S1, S, Numar, Arbore1, Arbore) :- parse(np, S1, S2, _, Arbore2), vp(C, S2, S, Numar, [Arbore1, Arbore2], Arbore).

% VP --> V NP PP
v(C, S1, S, Numar, Arbore1, Arbore) :- parse(np, S1, S2, _, Arbore2), parse(pp, S2, S3, _, Arbore3), vp(C, S3, S, Numar, [Arbore1, Arbore2, Arbore3], Arbore).
v(v, X, X, _, Arbore, Arbore).

% PP --> P NP
p(C, S1, S, _, Arbore1, Arbore) :- parse(np, S1, S2, Numar, Arbore2), pp(C, S2, S, Numar, [Arbore1, Arbore2], Arbore).
p(p, X, X, _, Arbore, Arbore).

% Lexicon
cuvant(det, _, [the|X], X).
cuvant(det, plural, [all|X], X).
cuvant(det, singular, [every|X], X).
cuvant(p, _, [near|X], X).
cuvant(conj, _, [and|X], X).
cuvant(n, singular, [dog|X], X).
cuvant(n, plural, [dogs|X], X).
cuvant(n, singular, [cat|X], X).
cuvant(n, plural, [cats|X], X).
cuvant(n, singular, [elephant|X], X).
cuvant(n, plural, [elephants|X], X).
cuvant(v, plural, [chase|X], X).
cuvant(v, singular, [chases|X], X).
cuvant(v, singular, [see|X], X).
cuvant(v, plural, [sees|X], X).
cuvant(v, singular, [amuse|X], X).
cuvant(v, plural, [amuses|X], X).

% Exemplu apel: parse_sentence([the, dog, see, the, cat]). - propozitia
% trebuie sa fie conforma cu gramatica pentru un raspuns yes.
parse_sentence(S, Arbore) :- parse(s, S, [], _, Arbore).
