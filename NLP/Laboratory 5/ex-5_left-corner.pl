% Laboratory 5

parse(Categorie, [Cuvant | SirRamas], Sir, Numar, Arbore) :-
 cuvant(Subcategorie, Numar, Cuvant),
 completeaza(Subcategorie, Categorie, SirRamas, Sir, Numar, [Subcategorie, [Cuvant]], Arbore).

parse_lista([Categorie | Categorii], Sir1, Sir, [Numar|Numere], [Arbore1 | Arbore]) :-
 parse(Categorie, Sir1, Sir2, Numar, Arbore1),
 parse_lista(Categorii, Sir2, Sir, Numere, Arbore).

parse_lista([], Sir, Sir, [], []).

completeaza(Subcategorie, Subcategorie, Sir, Sir, _, Arbore, Arbore).
completeaza(Subcategorie, Categorie, Sir1, Sir, Numar, Arbore, ArboreRedus) :-
 regula(Parinte, NumarParinte, [Numar|Numere], [Subcategorie | Rest]),
 parse_lista(Rest, Sir1, Sir2, Numere, Arbore1),
 completeaza(Parinte, Categorie, Sir2, Sir, NumarParinte, [Parinte, [Arbore | Arbore1]], ArboreRedus).

regula(s, _, [Numar, Numar], [np, vp]).
regula(np, Numar, [Numar, Numar], [det, n]).
regula(np, plural, [Numar, _, Numar], [np, conj, np]).
regula(vp, Numar, [Numar, _], [v, np]).
regula(vp, Numar, [Numar, _, _], [v, np, pp]).
regula(pp, Numar, [_, Numar], [p, np]).

cuvant(det, _, the).
cuvant(det, plural, all).
cuvant(det, singular, every).
cuvant(p, _, near).
cuvant(conj, _, and).
cuvant(n, singular, dog).
cuvant(n, plural, dogs).
cuvant(n, singular, cat).
cuvant(n, plural, cats).
cuvant(n, singular, elephant).
cuvant(n, plural, elephants).
cuvant(v, plural, chase).
cuvant(v, singular, chases).
cuvant(v, singular, see).
cuvant(v, plural, sees).
cuvant(v, singular, amuse).
cuvant(v, plural, amuses).

% Exemplu apel: parse_sentence([all, dogs, sees, the, cat], A). - propozitia
% trebuie sa fie conforma cu gramatica pentru un raspuns yes.
parse_sentence(X, A) :- parse(s, X, [], _, A).
