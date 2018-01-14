% Laboratory 5

parse(Categorie, [Cuvant | SirRamas], Sir) :-
 cuvant(Subcategorie, Cuvant),
 completeaza(Subcategorie, Categorie, SirRamas, Sir).

parse_lista([Categorie | Categorii], Sir1, Sir) :-
 parse(Categorie, Sir1, Sir2),
 parse_lista(Categorii, Sir2, Sir).

parse_lista([], Sir, Sir).

completeaza(Subcategorie, Subcategorie, Sir, Sir).
completeaza(Subcategorie, Categorie, Sir1, Sir) :-
 regula(Parinte, [Subcategorie | Rest]),
 parse_lista(Rest, Sir1, Sir2),
 completeaza(Parinte, Categorie, Sir2, Sir).

regula(s, [np, vp]).
regula(np, [det, n]).
regula(np, [np, conj, np]).
regula(vp, [v, np]).
regula(vp, [v, np, pp]).
regula(pp, [p, np]).

cuvant(det, the).
cuvant(det, all).
cuvant(det, every).
cuvant(p, near).
cuvant(conj, and).
cuvant(n, dog).
cuvant(n, dogs).
cuvant(n, cat).
cuvant(n, cats).
cuvant(n, elephant).
cuvant(n, elephants).
cuvant(v, chase).
cuvant(v, chases).
cuvant(v, see).
cuvant(v, sees).
cuvant(v, amuse).
cuvant(v, amuses).

% Exemplu apel: parse_sentence([the, dog, see, the, cat]). - propozitia
% trebuie sa fie conforma cu gramatica pentru un raspuns yes.
parse_sentence(X) :- parse(s, X, []).
