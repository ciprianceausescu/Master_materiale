% 1. Modificati parser-ul top-down descris astfel incat el sa realizeze intotdeauna o reprezentare a arborelui de derivare
% (chiar si atunci cand regulile sunt date in forma lor initiala, adica fara argumente).

recunoaste(Categorie, S1, S2) :- regula(Categorie, Fii), imperecheaza(Fii, S1, S2).

imperecheaza([], S, S).
imperecheaza([Cuvant], [Cuvant|S], S).
imperecheaza([Categorie|Categorii], S1, S3) :- recunoaste(Categorie, S1, S2), imperecheaza(Categorii, S2, S3).

regula(s, [np, vp]).
regula(vp, [v]).
regula(vp, [v, np]).
regula(np, ['Dr. Popescu']).
regula(np, ['Colentina']).
regula(np, [surori]).
regula(np, [pacienti]).
regula(v, [angajeaza]).
regula(v, [omoara]).

% ?- recunoaste(s, ['Colentina', angajeaza, surori], []).



parse_lista([C|Cs], S1, S) :- parse(C, S1, S2), parse_lista(Cs, S2, S).
parse_lista([], S, S).

parse(C, [Cuvant|S], S) :- cuvant(C, Cuvant).
parse(C, S1, S) :- regula(C, Cs), parse_lista(Cs, S1, S).

cuvant(np, 'Colentina').
cuvant(np, surori).
cuvant(v, angajeaza).

% ?- parse_lista([s], ['Colentina', angajeaza, surori], []).