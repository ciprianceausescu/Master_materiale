% Laboratory 5

parse(Cateorie, [Cuvant|S], S, [Cateorie,[Cuvant]]) :-
	cuvant(Cateorie, Cuvant).

parse(Cateorie, S1, S, [Cateorie, Arbore]) :-
	regula(Cateorie, Fii),
	parse_lista(Fii, S1, S, Arbore).

parse_lista([C|Categorii], S1, S, [A|Arbore]) :-
	parse(C, S1, S2, A),
	parse_lista(Categorii, S2, S, Arbore).
parse_lista([], S, S, []).

regula(s,[np,vp]).
regula(vp,[v,np]).
regula(np,[det,n]).

cuvant(det,un).
cuvant(det,niste).
cuvant(det,o).
cuvant(n,elev).
cuvant(n,elevi).
cuvant(n,carte).
cuvant(v,iubeste).
cuvant(v,iubesc).

% Exemplu apel: parse_sentence(X, A). - va genera rand pe rand cate o combinatie
parse_sentence(X, Arbore) :- parse(s, X, [], Arbore).

% Exemplu apel: generate_all(L). - va genera toate combinatiile
generate_all(L) :- findall(X, parse_sentence(X, _), L).
