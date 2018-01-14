% Laboratory 5

parse(Sir, Rezultat, Numar, Arbore) :-
	depl_red(Sir, [], Rezultat, Numar, [], Arbore).

% Predicatul depl_red(S, Stiva, Rezultat) analizeaza sirul de intrare S, unde
% Stiva este lista categoriilor analizate pana la momentul curent. Atunci cand
% Sirul de intrare devine vid, inseamna ca toate cuvintele au fost deplasate in
% stiva si reduse, deci stiva contine chiar rezultatul.
% Predicatul depl_red se defineste recusiv astfel:

depl_red(Sir, Stiva, Rezultat, Numar, Arbore, ArboreRedus) :-
	deplasare(Stiva, Sir, StivaNoua, SirRedus),
	reducere(StivaNoua, StivaRedusa, Numar, NumarRedus, Arbore, Arbore1),
	depl_red(SirRedus, StivaRedusa, Rezultat, NumarRedus, Arbore1, ArboreRedus).

depl_red([], Rezultat, Rezultat, _, Arbore, Arbore).


% OBSERVATIE: predicatul deplasare esueaza daca sirul de intrare este vid, Sirul
% de intrare devine vid atunci cand toate cuvintele sale au fost deplasate in
% stiva si reduse, deci in cazul tratat de clauza depl_red anterioara.
% Astfel, in cazul general, predicatul deplasare se defineste astfel:

deplasare(Stiva, [Cuvant|SirRamas], [Cuvant|Stiva], SirRamas).


% Predicatul reducere(Stiva, StivaNoua) reducere in mod repetat varful stivei,
% pentru a forma constituenti mai putini, dar mai ampli. Definitia recusiva este:

reducere(Stiva, StivaRedusa, Numar, NumarRedus, Arbore, ArboreRedus) :-
	iregula(Stiva, Stiva1, Numar, Numar1, Arbore, Arbore1),
	reducere(Stiva1, StivaRedusa, Numar1, NumarRedus, Arbore1, ArboreRedus).

reducere(Stiva, Stiva, Numar, Numar, Arbore, Arbore).


% Definita a tinut cont de cazul cand nu exista nici o iregula, deci nu se poate
% efectua nici o reducere. La clauza reducere(Stiva, Stiva) se ajunge atunci
% cand nu se poate aplica o iregula si stiva ramane neschimbata.

% Regulile PS ale gramaticii se memoreaza tot inapoi.

iregula([vp, np|X], [s|X], [Numar, Numar | Numere], [ _ | Numere], [Arbore2, Arbore1 | Arbore], [[s, Arbore1, Arbore2] | Arbore]).
iregula([v|X], [vp|X], [Numar | Numere], [Numar | Numere], [Arbore1 | Arbore], [[vp, Arbore1] | Arbore]).
iregula([np, v|X], [vp|X], [_, Numar | Numere], [Numar | Numere], [Arbore2, Arbore1 | Arbore], [[vp, Arbore1, Arbore2] | Arbore]).
iregula([n | X], [np | X], [Numar | Numere], [Numar | Numere], [Arbore1 | Arbore] , [[np, Arbore1] | Arbore]).
iregula([n, det|X], [np|X], [Numar, Numar | Numere], [ Numar | Numere], [Arbore2, Arbore1 | Arbore], [[np, Arbore1, Arbore2] | Arbore]).

% Pentru ca lexiconul sa poata fi utilizat este nevoie, in plus, de specificarea
% urmatoarei reguli inapoi:

iregula([Cuvant|X], [Categorie|X], Numere, [Numar | Numere], Arbore, [[Categorie, [Cuvant]] | Arbore]) :- cuvant(Categorie, Numar, Cuvant).

% Lexiconul este:
cuvant(n, singular, ana).
cuvant(n, plural, mere).
cuvant(n, plural, baietii).
cuvant(v, singular, are).
cuvant(v, plural, au).
cuvant(det, plural, multe).

% Exemplu apel: parse_sentence([ana, are, mere], A).
parse_sentence(X, Arbore) :- parse(X, [s], [], Arbore).
