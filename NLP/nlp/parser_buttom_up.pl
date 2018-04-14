parse(S, Rez) :- depl_red(S, [], Rez).

depl_red(S, Stiva, Rez) :- deplasare(Stiva, S, StivaNoua, S1), reducere(StivaNoua, StivaRed), depl_red(S1, StivaRed, Rez).
depl_red([], Rez, Rez).

deplasare(X, [H|Y], [H|X], Y).

reducere(Stiva, StivaRed) :- iregula(Stiva, Stiva1), reducere(Stiva1, StivaRed).
reducere(Stiva, Stiva).

iregula([vp, np|X], [s|X]).
iregula([n, art|X], [np|X]).
iregula([v|X], [vp|X]).
iregula([un|X], [art|X]).
iregula([caine|X], [n|X]).
iregula([latra|X], [v|X]).