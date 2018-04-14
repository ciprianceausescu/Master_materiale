:- module(hamming, [hamming/3]).

% ?- bench([hamming(10,3,64),hamming(10,5,9)]).

:- use_module(library(clpfd)).
:- use_module(library(lists), [append/2,transpose/2]).

%%% Find a binary Hamming code (N,D) with K elements.
%%% K = #words
%%% N = word size
%%% D: sum of pairwise difference >= D

hamming(N, D, K) :-
	length(Words, K),
	words(Words, N),
	Words = [Head|_],
	domain(Head, 0, 0),
	lex_chain(Words, [op(#<)]),
	transpose(Words, Columns),
	lex_chain(Columns),
	constraints(Words, D),
	append(Words, Vars),
	labeling([], Vars), !,
	draw(Words).

draw([]).
draw([Row|Rows]) :-
	asciify(Row, String),
	format('~s\n', [String]),
	draw(Rows).

asciify([], []).
asciify([R|Rs], [S|Ss]) :-
	S is R+"0",
	asciify(Rs, Ss).

words([], _).
words([Word|Words], N) :-
	length(Word, N),
	domain(Word, 0, 1),
	words(Words, N).

constraints([], _).
constraints([Word|Words], D) :-
	constraints(Words, Word, D),
	constraints(Words, D).

constraints([], _, _).
constraints([Word|Words], First, D) :-
	diffs(Word, First, Bs),
	sum(Bs, #>=, D),
	constraints(Words, First, D).

diffs([], [], []).
diffs([A|As], [B|Bs], [C|Cs]) :-
	(A #\ B) #<=> C,
	diffs(As, Bs, Cs).

