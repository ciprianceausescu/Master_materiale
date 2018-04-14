/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Balanced Incomplete Block Design
 * Author    : Mats Carlsson
 *
 * The goal is to find a VxB binary matrix with
 * R ones in each row, K ones in each column,
 * the scalar product of any two rows being Lambda.
 */

/* ?- bench([bibd([rl,up,lex], 10, 90, 27, 3, 6),
	     bibd([rl,up,lex], 15, 70, 14, 3, 2),
	     bibd([rl,up,lex], 12, 88, 22, 3, 4),
	     bibd([rl,up,lex], 9, 120, 40, 3, 10),
	     bibd([rl,up,lex], 10, 120, 36, 3, 8),
	     bibd([rl,up,lex], 13, 104, 24, 3, 4)]).
*/

:- module(bibd, [bibd/6]).

:- use_module(library(lists), [
	nth1/3,
        reverse/2
	]).
:- use_module(library(clpfd)).

% 	bibd([rl,up,lex], 8, 14, 7, 4, 3).
% SUCCEEDS, 43 bks
% 	bibd(lrd, 8, 14, 7, 4, 3).
% SUCCEEDS, 49 bks
% Row-major order, R to L: up - 43 bks, down - 65 bks
% Row-major order: up - 130 bks, down - 49 bks
% Column-major order: up - >50000 bks, down - >50000 bks
% Static variable ordering [Frisch et al., 2002]: up - 7908 bks, down - >19000 bks

% 	bibd([rl,up,lex], 6, 50, 25, 3, 10).
% SUCCEEDS, 143 bks
% 	bibd(lrd, 6, 50, 25, 3, 10).
% SUCCEEDS, 720 bks
% Row-major order, R to L: up - 143 bks, down - 400 bks.
% Row-major order: up - 926 bks, down - 720 bks.
% Static variable ordering [Frisch et al., 2002]: up - 2787 bks, down - >30000 bks

% 	bibd([rl,up,lex], 6, 60, 30, 3, 12).
% SUCCEEDS, 205 bks
% Row-major order, R to L: up - 205 bks, down - 632 bks.
% Row-major order: up - 1665 bks, down - 1176 bks.
% Static variable ordering [Frisch et al., 2002]: up - 5755 bks, down - >36000 bks

% 	bibd([rl,up,lex], 6, 70, 35, 3, 10).
% FAILS
% Row-major order, R to L: FAILURE

% 	bibd([rl,up,lex], 10, 90, 27, 3, 6).
% SUCCEEDS, 450 bks
% Row-major order, R to L: 450 bks

% 	bibd([rl,up,lex], 9, 108, 36, 3, 9).
% SUCCEEDS, 94694 bks
% Row-major order, R to L: 94694 bks

% 	bibd([rl,up,lex], 15, 70, 14, 3, 2).
% SUCCEEDS, 116 bks
% Row-major order, R to L: 116 bks

% 	bibd([rl,up,lex], 12, 88, 22, 3, 4).
% SUCCEEDS, 290 bks
% Row-major order, R to L: 290 bks

% 	bibd([rl,up,lex], 9, 120, 40, 3, 10).
% SUCCEEDS, 305 bks
% Row-major order, R to L: 496 bks

% 	bibd([rl,up,lex], 10, 120, 36, 3, 8).
% SUCCEEDS, 890 bks
% Row-major order, R to L: 890 bks

% 	bibd([rl,up,lex], 13, 104, 24, 3, 4).
% SUCCEEDS, 212 bks
% Row-major order, R to L: 212 bks

% 	bibd([rl,up,lex], 22, 33, 12, 8, 4).
% OPEN INSTANCE, >6247634 bks

bibd([Order,Lab,Lex], V, B, R, K, Lambda) :-
	bibd(Lex, V, B, R, K, Lambda, _Cells, Rows),
	bibd_order(Order, Rows, Vars),
	labeling([Lab], Vars),
	draw(Rows).

bibd_order(lr, Rows, Vars) :-
	conc(Rows, Vars, []).
bibd_order(rl, Rows, Vars) :-
	conc_rev(Rows, Vars, []).
bibd_order(lrr, Rows, Vars) :-
	conc(Rows, Rev, []),
	reverse(Rev, Vars).
bibd_order(rlr, Rows, Vars) :-
	conc_rev(Rows, Rev, []),
	reverse(Rev, Vars).


% append a list of lists
conc([]) --> [].
conc([X|Xs]) --> conc1(X), conc(Xs).

% append a list of reversed lists
conc_rev([]) --> [].
conc_rev([X|Xs]) --> conc2(X), conc_rev(Xs).

conc1([]) --> [].
conc1([X|Xs]) --> [X], conc1(Xs).

conc2([]) --> [].
conc2([X|Xs]) --> conc2(Xs), [X].

draw([]).
draw([Row|Rows]) :-
	asciify(Row, String),
	format('~s\n', [String]),
	draw(Rows).

asciify([], []).
asciify([R|Rs], [S|Ss]) :-
	S is R+"0",
	asciify(Rs, Ss).

bibd(lex, V, B, R, K, Lambda, Cells, Rows) :-
	VC is V*B,
	length(Cells, VC),
	domain(Cells, 0, 1),
	rows(Cells, B, Rows),
	columns(0, B, Rows, Columns),
	lex_chain(Rows, [op(#<)/*,among(R,R,[1])*/]),
	lex_chain(Columns, [op(#=<)/*,among(K,K,[1])*/]),
	each_sum(Rows, R),
	each_sum(Columns, K),
	each_product(Rows, Lambda).
bibd(antilex, V, B, R, K, Lambda, Cells, Rows) :-
	VC is V*B,
	length(Cells, VC),
	domain(Cells, 0, 1),
	rows(Cells, B, Rows),
	columns(0, B, Rows, Columns),
	reverse(Rows, RevRows),
	reverse(Columns, RevColumns),
	lex_chain(RevRows, [op(#<)/*,among(R,R,[1])*/]),
	lex_chain(RevColumns, [op(#=<)/*,among(K,K,[1])*/]),
	each_sum(Rows, R),
	each_sum(Columns, K),
	each_product(Rows, Lambda).

each_sum([], _).
each_sum([R|Rs], S) :-
	sum(R, #=, S),
	each_sum(Rs, S).

rows([], _, []) :- !.
rows(Cells, B, [Row|Rows]) :-
	row(0, B, Row, Cells, Cells1),
	rows(Cells1, B, Rows).

row(B, B, []) --> !.
row(I, B, [X|Xs]) --> [X],
	{J is I+1},
	row(J, B, Xs).

columns(B, B, _, []) :- !.
columns(I, B, Rows, [Col|Cols]) :-
	J is I+1,
	column(Rows, J, Col),
	columns(J, B, Rows, Cols).

column([], _, []).
column([Row|Rows], J, [C|Col]) :-
	nth1(J, Row, C),
	column(Rows, J, Col).

each_product([], _).
each_product([Row|Rows], Lambda) :-
	each_product(Rows, Row, Lambda),
	each_product(Rows, Lambda).

each_product([], _, _).
each_product([Row|Rows], Row0, Lambda) :-
	product(Row, Row0, S),
	sum(S, #=, Lambda),
	each_product(Rows, Row0, Lambda).

product([], [], []).
product([X|Xs], [Y|Ys], [XY|XYs]) :-
	X #/\ Y #<=> XY,
	product(Xs, Ys, XYs).


