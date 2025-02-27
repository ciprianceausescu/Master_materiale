/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Car Sequencing Problem
 * Author    : Mats Carlsson
 * Adapted from code by Bjorn Carlson.
 * 
 * The problem is to fill partially filled 9x9 squares of 81 squares such that
 * each row and column are permutations of [1,...,9], and each 3x3 square, 
 * where the leftmost column modulo 3 is 0, is a permutation of [1,...,9].  
 */ 
:- module(suudoku, [suudoku/3]).
:- use_module(library(lists), [append/2]).
:- use_module(library(clpfd)).

suudoku(Lab, P, Consistency) :-
	Rows = [R1,R2,R3,R4,R5,R6,R7,R8,R9],
	problem(P, Rows),
	append(Rows, Vars),
	domain(Vars, 1, 9),
	row_constraint(Rows, Consistency),
	column_constraint(R1, R2, R3, R4, R5, R6, R7, R8, R9, Consistency),
	block_constraint(R1, R2, R3, Consistency),
	block_constraint(R4, R5, R6, Consistency),
	block_constraint(R7, R8, R9, Consistency),
	labeling(Lab, Vars),
	display_rows(Rows).

display_rows([]).
display_rows([[X1,X2,X3,X4,X5,X6,X7,X8,X9]|Rows]) :-
	format('~d ~d ~d ~d ~d ~d ~d ~d ~d \n', [X1,X2,X3,X4,X5,X6,X7,X8,X9]),
	display_rows(Rows).

row_constraint([], _Consistency).
row_constraint([R|Rt], Consistency) :-
	all_distinct(R, [consistency(Consistency)]),
	row_constraint(Rt, Consistency).

column_constraint([], [], [], [], [], [], [], [], [], _Consistency).
column_constraint([X1|R1], [X2|R2], [X3|R3], [X4|R4], [X5|R5], [X6|R6], [X7|R7], [X8|R8], [X9|R9], Consistency) :-
	all_distinct([X1,X2,X3,X4,X5,X6,X7,X8,X9], [consistency(Consistency)]),
	column_constraint(R1, R2, R3, R4, R5, R6, R7, R8, R9, Consistency).

block_constraint([], [], [], _Consistency).
block_constraint([X1,X2,X3|R1], [X4,X5,X6|R2], [X7,X8,X9|R3], Consistency) :-
	all_distinct([X1,X2,X3,X4,X5,X6,X7,X8,X9], [consistency(Consistency)]),
	block_constraint(R1, R2, R3, Consistency).

problem(1, P) :- % shokyuu
    P=[[1,_,_,8,_,4,_,_,_],
       [_,2,_,_,_,_,4,5,6],
       [_,_,3,2,_,5,_,_,_],
       [_,_,_,4,_,_,8,_,5],
       [7,8,9,_,5,_,_,_,_],
       [_,_,_,_,_,6,2,_,3],
       [8,_,1,_,_,_,7,_,_],
       [_,_,_,1,2,3,_,8,_],
       [2,_,5,_,_,_,_,_,9]].

problem(2, P) :-  % shokyuu
    P=[[_,_,2,_,3,_,1,_,_],
       [_,4,_,_,_,_,_,3,_],
       [1,_,5,_,_,_,_,8,2],
       [_,_,_,2,_,_,6,5,_],
       [9,_,_,_,8,7,_,_,3],
       [_,_,_,_,4,_,_,_,_],
       [8,_,_,_,7,_,_,_,4],
       [_,9,3,1,_,_,_,6,_],
       [_,_,7,_,6,_,5,_,_]].

problem(3, P) :-  % chuukyuu
    P=[[_,_,_,_,_,_,3,_,_],
       [_,_,_,8,5,_,_,1,_],
       [_,_,2,_,_,4,_,_,9],
       [_,3,_,_,_,2,_,_,4],
       [8,_,_,_,6,_,_,_,1],
       [7,_,_,9,_,_,_,5,_],
       [1,_,_,6,_,_,7,_,_],
       [_,9,_,_,2,3,_,_,_],
       [_,_,4,_,_,_,_,_,_]].

problem(4, P) :-  % joukyuu
    P=[[_,7,9,_,_,_,_,_,1],
       [6,_,_,_,_,_,3,8,_],
       [_,_,_,_,4,2,_,_,_],
       [_,_,3,9,_,_,_,_,_],
       [7,8,_,_,_,_,_,2,5],
       [_,_,_,_,_,4,8,_,_],
       [_,_,_,3,1,_,_,_,_],
       [_,5,6,_,_,_,_,_,7],
       [2,_,_,_,_,_,4,3,_]].

problem(5, P) :-  % shokyuu; from Mr. Horai
    P=[[_,5,_,7,_,1,_,4,_],
       [7,_,3,_,_,_,1,_,2],
       [_,8,_,4,_,6,_,9,_],
       [9,_,4,_,6,_,8,_,3],
       [_,_,_,8,_,7,_,_,_],
       [1,_,8,_,5,_,6,_,9],
       [_,1,_,6,_,3,_,8,_],
       [5,_,6,_,_,_,7,_,1],
       [_,3,_,5,_,9,_,2,_]].

problem(6, P) :- % Hard: suudoku2 99 (1989)
    P=[[8,_,_,_,_,5,_,_,_],
       [_,1,2,3,_,_,6,_,_],
       [_,4,5,6,_,_,_,2,_],
       [_,7,8,_,_,_,_,_,1],
       [_,_,_,_,9,_,_,_,_],
       [9,_,_,_,_,_,8,7,_],
       [_,2,_,_,_,6,5,4,_],
       [_,_,4,_,_,3,2,1,_],
       [_,_,_,1,_,_,_,_,9]].

end_of_file.

| ?- suudoku(P).
1 5 6 8 9 4 3 2 7 
9 2 8 7 3 1 4 5 6 
4 7 3 2 6 5 9 1 8 
3 6 2 4 1 7 8 9 5 
7 8 9 3 5 2 6 4 1 
5 1 4 9 8 6 2 7 3 
8 3 1 5 4 9 7 6 2 
6 9 7 1 2 3 5 8 4 
2 4 5 6 7 8 1 3 9 
P = 1 ? ;
7 8 2 4 3 5 1 9 6 
6 4 9 8 2 1 7 3 5 
1 3 5 7 9 6 4 8 2 
3 7 4 2 1 9 6 5 8 
9 6 1 5 8 7 2 4 3 
5 2 8 6 4 3 9 7 1 
8 5 6 9 7 2 3 1 4 
2 9 3 1 5 4 8 6 7 
4 1 7 3 6 8 5 2 9 
P = 2 ? ;
4 5 8 2 9 1 3 6 7 
3 7 9 8 5 6 4 1 2 
6 1 2 3 7 4 5 8 9 
9 3 6 5 1 2 8 7 4 
8 2 5 4 6 7 9 3 1 
7 4 1 9 3 8 2 5 6 
1 8 3 6 4 9 7 2 5 
5 9 7 1 2 3 6 4 8 
2 6 4 7 8 5 1 9 3 
P = 3 ? ;
4 7 9 8 6 3 2 5 1 
6 1 2 5 9 7 3 8 4 
5 3 8 1 4 2 6 7 9 
1 2 3 9 8 5 7 4 6 
7 8 4 6 3 1 9 2 5 
9 6 5 2 7 4 8 1 3 
8 4 7 3 1 9 5 6 2 
3 5 6 4 2 8 1 9 7 
2 9 1 7 5 6 4 3 8 
P = 4 ? ;
6 5 9 7 2 1 3 4 8 
7 4 3 9 8 5 1 6 2 
2 8 1 4 3 6 5 9 7 
9 7 4 1 6 2 8 5 3 
3 6 5 8 9 7 2 1 4 
1 2 8 3 5 4 6 7 9 
4 1 2 6 7 3 9 8 5 
5 9 6 2 4 8 7 3 1 
8 3 7 5 1 9 4 2 6 
P = 5 ? ;
8 6 9 2 7 5 1 3 4 
7 1 2 3 4 9 6 8 5 
3 4 5 6 1 8 9 2 7 
4 7 8 5 6 2 3 9 1 
2 3 1 8 9 7 4 5 6 
9 5 6 4 3 1 8 7 2 
1 2 7 9 8 6 5 4 3 
6 9 4 7 5 3 2 1 8 
5 8 3 1 2 4 7 6 9 
P = 6 ? ;
no
