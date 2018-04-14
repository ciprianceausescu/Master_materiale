% SCCS: @(#)str_suite.pl	21.2 05/16/95
% test file for structs package

:- load_files(library(str_decl), [when(compile_time)]).
:- use_module(library(structs)).
 
:- foreign_type
	intgr		= integer_32,
	bool		= enum([false,true]),
	position	= struct([	x:integer_32,
					y:integer_32
				]),
	size		= struct([	width:integer_16,
					height:integer_16
				]),
	mongo		= struct([	a:intgr,
					b:integer_16,
					c:integer_8,
					d:unsigned_16,
					e:unsigned_8,
					f:float_32,
					g:float,
					h:atom,
					i:string,
					j:address,
					k:array(81,integer_8),
					l:size,
					m:pointer(position),
					n:pointer(belch),
					o:bool,
					p:integer,
					q:pointer(mongo)
				 ]),
	uex		= union([	a:integer_32,
					b:integer,
					c:float
				]).
 
:- foreign_type
	region		= struct([size:size, position:position]),
	belch		= opaque.
 
:- foreign_type
	aliastest	= integer_32,
	aliasarray	= array(10, aliastest).

foreign_resource(str_suite,
	[test1,test2,test3,test4,test5,test6,
	 test7_1,test7_2,test8_1,test8_2,test8_3]).
 
foreign(test1, c, test1(+integer,+integer,[-pointer(position)])).
foreign(test2, c, test2(+integer,[-integer])).
foreign(test3, c, test3(+pointer(size),-integer,-integer)).
foreign(test4, c, test4(+atom, [-pointer(mongo)])).
foreign(test5, c, test5(+pointer(aliasarray), [-aliastest])).
foreign(test6, c, test6(+bool,-bool)).
foreign(test7_1, c, test7_1(+integer,[-pointer(uex)])).
foreign(test7_2, c, test7_2(+float,[-pointer(uex)])).
foreign(test8_1, c, test8_1(+pointer(uex),[-integer])).
foreign(test8_2, c, test8_2(+pointer(uex),[-integer])).
foreign(test8_3, c, test8_3(+pointer(uex),[-float])).

:- load_foreign_resource(str_suite).

%%% splfr -v --structs str_suite.pl str_suite.c
 
quiet :-
	dotest(quiet).

verbose :-
	dotest(verbose).

dotest(Verbosity) :-
	dotest1(Verbosity),
	dotest2(Verbosity),
	dotest3(Verbosity),
	dotest4(Verbosity),
	dotest5(Verbosity),
	dotest6(Verbosity),
	dotest7(Verbosity),
	dotest8(Verbosity).

dotest1(Verbosity) :-
	test(test1(345,678,Pos), Verbosity, 1),
	test(get_contents(Pos,x,345), Verbosity, 1),
	test(get_contents(Pos,y,678), Verbosity, 1).

dotest2(Verbosity) :-
	test(test2(9876,9877), Verbosity, 2).

dotest3(Verbosity) :-
	test(new(size,Size), Verbosity, 3),
	test(put_contents(Size,width,654), Verbosity, 3),
	test(put_contents(Size,height,456), Verbosity, 3),
	test(get_contents(Size,width,654), Verbosity, 3),
	test(get_contents(Size,height,456), Verbosity, 3),
	test(test3(Size,654,456), Verbosity, 3).

dotest4(Verbosity) :-
	test(test4(forty_two,Mongo), Verbosity, 4),
	test(get_contents(Mongo,a,42), Verbosity, 4),
	test(get_contents(Mongo,b,42), Verbosity, 4),
	test(get_contents(Mongo,c,42), Verbosity, 4),
	test(get_contents(Mongo,d,42), Verbosity, 4),
	test(get_contents(Mongo,e,42), Verbosity, 4),
	test(get_contents(Mongo,f,42.0), Verbosity, 4),
	test(get_contents(Mongo,g,42.0), Verbosity, 4),
	test(get_contents(Mongo,h,forty_two), Verbosity, 4),
	test(get_contents(Mongo,i,'forty-two'), Verbosity, 4),
	test(get_contents(Mongo,j,0), Verbosity, 4),
	test(get_address(Mongo,k,Array), Verbosity, 4),
	test(get_contents(Array,0,0'f), Verbosity, 4),
	test(get_contents(Array,1,0'o), Verbosity, 4),
	test(get_contents(Array,2,0'r), Verbosity, 4),
	test(get_contents(Array,3,0't), Verbosity, 4),
	test(get_address(Mongo,l,Size), Verbosity, 4),
	test(get_contents(Size,width,42), Verbosity, 4),
	test(get_contents(Size,height,42), Verbosity, 4),
	test(get_contents(Mongo,m,Position), Verbosity, 4),
	test(get_contents(Position,x,42), Verbosity, 4),
	test(get_contents(Position,y,42), Verbosity, 4),
	test(get_contents(Mongo,n,Null), Verbosity, 4),
	test(null_foreign_term(Null,belch), Verbosity, 4),
	test(get_contents(Mongo,o,true), Verbosity, 4),
	test(get_contents(Mongo,p,42), Verbosity, 4),
	test(get_contents(Mongo,q,Null2), Verbosity, 4),
	test(null_foreign_term(Null2,mongo), Verbosity, 4).


dotest5(Verbosity) :-
	test(new(aliasarray, Array), Verbosity, 5),
	test(put_contents(Array, 1, 42), Verbosity, 5),
	test(get_contents(Array, 1, N), Verbosity, 5),
	test(N =:= 42, Verbosity, 5),
	test(test5(Array, N1), Verbosity, 5),
	test(N1 =:= 42, Verbosity, 5),
	test(get_address(Array, 1, Ptr), Verbosity, 5),
	test(get_contents(Ptr, contents, N2), Verbosity, 5),
	test(N2 =:= 42, Verbosity, 5).

dotest6(Verbosity) :-
	test(test6(true, N), Verbosity, 6),
	test(N == false, Verbosity, 6),
	test(test6(false, true), Verbosity, 6).

dotest7(Verbosity) :-
    	test(test7_1(345,Uexp1), Verbosity, 7),
	test(get_contents(Uexp1,a,345), Verbosity, 7),
	test(test7_2(333.6,Uexp2), Verbosity, 7),
	test(get_contents(Uexp2,c,333.6), Verbosity, 7).

dotest8(Verbosity) :-
    	test(new(uex,Uexp), Verbosity, 8),
	test(put_contents(Uexp,a,654), Verbosity, 8),
	test(test8_1(Uexp,654), Verbosity, 8),
	test(put_contents(Uexp,b,142857), Verbosity, 8),
	test(test8_2(Uexp,142857), Verbosity, 8),
	test(put_contents(Uexp,c,333.6), Verbosity, 8),
	test(test8_3(Uexp,333.6), Verbosity, 8).
	
test(Goal, Verbosity, N) :-
	(   on_exception(Err,call(Goal),test_err(Err)) ->
		Success = succeeded
	;   Success = failed
	),
	(   (Success == failed ; Verbosity = verbose ) ->
		format('test ~w ~w:  ~q~n', [N, Success, Goal])
	;   true
	).

test_err(Err) :-
	print_message(error,Err),
	fail.


runtime_entry(start) :-
	(   unix(argv(['-q'])) ->
		quiet
	;   verbose
	).
