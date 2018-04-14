/* Copyright(C) 1997, Swedish Institute of Computer Science */

:- meta_predicate
	labeling(:, ?),
	minimize(:, ?),
	maximize(:, ?).

:- dynamic incumbent/2.

labeling(Options, Variables) :-
	Goal = labeling(Options,Variables),
	prolog:get_module(Options, Options2, _),
	must_be(Options2, proper_list(callable), Goal, 1),
	labeling_options(Options2, opt(leftmost,all,step(up),_,33554431,any),
				   opt(Sel,Sol,Enum,K,DU,TimeOut),
				   Variables, Goal),
	must_be_list_of_finite_dvar(Variables, Goal, 2),
	labeling(Sol, Variables, Sel, Enum, K, DU, TimeOut).

labeling_options([], Opt, Opt, _, _).
labeling_options([X|L], Opt0, Opt, Vs, Goal) :- !,
	(   labeling_option(X, Goal, Opt0, Opt1) -> true
        ;   illarg(domain(term,labeling_options), Goal, 1, X)
        ),
	labeling_options(L, Opt1, Opt, Vs, Goal).

% statistics option:
labeling_option(assumptions(K), _,
		opt(Sel,Sol,Enum,_,DU,TO),
		opt(Sel,Sol,Enum,K,DU,TO)).
% variable choice options:
labeling_option(leftmost, _,
		opt(_,Sol,Enum,K,DU,TO),
		opt(leftmost,Sol,Enum,K,DU,TO)).
labeling_option(min, _,
		opt(_,Sol,Enum,K,DU,TO),
		opt(min,Sol,Enum,K,DU,TO)).
labeling_option(max, _,
		opt(_,Sol,Enum,K,DU,TO),
		opt(max,Sol,Enum,K,DU,TO)).
labeling_option(ff, _,
		opt(_,Sol,Enum,K,DU,TO),
		opt(ff,Sol,Enum,K,DU,TO)).
labeling_option(ffc, _,
		opt(_,Sol,Enum,K,DU,TO),
		opt(ffc,Sol,Enum,K,DU,TO)).
labeling_option(variable(Sel), Goal,
		opt(_,Sol,Enum,K,DU,TO),
		opt(variable(M:Sel),Sol,Enum,K,DU,TO)) :-
	Goal = labeling(Options,_),
	prolog:get_module(Options, _, M).
% value choice options
labeling_option(enum, _,
		opt(Sel,Sol,Enum,K,DU,TO),
		opt(Sel,Sol,enum(A),K,DU,TO)) :-
	arg(1, Enum, A).
labeling_option(step, _,
		opt(Sel,Sol,Enum,K,DU,TO),
		opt(Sel,Sol,step(A),K,DU,TO)) :-
	arg(1, Enum, A).
labeling_option(bisect, _,
		opt(Sel,Sol,Enum,K,DU,TO),
		opt(Sel,Sol,bisect(A),K,DU,TO)) :-
	arg(1, Enum, A).
labeling_option(up, _,
		opt(Sel,Sol,Enum0,K,DU,TO),
		opt(Sel,Sol,Enum,K,DU,TO)) :-
	Enum0 =.. [F|_], Enum =.. [F,up].
labeling_option(down, _,
		opt(Sel,Sol,Enum0,K,DU,TO),
		opt(Sel,Sol,Enum,K,DU,TO)) :-
	Enum0 =.. [F|_], Enum =.. [F,down].
labeling_option(value(Enum), Goal,
		opt(Sel,Sol,_,K,DU,TO),
		opt(Sel,Sol,value(M:Enum),K,DU,TO)) :-
	Goal = labeling(Options,_),
	prolog:get_module(Options, _, M).
% solution options
labeling_option(discrepancy(DU), _,
		opt(Sel,Sol,Enum,K,_,TO),
		opt(Sel,Sol,Enum,K,DU,TO)).
labeling_option(all, _,
		opt(Sel,_,Enum,K,DU,TO),
		opt(Sel,all,Enum,K,DU,TO)).
labeling_option(minimize(X), Goal,
		opt(Sel,_,Enum,K,DU,TO),
		opt(Sel,minimize(X,Goal),Enum,K,DU,TO)) :-
	arg_attribute(X, _, Goal, 1).
labeling_option(maximize(X), Goal,
		opt(Sel,_,Enum,K,DU,TO),
		opt(Sel,maximize(X,Goal),Enum,K,DU,TO)) :-
	arg_attribute(X, _, Goal, 1).
labeling_option(time_out(Limit,Flag), _,
		opt(Sel,Sol,Enum,K,DU,_),
		opt(Sel,Sol,Enum,K,DU,time_out(Limit,Flag))).

labeling(all, Variables, Sel, Enum, K, DU, TO) :-
	'$fd_debug'(FDBG,FDBG,1),
	nobb_labeling_top(TO, Variables, param(Sel,Enum,FDBG), 0, K, nobb(DU)).
labeling(minimize(Value,Goal), Variables, Sel, Enum, K, DU, TO) :-
	list_of_zeros(Variables, Zeros),
	'$fd_minint_maxint'(_Minint, Maxint),
	asserta(incumbent(Maxint,Zeros), Ref),
	call_cleanup(bb_labeling_top(TO, Variables, Sel, Enum, 0, K,
				     bb(minimize(Value),Ref,Goal,DU)),
	             erase(Ref)).
labeling(maximize(Value,Goal), Variables, Sel, Enum, K, DU, TO) :-
	list_of_zeros(Variables, Zeros),
	'$fd_minint_maxint'(Minint, _Maxint),
	asserta(incumbent(Minint,Zeros), Ref),
	call_cleanup(bb_labeling_top(TO, Variables, Sel, Enum, 0, K,
				     bb(maximize(Value),Ref,Goal,DU)),
	             erase(Ref)).

list_of_zeros([], []).
list_of_zeros([_|L1], [0|L2]) :- list_of_zeros(L1, L2).

bb_labeling_top(any, Variables, Sel, Enum, I, K, BB) :-
	bb_labeling(Variables, Sel, Enum, I, K, BB, _).
bb_labeling_top(time_out(Time,Flag), Variables, Sel, Enum, I, K, BB) :-
	time_out(clpfd:bb_labeling_timeout(Variables, Sel, Enum, I, K, BB, Flag),
		 Time, _).

bb_labeling_timeout(Variables, Sel, Enum, I, K, BB, Flag) :-
	BB = bb(MinMax,Ref,_,_), 
	on_exception(time_out,
		     bb_labeling(Variables, Sel, Enum, I, K, BB, Flag),
		     bb_labeling_2(MinMax, Ref, Variables, Flag)).

bb_labeling(Variables, Sel, Enum, I, K, BB, _) :-
	BB = bb(MinMax,Ref,Goal,_),
	'$fd_debug'(FDBG,FDBG,1),
	nobb_labeling(Variables, param(Sel,Enum,FDBG), I, K, BB),
	arg(1, MinMax, Value),
	(   var(Value) -> illarg(var, Goal, 1)
        ;   prolog:'$ptr_ref'(Ptr, Ref),
	    '$fd_update_incumbent'(Ptr, Value, Variables)
        ),
	fail.
bb_labeling(Variables, _, _, _, _, bb(MinMax,Ref,_,_), success) :-
	bb_labeling_2(MinMax, Ref, Variables, _).

bb_labeling_2(minimize(ValueV), Ref, VariablesV, time_out) :-
	clause(incumbent(Value,Variables), true, Ref),
	'$fd_minint_maxint'(_Minint, Maxint),
	Value < Maxint,
	unify_list([ValueV|VariablesV], [Value|Variables], 1).
bb_labeling_2(maximize(ValueV), Ref, VariablesV, time_out) :-
	clause(incumbent(Value,Variables), true, Ref),
	'$fd_minint_maxint'(Minint, _Maxint),
	Value > Minint,
	unify_list([ValueV|VariablesV], [Value|Variables], 1).

% [MC] 3.11.1 begin
% Nice idea, but in theory and practice, only pays off on loosely
% constrained problems with small domains.  Worst case O(d*n*lg n),
% n variables, d domain elements.
% nobb_labeling_top(L1, Param, I, K, BB) :-
% 	Param = param(Key1,Enum,FDBG),
% 	do_heap_labeling(Key1, L1, Key2, L2), !,
% 	Global = global(StateM,true,StatusM,_Ent,true),
% 	create_mutable(0, StatusM),
% 	State1 = state(L2,Key2,_/*handle*/,0/*stamp*/),
% 	create_mutable(State1, StateM),
% 	'$fd_labeling'(State1, State2, X2, Global),
% 	update_mutable(State2, StateM),
% 	heap_labeling(X2, StateM, L2, Enum, FDBG, I, K, BB).
% nobb_labeling_top(L, Param, I, K, BB) :-
% 	nobb_labeling(L, Param, I, K, BB).

% do_heap_labeling(min, L, min, L).
% do_heap_labeling(max, L, max, L).
% do_heap_labeling(ff , L, dom, L).
% do_heap_labeling(ffc, L1,dom, L2) :-
% 	tag_by_degree(L1, KL1),
% 	keysort(KL1, KL2),
% 	values(KL2, L2).

% tag_by_degree([], []).
% tag_by_degree([X|L1], [NegDegree-X|L2]) :-
% 	var(X), !,
% 	get_fd_suspensions(X, Lists),
% 	arg(1, Lists, Degree),
% 	NegDegree is -Degree,
% 	tag_by_degree(L1, L2).
% tag_by_degree([_|L1], L2) :-
% 	tag_by_degree(L1, L2).

% heap_labeling(X1, StateM, L, Enum, FDBG, I, K, BB) :-
% 	var(X1), !,
% 	fdbg_start_labeling(FDBG, X1),
% 	labeling_cont(Enum, X1, L, L, _, BB, BB1),
% 	J is I+1,
% 	get_mutable(State1, StateM),
% 	'$fd_labeling'(State1, State2, X2, _),
% 	update_mutable(State2, StateM),
% 	heap_labeling(X2, StateM, L, Enum, FDBG, J, K, BB1).
% heap_labeling(_, _, _, _, _, K, K, _).
% [MC] 3.11.1 end
	

nobb_labeling_top(any, L, Param, I, K, BB) :-
	nobb_labeling_fast(L, Param, I, K, BB).
nobb_labeling_top(time_out(Time,Flag), L, Param, I, K, BB) :-
	time_out(clpfd:nobb_labeling_fast(L, Param, I, K, BB), Time, Flag).

nobb_labeling_fast(L, Param, I, K, nobb(33554431)) :-
	labeling_accelerator(Param, A), !,
	nobb_labeling_fast(A, L, I, K).
nobb_labeling_fast(L, Param, I, K, BB) :-
	nobb_labeling(L, Param, I, K, BB).

labeling_accelerator(param(Sel,Enum,off), Case) :-
	labeling_accelerator(Sel, Enum, Case).

labeling_accelerator(leftmost, enum(X), leftmost_enum(X)).
labeling_accelerator(leftmost, step(X), leftmost_step(X)).
labeling_accelerator(leftmost, bisect(X), leftmost_bisect(X)).
labeling_accelerator(ff, enum(X), ff_enum(X)).
labeling_accelerator(ff, step(X), ff_step(X)).
labeling_accelerator(ff, bisect(X), ff_bisect(X)).

nobb_labeling_fast(leftmost_enum(X), L, I, K) :-
	nobb_labeling_leftmost_enum(L, X, I, K).
nobb_labeling_fast(leftmost_step(X), L, I, K) :-
	nobb_labeling_leftmost_step(L, X, I, K).
nobb_labeling_fast(leftmost_bisect(X), L, I, K) :-
	nobb_labeling_leftmost_bisect(L, X, I, K).
nobb_labeling_fast(ff_enum(X), L, I, K) :-
	nobb_labeling_ff_enum(L, X, I, K).
nobb_labeling_fast(ff_step(X), L, I, K) :-
	nobb_labeling_ff_step(L, X, I, K).
nobb_labeling_fast(ff_bisect(X), L, I, K) :-
	nobb_labeling_ff_bisect(L, X, I, K).

nobb_labeling_leftmost_enum([], _, K, K).
nobb_labeling_leftmost_enum([X|L], Param, I, K) :-
	var(X), !,
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(Set,_Min,_Max,_Size),
	indomain_fast(Param, X, Set),
	J is I+1,
	nobb_labeling_leftmost_enum(L, Param, J, K).
nobb_labeling_leftmost_enum([_|L], Param, I, K) :-
	nobb_labeling_leftmost_enum(L, Param, I, K).

nobb_labeling_leftmost_step([], _, K, K).
nobb_labeling_leftmost_step([X|L], Param, I, K) :-
	var(X), !,
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(_Set,Min,Max,_Size),
	labeling_step_fast(Param, Min, Max, X),
	J is I+1,
	nobb_labeling_leftmost_step([X|L], Param, J, K).
nobb_labeling_leftmost_step([_|L], Param, I, K) :-
	nobb_labeling_leftmost_step(L, Param, I, K).

nobb_labeling_leftmost_bisect([], _, K, K).
nobb_labeling_leftmost_bisect([X|L], Param, I, K) :-
	var(X), !,
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(_Set,Min,Max,_Size),
	labeling_bisect_fast(Param, Min, Max, X),
	J is I+1,
	nobb_labeling_leftmost_bisect([X|L], Param, J, K).
nobb_labeling_leftmost_bisect([_|L], Param, I, K) :-
	nobb_labeling_leftmost_bisect(L, Param, I, K).

nobb_labeling_ff_enum([], _, K, K).
nobb_labeling_ff_enum([X0|L], Param, I, K) :-
	var(X0), !,
	'$fd_delete'([X0|L], X, 3),
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(Set,_Min,_Max,_Size),
	indomain_fast(Param, X, Set),
	J is I+1,
	nobb_labeling_ff_enum([X0|L], Param, J, K).
nobb_labeling_ff_enum([_|L], Param, I, K) :-
	nobb_labeling_ff_enum(L, Param, I, K).

nobb_labeling_ff_step([], _, K, K).
nobb_labeling_ff_step([X0|L], Param, I, K) :-
	var(X0), !,
	'$fd_delete'([X0|L], X, 3),
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(_Set,Min,Max,_Size),
	labeling_step_fast(Param, Min, Max, X),
	J is I+1,
	nobb_labeling_ff_step([X0|L], Param, J, K).
nobb_labeling_ff_step([_|L], Param, I, K) :-
	nobb_labeling_ff_step(L, Param, I, K).

nobb_labeling_ff_bisect([], _, K, K).
nobb_labeling_ff_bisect([X0|L], Param, I, K) :-
	var(X0), !,
	'$fd_delete'([X0|L], X, 3),
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(_Set,Min,Max,_Size),
	labeling_bisect_fast(Param, Min, Max, X),
	J is I+1,
	nobb_labeling_ff_bisect([X0|L], Param, J, K).
nobb_labeling_ff_bisect([_|L], Param, I, K) :-
	nobb_labeling_ff_bisect(L, Param, I, K).

labeling_step_fast(up, Min, _, X) :-
	labeling_singleton(X, Min, step).
labeling_step_fast(up, Min, _, X) :-
	Min1 is Min+1, 
	labeling_min(X, Min1, step).
labeling_step_fast(down, _, Max, X) :-
	labeling_singleton(X, Max, step).
labeling_step_fast(down, _, Max, X) :-
	Max1 is Max-1, 
	labeling_max(X, Max1, step).

labeling_bisect_fast(up, Min, Max, X) :-
	N is (Min+Max)>>1,
	labeling_max(X, N, bisect).
labeling_bisect_fast(up, Min, Max, X) :-
	N is (Min+Max)>>1,
	N1 is N+1, 
	labeling_min(X, N1, bisect).
labeling_bisect_fast(down, Min, Max, X) :-
	N is (Min+Max)>>1,
	N1 is N+1, 
	labeling_min(X, N1, bisect).
labeling_bisect_fast(down, Min, Max, X) :-
	N is (Min+Max)>>1,
	labeling_max(X, N, bisect).

indomain_fast(up, X, [[A|B]|R]) :-
	indomain_fast(R, A, B, Val),
	labeling_singleton(X, Val, indomain_up).
indomain_fast(down, X, R) :-
	reverse(R, [[A|B]|R1]),
	indomain_fast_rev(R1, A, B, Val),
	labeling_singleton(X, Val, indomain_down).

indomain_fast([], A, A, V) :- !,
	V = A.
indomain_fast(_, A, _, A).
indomain_fast(R, A, B, V) :-
	A < B, !,
	A1 is A+1,
	indomain_fast(R, A1, B, V).
indomain_fast([[A|B]|R], _, _, V) :-
	indomain_fast(R, A, B, V).

indomain_fast_rev([], B, B, V) :- !,
	V = B.
indomain_fast_rev(_, _, B, B).
indomain_fast_rev(R, A, B, V) :-
	A < B, !,
	B1 is B-1,
	indomain_fast_rev(R, A, B1, V).
indomain_fast_rev([[A|B]|R], _, _, V) :-
	indomain_fast_rev(R, A, B, V).


% labeling, general case
nobb_labeling([], _, K, K, _).
nobb_labeling(LL, Param, I, K, BB) :-
	LL = [X|_],
	var(X), !,
	Param = param(Selector,Enum,FDBG),
	delete(Selector, LL, X1, L1),
	fdbg_start_labeling(FDBG, X1),
	labeling_cont(Enum, X1, L1, LL, R, BB, BB1),
	J is I+1,
	nobb_labeling(R, Param, J, K, BB1).
nobb_labeling([_|L], Param, I, K, BB) :-
	nobb_labeling(L, Param, I, K, BB).


%% SzT 2001.09.10, changes for FDBG

fdbg_start_labeling(Var) :-
	'$fd_debug'(FDBG,FDBG,1),
	fdbg_start_labeling(FDBG, Var).

% to indicate the start and failure of labeling
% FDBG puts advice on this!
fdbg_start_labeling(off, _Var).
fdbg_start_labeling(on, _Var).
fdbg_start_labeling(on, _Var) :- fail.

% to indicate one labeling step in user-defined labeling
% FDBG puts advice on this!
fdbg_labeling_step(_Var, _Step).

% the built-in labeling uses the following predicates to indicate a
% labeling step and to make the appropriate narrowing (reducing the
% domain to a singleton, changing the maximum, or changing the minimum,
% respectively)

% FDBG puts advice on this!
labeling_singleton(T, C, _Mode) :-
	'$fd_in_interval'(T, C, C, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

% FDBG puts advice on this!
labeling_max(T, C, _Mode) :-
	'$fd_in_interval'(T, inf, C, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

% FDBG puts advice on this!
labeling_min(T, C, _Mode) :-
	'$fd_in_interval'(T, C, sup, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).


labeling_cont(value(Enum), X, L, LL, LL, BB, BB1) :-
	call(Enum, X, L, BB, BB1).
labeling_cont(enum(Arg), X, L, _LL, L, BB, BB1) :-
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(Set,_Min,_Max,_Size),
	indomain(Arg, X, Set, BB, BB1).
labeling_cont(step(Arg), X, L, LL, R, BB, BB1) :-
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(_Set,Min,Max,_Size),
	labeling_step(Arg, Min, Max, X, L, LL, R, BB, BB1).
labeling_cont(bisect(Arg), X, _L, LL, LL, BB, BB1) :-
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(_Set,Min,Max,_Size),
	labeling_bisect(Arg, Min, Max, X, BB, BB1).

/*** obsolete as of 3.9
labeling_dual(up, X, _, Var, BB, BB) :-
	labeling_singleton(Var, X, dual).
labeling_dual(up, _, X, Var, BB, BB1) :-
	later_bound(BB, BB1),
	labeling_singleton(Var, X, dual).
labeling_dual(down, _, X, Var, BB, BB) :-
	labeling_singleton(Var, X, dual).
labeling_dual(down, X, _, Var, BB, BB1) :-
	later_bound(BB, BB1),
	labeling_singleton(Var, X, dual).
***/

labeling_step(up, Min, _, X, L, _,  L, BB, BB) :-
	labeling_singleton(X, Min, step).
labeling_step(up, Min, _, X,   _, LL, LL, BB, BB1) :-
	later_bound(BB, BB1),	% TODO: keeps X
	Min1 is Min+1, 
	labeling_min(X, Min1, step).
labeling_step(down, _, Max, X, L, _,  L, BB, BB) :-
	labeling_singleton(X, Max, step).
labeling_step(down, _, Max, X,   _, LL, LL, BB, BB1) :-
	later_bound(BB, BB1),	% TODO: keeps X
	Max1 is Max-1, 
	labeling_max(X, Max1, step).

labeling_bisect(up, Min, Max, X, BB, BB) :-
	N is (Min+Max)>>1,
	labeling_max(X, N, bisect).
labeling_bisect(up, Min, Max, X, BB, BB1) :-
	later_bound(BB, BB1),	% TODO: var count?
	N is (Min+Max)>>1,
	N1 is N+1, 
	labeling_min(X, N1, bisect).

labeling_bisect(down, Min, Max, X, BB, BB) :-
	N is (Min+Max)>>1,
	N1 is N+1, 
	labeling_min(X, N1, bisect).
labeling_bisect(down, Min, Max, X, BB, BB1) :-
	later_bound(BB, BB1),	% TODO: var count?
	N is (Min+Max)>>1,
	labeling_max(X, N, bisect).


indomain(X) :-
	integer(X), !.
indomain(X) :-
	var(X),
	get_atts(X, fd_attribute(_,DomM,_)),
	get_mutable(Dom, DomM),
	Dom = dom(Set,Min,Max,_Size),
	integer(Min),
	integer(Max), !,
	Size is Max-Min+1,
	BB = nobb(Size),
	'$fd_debug'(FDBG,FDBG,1),
	fdbg_start_labeling(FDBG, X),
	indomain(up, X, Set, BB, _).
indomain(X) :-
	fd_argument_error(indomain(X), 1, X).

% precondition: X is not connected as in Constraint #<=> X
indomain(up, X, [[A|B]|R], BB, BB1) :-
	indomain(R, A, B, Val, BB, BB1),
	labeling_singleton(X, Val, indomain_up).
indomain(down, X, R, BB, BB1) :-
	reverse(R, [[A|B]|R1]),
	indomain_rev(R1, A, B, Val, BB, BB1),
	labeling_singleton(X, Val, indomain_down).

% the first clause is redundant, but avoids leaving choicepoint behind
indomain([], A, A, V, BB, BB1) :- !,
	BB1 = BB,
	V = A.
indomain(_, A, _, A, BB, BB).
indomain(R, A, B, V, BB, BB1) :-
	A < B, !,
	A1 is A+1,
	indomain_later(R, A1, B, V, BB, BB1).
indomain([[A|B]|R], _, _, V, BB, BB1) :-
	indomain_later(R, A, B, V, BB, BB1).

% the first clause is redundant, but avoids leaving choicepoint behind
indomain_rev([], B, B, V, BB, BB1) :- !,
	BB1 = BB,
	V = B.
indomain_rev(_, _, B, B, BB, BB).
indomain_rev(R, A, B, V, BB, BB1) :-
	A < B, !,
	B1 is B-1,
	indomain_rev_later(R, A, B1, V, BB, BB1).
indomain_rev([[A|B]|R], _, _, V, BB, BB1) :-
	indomain_rev_later(R, A, B, V, BB, BB1).

% the first clause is redundant, but avoids leaving choicepoint behind
indomain_later([], A, A, V, BB, BB1) :- !,
	later_bound(BB, BB1),
	V = A.
indomain_later(_, A, _, V, BB, BB1) :-
	later_bound(BB, BB1),
	V = A.
indomain_later(R, A, B, V, BB, BB1) :-
	A < B, !,
	A1 is A+1,
	indomain_later(R, A1, B, V, BB, BB1).
indomain_later([[A|B]|R], _, _, V, BB, BB1) :-
	indomain_later(R, A, B, V, BB, BB1).

% the first clause is redundant, but avoids leaving choicepoint behind
indomain_rev_later([], B, B, V, BB, BB1) :- !,
	later_bound(BB, BB1),
	V = B.
indomain_rev_later(_, _, B, V, BB, BB1) :-
	later_bound(BB, BB1),
	V = B.
indomain_rev_later(R, A, B, V, BB, BB1) :-
	A < B, !,
	B1 is B-1,
	indomain_rev_later(R, A, B1, V, BB, BB1).
indomain_rev_later([[A|B]|R], _, _, V, BB, BB1) :-
	indomain_rev_later(R, A, B, V, BB, BB1).

%%% The bounding rule if branch-and-bound search.

first_bound(BB, BB).

%% [MC] 3.8.6: made determinate
later_bound(nobb(DU0), nobb(DU)) :-
	DU0>0, DU is DU0-1.
later_bound(bb(minimize(Value),Ref,Goal,DU0),
	    bb(minimize(Value),Ref,Goal,DU)) :- !,
	DU0>0, DU is DU0-1,
	prolog:'$ptr_ref'(Ptr, Ref),
	'$fd_incumbent_bound'(Ptr, Bound),
	Min is Bound-1,
	'$fd_in_interval'(Value, inf, Min, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
later_bound(bb(maximize(Value),Ref,Goal,DU0),
	    bb(maximize(Value),Ref,Goal,DU)) :-
	DU0>0, DU is DU0-1,
	prolog:'$ptr_ref'(Ptr, Ref),
	'$fd_incumbent_bound'(Ptr, Bound),
	Max is Bound+1,
	'$fd_in_interval'(Value, Max, sup, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).


domain_set(dom(X,_,_,_), X).

domain_min(dom(_,X,_,_), X).

domain_max(dom(_,_,X,_), X).

domain_size(dom(_,_,_,X), X).

fd_min(X, Min) :-
	var(X), !,
	(   get_fd_domain(X, Dom)
	->  domain_min(Dom, Min)
	;   Min = inf
	).
fd_min(X, Min) :-
	integer(X), !, Min = X.
fd_min(X, Min) :-
	fd_argument_error(fd_min(X,Min), 1, X).

fd_max(X, Max) :-
	var(X), !,
	(   get_fd_domain(X, Dom)
	->  domain_max(Dom, Max)
	;   Max = sup
	).
fd_max(X, Max) :-
	integer(X), !, Max = X.
fd_max(X, Max) :-
	fd_argument_error(fd_max(X,Max), 1, X).

fd_size(X, Size) :-
	var(X), !,
	(   get_fd_domain(X, Dom)
	->  domain_size(Dom, Size)
	;   Size = sup
	).
fd_size(X, Size) :-
	integer(X), !, Size = 1.
fd_size(X, Size) :-
	fd_argument_error(fd_size(X,Size), 1, X).

fd_degree(X, Degree) :-
	var(X), !,
	(   get_fd_suspensions(X, Lists)
	->  arg(1, Lists, Degree)
	;   Degree = 0
	).
fd_degree(X, Degree) :-
	integer(X), !, Degree = 0.
fd_degree(X, Degree) :-
	fd_argument_error(fd_degree(X,Degree), 1, X).

fd_set(X, Copy) :-
	var(X), !,
	(   get_fd_domain(X, Dom)
	->  domain_set(Dom, Set)
	;   Set = [[inf|sup]]
	),
	Set = [[A|B]|C],
	Copy = [[A|B]|C].
fd_set(X, Set) :-
	integer(X),
	\+prolog:'$large_data'(0, X, _), !,
	Set = [[X|X]].
fd_set(X, Set) :-
	fd_argument_error(fd_set(X,Set), 1, X).

fd_dom(X, R) :-
	var(X), !,
	(   get_fd_domain(X, Dom)
	->  domain_set(Dom, Set),
	    fdset_to_range(Set, R)
	;   R = (inf..sup)
	).
fd_dom(X, R) :-
	integer(X),
	\+prolog:'$large_data'(0, X, _), !,
	R = {X}.
fd_dom(X, Dom) :-
	fd_argument_error(fd_dom(X,Dom), 1, X).

% [MC] 3.11.1 begin
delete(leftmost, [X|L], X, L).
delete(min, LL, X1, LL) :-
	'$fd_delete'(LL, X1, 1).
% 	LL = [X|L],
% 	fd_min(X, Rank),
% 	deletemin(L, Rank, X, X1).
delete(max, LL, X1, LL) :-
	'$fd_delete'(LL, X1, 2).
% 	LL = [X|L],
% 	fd_max(X, Rank),
% 	deletemax(L, Rank, X, X1).
delete(ff, LL, X1, LL) :-
	'$fd_delete'(LL, X1, 3).
% 	LL = [X|L],
% 	fd_rank(X, Rank),
% 	deleteff(L, Rank, X, X1).
delete(ffc, LL, X1, LL) :-
	'$fd_delete'(LL, X1, 4).
% 	LL = [X|L],
% 	fd_rankc(X, Rank),
% 	deleteffc(L, Rank, X, X1).
delete(variable(Sel), LL, X1, L1) :-
	call(Sel, LL, X1, L1).

% deletexx(+Vars, +Rank, +Best, -Var).

% deletemin([], _, X, X).
% deletemin([X0|L0], Rank1, X1, X) :-
% 	(   integer(X0)
% 	->  deletemin(L0, Rank1, X1, X)
% 	;   fd_min(X0, Rank0),
% 	    (   Rank0 < Rank1
% 	    ->  deletemin(L0, Rank0, X0, X)
% 	    ;   deletemin(L0, Rank1, X1, X)
% 	    )
% 	).

% deletemax([], _, X, X).
% deletemax([X0|L0], Rank1, X1, X) :-
% 	(   integer(X0)
% 	->  deletemax(L0, Rank1, X1, X)
% 	;   fd_max(X0, Rank0),
% 	    (   Rank0 > Rank1
% 	    ->  deletemax(L0, Rank0, X0, X)
% 	    ;   deletemax(L0, Rank1, X1, X)
% 	    )
% 	).

% deleteff(_ , 2, X, X) :- !.
% deleteff([], _, X, X).
% deleteff([X0|L0], Rank1, X1, X) :-
% 	(   integer(X0)
% 	->  deleteff(L0, Rank1, X1, X)
% 	;   fd_rank(X0, Rank0),
% 	    (   Rank0 @< Rank1
% 	    ->  deleteff(L0, Rank0, X0, X)
% 	    ;   deleteff(L0, Rank1, X1, X)
% 	    )
% 	).

% deleteffc([], _, X, X).
% deleteffc([X0|L0], Rank1, X1, X) :-
% 	(   integer(X0)
% 	->  deleteffc(L0, Rank1, X1, X)
% 	;   fd_rankc(X0, Rank0),
% 	    (   Rank0 @< Rank1
% 	    ->  deleteffc(L0, Rank0, X0, X)
% 	    ;   deleteffc(L0, Rank1, X1, X)
% 	    )
% 	).

% fd_rank(X, Dsize) :-
% 	get_fd_domain(X, Dom),
% 	domain_size(Dom, Dsize).

% fd_rankc(X, Dsize-NegDegree) :-
% 	get_fd_domain(X, Dom),
% 	domain_size(Dom, Dsize),
% 	get_fd_suspensions(X, Lists),
% 	arg(1, Lists, Degree),
% 	NegDegree is -Degree.

minimize(Goal, Var) :-
	findall(Goal-Var, (Goal -> true), [Best1-UB1]),
	minimize(Goal, Var, Best1, UB1).

minimize(Goal, Var, _, UB) :- var(UB), !,
	illarg(var, minimize(Goal,Var), 2).
minimize(Goal, Var, _, UB) :-
	Var #< UB,
	findall(Goal-Var, (Goal -> true), [Best1-UB1]), !,
	minimize(Goal, Var, Best1, UB1).
minimize(Goal, Var, Best, UB) :-
	minmax_unify(Goal, Var, Best, UB).

maximize(Goal, Var) :-
	findall(Goal-Var, (Goal -> true), [Best1-LB1]),
	maximize(Goal, Var, Best1, LB1).

maximize(Goal, Var, _, LB) :- var(LB), !,
	illarg(var, maximize(Goal,Var), 2).
maximize(Goal, Var, _, LB) :-
	Var #> LB,
	findall(Goal-Var, (Goal -> true), [Best1-LB1]), !,
	maximize(Goal, Var, Best1, LB1).
maximize(Goal, Var, Best, UB) :-
	minmax_unify(Goal, Var, Best, UB).

minmax_unify(M:Goal, Var, M:Best, LB) :- !,
	minmax_unify(Goal, Var, Best, LB).
minmax_unify(labeling(Opt,VarV), CostV, labeling(Opt,Var), Cost) :- !,
	unify_list([CostV|VarV], [Cost|Var], 1).
minmax_unify(Goal, Var, Goal, Var).

unify_list([], [], 0) :-
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
unify_list([X|Xs], [Y|Ys], Flag) :-
	'$fd_in_interval'(X, Y, Y, Flag),
	unify_list(Xs, Ys, 0).



end_of_file.

/* To be replaced by something more general. */

%   order_resource(+Options, +Resource) 
%   where Options is a list of key words specifying a heuristic,
%   Resource represents a resource as returned by the resource(-Resource)
%   option to cumulative/3 on which tasks must be serialized.  True if
%   a total ordering can be imposed on the task; enumerating all such
%   orderings via backtracking.
%   [first,est] (the default) and [last,lct] can be good heuristics.

order_resource(Options, Resource) :-
	must_be(Options, proper_list(callable), order_resource(Options,Resource), 1),
	order_options(Options, opt(first,est), opt(Edge,Key)),
	order_resource(Edge, Key, Resource).

order_options([]) --> [].
order_options([X|Xs]) --> order_option(X), order_options(Xs).

order_option(first, opt(_,Key), opt(first,Key)).
order_option(last, opt(_,Key), opt(last,Key)).
order_option(est, opt(Edge,_), opt(Edge,est)).
order_option(lst, opt(Edge,_), opt(Edge,lst)).
order_option(ect, opt(Edge,_), opt(Edge,ect)).
order_option(lct, opt(Edge,_), opt(Edge,lct)).


order_resource(first, Key, resource(Tasks,Matrix)) :-
	order_resource_first(Tasks, Matrix, Key).
order_resource(last, Key, resource(Tasks,Matrix)) :-
	order_resource_last(Tasks, Matrix, Key).

tasks_slack(Tasks, Slack) :-
	'$fd_minint_maxint'(Minint, Maxint),
	tasks_slack(Tasks, 0, Dur, Maxint, EST, Minint, LCT),
	Slack is LCT-EST-Dur.

% useful in JSP heuristics
tasks_slack([], Dur, Dur, EST, EST, LCT, LCT).
tasks_slack([T|Tasks], Dur0, Dur, EST0, EST, LCT0, LCT) :-
	task(dur, T, Dur1),
	task(est, T, EST1),
	task(lct, T, LCT1),
	Dur2 is Dur0+Dur1,
	EST2 is min(EST0,EST1),
	LCT2 is max(LCT0,LCT1),
	tasks_slack(Tasks, Dur2, Dur, EST2, EST, LCT2, LCT).

tasks_duration([], Dur, Dur).
tasks_duration([T|Tasks], Dur0, Dur) :-
	task(dur, T, Dur1),
	Dur2 is Dur0 + Dur1,
	tasks_duration(Tasks, Dur2, Dur).

% Val1 (Val2) is the smallest (next smallest) Key value of Tasks
tasks_smallest([], _, Val1, Val1, Val2, Val2).
tasks_smallest([T|Tasks], Key, Val1a, Val1, Val2a, Val2) :-
	task(Key, T, New),
	Val1b is min(Val1a,New),
	Val2b is max(Val1a,min(Val2a,New)),
	tasks_smallest(Tasks, Key, Val1b, Val1, Val2b, Val2).

% Val1 (Val2) is the greatest (next greatest) Key value of Tasks
tasks_greatest([], _, Val1, Val1, Val2, Val2).
tasks_greatest([T|Tasks], Key, Val1a, Val1, Val2a, Val2) :-
	task(Key, T, New),
	Val1b is max(Val1a,New),
	Val2b is min(Val1a,max(Val2a,New)),
	tasks_greatest(Tasks, Key, Val1b, Val1, Val2b, Val2).

% {t in Tasks | total duration =< LCT(Tasks)-EST(t)} can be first in Tasks
order_resource_first([], _Mat, _Key) :- !.
order_resource_first([_], _Mat, _Key) :- !.
order_resource_first(Tasks, Mat, Key) :-
	can_be_first(Tasks, [], Key, First0),
	keysort(First0, First),
	order_resource_first(Tasks, First, Mat, Key).

% {t in Tasks | total duration =< LCT(t)-EST(Tasks)} can be last in Tasks
order_resource_last([], _Mat, _Key) :- !.
order_resource_last([_], _Mat, _Key) :- !.
order_resource_last(Tasks, Mat, Key) :-
	can_be_last(Tasks, [], Key, Last0),
	keysort(Last0, Last),
	order_resource_last(Tasks, Last, Mat, Key).

order_resource_first(Tasks, [_-F|_], Mat, Key) :-
	task(id, F, I),
	task(dur, F, D),
	'$fd_begin',
	tell_after(Tasks, I, D, Mat, Rest),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global),
	order_resource_first(Rest, Mat, Key).
order_resource_first(Tasks, [_|First], Mat, Key) :-
	order_resource_first(Tasks, First, Mat, Key).

order_resource_last(Tasks, [_-F|_], Mat, Key) :-
	task(id, F, I),
	'$fd_begin',
	tell_before(Tasks, I, Mat, Rest),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global),
	order_resource_last(Rest, Mat, Key).
order_resource_last(Tasks, [_|Last], Mat, Key) :-
	order_resource_last(Tasks, Last, Mat, Key).

can_be_first(Tasks, Order, Key, First0) :-
	tasks_duration(Tasks, 0, Dur),
	'$fd_minint_maxint'(Minint, Maxint),
	tasks_smallest(Tasks, lst, Maxint, LST1, Maxint, LST2),
	tasks_greatest(Tasks, lct, Minint, LCT1, Minint, LCT2),
	can_be_first(Tasks, Order, Dur, LCT1, LCT2, LST1, LST2, Key, First0).

can_be_first([], _, _, _, _, _, _, _, []).
can_be_first([T|Rest], Order, Dur, LCT1, LCT2, LST1, LST2, Key, [Rank-T|First]) :-
	task(all, T, _Id, EST, LST, ECT, LCT),
	(   LCT=:=LCT1 -> LCT2-EST >= Dur; LCT1-EST >= Dur   ),
	(   LST=:=LST1 -> ECT =< LST2; ECT =< LST1   ), !,
	task(Key, T, Rank),
	can_be_first(Rest, Order, Dur, LCT1, LCT2, LST1, LST2, Key, First).
can_be_first([_|Rest], Order, Dur, LCT1, LCT2, LST1, LST2, Key, First) :-
	can_be_first(Rest, Order, Dur, LCT1, LCT2, LST1, LST2, Key, First).

can_be_last(Tasks, Order, Key, Last0) :-
	tasks_duration(Tasks, 0, Dur),
	'$fd_minint_maxint'(Minint, Maxint),
	tasks_greatest(Tasks, ect, Minint, ECT1, Minint, ECT2),
	tasks_smallest(Tasks, est, Maxint, EST1, Maxint, EST2),
	can_be_last(Tasks, Order, Dur, EST1, EST2, ECT1, ECT2, Key, Last0).

can_be_last([], _, _, _, _, _, _, _, []).
can_be_last([T|Rest], Order, Dur, EST1, EST2, ECT1, ECT2, Key, [Rank-T|Last]) :-
	task(all, T, _Id, EST, LST, ECT, LCT),
	(   EST=:=EST1 -> LCT-EST2 >= Dur; LCT-EST1 >= Dur   ),
	(   ECT=:=ECT1 -> LST >= ECT2; LST >= ECT1   ), !,
	task(Key, T, Rank0), Rank is -Rank0,
	can_be_last(Rest, Order, Dur, EST1, EST2, ECT1, ECT2, Key, Last).
can_be_last([_|Rest], Order, Dur, EST1, EST2, ECT1, ECT2, Key, Last) :-
	can_be_last(Rest, Order, Dur, EST1, EST2, ECT1, ECT2, Key, Last).

% Enforce: all tasks after I
tell_after([], _I, _D, _Mat, []).
tell_after([Tj|Tasks], I, D, Mat, [Tj|Rest]) :-
	task(id, Tj, J),
	I =\= J, !,
	dvar_gtz(I, D, J, Mat),	% tell Dji #>= D
	tell_after(Tasks, I, D, Mat, Rest).
tell_after([_|Tasks], I, D, Mat, Rest) :-
	tell_after(Tasks, I, D, Mat, Rest).

% Enforce: all tasks before I
tell_before([], _I, _Mat, []).
tell_before([Tj|Tasks], I, Mat, [Tj|Rest]) :-
	task(id, Tj, J),
	I =\= J, !,
	task(dur, Tj, D),
	dvar_gtz(J, D, I, Mat),	% tell Dij #>= D
	tell_before(Tasks, I, Mat, Rest).
tell_before([_|Tasks], I, Mat, Rest) :-
	tell_before(Tasks, I, Mat, Rest).

dvar_gtz(I, D, J, Diffs) :-
	avl_fetch(J-I, Diffs, Dji), !,
	'$fd_in_interval'(Dji, D, sup, 0).
dvar_gtz(I, D, J, Diffs) :-
	avl_fetch(I-J, Diffs, Dij), !,
	MD is -D,
	'$fd_in_interval'(Dij, inf, MD, 0).

task(id, task(_,_,_,_,I), I).
task(dur, task(_,_-DurM,_,_,_), Dur) :-
	get_mutable_rec(DurD, DurM),
	DurD = dom(_,Dur,_,_).
task(est, task(_-SM,_,_,_,_), Est) :-
	get_mutable_rec(SD, SM),
	SD = dom(_,Est,_,_).
task(lst, task(_-SM,_,_,_,_), Lst) :-
	get_mutable_rec(SD, SM),
	SD = dom(_,_,Lst,_).
task(ect, task(_,_,_-EM,_,_), Ect) :-
	get_mutable_rec(ED, EM),
	ED = dom(_,Ect,_,_).
task(lct, task(_,_,_-EM,_,_), Lct) :-
	get_mutable_rec(ED, EM),
	ED = dom(_,_,Lct,_).
task(all, task(_-SM,_,_-EM,_,Id), Id, Est, Lst, Ect, Lct) :-
	get_mutable_rec(SD, SM),
	SD = dom(_,Est,Lst,_),
	get_mutable_rec(ED, EM),
	ED = dom(_,Ect,Lct,_).


