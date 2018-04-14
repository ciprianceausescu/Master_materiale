/* Copyright(C) 1997, Swedish Institute of Computer Science */

%%% Target library for arithmetic expressions:
%%%	'x+y=t'/3
%%%	'ax=t'/3
%%%	'ax+y=t'/4
%%%	'ax+by=t'/5
%%%	't=c'/2	
%%%	't=<c'/2
%%%	't\\=c'/2
%%%	't>=c'/2
%%%	't=u+c'/3
%%%	't=<u+c'/3
%%%	't\\=u+c'/3
%%%	't>=u+c'/3
%%%	't+u=c'/3
%%%	't+u=<c'/3
%%%	't+u\\=c'/3
%%%	't+u>=c'/3
%%%	't=x+y+c'/4
%%%	'x+y=u+c'/4
%%%	'x+y+z=c'/4

/***
't=c'(T,C) +:
	T in {C}.

't=<c'(T,C) +:
	T in inf..C.

't>=c'(T,C) +:
	T in C..sup.

't\\=c'(T,C) +:
	T in \{C}.

Type checking done at compile time.
***/

't=c'(T,C) :-
	check_arguments_error(1, T#=C),
	propagate_interval(T, C, C).

't=<c'(T,C) :- 
	check_arguments_error(1, T#=<C),
	propagate_interval(T, inf, C).

't>=c'(T,C) :- 
	check_arguments_error(1, T#>=C),
	propagate_interval(T, C, sup).

't\\=c'(T,C) :-
	check_arguments_error(1, T#\=C),
	'$fd_range'(C, C, R, 1),
	'$fd_dom_complement'(R, R1),
	prune_and_propagate(T, R1).

%%% indexicals for arithmetic

'ax=t'(A,X,T) +:
	X in   min(T) /> A..max(T) /< A,
	T in !(min(X) *  A..max(X) *  A).

'x+y=t'(X,Y,T) +:
	X in !(min(T) - max(Y)..max(T) - min(Y)),
	Y in !(min(T) - max(X)..max(T) - min(X)),
	T in !(min(X) + min(Y)..max(X) + max(Y)).

% Now domain consistent!
't+u=c'(T,U,C) +:
	T in !(C - dom(U)),
	U in !(C - dom(T)).

% Now domain consistent!
'x+c=y'(X,C,Y) +:
	X in !(dom(Y) - C),
	Y in !(dom(X) + C).

% Now domain consistent!
't=u+c'(T,U,C) +:
	T in !(dom(U) + C),
	U in !(dom(T) - C).

't=<u+c'(T,U,C) +:
	T in inf..max(U)+C,
	U in min(T) - C..sup.

't\\=u+c'(T,U,C) +:
	T in \{U + C},
	U in \{T - C}.

't>=u+c'(T,U,C) +:
	T in min(U) + C..sup,
	U in inf..max(T) - C.

'ax+c=t'(A,X,C,Y) +:
	X in  (min(Y) - C) /> A..(max(Y) - C) /< A,
	Y in !(min(X)*A + C    .. max(X)*A + C).

'ax+y=t'(A,X,Y,Z) +:
	X in  (min(Z) - max(Y)) /> A..(max(Z) - min(Y)) /< A,
	Y in !(min(Z) - max(X)*A    .. max(Z) - min(X)*A),
	Z in !(min(X)*A + min(Y)    .. max(X)*A + max(Y)).

't+u=<c'(T,U,C) +:
	T in inf..C - min(U),
	U in inf..C - min(T).

't+u\\=c'(T,U,C) +:
	T in \{C - U},
	U in \{C - T}.

't+u>=c'(T,U,C) +:
	T in C - max(U)..sup,
	U in C - max(T)..sup.

% obsolete, backward compatibility?
'ax+by=t'(A,X,B,Y,Z) +:
	X in  (min(Z) - max(Y)*B) /> A.. (max(Z) - min(Y)*B) /< A,
	Y in  (min(Z) - max(X)*A) /> B.. (max(Z) - min(X)*A) /< B,
	Z in !(min(X)*A + min(Y)*B    ..  max(X)*A + max(Y)*B).

'x+y=u+c'(X,Y,U,C) +:
	X in !(min(U) - max(Y) + C..max(U) - min(Y) + C),
	Y in !(min(U) - max(X) + C..max(U) - min(X) + C),
	U in !(min(X) + min(Y) - C..max(X) + max(Y) - C).

't=x+y+c'(T,X,Y,C) :-
	'x+y+c=z'(X,Y,C,T).

'x+y+c=z'(X,Y,C,Z) +:
	X in !(min(Z) - max(Y) - C..max(Z) - min(Y) - C),
	Y in !(min(Z) - max(X) - C..max(Z) - min(X) - C),
	Z in !(min(X) + min(Y) + C..max(X) + max(Y) + C).

'ax+y+c=z'(A,X,Y,C,Z) +:
	X in  (min(Z) -	  max(Y) - C)/>A.. (max(Z) -   min(Y) - C)/<A,
	Y in !(min(Z) -   max(X)*A - C	..  max(Z) -   min(X)*A - C),
	Z in !(min(X)*A + min(Y) + C	..  max(X)*A + max(Y) + C).

'x+y+z=t'(X,Y,Z,T) +:
	X in !(min(T) - max(Y) - max(Z)..max(T) - min(Y) - min(Z)),
	Y in !(min(T) - max(X) - max(Z)..max(T) - min(X) - min(Z)),
	Z in !(min(T) - max(X) - max(Y)..max(T) - min(X) - min(Y)),
	T in !(min(X) + min(Y) + min(Z)..max(X) + max(Y) + max(Z)).

'x+y+z=c'(X,Y,Z,C) +:
	X in !(C - max(Y) - max(Z)..C - min(Y) - min(Z)),
	Y in !(C - max(X) - max(Z)..C - min(X) - min(Z)),
	Z in !(C - max(X) - max(Y)..C - min(X) - min(Y)).

'ax+y+z=t'(A,X,Y,Z,T) +:
	X in  (min(T) -	  max(Y) - max(Z)) /> A..
	      (max(T) -	  min(Y) - min(Z)) /< A,

	Y in !(min(T) - max(X)*A - max(Z).. 
	       max(T) - min(X)*A - min(Z)),

	Z in !(min(T) - max(X)*A - max(Y)..
	       max(T) - min(X)*A - min(Y)),

	T in !(min(X)*A + min(Y) + min(Z)..
	       max(X)*A + max(Y) + max(Z)).


%%% Utilities for globals.

arg_attribute(X, Attr, _, _) :-
	'$fd_arg_attribute'(X, 0, Attr), !.
arg_attribute(X, _, Goal, ArgNo) :-
	fd_argument_error(Goal, ArgNo, X).

finite_arg_attribute(X, Attr, _, _) :-
	'$fd_arg_attribute'(X, 1, Attr), !.
finite_arg_attribute(X, _, Goal, ArgNo) :-
	var(X), !,
	illarg(var, Goal, ArgNo, X).
finite_arg_attribute(X, _, Goal, ArgNo) :-
	fd_argument_error(Goal, ArgNo, X).

must_be_dvar_list(List, _, _) :-
	'$fd_dvar_list'(List, 0), !.
must_be_dvar_list(List, Goal, ArgNo) :-
	not_dvar_list(List, Goal, ArgNo).

not_dvar_list(List, Goal, ArgNo) :- var(List), !,
	illarg(var, Goal, ArgNo, List).
not_dvar_list([Arg|List], Goal, ArgNo) :- !,
	arg_attribute(Arg, _, Goal, ArgNo),
	not_dvar_list(List, Goal, ArgNo).
not_dvar_list(List, Goal, ArgNo) :-
	illarg(domain(term,list), Goal, ArgNo, List).

must_be_list_of_finite_dvar(List, _, _) :-
	'$fd_dvar_list'(List, 1), !.
must_be_list_of_finite_dvar(List, Goal, ArgNo) :-
	not_list_of_finite_dvar(List, Goal, ArgNo).

not_list_of_finite_dvar(L, Goal, ArgNo) :- var(L), !,
	illarg(var, Goal, ArgNo, L).
not_list_of_finite_dvar([X|L], Goal, ArgNo) :- !,
	finite_arg_attribute(X, _, Goal, ArgNo),
	not_list_of_finite_dvar(L, Goal, ArgNo).
not_list_of_finite_dvar(L, Goal, ArgNo) :-
	illarg(domain(term,list), Goal, ArgNo, L).

/****************************************************************/
/* new all_different/[1,2], all_distinct/[1,2]                  */
/****************************************************************/

all_different(Xs) :-
	all_different(Xs, [], opt(local,val,[],false), all_different(Xs)).

all_different(Xs, Opt) :-
	all_different(Xs, Opt, opt(local,val,[],false), all_different(Xs,Opt)).

all_distinct(Xs) :-
	all_different(Xs, [], opt(global,dom,[],false), all_distinct(Xs)).

all_distinct(Xs, Opt) :-
	all_different(Xs, Opt, opt(global,dom,[],false), all_distinct(Xs,Opt)).

circuit(Xs) :-
	all_different(Xs, [], opt(global,dom,[],true), circuit(Xs)).

all_different(Xs, Options, Opt0, Goal) :-
	must_be(Options, proper_list(callable), Goal, 2),
	all_diff_options(Options, Opt0, Opt, Goal, 2),
	must_be_dvar_list(Xs, Goal, 1),
	Opt = opt(Cons,On,_,Circuit),
	dvar_list_susp(Xs, Vec, On, Goal, 1, Susp, []),
	(   Susp = [F|_] -> functor(F, SuspF, 1)
	;   SuspF = none
	),
	(   SuspF==val -> SuspVal = Susp
	;   on_val(Xs, SuspVal, [])
	),
	(   Circuit==true
	->  Flag=1,
	    not_self(Xs, 0),
	    fd_global_internal(circuit(Xs), state(Vec,0,_Handle3,0), SuspVal,
			       _, true, 4) % non-idempotent
	;   Flag=0
	),
	(   Cons==global
	->
 	    fd_global_internal(all_different(Xs), state(Vec,0,_Handle1,0), SuspVal,
 			       _, true, 4), % non-idempotent
	    fd_global_internal(all_distinct(Xs), f(Vec,0,0,Flag,SuspF,_Handle2,0), []/*Susp*/,
			       _, clpfd:Goal, 0)
	;   Cons==bound
        ->  fd_global_internal(bc_alldiff(Xs), f(Vec,0,SuspF,_Handle2,0), []/*Susp*/,
			       _, clpfd:Goal, 0)
	; /*Cons==local ->*/
	    fd_global_internal(all_different(Xs), state(Vec,0,_Handle2,0), SuspVal,
			       _, clpfd:Goal, 4) % non-idempotent
	).

dvar_list_susp([], [], _On, _Goal, _ArgNo) --> [].
dvar_list_susp([X|Xs], [X-M|Vec], On, Goal, ArgNo) -->
	on(On, X),
	{arg_attribute(X, M, Goal, ArgNo)},
	dvar_list_susp(Xs, Vec, On, Goal, ArgNo).

finite_dvar_list_susp([], [], _On, _Goal, _ArgNo) --> [].
finite_dvar_list_susp([X|Xs], [X-M|Vec], On, Goal, ArgNo) -->
	on(On, X),
	{finite_arg_attribute(X, M, Goal, ArgNo)},
	finite_dvar_list_susp(Xs, Vec, On, Goal, ArgNo).

all_diff_options([], Opt, Opt, _, _).
all_diff_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   all_diff_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,all_diff_option), Goal, ArgNo, X)
        ),
	all_diff_options(L, Opt1, Opt, Goal, ArgNo).

all_diff_option(on(On), opt(Cons,_,Cost,Circ), opt(Cons,On,Cost,Circ)) :-
	on(On, _, _, _).
all_diff_option(consistency(Arg), opt(_,_,Cost,Circ), opt(Cons,On,Cost,Circ)) :-
	consistency_option(Arg, Cons, On).
all_diff_option(cost(V,Mat), opt(Cons,On,_,Circ), opt(Cons,On,cost(V,VM,Mat),Circ)) :-
	arg_attribute(V, VM, 0, 3).
all_diff_option(circuit(Circ), opt(Cons,On,Cost,_), opt(Cons,On,Cost,Circ)) :-
	bool_option(Circ, _).

/****************************************************************/
/* element/3							*/
/****************************************************************/

element(Index, Xs, Value) :-
	Goal = element(Index, Xs, Value),
	arg_attribute(Index, IndexM, Goal, 1),
	arg_attribute(Value, ValueM, Goal, 3),
	must_be_dvar_list(Xs, Goal, 2),
	length(Xs, N),
	(   ground(Xs) -> IsGround=1, Xs2=Xs, Susp=SuspTail
	;   IsGround=0,
	    dvar_list_susp(Xs, Xs2, none, Goal, 2, Susp, SuspTail)
	),
	SuspTail = [dom(Index),minmax(Value)],
	fd_global(Goal, f(Index-IndexM,Value-ValueM,Xs2,N,IsGround,_Handle,0), Susp).

/****************************************************************/
/* minimum/2, maximum/2						*/
/****************************************************************/

minimum(Y, Xs) :-
	minmax(Y, Xs, minimum(Y,Xs), 0).

maximum(Y, Xs) :-
	minmax(Y, Xs, maximum(Y,Xs), 1).

minmax(Y, Xs, Goal, IsMax) :-
	must_be_dvar_list(Xs, Goal, 2),
	length(Xs, N),
	arg_attribute(Y, YM, Goal, 1),
	dvar_list_susp(Xs, Xs2, none, Goal, 2, Susp, [minmax(Y)]),
	fd_global(Goal, f(Y-YM,Xs2,N,IsMax,_Handle,0), Susp).

/****************************************************************/
/* nvalue/2        						*/
/****************************************************************/

nvalue(N, Xs) :-
	Goal = nvalue(N, Xs),
	must_be_dvar_list(Xs, Goal, 2),
	arg_attribute(N, NM, Goal, 1),
	finite_dvar_list_susp(Xs, Xs2, dom, Goal, 2, Susp, [minmax(N)]),
	fd_global(Goal, f(N-NM,Xs2,0,0,_Handle,0), Susp).

/****************************************************************/
/* assignment/[2,3]						*/
/****************************************************************/

% assignment(X1...Xn, Y1...Yn) is true if 
% all Xi,Yi in 1..n and Xi=j iff Yj=i

assignment(Xs, Ys) :-
	assignment(Xs, Ys, [], opt(global,dom,[],false), assignment(Xs,Ys)).

assignment(Xs, Ys, Options) :-
	assignment(Xs, Ys, Options, opt(global,dom,[],false), assignment(Xs,Ys,Options)).

circuit(Xs, Ys) :-
	assignment(Xs, Ys, [], opt(global,dom,[],true), circuit(Xs,Ys)).

circuit(Xs, Ys, Options) :-
	assignment(Xs, Ys, Options, opt(global,dom,[],true), circuit(Xs,Ys,Options)).

assignment(Xs, Ys, Options, Opt0, Goal) :-
	must_be(Options, proper_list(callable), Goal, 3),
	all_diff_options(Options, Opt0, Opt, Goal, 3),
	must_be_dvar_list(Xs, Goal, 1),
	length(Xs, N),
	length(Ys, N),
	must_be_dvar_list(Ys, Goal, 2),
	'$fd_range'(1, N, Set, 1),
	domain(Xs, Set),
	domain(Ys, Set),
	Opt = opt(Cons,On,Cost,Circuit),
	assignment_state(Xs, Ys, 0, XVec, YVec, On, Goal, Susp, Susp1),
	(   Cost==[] -> Susp1 = [], Flag0 = 0
	;   Cost = cost(C,_,_), Susp1 = [dom(C)], Flag0 = 2
	),
	on_val(Xs, SuspVal, Susp2),
	on_val(Ys, Susp2, []),
	fd_global_internal(pairing(Xs,Ys), state(XVec,YVec,0,_Handle1,0), SuspVal,
			   _, clpfd:Goal, 4), % non-idempotent
	(   Circuit==true
	->  Flag is Flag0+1,
	    not_self(Xs, 0),
	    not_self(Ys, 0),
	    fd_global_internal(circuit(Xs), state(XVec,0,_Handle2,0), SuspVal,
			       _, true, 4), % non-idempotent
	    fd_global_internal(circuit(Ys), state(YVec,0,_Handle3,0), SuspVal,
			       _, true, 4) % non-idempotent
	;   Flag = Flag0
	),
	(   Flag>=2
	->  fd_global_internal(assignment(Xs,Ys),
			       f(XVec,YVec,0,Flag,Cost,0/*cost so far*/,_Handle4,0), Susp,
			       Global, true, 0),
	    Global = global(StateM,_,_,_,_),
	    assignment_helpers(Xs, C, 0, StateM)
	;   Cons==local -> true
	;   fd_global_internal(assignment(Xs,Ys), f(XVec,YVec,0,Flag,Cost,0/*cost so far*/,_Handle4,0), Susp,
			       _, true, 0)
	).

assignment_helpers([], _, _, _).
assignment_helpers([X|Xs], C, I, StateM) :-
	fd_global_internal(assignment_helper(X,C), f(I,X,StateM), [val(X)],
			   _, true, 0),
	J is I+1,
	assignment_helpers(Xs, C, J, StateM).

assignment_state([], [], _, [], [], _On, _Goal) --> [].
assignment_state([X|Xs], [Y|Ys], I, [X-XM|XVec], [Y-YM|YVec], On, Goal) -->
	on(On, X),
	on(On, Y),
	{arg_attribute(X, XM, Goal, 1),
	 arg_attribute(Y, YM, Goal, 2),
	 J is I+1},
	assignment_state(Xs, Ys, J, XVec, YVec, On, Goal).

not_self([], _).
not_self([Y|Ys], I) :-
	J is I+1,
	't\\=c'(Y, J),
	not_self(Ys, J).

/****************************************************************/
/* cumulative/[1,2]                        			*/
/****************************************************************/

cumulative(Tasks) :-
	cumulative(Tasks, []).

cumulative(Tasks, Options) :-
	Goal = cumulative(Tasks,Options),
	must_be(Options, proper_list(callable), Goal, 2),
	cumulative_options(Options,
			   opt(1,    _,                    [], 0),
			   opt(Limit,resource(Tasks2,Map2),Ps1,Flags),
			   Goal, 2),
	finite_arg_attribute(Limit, LA, Goal, 2),
	cumulative_convert(Tasks, Tasks2, Goal, 1),
	cumulative_suspend(Tasks, Susp, Susp1),
	(   Flags/\1 =:= 0 -> Ps2 = Ps1
	;   diff_pairs(Ps1, KV1),
	    list_to_avl(KV1, Map1),
	    task_diffs_to_avl(Tasks2, Map1, Map2, Ps2, Ps1)
	),
	cumulative_precedences(Ps2, Ps3, Goal, 2, Susp1, [minmax(Limit)]),
	length(Tasks2, N),
	fd_global_internal(cumulative(Tasks,Options),
			   f(N,Tasks2,Ps3,Limit-LA,Flags,N,0,_Handle,0),
			   Susp, _, clpfd:Goal, 0).

diff_pairs([], []).
diff_pairs([K#=V|L1], [K-V|L2]) :-
	diff_pairs(L1, L2).

task_diffs_to_avl([], Map, Map) --> [].
task_diffs_to_avl([Task|Tasks], Map1, Map3) -->
	{Task = task(_,_,_,_,ID)},
	task_diffs_to_avl(Tasks, ID, Map1, Map2),
	task_diffs_to_avl(Tasks, Map2, Map3).

task_diffs_to_avl([], _, Map, Map) --> [].
task_diffs_to_avl([Task|Tasks], ID1, Map1, Map3) -->
	{Task = task(_,_,_,_,ID2)},
	{K12 = ID1-ID2},
	(   {avl_fetch(K12, Map1, _) -> Map1 = Map2}
	;   {avl_fetch(ID2-ID1, Map1, _) -> Map1 = Map2}
	;   {avl_store(K12, Map1, D12, Map2)}, [K12 #= D12]
	),
	task_diffs_to_avl(Tasks, ID1, Map2, Map3).

cumulative_options([], Opt, Opt, _, _).
cumulative_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   cumulative_option(X, Goal, Opt0, Opt1) -> true
        ;   illarg(domain(term,cumulative_option), Goal, ArgNo, X)
        ),
	cumulative_options(L, Opt1, Opt, Goal, ArgNo).

% opt(Limit,Resource,Precedences,Flags) where
% 0x1 = precedences
% 0x2 = stronger than O(N log N) algos
cumulative_option(limit(Lim), _, opt(_,R,Ps,Flags), opt(Lim,R,Ps,Flags)).
cumulative_option(precedences(Ps), _, opt(Lim,R,_,Flags), opt(Lim,R,Ps,Flags)).
% cumulative_option(resource(R), _, opt(Lim,_,Ps,Flags0), opt(Lim,R,Ps,Flags)) :-
% 	Flags is (Flags0 /\ -2) \/ 1.
cumulative_option(global(B), _, opt(Lim,R,Ps,Flags0), opt(Lim,R,Ps,Flags)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -3) \/ (Value<<1).

cumulative_convert([], [], _, _).
cumulative_convert([task(Org,Dur,End,Height,Machine)|Tasks1],
		   [task(Org-OA,Dur-DA,End-EA,Height-HA,Machine)|Tasks2],
		   Goal, ArgNo) :-
	finite_arg_attribute(Org, OA, Goal, ArgNo),
	finite_arg_attribute(Dur, DA, Goal, ArgNo),
	finite_arg_attribute(End, EA, Goal, ArgNo),
	finite_arg_attribute(Height, HA, Goal, ArgNo),
	must_be(Machine, integer, Goal, ArgNo),
	cumulative_convert(Tasks1, Tasks2, Goal, ArgNo).

cumulative_suspend([]) --> [].
cumulative_suspend([task(Org,Dur,End,Height,_)|Tasks1]) -->
	on_minmax([Org,Dur,End,Height]),
	cumulative_suspend(Tasks1).

cumulative_precedences([], [], _Goal, _ArgNo) --> [].
cumulative_precedences([J-I #= D|Ps1], [f(J,I,D-DA)|Ps2], Goal, ArgNo) -->
	{arg_attribute(D, DA, Goal, ArgNo)},
	on_minmax([D]),
	cumulative_precedences(Ps1, Ps2, Goal, ArgNo).

/****************************************************************/
/* sum/3, scalar_product/[4,5]					*/
/****************************************************************/

sum(Xs, Rel, S) :-
	ones(Xs, Cs),
	scalar_product(Cs, Xs, Rel, S).

ones([], []).
ones([_|L1], [1|L2]) :- ones(L1, L2).

scalar_product(Cs, Xs, Rel, S) :-
	scalar_product(Cs, Xs, Rel, S, []).

scalar_product(Cs, Xs, Rel, S, Options) :-
	Goal = scalar_product(Cs,Xs,Rel,S,Options),
	Goal4 = scalar_product(Cs,Xs,Rel,S),
	must_be(Options, proper_list(callable), Goal, 5),
	scalar_options(Options, opt(bound,minmax), opt(Cons,_On), Goal, 5),
	must_be_dvar_list(Xs, Goal, 2),
	must_be_dvar_list([S], Goal, 4),
	keys_values_pairs(Xs, Cs, L1),
	keysort(L1, L2),
	keyfuse(L2, L3),
	keys_and_values(L3, Xs1, Cs1),
	scalar_state(Cs1, Xs1, none, S, Vec, Sum, Susp, Goal),
	length(Vec, N),
	(   Rel==(#=),
	    \+'$fd_coref'(Susp)
	->  sp_strength_reduce(N, Vec, Sum, Goal4, Susp)
	;   scalar_op(Rel, Op, Sum, RHS),
	    fd_global(Goal4,
		      state(Vec,Op,RHS,0/*Nground*/,_,0),
		      Susp)
	),
	(   Cons\==global -> true
	;   Rel\==(#=) -> true
	;   ac_linear(Cs1, Xs1, S, Vec, Sum, Goal)
	).

ac_linear(Cs, Xs, S) :-
	Goal = scalar_product(Cs,Xs,#=,S,[consistency(domain)]), % for error handling
	scalar_state(Cs, Xs, none, S, Vec, Sum, _, Goal),
	ac_linear(Cs, Xs, S, Vec, Sum, Goal).

ac_linear(Cs, Xs, S, Vec, Sum, Goal) :-
	must_be_list_of_finite_dvar(Xs, Goal, 2),
	must_be_list_of_finite_dvar([S], Goal, 4),
	ac_linear_vector(Vec, Vec2, Susp2),
	length(Vec, N),
	fd_global(ac_linear(Cs,Xs,S),
		  state(Vec2,N,Sum,N/*Ntargets*/,_,0),
		  Susp2).


ac_linear_vector([], [], []).
ac_linear_vector([f(A,B,C)|V1], [f(A,B,C,D)|V2], [dom(B)|Susp]) :-
	create_mutable([], D),	% failed set
	ac_linear_vector(V1, V2, Susp).

scalar_options([], Opt, Opt, _, _).
scalar_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   scalar_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,scalar_option), Goal, ArgNo, X)
        ),
	scalar_options(L, Opt1, Opt, Goal, ArgNo).

scalar_option(consistency(Arg), opt(_,_), opt(Cons,On)) :-
	consistency_option(Arg, Cons, On).


scalar_op(#=<, 1, Sum, Sum).
scalar_op(#<,  1, Sum, Sum1) :- Sum1 is Sum-1.
scalar_op(#>=, 2, Sum, Sum).
scalar_op(#>,  2, Sum, Sum1) :- Sum1 is Sum+1.
scalar_op(#=,  3, Sum, Sum).
scalar_op(#\=, 4, Sum, Sum).

%%% This is not an exhaustive list.

sp_strength_reduce(0, [], S, _Goal, _Susp) :- !, S = 0.
sp_strength_reduce(1, [f(C1,X1,_M1)], S, _Goal, _Susp) :- !,
	S mod C1 =:= 0,
	S1 is S//C1,
	't=c'(X1, S1).
sp_strength_reduce(2, [f(C1,X1,M1),f(C2,X2,M2)], S, Goal, Susp) :-
	'$fd_debug'(off,off,1), !,
	sp_strength_reduce_2(C1, X1, M1, C2, X2, M2, S, Goal, Susp).
sp_strength_reduce(3, [f(C1,X1,M1),f(C2,X2,M2),f(C3,X3,M3)], S, Goal, Susp) :-
	'$fd_debug'(off,off,1), !,
	sp_strength_reduce_3(C1, X1, M1, C2, X2, M2, C3, X3, M3, S, Goal, Susp).
sp_strength_reduce(_, Vec, Sum, Goal, Susp) :-
	fd_global(Goal, state(Vec,3,Sum/*RHS*/,0/*Nground*/,_,0), Susp).


sp_strength_reduce_2(1, X1, _M1, 1, X2, _M2, S, _Goal, _Susp) :- !,
	't+u=c'(X1, X2, S).
sp_strength_reduce_2(-1, X1, _M1, 1, X2, _M2, S, _Goal, _Susp) :- !,
	't=u+c'(X2, X1, S).
sp_strength_reduce_2(1, X1, _M1, -1, X2, _M2, S, _Goal, _Susp) :- !,
	't=u+c'(X1, X2, S).
sp_strength_reduce_2(-1, X1, _M1, -1, X2, _M2, S, _Goal, _Susp) :- !,
	S1 is -S, 
	't+u=c'(X1, X2, S1).
sp_strength_reduce_2(1, X1, _M1, C2, X2, _M2, S, _Goal, _Susp) :- C2>0, !,
	'ax+y=t'(C2, X2, X1, S).
sp_strength_reduce_2(1, X1, _M1, C2, X2, _M2, S, _Goal, _Susp) :- C2<0, !,
	C3 is -C2,
	'ax+y=t'(C3, X2, S, X1).
sp_strength_reduce_2(C1, X1, _M1, 1, X2, _M2, S, _Goal, _Susp) :- C1>0, !,
	'ax+y=t'(C1, X1, X2, S).
sp_strength_reduce_2(C1, X1, _M1, 1, X2, _M2, S, _Goal, _Susp) :- C1<0, !,
	C3 is -C1,
	'ax+y=t'(C3, X1, S, X2).
% sp_strength_reduce_2(C1, X1, _M1, C2, X2, _M2, S, _Goal, _Susp) :- C1>0, C2>0, !,
% 	S mod gcd(C1,C2) =:= 0,
% 	'ax+by=t'(C1, X1, C2, X2, S).
% sp_strength_reduce_2(C1, X1, _M1, C2, X2, _M2, S, _Goal, _Susp) :- C1<0, C2<0, !,
% 	C3 is -C1, C4 is -C2, S1 is -S,
% 	S mod gcd(C3,C4) =:= 0,
% 	'ax+by=t'(C3, X1, C4, X2, S1).
sp_strength_reduce_2(C1, X1, M1, C2, X2, M2, Sum, Goal, Susp) :-
	fd_global(Goal, state([f(C1,X1,M1),f(C2,X2,M2)],3,Sum/*RHS*/,0/*Nground*/,_,0), Susp).

sp_strength_reduce_3(1, X1, _M1, 1, X2, _M2, 1, X3, _M3, S, _Goal, _Susp) :- !,
	'x+y+z=c'(X1, X2, X3, S).
sp_strength_reduce_3(-1, X1, _M1, 1, X2, _M2, 1, X3, _M3, S, _Goal, _Susp) :- !,
	'x+y=u+c'(X2, X3, X1, S).
sp_strength_reduce_3(1, X1, _M1, -1, X2, _M2, 1, X3, _M3, S, _Goal, _Susp) :- !,
	'x+y=u+c'(X1, X3, X2, S).
sp_strength_reduce_3(-1, X1, _M1, -1, X2, _M2, 1, X3, _M3, S, _Goal, _Susp) :- !,
	't=x+y+c'(X3, X1, X2, S).
sp_strength_reduce_3(1, X1, _M1, 1, X2, _M2, -1, X3, _M3, S, _Goal, _Susp) :- !,
	'x+y=u+c'(X1, X2, X3, S).
sp_strength_reduce_3(-1, X1, _M1, 1, X2, _M2, -1, X3, _M3, S, _Goal, _Susp) :- !,
	't=x+y+c'(X2, X3, X1, S).
sp_strength_reduce_3(1, X1, _M1, -1, X2, _M2, -1, X3, _M3, S, _Goal, _Susp) :- !,
	't=x+y+c'(X1, X3, X2, S).
sp_strength_reduce_3(-1, X1, _M1, -1, X2, _M2, -1, X3, _M3, S, _Goal, _Susp) :- !,
	S1 is -S,
	'x+y+z=c'(X1, X2, X3, S1).
sp_strength_reduce_3(C1, X1, M1, C2, X2, M2, C3, X3, M3, Sum, Goal, Susp) :-
	fd_global(Goal, state([f(C1,X1,M1),f(C2,X2,M2),f(C3,X3,M3)],3,Sum/*RHS*/,0/*Nground*/,_,0), Susp).


scalar_state(Cs, Xs, Rel, S, Vec, Sum, Susp, Goal) :-
	integer(S), !,
	scalar_vector(Cs, Xs, Rel, S, Vec, Sum, Susp, Goal).
scalar_state(Cs, Xs, Rel, S, Vec, Sum, Susp, Goal) :-
	scalar_vector([-1|Cs], [S|Xs], Rel, 0, Vec, Sum, Susp, Goal).

scalar_vector([], [], _, Sum, [], Sum, [], _Goal).
scalar_vector([C|Cs], [X|Xs], Rel, S0, Vec, S, Susp, Goal) :-
	integer(X), !,
	S1 is S0-C*X,
	scalar_vector(Cs, Xs, Rel, S1, Vec, S, Susp, Goal).
scalar_vector([C|Cs], [X|Xs], Rel, S0, [f(C,X,M)|Vec], S, [Sus|Susp], Goal) :- 
	C=\=0, !,
	arg_attribute(X, M, Goal, 2),
	susp_type(Rel, C, X, Sus),
	scalar_vector(Cs, Xs, Rel, S0, Vec, S, Susp, Goal).
scalar_vector([_|Cs], [_|Xs], Rel, S0, Vec, S, Susp, Goal) :-
	scalar_vector(Cs, Xs, Rel, S0, Vec, S, Susp, Goal).

susp_type(#<, C, X, Type) :- susp_type(#=<, C, X, Type).
susp_type(#=<, C, X, Type) :- C>0 -> Type = min(X); Type = max(X).
susp_type(#>, C, X, Type) :- susp_type(#>=, C, X, Type).
susp_type(#>=, C, X, Type) :- C>0 -> Type = max(X); Type = min(X).
susp_type(#=, _, X, minmax(X)).
susp_type(#\=, _, X, val(X)).
susp_type(dom, _, X, dom(X)).
susp_type(none, _, X, none(X)).

/****************************************************************/
/* 'x*x=y'/2							*/
/****************************************************************/

'x*x=y'(X, Y) :-
	integer(X), !,
	Y is X*X.
'x*x=y'(X, Y) :-
	integer(Y), !,
	(   Y =:= 0 -> '$fd_range'(0, 0, R, 1)
	;   Y >= 0,
	    X1 is integer(sqrt(Y)),
	    X1*X1 =:= Y,
	    X0 is -X1,
	    fd_min(X, Xmin),
	    fd_max(X, Xmax),
	    (   integer(Xmin), X0<Xmin
	    ->  '$fd_range'(X1, X1, R, 1)
	    ;   integer(Xmax), X1>Xmax
	    ->  '$fd_range'(X0, X0, R, 1)
	    ;   '$fd_range'(X0, X1, R, 1)
	    )
	),
	prune_and_propagate_chk(X, R).
'x*x=y'(X, Y) :-
	Goal = 'x*x=y'(X,Y),
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Y, YM, Goal, 2),
	Y in 0..sup,
	on_minmax([X,Y], Susp, []),
	fd_global_internal(Goal, state(X,XM,Y,YM,_handle,0), Susp, _, clpfd:Goal, 4/*non-fixp*/).

/****************************************************************/
/* 'x*y=z'/3							*/
/****************************************************************/

'x*y=z'(X, Y, Z) :-
	Goal = 'x*y=z'(X,Y,Z),
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Y, YM, Goal, 2),
	arg_attribute(Z, ZM, Goal, 3),
	on_minmax([X,Y,Z], Susp, []),
	fd_global_internal(Goal, state(X,XM,Y,YM,Z,ZM,_handle,0), Susp, _, clpfd:Goal, 4/*non-fixp*/).

% for compatibility with formulas saved prior to 3.11.2:
'-ax=t'(A,X,T) +:
	X in   -max(T) /< A .. -min(T) /> A,
	T in !(-max(X) *  A .. -min(X) *  A).

/****************************************************************/
/* 'x/y=z'/3							*/
/****************************************************************/

'x/y=z'(X, Y, Z) :-
	Goal = 'x/y=z'(X,Y,Z),
	't\\=c'(Y, 0),
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Y, YM, Goal, 2),
	arg_attribute(Z, ZM, Goal, 3),
	on_minmax([X,Y,Z], Susp, []),
	fd_global_internal(Goal, state(X,XM,Y,YM,Z,ZM,_handle,0), Susp, _, clpfd:Goal, 4/*non-fixp*/). 

/****************************************************************/
/* 'x mod y=z'/3						*/
/****************************************************************/

'x mod y=z'(X, Y, Z) :-
	Goal = 'x mod y=z'(X,Y,Z),
	't\\=c'(Y, 0),
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Y, YM, Goal, 2),
	arg_attribute(Z, ZM, Goal, 3),
	on_minmax([X,Y,Z], Susp, []),
	fd_global_internal(Goal, state(X,XM,Y,YM,Z,ZM,_handle,0), Susp, _, clpfd:Goal, 4/*non-fixp*/).

/****************************************************************/
/* min/3, max/3 etc						*/
/****************************************************************/

'min(x,y)=z'(X, Y, Z) :-
	X #>= Z,
	Y #>= Z,
	'oneof(x,y)=z'(X, Y, Z).

'max(x,y)=z'(X, Y, Z) :-
	X #=< Z,
	Y #=< Z,
	'oneof(x,y)=z'(X, Y, Z).

'oneof(x,y)=z'(X, Y, Z) :-
	'$fd_debug'(on,on,1), !,
	oneof(X, Y, Z).
'oneof(x,y)=z'(X, Y, Z) :-
	'oneof(x,y)=z IND'(X, Y, Z).

'oneof(x,y)=z IND'(X, Y, Z) +:
	X in !(((dom(Y)/\dom(Z)) ? (inf..sup)) \/ dom(Z)),
	Y in !(((dom(X)/\dom(Z)) ? (inf..sup)) \/ dom(Z)),
	Z in !((dom(X)\/dom(Y))).

'|x|=y'(X,Y) :-
	'$fd_debug'(on,on,1), !,
	abs(X, Y).
'|x|=y'(X,Y) :-
	't>=c'(Y, 0),				% do this outside the loop
	'|x|=y 1'(X,Y).

'|x|=y 1'(X,Y) +:
	X in !(dom(Y) \/ (0-dom(Y))),
	Y in !(dom(X) \/ (0-dom(X))).

%%% domain consistent addition
plus(X, Y, Z) +:
	Z in !(dom(X)+dom(Y)),
	X in !(dom(Z)-dom(Y)),
	Y in !(dom(Z)-dom(X)).

%%% Support for reified constraints (domain reasoning for now)

% optimized for common cases
'x=y'(X, Y) :- integer(Y), !,
	't=c'(X, Y).
'x=y'(X, Y) :- integer(X), !,
	't=c'(Y, X).
'x=y'(X, Y) :-
	't=u'(X, Y).


% Now domain consistent!
't=u'(T, U) :-
	'$fd_debug'(on,on,1), !,
	T = U.
't=u'(U, U). % [MC] 3.11.3
%	't=u IND'(T, U).

't=u IND'(X,Y) +:
	X in !(dom(Y)),
	Y in !(dom(X)).
't=u IND'(X,Y) -:
	X in \{Y},
	Y in \{X}.
% 't=u IND'(X,Y) +?				% covered by next rule
%	X in {Y}.
't=u IND'(X,Y) -?
	X in \dom(Y).

% Now domain consistent!
% just the negation of the above
'x\\=y'(T, U) :-
	'$fd_debug'(on,on,1), !,
	eq_iff(T, U, 0).
'x\\=y'(T, U) :-
	'x\\=y IND'(T, U).

'x\\=y IND'(X,Y) -:
	X in !(dom(Y)),
	Y in !(dom(X)).
'x\\=y IND'(X,Y) +:
	X in \{Y},
	Y in \{X}.
% 'x\\=y IND'(X,Y) -?			% covered by next rule
%	X in {Y}.
'x\\=y IND'(X,Y) +?
	X in \dom(Y).

'x=<y'(T, U) :-
	'$fd_debug'(on,on,1), !,
	le_iff(T, U, 1).
'x=<y'(T, U) :-
	'x=<y IND'(T, U).

'x=<y IND'(X,Y) +:
	X in inf..max(Y),
	Y in min(X)..sup.
'x=<y IND'(X,Y) -:
	X in (min(Y)+1)..sup,
	Y in inf..(max(X)-1).
'x=<y IND'(X,Y) +?
	X in inf..min(Y).
'x=<y IND'(X,Y) -?				% NOT covered by prev rule
	X in (max(Y)+1)..sup.

in_aux_rt(X, Expr) :-
	ground(Expr),
	set_expression_check(Expr, Set, X in Expr, 2),
	prune_and_propagate_chk(X, Set).

in_aux_rt(X, Expr, B) :-
	ground(Expr),
	set_expression_check(Expr, Set, X in Expr #<=> B, 1),
	in_set_iff_rt(X, Set, B).


in_set_aux_rt(X, Set) :-
	'$fd_size'(Set, _, 1), !,
	'$fd_dom_union'(Set, [], Copy),	% Set could occur multiple times
	prune_and_propagate_chk(X, Copy).
in_set_aux_rt(X, Set) :-
	ill_formed_constraint(X in_set Set, Goal), call(Goal).

in_set_aux_rt(X, Set, Bool) :-
	'$fd_size'(Set, _, 1), !,
	'$fd_dom_union'(Set, [], Copy),	% Set could occur multiple times
	in_set_iff_rt(X, Copy, Bool).
in_set_aux_rt(X, Set, B) :-
	ill_formed_constraint(X in_set Set #<=> B, Goal), call(Goal).

in_set_iff_rt(X, Set, B) :-
	'$fd_debug'(off,off,1), !,
	Constraint = in_set_ix(X,Set),
	'$fd_find_definition'(Constraint, clpfd, DefPtr),
	arg_attribute(X, A, Constraint, 1),
	Attv =  in_set_ix(A,Set),
	iff_aux(DefPtr, Constraint, Attv, B).
in_set_iff_rt(X, Set, B) :-
	in_set_iff(X, Set, B).

in_set_ix(X, Set) +:
	X in set(Set).
in_set_ix(X, Set) -:
	X in \set(Set).
in_set_ix(X, Set) +?
	X in set(Set).

%% fdbg support

in_set_iff(X, Set, B) :-
	Goal = in_set_iff(X,Set,B),
	arg_attribute(X, XMut, Goal, 1),
	arg_attribute(B, BMut, Goal, 3),
	propagate_interval(B, 0, 1),
	fd_global(Goal, state(X,XMut,Set,B,BMut,_handle,0), [dom(X),dom(B)]).

eq_iff(X, Y, B) :-
	Goal = eq_iff(X,Y,B),
	arg_attribute(X, XMut, Goal, 1),
	arg_attribute(Y, YMut, Goal, 2),
	arg_attribute(B, BMut, Goal, 3),
	propagate_interval(B, 0, 1),
	(   B==1 -> X = Y
	%;   B==0
	%->  fd_global(Goal, state(X,XMut,Y,YMut,B,BMut,_handle,0), [val(X),val(Y),dom(B)])
	%;   fd_global(Goal, state(X,XMut,Y,YMut,B,BMut,_handle,0), [dom(X),dom(Y),dom(B)])
	;   fd_global(Goal, state(X,XMut,Y,YMut,B,BMut,_handle,0), [])
	).

le_iff(X, Y, B) :-
	Goal = le_iff(X,Y,B),
	arg_attribute(X, XMut, Goal, 1),
	arg_attribute(Y, YMut, Goal, 2),
	arg_attribute(B, BMut, Goal, 3),
	propagate_interval(B, 0, 1),
	fd_global(Goal, state(X,XMut,Y,YMut,B,BMut,_handle,0), [minmax(X),minmax(Y),dom(B)]).

oneof(X, Y, Z) :-
	Goal = oneof(X,Y,Z),
	arg_attribute(X, XMut, Goal, 1),
	arg_attribute(Y, YMut, Goal, 2),
	arg_attribute(Z, ZMut, Goal, 3),
	fd_global(Goal, state(X,XMut,Y,YMut,Z,ZMut,_handle,0), [dom(X),dom(Y),dom(Z)]).

abs(X, Y) :-
	Goal = abs(X,Y),
	arg_attribute(X, XMut, Goal, 1),
	arg_attribute(Y, YMut, Goal, 2),
	fd_global(Goal, state(X,XMut,Y,YMut,_handle,0), [dom(X),dom(Y)]).

%% end fdbg support

/****************************************************************/
/* bool/4							*/
/****************************************************************/

bool(Fun, X, Y, Z) :-
	Goal = bool(Fun,X,Y,Z),
	arg_attribute(X, XMut, Goal, 1),
	arg_attribute(Y, YMut, Goal, 2),
	arg_attribute(Z, ZMut, Goal, 3),
	propagate_interval(X, 0, 1),
	propagate_interval(Y, 0, 1),
	propagate_interval(Z, 0, 1),
	on_dom([X,Y,Z], Susp, []),
	fd_global(Goal, state(X,XMut,Y,YMut,Z,ZMut,Fun,_handle,0), Susp).

% FOR BACKWARD COMPATIBILITY!

'\\p'(P, B) :- bool(6, P, B, 1).

'p/\\q'(P, Q, B) :- bool(0, P, Q, B).

'p\\q'(P, Q, B) :- bool(6, P, Q, B).

'p\\/q'(P, Q, B) :- bool(3, P, Q, B).

'p=>q'(P, Q, B) :- bool(4, Q, P, B).

'p<=>q'(P, Q, B) :- bool(7, P, Q, B).

/****************************************************************/
/* domain/3							*/
/****************************************************************/

domain(Vars, Min, Max) :-
	Goal = domain(Vars,Min,Max),
	must_be_dvar_list(Vars, Goal, 1),
	set_expression_check(Min..Max, Set, Goal, 0),
	domain(Vars, Set).

% FDBG puts advice on this!
domain([], _Set) :- !.
domain(Vars, Set) :-
	Set = [[A|B]], !,
	domain1(Vars, A, B, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
domain(Vars, Set) :-
	domain2(Vars, Set, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

% interval
domain1([], _, _, _).
domain1([X|Xs], A, B, Init) :-
	'$fd_in_interval'(X, A, B, Init),
	domain1(Xs, A, B, 0).

% set --- must maintain copies if multiple occs
domain2([], _, _).
domain2([X|Xs], Set, 1) :- !,
	'$fd_in_set'(X, Set, 1),
	domain2(Xs, Set, 0).
domain2([X|Xs], Set, 0) :-
	'$fd_dom_union'(Set, [], Copy),	% Set could occur multiple times
	'$fd_in_set'(X, Copy, 0),
	domain2(Xs, Set, 0).

%%% predicates corresponding to macro-expanded constraints

X in Expr :-
	in_aux_rt(X, Expr).

X in_set Set :-
	in_set_aux_rt(X, Set).

X #= Y :-
	fd_goal_expansion(X #= Y, clpfd, Goal),
	Goal.

X #\= Y :-
	fd_goal_expansion(X #\= Y, clpfd, Goal),
	Goal.

X #< Y :-
	fd_goal_expansion(X #< Y, clpfd, Goal),
	Goal.

X #=< Y :-
	fd_goal_expansion(X #=< Y, clpfd, Goal),
	Goal.

X #> Y :-
	fd_goal_expansion(X #> Y, clpfd, Goal),
	Goal.

X #>= Y :-
	fd_goal_expansion(X #>= Y, clpfd, Goal),
	Goal.

:- meta_predicate #\(:).
#\ Q :-
	fd_goal_expansion(#\ Q, clpfd, Goal),
	Goal.

:- meta_predicate #/\(:,:).
P #/\ Q :-
	fd_goal_expansion(P #/\ Q, clpfd, Goal),
	Goal.

:- meta_predicate #\(:,:).
P #\ Q :-
	fd_goal_expansion(P #\ Q, clpfd, Goal),
	Goal.

:- meta_predicate #\/(:,:).
P #\/ Q :-
	fd_goal_expansion(P #\/ Q, clpfd, Goal),
	Goal.

:- meta_predicate #=>(:,:).
P #=> Q :-
	fd_goal_expansion(P #=> Q, clpfd, Goal),
	Goal.

:- meta_predicate #<=(:,:).
P #<= Q :-
	fd_goal_expansion(P #<= Q, clpfd, Goal),
	Goal.

:- meta_predicate #<=>(:,:).
P #<=> Q :-
	fd_goal_expansion(P #<=> Q, clpfd, Goal),
	Goal.

/****************************************************************/
/* disjoint1/[1,2]                          			*/
/****************************************************************/

disjoint1(Items) :-
	disjoint1(Items, []).

disjoint1(Items, Options) :-
	Goal = disjoint1(Items,Options),
	must_be(Options, proper_list(callable), Goal, 2),
	disjoint1_options(Options, opt(0,inf,sup,[]), Opt, Goal, 2),
	(   Opt = opt(Flags,Min,B,_),
	    Flags /\ 2 =:= 2
	->  Max is B-1
	;   Min = inf, Max = sup
	),
	mkitems(Items, Items2, Min, Max, Goal, Susp, []),
	length(Items2, N),
	fd_global(Goal, f(N,Opt,Items2,N,0,_Handle,0), Susp).


disjoint1_options([], Opt, Opt, _, _).
disjoint1_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   disjoint1_option(X, Goal, Opt0, Opt1) -> true
        ;   illarg(domain(term,disjoint1_option), Goal, ArgNo, X)
        ),
	disjoint1_options(L, Opt1, Opt, Goal, ArgNo).

% opt(0bAMXD,Min,Max,Margins) where
% A = global
% M = Margins \== []
% X = wrap-around
% D = decomposition
% Min..Max is the interval subject to wrap-around
% Margins = list of margin(Type1,Type2,Diff) = list of extra margins
disjoint1_option(decomposition(B), _, opt(Flags0,Min,Max,Ms), opt(Flags,Min,Max,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -2) \/ Value.
disjoint1_option(global(B), _, opt(Flags0,Min,Max,Ms), opt(Flags,Min,Max,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -9) \/ (Value<<3).
disjoint1_option(wrap(Min,Max), _, opt(Flags0,_,_,Ms), opt(Flags,Min,Max,Ms)) :-
	(   Min==inf, Max==sup -> Flags is Flags0 /\ -3
	;   integer(Min),
	    integer(Max),
	    Min < Max,
	    Flags is (Flags0 /\ -3) \/ 2
	).
disjoint1_option(margin(T1,T2,D), _, opt(Flags0,Min,Max,Tail), opt(Flags,Min,Max,[margin(T1,T2,D)|Tail])) :-
	Flags is (Flags0 /\ -5) \/ 4.
disjoint1_option(lean(B), _, opt(Flags0,Min,Max,Ms), opt(Flags,Min,Max,Ms)) :-
	bool_option(B, Value),	% optimize for the incremental case
	Flags is (Flags0 /\ -33) \/ (Value<<5).


mkitems([], [], _, _, _) --> [].
mkitems([X|Xs], [item(S,SM,D,DM,Type)|Items], Min, Max, Goal) -->
	[minmax(S),min(D)],
	{arg(1,X,S), arg(2,X,D)},
	{arg(3,X,Type) -> true; Type=0},
	{finite_arg_attribute(S, SM, Goal, 1)},
	{finite_arg_attribute(D, DM, Goal, 1)},
	{propagate_interval(S, Min, Max)},
	mkitems(Xs, Items, Min, Max, Goal).

/****************************************************************/
/* disjoint2/[1,2]                          			*/
/****************************************************************/

disjoint2(Items) :-
	disjoint2(Items, []).

disjoint2(Items, Options) :-
	Goal = disjoint2(Items,Options),
	must_be(Options, proper_list(callable), Goal, 2),
	disjoint2_options(Options, opt(0,inf,sup,inf,sup,[]), Opt, Goal, 2),
	Opt = opt(Flags,Min1,B1,Min2,B2,_),
	(   Flags /\ 2 =:= 2
	->  Max1 is B1-1
	;   Min1 = inf, Max1 = sup
	),
	(   Flags /\ 4 =:= 4
	->  Max2 is B2-1
	;   Min2 = inf, Max2 = sup
	),
	mkitems(Items, Items2, Min1, Max1, Min2, Max2, Goal, Susp, []),
	length(Items2, N),
	fd_global(Goal, f(N,Opt,Items2,N,0,_Handle,0), Susp).


disjoint2_options([], Opt, Opt, _, _).
disjoint2_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   disjoint2_option(X, Goal, Opt0, Opt1) -> true
        ;   illarg(domain(term,disjoint2_option), Goal, ArgNo, X)
        ),
	disjoint2_options(L, Opt1, Opt, Goal, ArgNo).

% opt(0bAMYXD,Min1,Max1,Min2,Max2,Margins) where
% A = global
% M = Margins \== []
% Y = wrap-around in Y dim
% X = wrap-around in X dim
% D = decomposition
% Min..Max is the interval subject to wrap-around
% Margins = list of margin(Type1,Type2,Diff1,Diff2) = list of extra margins
disjoint2_option(decomposition(B), _, 
		 opt(Flags0,Min1,Max1,Min2,Max2,Ms), 
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -2) \/ Value.
disjoint2_option(global(B), _, 
		 opt(Flags0,Min1,Max1,Min2,Max2,Ms), 
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -17) \/ (Value<<4).
disjoint2_option(wrap(Min1,Max1,Min2,Max2), _, 
		 opt(Flags0,_,_,_,_,Ms), 
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	(   Min1==inf, Max1==sup -> Flags1 is (Flags0 /\ -3)
	;   integer(Min1),
	    integer(Max1),
	    Min1 < Max1,
	    Flags1 is (Flags0 /\ -3) \/ 2
	),
	(   Min2==inf, Max2==sup -> Flags is (Flags1 /\ -5)
	;   integer(Min2),
	    integer(Max2),
	    Min2 < Max2,
	    Flags is (Flags1 /\ -5) \/ 4
	).
disjoint2_option(margin(T1,T2,D1,D2), _, 
		 opt(Flags0,Min1,Max1,Min2,Max2,Tail), 
		 opt(Flags,Min1,Max1,Min2,Max2,[margin(T1,T2,D1,D2)|Tail])) :-
	Flags is (Flags0 /\ -9) \/ 8.
disjoint2_option(lean(B), _, 
		 opt(Flags0,Min1,Max1,Min2,Max2,Ms), 
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	bool_option(B, Value),	% optimize for the incremental case
	Flags is (Flags0 /\ -33) \/ (Value<<5).
disjoint2_option(synchronization(B), _, 
		 opt(Flags0,Min1,Max1,Min2,Max2,Ms), 
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -65) \/ (Value<<6).


% S1,S2 - start variables
% SM1,SM2 - start domain mutables
% D1,D2 - durations
% Type - type of object (optional)
mkitems([], [], _, _, _, _, _) --> [].
mkitems([X|Xs], [item(S1,SM1,D1,DM1,S2,SM2,D2,DM2,Type)|Items],
	Min1, Max1, Min2, Max2, Goal) -->
	[minmax(S1),min(D1),minmax(S2),min(D2)],
	{arg(1,X,S1), arg(2,X,D1), arg(3,X,S2), arg(4,X,D2)},
	{arg(5,X,Type) -> true; Type=0},
	{finite_arg_attribute(S1, SM1, Goal, 1)},
	{finite_arg_attribute(D1, DM1, Goal, 1)},
	{finite_arg_attribute(S2, SM2, Goal, 1)},
	{finite_arg_attribute(D2, DM2, Goal, 1)},
	{propagate_interval(S1, Min1, Max1)},
	{propagate_interval(S2, Min2, Max2)},
	mkitems(Xs, Items, Min1, Max1, Min2, Max2, Goal).


/****************************************************************/
/* case/[3,4]							*/
/****************************************************************/

% Consistency rules:
% VARS(DAG) = VARS(Template)
% VARS(OnSpec) \subseteq VARS(Template)
% VARS(PruneSpec) \subseteq VARS(Template)
% VARS(Tuples) \disjoint VARS(Template)
% every ID integer and unique
% all paths complete
case(Template, Tuples, Dag) :-
	case(Template, Tuples, Dag, []).

case(Template, Tuples, Dag, Options) :-
	Goal = case(Template,Tuples,Dag,Options),
	(   select(leaves(TLeaf,Leaves), Options, Options1)
	->  UseLeaf = 1,
	    Template1 = Template-TLeaf,
	    keys_values_pairs(Tuples, Leaves, Tuples1)
	;   Options1 = Options,
	    UseLeaf = 0,
	    TLeaf = [],
	    Leaves = [],
	    Template1 = Template,
	    Tuples1 = Tuples
	),
	prolog:'$term_variables'(Template1, Vars), % TLeaf must be last!
	sort(Vars, TemplateVars),
	prolog:term_variables(Dag-TLeaf, DagVars),
	prolog:term_variables(Options1, OptionsVars),
	prolog:term_variables(Tuples1, TuplesVars),
	(   DagVars==TemplateVars -> true
	;   illarg(consistency(Template,Dag,''), Goal, 3)
	),
	(   ord_subset(OptionsVars, TemplateVars) -> true
	;   illarg(consistency(Template,Options,''), Goal, 4)
	),
	(   ord_disjoint(TuplesVars, TemplateVars) -> true
	;   illarg(consistency(Template,Tuples,''), Goal, 2)
	),
	length(Vars, NVars),
	doms(NVars, Doms),
	must_be(Options1, proper_list(callable), Goal, 4),
	case_options(Options1, opt(Doms,Doms), opt(On,Prune), Vars, Goal, 4),
	length(Dag, NNodes),
	case_compile(Dag, Vars, Dag1, NChildren, IDs, Goal),
	(   UseLeaf=:=0 -> LeafSet = []
	;   LeafSet = [N],
	    var_nth(TLeaf, Vars, 0, N)
	),
	(   case_check_dag(Dag1, LeafSet, ID2Set, NVars)
	->  case_descendants(Dag1, ID2Set)
	;   illarg(consistency(Template,Dag,inconsistent_paths), Goal, 3)
	),
	list_to_fdset(IDs, IDset),
	domain(Leaves, IDset),
	case_post(Tuples1, Template1, Vars,
		  NVars, NNodes, NChildren, Dag1, On, Prune, UseLeaf, Goal).

doms(0, []) :- !.
doms(I, [dom|L]) :- J is I-1, doms(J, L).

case_options([], Opt, Opt, _, _, _).
case_options([X|L], Opt0, Opt, Vars, Goal, ArgNo) :-
	(   case_option(X, Vars, Opt0, Opt1) -> true
        ;   illarg(domain(term,case_option), Goal, ArgNo, X)
        ),
	case_options(L, Opt1, Opt, Vars, Goal, ArgNo).

case_option(on(Spec), Vars, opt(On0,Prune), opt(On,Prune)) :-
	case_spec(Spec, F, Var),
	var_nth(Var, Vars, 0, N),
	replace_nth(N, On0, F, On).
case_option(prune(Spec), Vars, opt(On,Prune0), opt(On,Prune)) :-
	case_spec(Spec, F, Var),
	var_nth(Var, Vars, 0, N),
	replace_nth(N, Prune0, F, Prune).

case_spec(dom(X), dom, X).
case_spec(min(X), min, X).
case_spec(max(X), max, X).
case_spec(minmax(X), minmax, X).
case_spec(val(X), val, X).
case_spec(none(X), none, X).

replace_nth(0, [_|L], F, [F|L]) :- !.
replace_nth(I, [X|L1], F, [X|L2]) :-
	J is I-1,
	replace_nth(J, L1, F, L2).

case_post(Tuples1, Template, Vars,
	  NVars, NNodes, NChildren, Dag, On, Prune, UseLeaf, Goal) :-
	copy_tuples(Tuples1, Tuples2, Template, Vars),
	case_susp(Tuples2, Tuples3, Goal, 0, Susp, []),
	length(Tuples3, N),
	fd_global_internal(case(_,_,_,_),
			   state(f(NVars,NNodes,NChildren,Tuples3,Dag,On,Prune,UseLeaf),N/*entailment counter*/,_Handle,0),
			   Susp, _, clpfd:Goal, 0).

copy_tuples([], [], _, _).
copy_tuples([Row1|Rows1], [Row2|Rows2], Template, Vars) :-
	copy_term(Template-Vars, Row1-Row2),
	copy_tuples(Rows1, Rows2, Template, Vars).

case_susp([], [], _, _) --> [].
case_susp([Row|Rows], [TRow|TRows], G, A) -->
	dvar_list_susp(Row, TRow, none, G, A),
	case_susp(Rows, TRows, G, A).	

case_compile(Dag, Vars, Dag1, NChildren, IDs, Goal) :-
	empty_avl(ID2Index0),
	case_map(Dag, 0, Dag1, ID2Index0, ID2Index, IDs0, Goal),
	sort(IDs0, IDs),
	(   same_length(IDs0, IDs) -> true
	;   illarg(consistency(Dag,Dag,''), Goal, 3)
	),
	case_compile_dag(Dag, Vars, Dag1, 0, NChildren, ID2Index, Goal).

case_compile_dag([], _, [], NC, NC, _, _).
case_compile_dag([Node|Nodes], Vars, [Node1|Nodes1], NC0, NC, ID2Index, Goal) :-
	Node = node(ID,Var,Children),
	Node1 = dagnode(ID,N,_,Children3,Children),
	var_nth(Var, Vars, 0, N),
	case_children(Children, ID2Index, Children0, Children1),
	keysort(Children1, Children2),
	append(Children0, Children2, Children3),
	(   \+(Children0 = [_,_|_]),
	    case_disjoint_descendants(Children3) -> true
	;   illarg(consistency(Children,Children,''), Goal, 3)
	),
	length(Children3, Len3),
	NC1 is NC0+Len3,
	case_compile_dag(Nodes, Vars, Nodes1, NC1, NC, ID2Index, Goal).

case_children([], _, [], []).
case_children([inf..Max|L1], ID2Index, [(inf..Max)-[]|Ch0], Ch2) :- !,
	case_children(L1, ID2Index, Ch0, Ch2).
case_children([(inf..Max)-ID|L1], ID2Index, [(inf..Max)-Index|Ch0], Ch2) :- !,
	(   integer(ID), avl_fetch(ID, ID2Index, Index) -> true
	;   Index = []
	),
	case_children(L1, ID2Index, Ch0, Ch2).
case_children([Min..Max|L1], ID2Index, Ch0, [(Min..Max)-[]|Ch2]) :- !,
	case_children(L1, ID2Index, Ch0, Ch2).
case_children([(Min..Max)-ID|L1], ID2Index, Ch0, [(Min..Max)-Index|Ch2]) :-
	(   integer(ID), avl_fetch(ID, ID2Index, Index) -> true
	;   Index = []
	),
	case_children(L1, ID2Index, Ch0, Ch2).

case_disjoint_descendants([]).
case_disjoint_descendants([(Min..Max)-_|Children]) :-
	le(Min, Max),
	case_disjoint_descendants(Children, Max).

case_disjoint_descendants([], _).
case_disjoint_descendants([(Min..Max)-_|Children], Pred) :-
	Pred\==Min,
	le(Pred, Min),
	le(Min, Max),
	case_disjoint_descendants(Children, Max).

le(inf, _) :- !.
le(_, sup) :- !.
le(A, B) :- integer(A), integer(B), A=<B.

case_map([], _, [], A, A, [], _).
case_map([node(ID,_,Children)|Nodes], I, [_|Nodes1], A0, A, IDs0, Goal) :-
	must_be(ID, integer, Goal, 3),
	case_leaf(Children, ID, IDs0, IDs),
	avl_store(ID, A0, I, A1),
	J is I+1,
	case_map(Nodes, J, Nodes1, A1, A, IDs, Goal).

case_leaf([_.._|_], ID) --> !, [ID].
case_leaf(_, _) --> [].

case_check_dag(DagNodes, Set0, A, NVars) :-
	dag_vertices_edges(DagNodes, Vertices, Edges),
	vertices_edges_to_ugraph(Vertices, Edges, Graph),
	top_sort(Graph, IDs0),
	reverse(IDs0, IDs),
	ord_list_to_avl([[]-Set0], A0),
	check_paths(IDs, DagNodes, A0, A),
	DagNodes = [dagnode(RootID,_,_,_,_)|_],
	avl_fetch(RootID, A, RootSet),
	length(RootSet, NVars).

dag_vertices_edges([], [], []).
dag_vertices_edges([dagnode(ID,_,_,_,Children)|Dag], [ID|IDs], Edges0) :-
	children_edges(Children, ID, Edges0, Edges),
	dag_vertices_edges(Dag, IDs, Edges).

children_edges([], _ID) --> [].
children_edges([_-X|Children], ID) --> {integer(X)}, !, [ID-X],
	children_edges(Children, ID).
children_edges([_|Children], ID) -->
	children_edges(Children, ID).

check_paths([], _, A, A).
check_paths([ID|IDs], DagNodes, A0, A) :-
	select(dagnode(ID,Var,_,_,Children), DagNodes, DagNodes1), !,
	children_set(Children, A0, Set),
	ord_add_element(Set, Var, Set1),
	Set\==Set1,
	avl_store(ID, A0, Set1, A1),
	check_paths(IDs, DagNodes1, A1, A).

children_set([Child|Children], A, Set) :-
	child_parts(Child, _, ID),
	avl_fetch(ID, A, Set0),
	children_set(Children, A, Set0, Set).
	
children_set([], _, Set, Set).
children_set([Child|Children], A, Set0, Set) :-
	child_parts(Child, _, ID),
	avl_fetch(ID, A, Set1),
	Set0==Set1,
	children_set(Children, A, Set0, Set).

child_parts(R-ID, R, ID) :- !.
child_parts(R, R, []).

case_descendants([], _).
case_descendants([dagnode(ID,_,FDSet,_,_)|DagNodes], ID2Set) :-
	avl_fetch(ID, ID2Set, Set),
	list_to_fdset(Set, FDSet),
	case_descendants(DagNodes, ID2Set).

/****************************************************************/
/* table/[2,3]        						*/
/****************************************************************/

table(Tuples, Extension1) :-
	table(Tuples, Extension1, []).

table(Tuples, Extension1, Options) :-
	Goal = table(Tuples,Extension1,Options),
	(   memberchk(consistency(Arg), Options)
	->  consistency_option(Arg, Cons, _)
	;   Cons = global
	),
	Tuples = [Tuple|_],
	length(Tuple, N),
	table_fdsets(Extension1, Extension2, Goal),
	(   N=:=0 -> true
	;   N=:=1, Cons==global
	->  append(Extension2, List),
	    fdset_union(List, FD),
	    append(Tuples, Vars),
	    domain(Vars, FD)
	;   N=:=2, Cons==global
	->  table_binary(Tuples, Extension2, Goal)
	;   table_general(Extension2, Dag, N),
	    length(Dag, NbNodes),
	    nb_children(Dag, 0, NbChildren),
	    length(Template, N),
	    consistency_spec(Cons, N, Spec),
	    case_post(Tuples, Template, Template, N, NbNodes, NbChildren, Dag,
		      Spec, Spec, 0, Goal)
	).

nb_children([], K, K).
nb_children([dagnode(_,_,_,Ch)|Dag], I, K) :-
	length(Ch, N),
	J is I+N,
	nb_children(Dag, J, K).

table_binary(Tuples, Rows1, Goal) :-
	all_events(0, 2, Rows1, Events),
	all_expand(Rows1, Events, Rows2, []),
	sort(Rows2, Rows3),
	table_tree(Rows3, Data, 2),
	keys_and_values_each(Data, Xs, Ys),
	relation_post(Xs, Ys, 0, Tuples, Goal).

keys_and_values_each([], [], []).
keys_and_values_each([Int-V1|L1], [[Int]|L2], [V2|L3]) :-
	keys_and_values(V1, V2, _),
	keys_and_values_each(L1, L2, L3).

table_fdsets([], [], _).
table_fdsets([Row1|Rows1], [Row2|Rows2], Goal) :-
	row_fdsets(Row1, Row2, Goal),
	table_fdsets(Rows1, Rows2, Goal).

row_fdsets([], [], _).
row_fdsets([X|Row1], [Y|Row2], Goal) :-
	(   integer(X) ->
	    union_expression_check(X, Y, Goal, 2)
	;   set_expression_check(X, Y, Goal, 2)
	),
	row_fdsets(Row1, Row2, Goal).

all_events(N, N, _, []) :- !.
all_events(I, N, Rows, [Ev2|Evs]) :-
	J is I+1,
	arg_events(Rows, J, Ev1, []),
	sort(Ev1, Ev2),
	all_events(J, N, Rows, Evs).

arg_events([], _) --> [].
arg_events([Row|Rows], A) -->
	{nth1(A, Row, Set)},
	set_events(Set),
	arg_events(Rows, A).

set_events([]) --> [].
set_events([[X|Y]|Set]) --> [X,Z],
	{Z is Y+1},
	set_events(Set).

all_expand([], _) --> [].
all_expand([Row|Rows], Events) -->
	all_split(Row, Events, []),
	all_expand(Rows, Events).

all_split([], [], Expansion) --> [Expansion].
all_split([Set|Row], [Ev|Events], Expansion) -->
	{split_set(Set, Ev, Int, [])},
	each_split(Int, Row, Events, Expansion).

split_set([], _) --> !.
split_set([Unit|Set], Events) --> [Unit],
	{Unit = [E|E]}, !,
	split_set(Set, Events).
split_set(Set1, Events1) --> [[X|X1]],
	{fdset_min(Set1, X)},
	{suffix(Events1, [X,X2|Events2])}, !,
	{X1 is X2-1},
	{fdset_parts(Sub, X, X1, [])},
	{fdset_subtract(Set1, Sub, Set2)},
	split_set(Set2, [X2|Events2]).

each_split([], _, _, _) --> [].
each_split([Int|Ints], Row, Events, Expansion1) -->
	{append(Expansion1, [Int], Expansion2)},
	all_split(Row, Events, Expansion2),
	each_split(Ints, Row, Events, Expansion1).

table_tree(Rows, Tree, N) :-
	table_tree(0, N, [], Tree, Rows, []).

table_tree(N, N, _, []) --> !, [_].
table_tree(I, N, Prefix, Tree2) -->
	{J is I+1},
	raw_tree(J, N, Prefix, Tree1),
	{merge_subtrees(Tree1, Tree2)}.

raw_tree(J, N, Prefix, [Elt-Tree1|Rest], [Row|S0], S) :-
	append(Prefix, [Elt|_], Row), !,
	append(Prefix, [Elt], Prefix1),
	table_tree(J, N, Prefix1, Tree1, [Row|S0], S1),
	raw_tree(J, N, Prefix, Rest, S1, S).
raw_tree(_, _, _, [], S, S).

merge_subtrees([], []).
merge_subtrees([[A|B]-T,[C|D]-T|L1], L2) :-
	B+1 =:= C, !,
	merge_subtrees([[A|D]-T|L1], L2).
merge_subtrees([X|L1], [X|L2]) :-
	merge_subtrees(L1, L2).


table_general(Rows1, Nodes, N) :-
	all_events(0, N, Rows1, Events),
	all_expand(Rows1, Events, Rows2, []),
	sort(Rows2, Rows3),
	table_tree(Rows3, Tree, N),
	empty_avl(A1),
	tree_to_dag(Tree, _ID, 0, A1, A2),
	avl_to_list(A2, KeyList),
	M is N-1,
	dag_nodes(KeyList, Nodes, 0, M).

tree_to_dag([], [], _, A, A) :- !.
tree_to_dag(Children, ID, Depth, A1, A3) :-
	Depth1 is Depth+1,
	children_to_dags(Children, Dags, Depth1, A1, A2),
	Node = node(Depth,Dags),
	(   avl_fetch(Node, A2, ID) -> A2 = A3
	;   avl_store(Node, A2, ID, A3)
	).

children_to_dags([], [], _, A, A).
children_to_dags([[Min|Max]-Tree|L1], [(Min..Max)-ID|L2], D, A1, A3) :-
	tree_to_dag(Tree, ID, D, A1, A2),
	children_to_dags(L1, L2, D, A2, A3).

dag_nodes([], [], _, _).
dag_nodes([node(D,Children)-J|L1], [dagnode(J,D,[[D|N]],Children)|L2], J, N) :-
	K is J+1,
	dag_nodes(L1, L2, K, N).

consistency_spec(global, N, Spec) :-
	multiple(N, dom, Spec).
consistency_spec(bound, N, Spec) :-
	multiple(N, minmax, Spec).
consistency_spec(local, N, Spec) :-
	multiple(N, value, Spec).

multiple(0, _, []) :- !.
multiple(I, X, [X|L]) :-
	J is I-1,
	multiple(J, X, L).

relation_post(Xs0, Ys0, Opt0, Pairs, Goal) :-
	keys_values_pairs(Xs0, Ys0, XY),
	(   Opt0=:=0,
	    all_disjoint(Xs0, Ys0, []) -> Opt = 2
	;   Opt = Opt0
	),
	fdset_union(Xs0, UX),
	fdset_union(Ys0, UY),
	rel_tuples(Pairs, Tuples, Susp, UX, UY, Goal),
	length(Tuples, NT),
	fd_global_internal(relation(_,_,_), f(XY,Opt,Tuples,NT/*entailment counter*/,_handle,0), Susp,
			   _, clpfd:Goal, 0).

rel_tuples([], [], [], _, _, _).
rel_tuples([[X,Y]|L1], [f(X,XM,UXMut,Y,YM,UYMut)|L2], [none(X),none(Y)|S], UX, UY, Goal) :-
	X in_set UX,
	Y in_set UY,
	create_mutable(UX, UXMut),
	create_mutable(UY, UYMut),
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Y, YM, Goal, 3),
	rel_tuples(L1, L2, S, UX, UY, Goal).

% Check whether the Y sets _per X interval_ are disjoint
all_disjoint([], [], _).
all_disjoint([[_]|Xs], [Inc|Ys], Set1) :-
	'$fd_dom_intersection'(Set1, Inc, []),
	'$fd_dom_union'(Set1, Inc, Set2),
	all_disjoint(Xs, Ys, Set2).

/****************************************************************/
/* cumulatives/[2,3]  						*/
/****************************************************************/

cumulatives(Tasks0, Machines) :-
	cumulatives(Tasks0, Machines, []).
		
cumulatives(Tasks, Machines1, Options) :-
	Goal = cumulatives(Tasks, Machines1, Options),
	must_be(Options, proper_list(callable), Goal, 3),
	cumulatives_options(Options, 0, Opt, Goal, 3),
	cumulatives_convert(Tasks, Tasks2, Goal, 1),
	cumulatives_suspend(Tasks, Susp, []),
	length(Tasks2, NT),
	length(Machines1, NM),
	sort(Machines1, Machines2),
	fd_global(Goal, f(NT,NM,Opt,Tasks2,Machines2,NT,0,_Handle,0), Susp).

cumulatives_options([], Opt, Opt, _, _).
cumulatives_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   cumulatives_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,cumulatives_option), Goal, ArgNo, X)
        ),
	cumulatives_options(L, Opt1, Opt, Goal, ArgNo).

%%% Valid options:
%%% bound(lower|upper)   ==> 0x0|0x1
%%% generalization(Bool) ==> 0x0|0x2
%%% task_intervals(Bool) ==> 0x0|0x4
%%% prune(all|next)      ==> 0x0|0x8

cumulatives_option(bound(B), Opt0, Opt) :-
	aux_option(bound, B, Value),
	Opt is (Opt0 /\ -2) \/ Value.
cumulatives_option(generalization(B), Opt0, Opt) :-
	bool_option(B, Value),
	Opt is (Opt0 /\ -3) \/ (Value<<1).
cumulatives_option(task_intervals(B), Opt0, Opt) :-
	bool_option(B, Value),
	Opt is (Opt0 /\ -9) \/ (Value<<2).
cumulatives_option(prune(B), Opt0, Opt) :-
	aux_option(prune, B, Value),
	Opt is (Opt0 /\ -17) \/ (Value<<3).

cumulatives_convert([], [], _, _).
cumulatives_convert([task(Org,Dur,End,Height,Machine)|Tasks1],
		    [task(Org-OA,Dur-DA,End-EA,Height-HA,Machine-MA)|Tasks2],
		    Goal, ArgNo) :-
	finite_arg_attribute(Org, OA, Goal, ArgNo),
	finite_arg_attribute(Dur, DA, Goal, ArgNo),
	finite_arg_attribute(End, EA, Goal, ArgNo),
	finite_arg_attribute(Height, HA, Goal, ArgNo),
	finite_arg_attribute(Machine, MA, Goal, ArgNo),
	cumulatives_convert(Tasks1, Tasks2, Goal, ArgNo).

cumulatives_suspend([]) --> [].
cumulatives_suspend([task(Org,Dur,End,Height,Machine)|Tasks1]) -->
	on_minmax([Org,Dur,End,Height]),
	on_dom([Machine]),
	cumulatives_suspend(Tasks1).

/****************************************************************/
/* global_cardinality/[2,3]					*/
/****************************************************************/

global_cardinality(Xs, Ys) :-
	global_cardinality(Xs, Ys, []).

global_cardinality(Xs, Ys, Opt) :-
	global_cardinality(Xs, Ys, Opt, opt(global,dom,0,[]), global_cardinality(Xs,Ys,Opt)).

global_cardinality(Vars, Vals1, Options, Opt0, Goal) :-
	must_be(Options, proper_list(callable), Goal, 3),
	gcc_options(Options, Opt0, Opt, Goal, 3),
	Opt = opt(Cons,On,Flag,Cost),	% Cost is [] or cost(UB,Matrix)
	must_be_dvar_list(Vars, Goal, 1),
	length(Vars, N),
	gcc_keys_counts(Vals1, Vals2, Goal, 2),
	keys_and_values(Vals1, KeyList, Counts),
	list_to_fdset(KeyList, KeySet),
	length(KeyList, M),
	'$fd_size'(KeySet, M, 1), !, % no duplicates
	keysort(Vals2, SVals),
	domain(Vars, KeySet),
	domain(Counts, 0, N),
	on_all(Vars, On, Susp, Susp1),
	on_minmax(Counts, Susp1, Susp2),
	(   Flag=\=0 ->
	    Cost=cost(C,_,_),
	    Susp2 = [dom(C)],
	    fd_global_internal(Goal,
			       f(N,M,Vars,SVals,Flag,Cost,0/*cost so far*/,_Handle,0),
			       Susp, Global, clpfd:Goal, 0),
	    Global = global(StateM,_,_,_,_),
	    global_cardinality_helpers(Vars, C, 0, StateM)
	;   Cons==local ->
	    Susp2=[],
	    fd_global_internal(local_cardinality(_,_),
			       f(N,M,Vars,SVals,_Handle,0),
			       Susp, Global, clpfd:Goal, 0)
	;   Susp2=[],
	    fd_global(Goal, f(N,M,Vars,SVals,Flag,Cost,0/*cost so far*/,_Handle,0), Susp)
	).
global_cardinality(_Vars, _Vals, _Options, _Opt0, Goal) :-
	illarg(domain(list(pair),global_cardinality_value_list), Goal, 2).

global_cardinality_helpers([], _, _, _).
global_cardinality_helpers([X|Xs], C, I, StateM) :-
	fd_global_internal(global_cardinality_helper(X,C), f(I,X,StateM), [val(X)],
			   _, true, 0),
	J is I+1,
	global_cardinality_helpers(Xs, C, J, StateM).

gcc_keys_counts(V, _, _Goal, _ArgNo) :- var(V), !, fail.
gcc_keys_counts([], [], _Goal, _ArgNo).
gcc_keys_counts([Val|Vals], [Val2|Vals2], Goal, ArgNo) :-
	nonvar(Val),
	Val = K-Count,
	Val2 = K-(Count-CA),
	arg_attribute(Count, CA, Goal, ArgNo),
	gcc_keys_counts(Vals, Vals2, Goal, ArgNo).

gcc_options([], Opt, Opt, _, _).
gcc_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   gcc_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,global_cardinality_option), Goal, ArgNo, X)
        ),
	gcc_options(L, Opt1, Opt, Goal, ArgNo).

gcc_option(consistency(Arg), opt(_,_,VM,Mat), opt(Cons,On,VM,Mat)) :-
	consistency_option(Arg, Cons, On).
gcc_option(on(On), opt(Cons,_,VM,Mat), opt(Cons,On,VM,Mat)) :-
	on(On, _, _, _).
gcc_option(cost(V,Mat), opt(Cons,On,_,_), opt(Cons,On,1,cost(V,VM,Mat))) :-
	arg_attribute(V, VM, 0, 3).

/****************************************************************/
/* sorting/3							*/
/****************************************************************/

sorting(Xs, Ps, Ys) :-
	Goal = sorting(Xs,Ps,Ys),
	must_be_dvar_list(Xs, Goal, 1),
	must_be_dvar_list(Ps, Goal, 2),
	must_be_dvar_list(Ys, Goal, 3),
	length(Xs, N),
	length(Ps, N),
	length(Ys, N),
	'$fd_range'(1, N, Set, 1),
	domain(Ps, Set),
	dvar_list_susp(Xs, XVec, minmax, Goal, 1, Susp , Susp1),
	dvar_list_susp(Ps, PVec, minmax, Goal, 2, Susp1, Susp2),
	dvar_list_susp(Ys, YVec, minmax, Goal, 3, Susp2, []),
	fd_global(Goal, f(XVec,PVec,YVec,N,
			  0/*NShaved*/,0/*YOffset*/,_Handle,0), Susp).

/****************************************************************/
/* lex_chain/[2,3]						*/
/****************************************************************/

lex_chain(Tuples) :-
	lex_chain(Tuples, []).

lex_chain(Tuples, Options) :-
	Goal = lex_chain(Tuples, Options),
	must_be(Options, proper_list(callable), Goal, 2),
	lex_chain_options(Options, opt(0,[]), Opt, Goal, 2),
	length(Tuples, NT),
	length(EntFlags, NT),	% keep track of entailed pairs
	each_must_be_list_of_finite_dvar(Tuples, NV, Goal, 1),
	each_dvar_list_susp(Tuples, Matrix, dom, Goal, 1, _Susp, []),
	Opt = opt(Flag,Among),
	(   Flag/\2 =:= 0 -> true
	;   Among = among(L,U,Vals),
	    Vals\==[], L=<U -> true
	;   illarg(consistency(Options,Options,''), Goal, 2)
	),
	fd_global(Goal, state(Matrix,NT,NV,Flag,Among,EntFlags,_/*handle*/,0/*stamp*/), []/*Susp*/).

lex_chain_options([], Opt, Opt, _, _).
lex_chain_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   lex_chain_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,lex_chain_option), Goal, ArgNo, X)
        ),
	lex_chain_options(L, Opt1, Opt, Goal, ArgNo).

lex_chain_option(op(#=<), opt(F0,A), opt(F,A)) :- !,
	F is (F0/\6).
lex_chain_option(op(#<), opt(F0,A), opt(F,A)) :- !,
	F is (F0/\6)\/1.
lex_chain_option(increasing, opt(F0,A), opt(F,A)) :- !,
	F is (F0/\3)\/4.
lex_chain_option(among(L,U,Vals0), opt(F0,_), opt(F,among(L,U,Vals))) :- !,
	F is (F0/\5)\/2,
	list_to_fdset(Vals0, Vals).

each_must_be_list_of_finite_dvar([], _, _, _).
each_must_be_list_of_finite_dvar([T|Ts], NV, Goal, ArgNo) :-
	length(T, NV),
	must_be_list_of_finite_dvar(T, Goal, ArgNo),
	each_must_be_list_of_finite_dvar(Ts, NV, Goal, ArgNo).

each_dvar_list_susp([], [], _, _, _) --> [].
each_dvar_list_susp([T|Ts], [R|Rs], What, Goal, ArgNo) -->
	dvar_list_susp(T, R, What, Goal, ArgNo),
	each_dvar_list_susp(Ts, Rs, What, Goal, ArgNo).

on_dom([]) --> [].
on_dom([X|Xs]) --> [dom(X)], on_dom(Xs).

on_minmax([]) --> [].
on_minmax([X|Xs]) --> [minmax(X)], on_minmax(Xs).

on_val([]) --> [].
on_val([X|Xs]) --> [val(X)], on_val(Xs).

%%% Option parsing

bool_option(-, _) :- !, fail.
bool_option(false, 0).
bool_option(true, 1).

aux_option(_, -, _) :- !, fail.
aux_option(bound, lower, 0) :- !.
aux_option(bound, upper, 1).
aux_option(prune, all, 0) :- !.
aux_option(prune, next, 1).

consistency_option(local, local, val).
consistency_option(value, local, val).
consistency_option(bound, bound, minmax).
consistency_option(bounds, bound, minmax).
consistency_option(global, global, dom).
consistency_option(domain, global, dom).

on_all([], _) --> [].
on_all([X|Xs], K) -->
	on(K, X),
	on_all(Xs, K).

on(_, X) --> {nonvar(X)}, !.
on(dom, X) --> [dom(X)].
on(min, X) --> [min(X)].
on(max, X) --> [max(X)].
on(minmax, X) --> [minmax(X)].
on(val, X) --> [val(X)].
on(none, X) --> [none(X)].
% the following are deprecated
on(domain, X) --> [dom(X)].
on(range, X) --> [minmax(X)].
on(value, X) --> [val(X)].
on(global, X) --> [dom(X)].
on(bound, X) --> [minmax(X)].
on(local, X) --> [val(X)].

keys_values_pairs([], [], []).
keys_values_pairs([X|Xs], [C|Cs], [X-C|XCs]) :-
	keys_values_pairs(Xs, Cs, XCs).

