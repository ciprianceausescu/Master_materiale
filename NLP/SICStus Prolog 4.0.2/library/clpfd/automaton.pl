% Proposed Prolog interface for a general automaton predicate,
% which decomposes the automaton to a system of constraints.

% Any transition not mentioned is assumed to go to an implicit failure node.

% An abstract grammar of the Prolog terms that we will need:

% Args ::= list of Arg /* they must all have the same shape */

% Arg  ::= term        /* most general shape of the Args */
                       /* its variables should be local to the constraint */

% Signature ::= list of dvar

% Nodes ::= list of NodeSpec

% NodeSpec ::= source(Node) /* Node is the initial state */
%            | sink(Node)   /* Node is an accept state */

% Node ::= atomic

% Arcs  ::= list of Arc

% Arc      ::= arc(Node1,integer,Node2)
%            | arc(Node1,integer,Node2,Exprs)
%            | arc(Node1,integer,Node2,(Cond -> Exprs))

% Conditional ::= (Cond -> Exprs)
%               | (Conditional ; Conditional)

% Exprs := list of FD arith expr /* over Counters, Arg and constants */
                                 /* same length as Counters */

% Cond := any constraint /* on Counters, Arg and constants */

% Counters ::= list of var    /* should be local to the constraint */

% Initial ::= list of integer /* same length as Counters */

% Final ::= list of dvar      /* same length as Counters */

% AutoConstraint ::= automaton(Args, Arg, Signature,
%                              Nodes, Arcs,
% 			       Counters, Initial, Final)

automaton(CtrArgs, CtrTuple, Signature,
	  Nodes, Arcs1,
	  Counters, Initial, Final) :-
	automaton_collect_nodes(Arcs1, Nodes1, []),
	sort(Nodes1, Nodes2),
	automaton_enum(Nodes2, 0, Nodes3, []),
	list_to_avl(Nodes3, Node2Int),
	automaton_map_arcs(Arcs1, Node2Int, Counters, Arcs2),
	sort(Arcs2, Arcs3),
	automaton_map_arcs_case(CtrTuple, Counters, Arcs3, Closure, Clause),
	automaton_state(source, Nodes, S1, Node2Int),
	automaton_state(sink, Nodes, Sn, Node2Int),
	automaton_thread(Signature, S1, Sn, Args, []),
	automaton_call(Closure, Args, Initial, Final, CtrArgs, Clause).

automaton_state(F, Nodes, S1, Avl) :-
	automaton_state_list(Nodes, F, List, Avl),
	list_to_fdset(List, Set),
	S1 in_set Set.

automaton_state_list([], _, [], _).
automaton_state_list([Node|Nodes], F, [Y|List], Avl) :-
	Node =.. [F,X], !,
	avl_fetch(X, Avl, Y),
	automaton_state_list(Nodes, F, List, Avl).
automaton_state_list([_|Nodes], F, List, Avl) :-
	automaton_state_list(Nodes, F, List, Avl).

automaton_collect_nodes([]) --> [].
automaton_collect_nodes([arc(N1,_,N2)|Arcs]) --> !, [N1,N2],
	automaton_collect_nodes(Arcs).
automaton_collect_nodes([arc(N1,_,N2,_)|Arcs]) --> !, [N1,N2],
	automaton_collect_nodes(Arcs).

automaton_enum([], _) --> [].
automaton_enum([N|Ns], I) --> [N-J],
	{J is I+1},
	automaton_enum(Ns, J).

automaton_map_arcs([], _, _, []).
automaton_map_arcs([Arc1|Arcs1], Map, Counters, [arc(From2,Via,To2,Expr)|Arcs2]) :-
	automaton_normalize(Arc1, Counters, arc(From1,Via,To1,Expr)),
	avl_fetch(From1, Map, From2),
	avl_fetch(To1, Map, To2),
	automaton_map_arcs(Arcs1, Map, Counters, Arcs2).

automaton_normalize(arc(From,Via,To), Counters, arc(From,Via,To,(true->Counters))) :- !.
automaton_normalize(arc(From,Via,To,Counters), _, arc(From,Via,To,(true->Counters))) :-
	\+functor(Counters, ->, 2),
	\+functor(Counters,  ;, 2), !.
automaton_normalize(Arc, _, Arc).

automaton_map_arcs_case(CtrTuple, Counters1, Arcs,
			closure(CaseArgs,Ls, case(Tuple,CaseArgs,CaseDag,[leaves(_,Ls)])),
			Clause) :-
	Tuple = f(From,Via,To),
	automaton_arcs_leaves(Arcs, Leaves1),
	sort(Leaves1, Leaves2),
	automaton_dag_tail(Leaves2, 1, Leaves3, To, CaseDagTail, []),
	ord_list_to_avl(Leaves3, Leaf2ID),
	automaton_compile(Arcs, From, Via, Leaf2ID, CaseDag, CaseDagTail),
	automaton_aux_code(Leaf, Leaves2, Counters2, AuxGoals, []),
	commafy(AuxGoals, AuxCode),
	Clause = (aux(CtrTuple,Counters1,Counters2,Leaf) :- AuxCode).

automaton_aux_code(Leaf, Transitions, Counters2) -->
	automaton_expression_code(Transitions, Expr2Var),
	automaton_cond_code(Transitions, Indices, CounterCases, 1, 1, Leaf, Expr2Var),
	automaton_emit(Leaf,Indices,Index),
	(   {transpose(CounterCases, CounterCasesT)} ->
	    automaton_index_counters_code(CounterCasesT, Counters2, Index)
	;   {Counters2 = []}
	).

automaton_expression_code(Transitions, Expr2Var) -->
	{automaton_all_exprs(Transitions, L1, [])},
	{sort(L1, L2)},
	automaton_expression_code1(L2, L3),
	{ord_list_to_avl(L3, Expr2Var)}.

automaton_all_exprs([]) --> [].
automaton_all_exprs([_-Dest|L]) -->
	{automaton_args(Dest, ;, Cases, [])},
	automaton_exprs(Cases),
	automaton_all_exprs(L).

automaton_exprs([]) --> [].
automaton_exprs([(_->E)|L]) -->
	dlist(E),
	automaton_exprs(L).

automaton_expression_code1([], []) --> [].
automaton_expression_code1([X|L1], [X-X|L2]) -->
	{simple(X)}, !,
	automaton_expression_code1(L1, L2).
automaton_expression_code1([X|L1], [X-Y|L2]) -->
	[Y #= X],
	automaton_expression_code1(L1, L2).

automaton_index_counters_code([], [], _) --> [].
automaton_index_counters_code([Row|Rows], [C|Cs], Index) -->
	automaton_emit(Index, Row, C),
	automaton_index_counters_code(Rows, Cs, Index).

automaton_cond_code([], [], [], _, _, _, _) --> [].
automaton_cond_code([_-Dest|Transitions], [Ix|Ixs], CtrCases1, NextL, NextI, Leaf, Expr2Var) -->
	{automaton_args(Dest, ;, Cases, [])},
	{length(Cases, NC)},
	(   {Cases=[(true->_)]}
	->  {Ix = NextI}
	;   {Cases=[(P->_)]}
	->  {Ix = NextI},
	    [Leaf#=NextL #=> P]
	;   {MaxI is NextI+NC-1},
	    [Leaf#=NextL #=> Ix in NextI..MaxI],
	    automaton_reify(Cases, Ix, NextI)
	),
	{automaton_counter_code(Cases, CtrCases1, CtrCases2, Expr2Var)},
	{NextL1 is NextL+1},
	{NextI1 is NextI+NC},
	automaton_cond_code(Transitions, Ixs, CtrCases2, NextL1, NextI1, Leaf, Expr2Var).

automaton_reify([], _, _) --> [].
automaton_reify([(P->_)|Cases], Ix, I) --> [P #<=> Ix#=I],
	{J is I+1},
	automaton_reify(Cases, Ix, J).

automaton_counter_code([], Columns, Columns, _).
automaton_counter_code([(_->Tuple)|Cases], [Col|Cols1], Cols2, Expr2Var) :-
	automaton_counter_code(Tuple, Col, Expr2Var),
	automaton_counter_code(Cases, Cols1, Cols2, Expr2Var).

automaton_counter_code([], [], _).
automaton_counter_code([X|Tuple], [Y|Col], Expr2Var) :-
	avl_fetch(X, Expr2Var, Y),
	automaton_counter_code(Tuple, Col, Expr2Var).

automaton_args(Term, F) -->
	{Term =.. [F,X,Y]}, !,
	automaton_args(X, F),
	automaton_args(Y, F).
automaton_args(X, _) --> [X].

automaton_emit(1, [X], X) --> !.
automaton_emit(N, List, Nth) -->
	{automaton_from(List, 1)}, !,
	{Nth = N}.
automaton_emit(_, [X|List], Nth) -->
	{automaton_same(List, X)}, !,
	{Nth = X}.
automaton_emit(N, List, Nth) -->
	[element(N, List, Nth)].

automaton_from([], _).
automaton_from([X|L], Y) :-
	X==Y, !,
	Z is Y+1,
	automaton_from(L, Z).

automaton_same([], _).
automaton_same([X|L], Y) :-
	X==Y, !,
	automaton_same(L, Y).
	 
automaton_arcs_leaves([], []).
automaton_arcs_leaves([arc(_,_,To,Expr)|L1], [To-Expr|L2]) :-
	automaton_arcs_leaves(L1, L2).

automaton_dag_tail([], _, [], _) --> [].
automaton_dag_tail([To1-Expr|L1], I, [(To1-Expr)-I|L2], To) -->
	[node(I,To,[To1..To1])],
	{J is I+1},
	automaton_dag_tail(L1, J, L2, To).

automaton_compile(Arcs, From, Via, Leaf2ID) --> !,
	[node(0,From,FromList)],
	automaton_compile_from(Arcs, FromList, -1, Via, Leaf2ID).

automaton_compile_from([Arc|Arcs], [(From..From)-ID|FromList], ID, Via, Leaf2ID) -->
	{Arc = arc(From,_,_,_)}, !,
	[node(ID,Via,ViaList2)],
	{automaton_compile_via([Arc|Arcs], Arcs1, From, ViaList1, Leaf2ID)},
	{automaton_peep(ViaList1, ViaList2)},
	{ID1 is ID-1},
	automaton_compile_from(Arcs1, FromList, ID1, Via, Leaf2ID).
automaton_compile_from([], [], _, _, _) --> [].

automaton_compile_via([Arc|Arcs], Arcs1, From, [(Via..Via)-ID|ViaList], Leaf2ID) :-
	Arc = arc(From,Via,To1,Expr), !,
	avl_fetch(To1-Expr, Leaf2ID, ID),
	automaton_compile_via(Arcs, Arcs1, From, ViaList, Leaf2ID).
automaton_compile_via(Arcs, Arcs, _, [], _).

automaton_peep([], []).
automaton_peep([(A..A)-ID|L1], [(A..B)-ID|L3]) :-
	automaton_peep_interval(L1, L2, A, B, ID),
	automaton_peep(L2, L3).

automaton_peep_interval([(B..B)-ID|L1], L2, A, C, ID) :-
	B =:= A+1, !,
	automaton_peep_interval(L1, L2, B, C, ID).
automaton_peep_interval(L, L, A, A, _).

automaton_call(closure(Args,Ls,Case), Args, C1, C2, CtrArgs, Clause) :-
	call(Case),
	automaton_map_counters(Ls, CtrArgs, C1, C2, Clause).

automaton_map_counters([], _, C1, C1, _).
automaton_map_counters([Leaf1|Ls], [CtrArg|CtrArgs], C1, C3, Clause) :-
	copy_term(Clause, (Head2:-Body2)),
	Head2 = aux(CtrArg, C1, C2, Leaf1),
	call(Body2),
	automaton_map_counters(Ls, CtrArgs, C2, C3, Clause).

automaton_thread([], S, S) --> [].
automaton_thread([Sig|Sigs], S1, SN) --> [f(S1,Sig,S2)],
	automaton_thread(Sigs, S2, SN).
	
