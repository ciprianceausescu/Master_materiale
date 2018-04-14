/* Copyright(C) 1997, Swedish Institute of Computer Science */

%   File       : clpfd.pl
%   Author     : Mats Carlsson
%   Updated    : 9 October 2000
%   Purpose    : Finite domains constraint solver

:- module(clpfd, [
	% enumeration
	indomain/1,
	labeling/2,
	first_bound/2,
	later_bound/2,
	minimize/2,
	maximize/2,
	/* order_resource/2, */
	% reflection
	fd_var/1,
	fd_min/2,
	fd_max/2,
	fd_size/2,
	fd_set/2,
	fd_dom/2,
	fd_degree/2,
	fd_statistics/0,
	fd_statistics/2,
	fd_neighbors/2,
	fd_closure/2,
	% constraints
	domain/3,
	iff/2,					% for compatibility
	in/2,
	in_set/2,
	all_different/1,
	all_different/2,
	all_distinct/1,
	all_distinct/2,
	element/3,
	minimum/2,
	maximum/2,
	nvalue/2,
	circuit/1,
	circuit/2,
	assignment/2,
	assignment/3,
	cumulative/1,
	cumulative/2,
        disjoint1/1,
        disjoint1/2,
        disjoint2/1,
        disjoint2/2,
        case/3,
        case/4,
        table/2,
        table/3,
	cumulatives/2,
	cumulatives/3,
	global_cardinality/2,
	global_cardinality/3,
	sum/3,
	scalar_product/4,
	scalar_product/5,
	sorting/3,
	lex_chain/1,
	lex_chain/2,
        automaton/8,
	#= /2,
	#\= /2,
	#< /2,
	#=< /2,
	#> /2,
	#>= /2,
	#\ /1,
	#/\ /2,
	#\ /2,
	#\/ /2,
	#=> /2,
	#<= /2,
	#<=> /2,
	% programming interface
	fd_global/3,
	fd_global/4,
	fd_flag/3,
	is_fdset/1,
	empty_fdset/1,
	fdset_parts/4,
	empty_interval/2,
	fdset_interval/3,
	fdset_singleton/2,
	fdset_min/2,
	fdset_max/2,
	fdset_size/2,
	list_to_fdset/2,
	fdset_to_list/2,
	range_to_fdset/2,
	fdset_to_range/2,
	fdset_add_element/3,
	fdset_del_element/3,
	fdset_disjoint/2,
	fdset_intersect/2,
	fdset_intersection/3,
	fdset_intersection/2,
	fdset_member/2,
	fdset_eq/2,
	fdset_subset/2,
	fdset_subtract/3,
	fdset_union/3,
	fdset_union/2,
	fdset_complement/2
		 ]).

:- use_module(library(atts)).

:- use_module(library(avl), [
        empty_avl/1, 
	avl_fetch/3,
	list_to_avl/2,
	ord_list_to_avl/2,
	avl_to_list/2,
	avl_store/4
	]).

:- use_module(library(lists), [
	append/2,
	last/2,
	nth1/3,
	suffix/2,
	reverse/2,
	select/3,
	keyclumped/2,
	keys_and_values/3,
	same_length/2,
	transpose/2
	]).

:- use_module(library(ordsets), [
	ord_disjoint/2,
	ord_subset/2,
	ord_subtract/3,
	ord_intersection/3,
	ord_union/2,
	ord_union/3,
	ord_union/4,
	ord_add_element/3,
	ord_del_element/3
	]).

:- use_module(library(ugraphs), [
	vertices_edges_to_ugraph/3,
	top_sort/2
	]).
				 
:- use_module(library(types), [
	illarg/3,
	illarg/4,
	must_be/4
	]).

:- use_module(library(between), [
	between/3
	]).

:- use_module(library(timeout), [
	time_out/3
	]).

:- op(1200, xfx, [+:,-:,+?,-?]).%% as :-
:- op(900,  xfx, iff).
:- op(760,  yfx, #<=>).
:- op(750,  xfy, #=>).
:- op(750,  yfx, #<=).
:- op(740,  yfx, #\/).
:- op(730,  yfx, #\).
:- op(720,  yfx, #/\).
:- op(710,   fy, #\).
:- op(700,  xfx, [in,in_set]).
:- op(700,  xfx, [#=,#\=,#<,#=<,#>,#>=]).
%- op(600,  xf,  !).  %% [MC] 3.11.1
:- op(550,  xfx, ..). %% higher than +,-,*...
:- op(500,  yfx, \).				% same as \/, /\
:- op(500,   fy, \).				% same as \/, /\
:- op(490,  yfx, ?).				% tighter than \/, /\
:- op(400,  yfx, [/>,/<]).			% divu, divd

:- dynamic
	full_answer/0.


/***
Some key data structures
========================

global(StateMutable,Constraint,StatusMutable,EntailmentFlag,Source)
-------------------------------------------------------------------

Denotes a propagator for a global constraint.

StateMutable : The value is the propagator's current state, i.e. the
               backtrackable, Prolog part of it. The built-in
	       propagators also have non-backtrackable state in C.
Constraint :   The actual constraint.
StatusMutable : The value is an integer, a bitmask of:
                0x1 - linked into dependency lists
                0x2 - in propagation queue
                0x4 - non-idempotent
                0x8 - currently being filtered
EntailmentFlag : Unbound if not yet entailed, 1 otherwise.
Source : Usually the same as Constraint, but not always.
         For example, some global constraint need auxiliary
	 constraints that should be invisible to the user if he
	 inspects the constraints attached to a domain variable, in
	 which case Source = true. Source is accessed by
	 clpfd:attribute_goal/2, the API invoked by frozen/2 and by
	 the top-level when it displays residual constraints.

ix(Ptr,Constraint,StatusMutable,EntailmentFlag,ZeroOne,AttrVector,ZeroOneAttr)
------------------------------------------------------------------------------

Denotes a propagator for an indexical constraint.

Ptr : a pointer to a struct indexical_info.
ZeroOne : 1 if entailed, 0 if disentailed, unbound otherwise
          Only used for reification, in which case it's a dvar.
AttrVector : Same as constraint, but with arguments replaced by their
             respective attribute terms.
ZeroOneAttr : Attribute term for ZeroOne

daemon(Global,AttrRef,StatusMutable,EntailmentFlag,Handle)
----------------------------------------------------------

Denotes a daemon for a global constraint.

Global : global/5 term.
AttrRef : a pointer to the attribute term to which the daemon is attached.
Handle : a term containing a pointer to the C state.
***/



%% extremely crude version: just filter out any non-query vars
project_attributes(_, _) :- full_answer, !.
project_attributes(QueryVars, AttVars) :-
	sort(QueryVars, QueryVars1),
	sort(AttVars, AttVars1),
	ord_subtract(AttVars1, QueryVars1, ElimVars),
	purify_vars(ElimVars).

attribute_goal(X, Goal) :-
	fd_set(X, Set),
	\+fdset_singleton(Set, _),		% temp created by fdvar=fdvar
	fdset_to_range(Set, Dom1),
	(   full_answer ->
	    get_fd_suspensions(X, Lists), % avoid '$fdlists' in source code
	    fdlists_suspensions(Lists, All, []),
	    sort(All, Sorted),
	    commafy(Sorted, X in Dom1, Goal)
	;   Goal = (X in Dom1)
	).

fdlists_suspensions(Lists) -->
	{arg(3, Lists, L3)},
	{arg(4, Lists, L4)},
	{arg(5, Lists, L5)},
	{arg(6, Lists, L6)},
	{arg(7, Lists, L7)},
	suspensions(L3),
	suspensions(L4),
	suspensions(L5),
	suspensions(L6),
	suspensions(L7).

suspensions([]) --> [].
suspensions([Item|L]) -->
	suspension(Item),
	suspensions(L).

suspension(_-Item) --> !,
	suspension(Item).
suspension(iff(Ix,B,K,A)) -->
	{var(A)},
	{Ix = ix(Ptr,Constraint,_,Ent,_,_,_)},
	{var(Ent)}, !,
	{'$fd_indexical_data'(Ptr, _, Module)},
	(   {var(B)} -> [Module:Constraint #<=> B]
	;   {B==0, K==0} -> [#\ Module:Constraint]
	;   {B==1, K==1} -> [Module:Constraint]
	;   []
	).
suspension(ix(Ptr,Constraint,_,Ent,ZeroOne,_,_)) -->
	{var(Ent), var(ZeroOne)}, !,
	{'$fd_indexical_data'(Ptr, Type, Module)},
	suspension_item(Type, Module:Constraint).
suspension(global(_,_,_,Ent,Source)) -->
	{var(Ent), Source\==true}, !,
	[Source].
suspension(daemon(Item,_,_,Ent,_)) -->
	{var(Ent)}, !,
	suspension(Item).
suspension(_) --> [].

suspension_item(0, C) --> [#\ C].
suspension_item(1, C) --> [C].
suspension_item(2, _) --> [].
suspension_item(3, _) --> [].

fd_statistics :-
	'$fd_statistics'(0, S0),
	'$fd_statistics'(1, S1),
	'$fd_statistics'(2, S2),
	'$fd_statistics'(3, S3),
	'$fd_statistics'(4, S4),
        format(user_error,
               'Resumptions: ~d\n\
Entailments: ~d\n\
Prunings: ~d\n\
Backtracks: ~d\n\
Constraints created: ~d\n', [S0,S1,S2,S3,S4]).

fd_statistics(Key, Value) :-
	statistics_code(Key, Code),
	'$fd_statistics'(Code, Value).

statistics_code(resumptions, 0).
statistics_code(entailments, 1).
statistics_code(prunings, 2).
statistics_code(backtracks, 3).
statistics_code(constraints, 4).

%% used by foreign resource's init function
%% called back from C
call_action(call(Goal)) :- Goal.
call_action(X in R) :- 
	set_expression_check(R, S, X in R, 2),
	'$fd_in_set'(X, S, 0).
call_action(X in_set S) :- 
	(   '$fd_size'(S, _, 1) ->
	    '$fd_in_set'(X, S, 0)
	;   illarg(domain(term,constraint), X in_set S, 2)
	).
call_action(X=I) :-
	must_be_fd_integer(I, X=I, 2),
	'$fd_range'(I, I, Set, 1),
	'$fd_in_set'(X, Set, 0).


:- dynamic foreign/3, foreign_resource/2.


foreign_resource(clpfd,
        [init(fd_initialize),
	 deinit(fd_deinitialize),
	 prolog_fd_size,
	 prolog_fd_range,
	 prolog_fd_cons,
	 prolog_fd_dom_complement,
	 prolog_fd_dom_subtract,
	 prolog_fd_dom_intersection,
	 prolog_fd_dom_union,
	 prolog_fd_dom_contains,
	 prolog_fd_dom_insert,
	 prolog_fd_dom_delete,
	 prolog_fd_dom_intersect,
	 prolog_fd_negate,
	 prolog_fd_arg_attribute,
	 prolog_fd_dvar_list,
	 prolog_fd_coref,
	 prolog_fd_begin,
	 prolog_fd_tell,
	 prolog_fd_check_arguments,
	 prolog_fd_install,
	 prolog_fd_post_reified,
	 prolog_fd_post_global,
	 prolog_fd_prune_and_enqueue,
	 prolog_fd_find_definition,
	 prolog_fd_indexical_data,
	 prolog_fd_global_enqueue,
	 prolog_fd_statistics,
	 prolog_fd_debug,
	 prolog_fd_overflow,
	 prolog_fd_set_singleton,
	 prolog_fd_in_set,
	 prolog_fd_in_interval,
	 prolog_fd_evaluate_indexical,
	 prolog_fd_enqueue_all,
	 prolog_fd_update_incumbent,
	 prolog_fd_incumbent_bound,
	 prolog_fd_minint_maxint,
	 prolog_fd_dispatch_global_fast,
	 prolog_fd_linear,
	 prolog_fd_square,
	 prolog_fd_product,
	 prolog_fd_quotient,
	 prolog_fd_modulo,
	 prolog_fd_cumulative,
	 prolog_fd_all_different,
	 prolog_fd_all_distinct,
	 prolog_fd_bc_alldiff,
	 prolog_fd_pairing,
	 prolog_fd_sorting,
	 prolog_fd_assignment,
	 prolog_fd_assignment_helper,
	 prolog_fd_circuit,
	 prolog_fd_relation,
	 prolog_fd_element,
	 prolog_fd_minmax,
	 prolog_fd_nvalue,
	 prolog_fd_in_set_iff,
	 % new
	 prolog_fd_eq_iff,
	 prolog_fd_le_iff,
	 prolog_fd_oneof,
	 prolog_fd_abs,
	 % end new
	 prolog_fd_bool,
	 prolog_fd_disjoint1,
	 prolog_fd_disjoint2,
	 prolog_fd_case,
	 prolog_fd_cumulatives,
	 prolog_fd_gcc,
	 prolog_fd_gcc_helper,
	 prolog_fd_lcc,
	 prolog_fd_lex_chain,
	 prolog_fd_ac_linear,
	 prolog_fd_labeling,
	 prolog_fd_delete
	]).

foreign(prolog_fd_size, '$fd_size'(+term,-term,[-integer])).
foreign(prolog_fd_range, '$fd_range'(+term,+term,-term,[-integer])).
foreign(prolog_fd_cons, '$fd_cons'(+term,+term,+term,-term,[-integer])).
foreign(prolog_fd_dom_complement, '$fd_dom_complement'(+term,-term)).
foreign(prolog_fd_dom_subtract, '$fd_dom_subtract'(+term,+term,-term)).
foreign(prolog_fd_dom_intersection, '$fd_dom_intersection'(+term,+term,-term)).
foreign(prolog_fd_dom_union, '$fd_dom_union'(+term,+term,-term)).
foreign(prolog_fd_dom_contains, '$fd_dom_contains'(+term,+term)).
foreign(prolog_fd_dom_insert, '$fd_dom_insert'(+term,+term,-term)).
foreign(prolog_fd_dom_delete, '$fd_dom_delete'(+term,+term,-term)).
foreign(prolog_fd_dom_intersect, '$fd_dom_intersect'(+term,+term,[-integer])).
foreign(prolog_fd_negate, '$fd_negate'(+term,+term,-term)).
foreign(prolog_fd_arg_attribute, '$fd_arg_attribute'(+term,+integer,-term)).
foreign(prolog_fd_dvar_list, '$fd_dvar_list'(+term,+integer)).
foreign(prolog_fd_coref, '$fd_coref'(+term)).
foreign(prolog_fd_begin, '$fd_begin').
foreign(prolog_fd_tell, '$fd_tell'(+term,+term,[-integer])).
foreign(prolog_fd_check_arguments, '$fd_check_arguments'(+term,-term)).
foreign(prolog_fd_install, '$fd_install'(+term,+atom,+integer,+integer,+term)).
foreign(prolog_fd_post_reified, '$fd_post_reified'(+term,+term,+term,+term,+term)).
foreign(prolog_fd_post_global, '$fd_post_global'(+term,+term,+integer,+term,+term,-term)).
foreign(prolog_fd_prune_and_enqueue, '$fd_prune_and_enqueue'(+term,+term)).
foreign(prolog_fd_find_definition, '$fd_find_definition'(+term,+atom,-term/*struct def.*/)).
foreign(prolog_fd_indexical_data, '$fd_indexical_data'(+term/*indexical ptr*/,-integer,[-atom])).
foreign(prolog_fd_global_enqueue, '$fd_global_enqueue'(+term)).
foreign(prolog_fd_statistics, '$fd_statistics'(+integer,[-integer])).
foreign(prolog_fd_debug, '$fd_debug'(+term,+term,[-integer])).
foreign(prolog_fd_overflow, '$fd_overflow'(+term,+term,[-integer])).
foreign(prolog_fd_set_singleton, '$fd_set_singleton'(+term,+term)).
foreign(prolog_fd_in_set, '$fd_in_set'(+term,+term,+integer)).
foreign(prolog_fd_in_interval, '$fd_in_interval'(+term,+term,+term,+integer)).
foreign(prolog_fd_evaluate_indexical, '$fd_evaluate_indexical'([-integer],-term)).
foreign(prolog_fd_enqueue_all, '$fd_enqueue_all'(+term)).
foreign(prolog_fd_update_incumbent, '$fd_update_incumbent'(+term/*struct instance*/,+term,+term)).
foreign(prolog_fd_incumbent_bound, '$fd_incumbent_bound'(+term/*struct instance*/,-term)).
foreign(prolog_fd_minint_maxint, '$fd_minint_maxint'(-integer,-integer)).
foreign(prolog_fd_dispatch_global_fast,
	'$fd_dispatch_global_fast'(+term,+term,-term,-term,+term,[-integer])).

%% dispatch_global_fast/4 targets:

foreign(prolog_fd_linear, '$fd_linear'(+term,-term,-term)).
foreign(prolog_fd_square, '$fd_square'(+term,-term,-term)).
foreign(prolog_fd_product, '$fd_product'(+term,-term,-term)).
foreign(prolog_fd_quotient, '$fd_quotient'(+term,-term,-term)).
foreign(prolog_fd_modulo, '$fd_modulo'(+term,-term,-term)).
foreign(prolog_fd_cumulative, '$fd_cumulative'(+term,-term,-term)).
foreign(prolog_fd_all_different, '$fd_all_different'(+term,-term,-term)).
foreign(prolog_fd_all_distinct, '$fd_all_distinct'(+term,-term,-term)).
foreign(prolog_fd_bc_alldiff, '$fd_bc_alldiff'(+term,-term,-term)).
foreign(prolog_fd_pairing, '$fd_pairing'(+term,-term,-term)).
foreign(prolog_fd_sorting, '$fd_sorting'(+term,-term,-term)).
foreign(prolog_fd_assignment, '$fd_assignment'(+term,-term,-term)).
foreign(prolog_fd_assignment_helper, '$fd_assignment_helper'(+term,-term,-term)).
foreign(prolog_fd_circuit, '$fd_circuit'(+term,-term,-term)).
foreign(prolog_fd_relation, '$fd_relation'(+term,-term,-term)).
foreign(prolog_fd_element, '$fd_element'(+term,-term,-term)).
foreign(prolog_fd_minmax, '$fd_minmax'(+term,-term,-term)).
foreign(prolog_fd_nvalue, '$fd_nvalue'(+term,-term,-term)).
foreign(prolog_fd_in_set_iff, '$fd_in_set_iff'(+term,-term,-term)).
foreign(prolog_fd_eq_iff, '$fd_eq_iff'(+term,-term,-term)).
foreign(prolog_fd_le_iff, '$fd_le_iff'(+term,-term,-term)).
foreign(prolog_fd_oneof, '$fd_oneof'(+term,-term,-term)).
foreign(prolog_fd_abs, '$fd_abs'(+term,-term,-term)).
foreign(prolog_fd_bool, '$fd_bool'(+term,-term,-term)).
foreign(prolog_fd_disjoint1, '$fd_disjoint1'(+term,-term,-term)).
foreign(prolog_fd_disjoint2, '$fd_disjoint2'(+term,-term,-term)).
foreign(prolog_fd_case, '$fd_case'(+term,-term,-term)).
foreign(prolog_fd_cumulatives, '$fd_cumulatives'(+term,-term,-term)).
foreign(prolog_fd_gcc, '$fd_gcc'(+term,-term,-term)).
foreign(prolog_fd_gcc_helper, '$fd_gcc_helper'(+term,-term,-term)).
foreign(prolog_fd_lcc, '$fd_lcc'(+term,-term,-term)).
foreign(prolog_fd_lex_chain, '$fd_lex_chain'(+term,-term,-term)).
foreign(prolog_fd_ac_linear, '$fd_ac_linear'(+term,-term,-term)).
foreign(prolog_fd_labeling, '$fd_labeling'(+term,-term,-term,+term)).
foreign(prolog_fd_delete, '$fd_delete'(+term,-term,+integer)).

:- load_foreign_resource(library(system(clpfd))).

:- ensure_loaded('clpfd/fdsets').
:- ensure_loaded('clpfd/ixq').
:- ensure_loaded('clpfd/enum').
:- ensure_loaded('clpfd/compiler').
:- ensure_loaded('clpfd/lib').
:- ensure_loaded('clpfd/automaton').
