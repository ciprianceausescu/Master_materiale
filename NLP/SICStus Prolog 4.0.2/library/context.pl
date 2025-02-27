/* Copyright (C) 1995, Swedish Institute of Computer Science. */

:- module(context, [ctxt/2]).

% This module does not export any predicates (only the undefined
% ctxt/2). It defines clauses of goal_expansion for generally useful
% expansions.

:- use_module(library(lists), [
	nth1/3
	]).

%%-----------------------------------------------------------------------
% Handling of context variables. Multiple lookups by key are replaced
% by a single unification. E.g:
%        ctxt(Ctxt,[key1-Value1,key3-Value3])
% is transformed into:
%        Ctxt = ctx(Value1,_,Value3,_,...)

% Return the line number of the first line of the term layout
condense_layout([FL0|_], FL) :- !, FL=FL0.
condense_layout(FL, FL).

% The item keys are defined in a list given as the directive
% ctxt_items/1 in the module being expanded. The directive is
% tranfered by means of clause expansion.

goal_expansion(ctxt(Ctxt,KeyValues), Lay0, Module, Goal, Lay) :-
	clause(Module:ctxt_items(Keys), _),
	length(Keys, Len),
	functor(Ctxt0, ctxt, Len),
	(   find_items(KeyValues, Keys, Ctxt0) ->
	    Goal = (Ctxt=Ctxt0),
            condense_layout(Lay0, Lay)          % keep line number only
	;   print_message(warning, failed(Module:ctxt(Ctxt,KeyValues))), fail
	).


:- multifile
	user:term_expansion/6.

user:term_expansion((:-ctxt_items(Keys)), _Lay0, Ids, [], [], [context|Ids]) :-
	nonmember(context, Ids), !,
	prolog_load_context(module, Module),
	retractall(Module:ctxt_items(_)),
	assertz(Module:ctxt_items(Keys)).

find_items([], _, _).
find_items([Key-Value|KeyValues], Keys, Ctxt) :-
	nth1(Nr, Keys, Key),
	arg(Nr, Ctxt, Value),
	find_items(KeyValues, Keys, Ctxt).
