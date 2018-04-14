%%% evaluate.pl
%%% Copyright (c) 2003 SICS AB. All rights reserved.
%%% -----------------------------------------------------------------
%%%
%%% Author  : Joakim Eriksson
%%% Created : 03-5-22
%%% Updated : $Date: 2006/05/02 16:35:12 $
%%%	      $Revision: 1.3 $
%%% Purpose : PrologBeans example
%%%

:- module(evaluate,[main/0,my_predicate/2]).
:- use_module(library(prologbeans)).
:- use_module(library(codesio),[read_from_codes/2]).

%% Register acceptable queries and start the server (using default port)
main:-
    register_query(evaluate(C,P), my_predicate(C, P)),
    start.

%% In this case we know that we have received a list of characters
%% that needs to be converted into an expression!
my_predicate(Chars, P) :-
    read_from_codes(Chars, X),
    P is X.

% This will be called if we build a run-time system
user:runtime_entry(start) :-
    main.
