:- module(dbg_ex, [
		   break/2,         % add a breakpoint
		   nobreakall/0,    % clear all breakpoints
		   replace_goal/2,  % replace a goal with another
		   invariant/3,     % state an invariant
		   count/2,         % set up counting
		   clear_counters/1,
		   get_counters/2,
		   list_counters/1  % support for counting
		  ]).

:- use_module(library(terms), [
	term_hash/2
	]).
:- use_module(library(lists), [
	reverse/2,
	select/3
	]).


% ---------- Condition macros ----------

:- multifile user:breakpoint_expansion/2.

% M:Cond: the Prolog goal M:Cond holds (i.e. _:_ gets automatically
% wrapped into true/1).
user:breakpoint_expansion(
            Module:Condition,
	    true(Module:Condition)).
% line1(BaseFile,Line): the current goal is in line Line, within a file
% whose name ends with either BaseFile or BaseFile.pl.
user:breakpoint_expansion(
            line1(BaseFile,Line),
	    [line(File,Line),dbg_ex:matching_file(BaseFile, File)]).
% file1(BaseFile): the current goal is in a file
% whose name ends with either BaseFile or BaseFile.pl.
user:breakpoint_expansion(
           file1(BaseFile),
	    [file(File),dbg_ex:matching_file(BaseFile, File)]).
% priv(Value): Value is made a member of the private field, following the
% open-ended list convention.
% Value has to be a compound term with a unique functor.
user:breakpoint_expansion(
            priv(Value),
            [private(P),lists:memberchk(Value,P)]).
% gpriv(Value): Value is made a member of the goal private field. following
% the open-ended list convention.
% Value has to be a compound term with a unique functor.
user:breakpoint_expansion(
            gpriv(Value),
            [goal_private(GP),lists:memberchk(Value,GP)]).
% skip (action): perform a skip to the current invocation.
user:breakpoint_expansion(
            skip, 
            [inv(I),skip(I)]).
% one_off (action): make a one-off breakpoint, removed at first
% activation.
user:breakpoint_expansion(
            one_off,
	    [bid(BID),true(remove_breakpoints(BID))]).
% invisible (action): ensure that the predicate in question becomes 
% invisible in the trace: neither its ports nor any predicate called within
% it will be traced.
user:breakpoint_expansion(
	    invisible, 
            [dbg_ex:invisible(NewMode),mode(NewMode),proceed,silent]).
% count(Key) (action): increment the counter associated with Key
user:breakpoint_expansion(
	    count(Key),
	    [dbg_ex:count_key(Key)]).


% ---------- Interactive debugger commands ----------

:- multifile user:debugger_command_hook/2.

% debugger command: N [<name>]    (name term)
% Store the current goal or its currently selected subterm under name
% <name>. If <name> is omitted, store under $unnamed.
% <name> is an arbitrary sequence of non-layout characters.
user:debugger_command_hook(unknown([0'N|ArgCodes],_), Actions) :-
        Actions = true, % don't change the action variables
	                % [] would mean [print,ask] :-).
	name_current_subterm(ArgCodes).

% debugger command: P [<name1> <name2> ...]    (print named terms)
% Print the terms stored under <name1>, <name2>, etc. If no names are
% given, print all the stored terms.
user:debugger_command_hook(unknown([0'P|ArgCodes],_), Actions) :-
        Actions = true,
	print_named_terms(ArgCodes).

% debugger command: Z <number>    (zip until Nth activation)
% It can only issued at a breakpoint activation. It then modifies the
% breakpoint in question, so that execution continues by zipping and this
% breakpoint will not be activated until the original test part succeeds
% for the <number>th time at a call port.
user:debugger_command_hook(unknown([0'Z|ArgCodes],_), Actions) :-
	Actions =[silent,flit,zip],
	ensure_zip_over_n_calls(ArgCodes).

% debugger command: G <Pred>/<Arity>
%                              (print ancestors with the given functor)
user:debugger_command_hook(unknown([0'G|ArgCodes],_), Actions) :-
	Actions = true,
	print_ancestors(ArgCodes).


% ---------- Exported predicates  ----------

% Add a breakpoint for M:Pred, with the test part of Spec extended with the
% pred(M:Pred) test. Note that Spec is not module name expanded, and it
% will be interpreted in the scope of module M (e.g. all true/1 conditions
% will be called in module M, by default).
% As opposed to spy/2, break/2 will always generate a single, possibly
% generic breakpoint.
:- meta_predicate break(:, +).
break(M:Pred, Spec) :-
	split_spec(Spec, Tests, Actions),
	add_breakpoint(M:[pred(Pred)|Tests]-Actions, _).

% Clear all breakpoints.
nobreakall :-
	remove_breakpoints(all).

% Create an advice-point such that all invocations subsumed by Old are
% silently replaced by the invocation New.
:- meta_predicate replace_goal(:, :).
replace_goal(Old, New) :-
	add_breakpoint([advice,call,goal(Old)]
		      -[flit(Old,New)], _).

% Create an advice-point for the goal MGoal, checking whether Invariant
% holds at Port. At most one invariant/3 can be issued for every port of
% every predicate. Normally used as a directive.
:- meta_predicate invariant(:, +, :).
invariant(MGoal, Port, Invariant) :-
	invariant_breakpoint(MGoal, Port, Invariant0, BP_Invariant),
	remove_matching_breakpoints(BP_Invariant),
	Invariant0 = Invariant,
	add_breakpoint(BP_Invariant, _).

% Set up an advice-point. Each time Spec is satisfied, it will increment
% the counter for Key. Note that the breakpoint conditions which instantiate
% Key should be put in the actions part of Spec.
count(Spec, Key) :-
	split_spec(Spec, Tests, Actions),
	add_breakpoint([advice,Tests]-[Actions,count(Key)], _).

% Clear all the counters associated with a key which unifies with Key.
clear_counters(Key) :-
	term_hash(Key, Hash),
	retractall(counter(Hash, Key, _)).

% Counters is a list of key-counter pairs for each key unifiable with Key. 
get_counters(Key, Counters) :-
	term_hash(Key, Hash),
	setof(Key-C, Hash^counter(Hash, Key, C), Counters).

% Print all key-counter pairs where key is unifiable with Key.
list_counters(Key) :-
	get_counters(Key, Counters),
	(   member(Key-C, Counters),
	    format('~w~t~d~45|\n', [Key,C]),
	    fail
	;   true
	).

% ---------- Utilities for breakpoint macros  ----------

matching_file(BaseFile, File) :-
	sub_atom(File, _, _, N, BaseFile),
	(   N == 0 -> true
	;   N == 3, atom_concat(_, '.pl', File)
	).

invisible(NewMode) :-
        execution_state([mode(M),port(P),inv(Inv),goal_private(GP)]),
        memberchk(mymode(MM), GP),
        (   P == call -> MM = M, NewMode = skip(Inv)
        ;   P = exit(_) -> NewMode = MM
        ;   NewMode = M
        ).

% ---------- Utilities for the interactive commands ----------

ensure_zip_over_n_calls(Codes) :-
	execution_state(bid(BID)),
	current_breakpoint(M:Spec, BID, on, _, _),
	parse_args(Codes, [Arg]),
	parse_integer(Arg, Count),
	split_spec(Spec, Tests0, Actions),
	(   tests_extended_with_counting(Tests1, _, _, Tests0) -> true
	;   Tests1 = Tests0
	),
	remove_breakpoints(BID),
	tests_extended_with_counting(Tests1, NewBID, Count, Tests),
	add_breakpoint(M:Tests-Actions, NewBID),
	clear_counters(countdown_zip(NewBID)).

tests_extended_with_counting(Tests, BID, Count, Tests1) :-
	append(Tests, [dbg_ex:count_reached(BID, Count)], Tests1), !.
tests_extended_with_counting(Tests, BID, Count, 
			     [Tests,dbg_ex:count_reached(BID, Count)]).

count_reached(BID, Count) :-
	execution_state(call),
	count_key(countdown_zip(BID), C),
	C >= Count.

name_current_subterm(Codes) :-
	parse_args(Codes, Args),
	named_term_storage(Mut, NTs),
	execution_state([show(Show),goal(_:G)]),
	(   Show = _-Sel
	->  select_subterm(Sel, G, Term)
	;   Term = G
	),
	(   Args = [Cs] -> parse_atom(Cs, Name)
	;   Args = [] -> Name = '$unnamed'
	),
	update_mutable([Name-Term|NTs], Mut).

select_subterm([], Term, Term).
select_subterm([S|Sel], Term, STerm) :-
	compound(Term),
	arg(S, Term, Ith),
	select_subterm(Sel, Ith, STerm).

named_term_storage(Mut, NTs) :-	
	execution_state(private(P)),
	memberchk(named_terms(Mut), P),
	(   var(Mut)
	->  create_mutable([], Mut), NTs = []
	;   get_mutable(NTs, Mut)
	).

print_named_terms(Codes) :-
	parse_args(Codes, Args),
	named_term_storage(_Mut, NTs),
	reverse(NTs, RNTs),
	(   args_contain_name(Args, Name),
	    member(Name-Term, RNTs),
	    print_name_value(Name, Term),
  	    fail
	;   true
	).

args_contain_name([], _).
args_contain_name(Args, Name) :-
	Args = [_|_],
	member(Arg, Args),
	atom_codes(Name, Arg).

print_name_value('$unnamed', Term) :- !,
	format('~w\n', [Term]).
print_name_value(Name, Term) :-
	format('~w = ~w\n', [Name,Term]).

print_ancestors(Codes) :-
	parse_args(Codes, [Arg]),
	parse_functor(Arg, Name, Arity),
	execution_state(inv(Inv)),
	collect_ancestors(Inv, Name, Arity, [], Goals),
	print_message(help, ancestors(Goals)).

collect_ancestors(Inv0, Name, Arity, Goals0, Goals) :-
	functor(Goal, Name, Arity), MGoal = M:Goal,
	execution_state(inv(Inv0), ancestor(MGoal, Inv)), !,
	execution_state(inv(Inv), [parent_inv(Inv1),depth(Depth),
				   goal(MGoal),exited(Exited)]),
	(   current_breakpoint(_, _, on, plain(M:Name/Arity), _) ->
	    Kind = plain
	;   Kind = none
	),
	(   Exited == true -> Port = exit
	;   Port = call
	),
	SubtermSel = [],
	BacktraceElem = goal(print,Kind,Inv,Depth,Port,SubtermSel,MGoal),
	Goals1 = [BacktraceElem|Goals0],
	collect_ancestors(Inv1, Name, Arity, Goals1, Goals).
collect_ancestors(_, _, _, Goals, Goals).


	

% ---------- Utilities for invariant/3 ----------

invariant_breakpoint(M:Goal, Port, Invariant,
		     [goal(M:Goal),advice]
		    -[port(Port),
		      dbg_ex:check_invariant(M:Goal,Port,Invariant)]).

check_invariant(_MGoal, _Port, Invariant) :-
	Invariant, !.
check_invariant(MGoal, Port, Invariant) :-
	print_message(informational, violated(MGoal, Port, Invariant)),
	trace.

:- multifile 'SU_messages':generate_message/3.
'SU_messages':generate_message(violated(Goal,Port,Invariant)) -->
        {numbervars(Invariant-Goal,0,_)},
        ['Invariant ~w violated for ~w at ~w port'-[Invariant,Goal,Port],nl].

remove_matching_breakpoints(BP_Invariant) :-
	current_breakpoint(BP_Invariant, BID, _, _,_),
	remove_breakpoints(BID), fail.
remove_matching_breakpoints(_).

% ---------- Utilities for counting ----------

:- dynamic counter/3.

count_key(Key) :-
	count_key(Key, _).

count_key(Key, C1) :-
	term_hash(Key, Hash),
	(   retract(counter(Hash, Key, C0)) -> true
	;   C0 = 0
	),
	C1 is C0+1,
	asserta(counter(Hash, Key, C1)).

% ---------- General utility predicates ----------

split_spec(Tests0-Actions0, Tests, Actions) :- !,
	Tests = Tests0, Actions = Actions0.
split_spec(-Actions0, Tests, Actions) :- !,
	Tests = [], Actions = Actions0.
split_spec(Tests, Tests, []).


% ---------- Parsing debugger command arguments ----------

parse_args([], []).
parse_args([C|Codes], Args) :-
	C =< 32, !,
	parse_args(Codes, Args).
parse_args(Codes0, [ArgCodes|Args]) :-
	ArgCodes = [_|_],
	parse_arg(Codes0, Codes1, ArgCodes),
	parse_args(Codes1, Args).

parse_arg([C|Codes0], Codes, ArgCodes) :-
	C > 32, !,
	ArgCodes = [C|ArgCodes0],
	parse_arg(Codes0, Codes, ArgCodes0).
parse_arg(Codes, Codes, []).

parse_integer(Codes, Int) :-
	on_exception(_, number_codes(Int, Codes), fail),
	integer(Int).

parse_atom(Codes0, Atom) :-
	(   Codes0 = [0'\'|Codes1] ->
	    append(Codes, "'", Codes1)
	;   Codes = Codes0
	),
	atom_codes(Atom, Codes).

parse_functor(Codes, Name, Arity) :-
	append(NameCodes, [0'/|ArityCodes], Codes), !,
	parse_atom(NameCodes, Name),
	parse_integer(ArityCodes, Arity).

end_of_file.

% ---------- Sample run ----------
% Commentary marked with %%%
$ cat example.pl 
:- module(fib, [fib/2]).

fib(0, 0).
fib(1, 1).
fib(I, F) :-
	I > 1,
	fib2(I, F1, F2), 
	F is F1+F2.

fib2(I, F1, F2) :-
	I1 is I-1, I2 is I-2,
	fib(I1, F1),
	fib(I2, F2).

fib3(2, 1, 0).
fib3(I, F1, F2) :-
	I > 2,
	I1 is I-1,
	fib3(I1, F2, F3),
	F1 is F2+F3.
$ sicstus
SICStus 3.10.0 (x86-linux-glibc2.1): Mon Dec  9 11:24:52 CET 2002
Licensed to SICS
| ?- compile(debugger_examples).
% compiling /home/peter/debugger_examples.pl...
(...)
% compiled /home/peter/debugger_examples.pl in module dbg_ex, 160 msec 97712 bytes
yes
| ?- prolog_flag(source_info, _, on).
yes
% source_info
| ?- [example].
% consulting /home/peter/example.pl...
%  module mymod imported into user
% consulted /home/peter/example.pl in module mymod, 0 msec 2400 bytes
yes
% source_info
          %%%  Demonstrating breakpoint macro line1/2 and the utility
          %%%  predicate break/2.
| ?- break(_, line1(example,13)).
% The debugger will first zip -- showing spypoints (zip)
% Generic spypoint added, BID=1
yes
% zip,source_info
| ?- fib(8, X).
in scope of a goal at line 13 in /home/peter/example.pl
 #      1      1 Call: mymod:fib(0,_1509) ? n
X = 21 ? 
yes
% zip,source_info
| ?- nobreakall.
% All breakpoints removed
yes
% zip,source_info
| ?- spy fib/2.
% Plain spypoint for mymod:fib/2 added, BID=1
yes
% zip,source_info
          %%%  Demonstrating the Z debugger command
| ?- fib(8, X).
 +      1      1 Call: fib(8,_475) ? Z 6
% Plain spypoint for mymod:fib/2, BID=1, removed (last)
% Conditional spypoint for mymod:fib/2 added, BID=1
in scope of a goal at line 12 in /home/peter/example.pl
 *      1      1 Call: mymod:fib(2,_1990) ? s
          %%%  Notice both invocation and depth stays 1
in scope of a goal at line 12 in /home/peter/example.pl
?       1      1 Exit: mymod:fib(2,1) ? Z 5
% Conditional spypoint for mymod:fib/2, BID=1, removed (last)
% Conditional spypoint for mymod:fib/2 added, BID=1
          %%%   8 ... 5 _ 4 _ 3 _ 2 _ 1
          %%%         |   |   |   \ _ 0
          %%%         |   |   \ _ 1*
          %%%         |   \ _ 2*_ 1*
          %%%         |       \ _ 0*
          %%%         \ _ 3
          %%%  The four calls to fib being zipped over now are
          %%%  marked with an asterisk (*).
in scope of a goal at line 13 in /home/peter/example.pl
 *      2      1 Call: mymod:fib(3,_1797) ? =
The debugger will first zip -- showing spypoints (zip)
Using leashing stopping at [call,exit,redo,fail,exception] ports
Undefined predicates will raise an exception (error)
Breakpoints:
      1 *  mymod:fib/2 if user:[dbg_ex:count_reached(1,5)]-[]
in scope of a goal at line 13 in /home/peter/example.pl
 *      2      1 Call: mymod:fib(3,_1797) ? n
X = 21 ? 
yes
% zip,source_info
| ?- nobreakall.
% All breakpoints removed
yes
% zip,source_info
| ?- debug, break(_, goal(fib(0,_))).
% The debugger will first leap -- showing spypoints (debug)
% Conditional spypoint for mymod:fib/2 added, BID=1
yes
% debug,source_info
          %%%  Demonstrating the G debugger command
| ?- fib(8, X).
in scope of a goal at line 13 in /home/peter/example.pl
 *     37     15 Call: mymod:fib(0,_1527) ? o 4
in scope of a goal at line 12 in /home/peter/example.pl
?      26     11 Exit: mymod:fib(3,2) ? G fib2/3
Ancestors:
        3      2 Call: mymod:fib2(8,_638,_639)
        8      4 Call: mymod:fib2(7,_786,_787)
       13      6 Call: mymod:fib2(6,_934,_935)
       18      8 Call: mymod:fib2(5,_1082,_1083)
       23     10 Call: mymod:fib2(4,2,_1231)
in scope of a goal at line 12 in /home/peter/example.pl
?      26     11 Exit: mymod:fib(3,2) ? g
Ancestors:
        1      1 Call: fib(8,_475)
        3      2 Call: mymod:fib2(8,_638,_639)
        6      3 Call: mymod:fib(7,_638)
        8      4 Call: mymod:fib2(7,_786,_787)
       11      5 Call: mymod:fib(6,_786)
       13      6 Call: mymod:fib2(6,_934,_935)
       16      7 Call: mymod:fib(5,_934)
       18      8 Call: mymod:fib2(5,_1082,_1083)
       21      9 Call: mymod:fib(4,_1082)
       23     10 Call: mymod:fib2(4,2,_1231)
in scope of a goal at line 12 in /home/peter/example.pl
?      26     11 Exit: mymod:fib(3,2) ? n
X = 21 ? 
yes
% debug,source_info
          %%%  Demonstrating the counting utilities.
| ?- clear_counters(_), nobreakall, nodebug.
% All breakpoints removed
% The debugger is switched off
yes
% source_info
| ?- count(module(mymod)-[pred(_:_Pred),port(_P)],_Pred-_P).
% Generic advice point added, BID=1
yes
% advice,source_info
| ?- fib(8, X).
X = 21 ? ;
no
% advice,source_info
| ?- list_counters(_).
> /2-call                                  67
> /2-fail                                  34
> /2-exit(det)                             33
fib/2-call                                 66
fib/2-fail                                 66
fib/2-redo                                 66
fib/2-exit(nondet)                         66
fib2/3-call                                33
fib2/3-fail                                33
fib2/3-redo                                33
fib2/3-exit(nondet)                        33
is/2-call                                  99
is/2-exit(det)                             99
yes
% advice,source_info
| ?- nobreakall.
% All breakpoints removed
yes
% source_info
          %%%  Demonstrating the replace_goal utility
          %%%  First fib/2 is run in debug mode, max_inv gives
          %%%  the number of invocations.
| ?- debug, fib(8, X), execution_state(max_inv(Inv)).
% The debugger will first leap -- showing spypoints (debug)
X = 21,
Inv = 232 ? 
yes
% debug,source_info
| ?- replace_goal(mymod:fib2(A,B,C), mymod:fib3(A,B,C)).
% Conditional advice point for mymod:fib2/3 added, BID=1
true ? 
yes
% debug,advice,source_info
          %%%  Now fib/2 is run so that each call of fib2/3 is
          %%%  replaced by the fast fib3/3, notice the much
          %%%  lower invocation count.
| ?- fib(8, X), execution_state(max_inv(Inv)).
X = 21,
Inv = 28 ? 
yes
% debug,advice,source_info
| ?- nobreakall, notrace.
% All breakpoints removed
% The debugger is switched off
yes
% source_info
          %%%  Demonstrating invariant checks
          %%%  At exit port fib2/3 is checked against fib3/3.
| ?- invariant(mymod:fib2(A,B,C), exit, mymod:fib3(A,B,C)).
% Conditional advice point for mymod:fib2/3 added, BID=1
true ? 
yes
% advice,source_info
| ?- fib(8, X).
X = 21 ? 
          %%%  No violations of the invariant found
yes
% advice,source_info
          %%%  An incorrect invariant is being stated.
| ?- invariant(mymod:fib2(A,B,C), exit, A=:=B+C+1).
% Conditional advice point for mymod:fib2/3, BID=1, removed (last)
% Conditional advice point for mymod:fib2/3 added, BID=1
true ? 
yes
% advice,source_info
| ?- fib(8, X).
          %%%  An example of invariant violation
% Invariant user:(5=:=3+2+1) violated for mymod:fib2(5,3,2) at exit port
% The debugger will first creep -- showing everything (trace)
in scope of a goal at line 7 in /home/peter/example.pl
?       4      4 Exit: mymod:fib2(5,3,2) ? a
% Execution aborted
% advice,source_info
| ?- nobreakall.
% All breakpoints removed
yes
% source_info
| ?- prolog_flag(source_info, _, off).
yes
          %%%  Demonstrating the N and P commands for
          %%%  naming and printing terms
| ?- trace, app([1,2,3], [X,Y], [X,Y|_]).
% The debugger will first creep -- showing everything (trace)
        1      1 Call: app([1,2,3],[_526,_546],[_526,_546|_616]) ? ^2
        1      1 Call: ^2 [_526,_546] ? N List2
        1      1 Call: ^2 [_526,_546] ? ^0 3
        1      1 Call: ^3 [_526,_546|_616] ? N List3
          %%%  The second and third argument of the
          %%%  top level goal is named List2 and List3	
        1      1 Call: ^3 [_526,_546|_616] ? 
        2      2 Call: mymod:app([2,3],[1,_546],[_546|_616]) ? P
List2 = [1,_546]
List3 = [1,_546|_616]
        2      2 Call: mymod:app([2,3],[1,_546],[_546|_616]) ? 
          %%%  Demonstrating the P command with arguments
        3      3 Call: mymod:app([3],[1,2],_616) ? P List3 List2
List3 = [1,2|_616]
List2 = [1,2]
        3      3 Call: mymod:app([3],[1,2],_616) ? 
        4      4 Call: mymod:app([],[1,2],_5778) ? P
List2 = [1,2]
List3 = [1,2,3|_5778]
        4      4 Call: mymod:app([],[1,2],_5778) ? 
        4      4 Exit: mymod:app([],[1,2],[1,2]) ? P
List2 = [1,2]
List3 = [1,2,3,1,2]
        4      4 Exit: mymod:app([],[1,2],[1,2]) ? 
        3      3 Exit: mymod:app([3],[1,2],[3,1,2]) ? 
        2      2 Exit: mymod:app([2,3],[1,2],[2,3,1,2]) ? 
        1      1 Exit: app([1,2,3],[1,2],[1,2,3,1,2]) ? 
X = 1,
Y = 2 ? 
yes
% trace
| ?- halt.
$ 
