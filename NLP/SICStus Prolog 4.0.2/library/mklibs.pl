% Build all the library object modules as hidden modules

% Return the line number of the first line of the term layout
condense_layout([FL0|_], FL) :- !, FL=FL0.
condense_layout(FL, FL).

:- multifile user:term_expansion/6.
user:term_expansion((:- module(M,E)), Lay0, Ids,
		      (:- module(M,E,[hidden(true)])), Lay1, [mklibs|Ids]) :-
	nonmember(mklibs, Ids),
        condense_layout(Lay0, Lay1).

:- dynamic use_error_message_hook/1.

:- multifile user:portray_message/2.
:- dynamic user:portray_message/2. % [PM] for abolish in handle_{warning,error}_message below

user:portray_message(error, Message) :-
	use_error_message_hook(Options), !,
	handle_error_message(Message, Options).

user:portray_message(warning, Message) :-
        use_error_message_hook(Options), !,
	handle_warning_message(Message, Options).


make(Module, Rest) :-
	make(Module, Rest, []).

make(Module, Rest, Options) :-
	prolog_flag(redefine_warnings, _, off),
        asserta(use_error_message_hook(Options)),
	absolute_file_name(., A),
	asserta(library_directory(A)),
	compile(library(Module)),
	wrap_files(Rest, Tail),
	save_files([library(Module)|Tail], library(Module)).

%% [PM] 3.8.5
%% Options is a list of
%% on_foreign_resource_error(Opt) --
%%         existence error when loading foreign resource (e.g.,
%%         run-time linker error)
%% on_error(Opt) -- any error
%% Where Opt is
%%  warn -- this is the SICStus standard behaviour, print the error
%%          and go on.
%%  halt -- (the default) call halt(1) to exit with an error
%%          code. Useful to tell make that something went wrong. This
%%          is used for most libraries.
%% on_warning(Opt) -- any warning
%% Where Opt is
%%  warn -- this is the SICStus standard behaviour, print the warning
%%          and go on.
%%  halt -- (the default) call halt(1) to exit with an error
%%          code. Useful to tell make that something went wrong.

handle_error_message(existence_error(load_foreign_resource(_),_,_,_,_), Options) :-
	member(on_foreign_resource_error(warn), Options), !,
	fail.
handle_error_message(_Message, Options) :-
	member(on_error(warn), Options), !,
	fail.
handle_error_message(Message, _Options) :- 
	%% This is the default
	%% member(on_error(halt), _Options), 
	!,
	abolish(user:portray_message/2),
	print_message(error, Message),
	halt(1).
handle_error_message(_Message, _Options) :-
	fail.

handle_warning_message(_Message, Options) :-
	member(on_warning(warn), Options), !,
	fail.
handle_warning_message(Message, Options) :-
   \+ \+ member(ignore_warning(Message), Options).
handle_warning_message(Message, _Options) :- 
	%% This is the default
	%% member(on_warning(halt), _Options), 
	!,
	abolish(user:portray_message/2),
        %% writeq(user_error, 'DBG_DBG_DBG_warning'(Message)),nl(user_error),
	print_message(error, Message),
	halt(1).
handle_warning_message(_Message, _Options) :-
	fail.


wrap_files([], []).
wrap_files([X|Xs], [library(X)|Ys]) :- wrap_files(Xs, Ys).

make_clpfd :-
	make(clpfd, ['clpfd/fdsets',
		     'clpfd/ixq',
		     'clpfd/enum',
		     'clpfd/compiler',
		     'clpfd/lib',
		     'clpfd/automaton'
		     ]).

make_chr :-
	make(chr, ['chr/b_globval',
		   'chr/hprolog',
		   'chr/hpattvars',
		   'chr/chr_runtime',
		   'chr/pairlist',
		   'chr/chr_hashtable_store',
		   'chr/binomialheap',
		   'chr/find',
		   'chr/a_star',
		   'chr/listmap',
		   'chr/clean_code',
		   'chr/builtins',
		   'chr/guard_entailment',
		   'chr/chr_compiler_errors',
		   'chr/chr_compiler_options',
		   'chr/chr_compiler_utility',
		   'chr/chr_translate'
		   ]).



make_clpq :-
	make(clpq, ['clpq/arith',
		    'clpq/arith_q',
		    'clpq/itf3',
		    'clpq/store',
		    'clpq/geler',
		    'clpq/nf',
		    'clpq/nfq',
		    'clpq/ordering',
		    'clpq/class',
		    'clpq/project',
		    'clpq/bv',
		    'clpq/ineq',
		    'clpq/redund',
		    'clpq/fourmotz',
		    'clpq/bb',
		    'clpq/dump'],
             [ignore_warning(import(_,nfq,clpq,private)),
              ignore_warning(import(_,classq,clpq,private))
             ]
             ).
 
make_clpr :-
	make(clpr, ['clpr/arith',
		    'clpr/arith_r',
		    'clpr/itf3',
		    'clpr/store',
		    'clpr/geler',
		    'clpr/nf',
		    'clpr/nfr',
		    'clpr/ordering',
		    'clpr/class',
		    'clpr/project',
		    'clpr/bv',
		    'clpr/ineq',
		    'clpr/redund',
		    'clpr/fourmotz',
		    'clpr/bb',
		    'clpr/dump'],
             [ignore_warning(import(_,nfr,clpr,private)),
              ignore_warning(import(_,classr,clpr,private))
             ]
            ).
