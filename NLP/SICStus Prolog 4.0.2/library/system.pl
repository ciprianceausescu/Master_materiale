/* -*- Mode: Prolog; -*-
/* Copyright(C) 1995-2006, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : SYSTEM.PL							     %
%   Authors : Mats Carlsson, Stefan Andersson				     %
%   Updated: 11 May 1996						     %
%   Purpose: Operating system utilities					     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(system, [
	environ/2,

	now/1,
	datime/1,
	datime/2,
	sleep/1

% Use sockets:current_host/1
%	host_name/1,

% Never did make much sense, use sockets:current_host/1 instead
%	host_id/1,

% [PM] 4.0 now in library(file_systems)
%	make_directory/1,
% [PM] 4.0 now in library(file_systems) as create_directory/[1,2]
%	working_directory/2,

%% [PM] 4.0 instead use open(temp(foo), write, S, [if_exists(generate_unique_name)]) 
%%	mktemp/2,
%%	tmpnam/1,

%% [PM] 4.0 replaced by library(process)
%	system/0,
%	system/1,
%	system/2,
%	shell/0,
%	shell/1,
%	shell/2,
%	exec/3,
%       popen/3,
%	pid/1,
%	kill/2,
%	wait/2,

		  ]).

:- use_module(library(types), [
	illarg/3,
	illarg/4,
	must_be/4
   ]).

:- dynamic foreign/3, foreign_resource/2.
:- discontiguous foreign/2, foreign/3. % [PM]

%@  This package contains utilities for invoking services from the operating
%@  system that does not fit elsewhere.
%@  
%@ @c   Certain predicates described below take names of files or
%@ @c   directories as arguments.  These must be given as atoms, and
%@ @c   the predicates below will not call @code{absolute_file_name/3} on
%@ @c   them.
%@ @c   
%@ @c   Some predicates are described as invoking the default shell.
%@ @c   Specifically this means invoking @file{/bin/sh} on UNIX platforms. Under
%@ @c   MSDOS, Windows and OS/2, the command interpreter given by the
%@ @c   environment variable @env{COMSPEC} is invoked.
%@  
%@  Exported predicates:
%@  
%@  @table @code

%@  @item now(@var{-When})
%@  @PLXindex {now/1 (system)}
%@  Unifies the current date and time as a UNIX timestamp with
%@  @var{When}.

now(When) :-
        sp_time(W),
        integer(W),                             % [] on failure
        When = W.

foreign(sp_time,  c, sp_time(-term)).

%@  @item datime(@var{-Datime})
%@  @PLXindex {datime/[1,2] (system)}
%@  Unifies @var{Datime} with the current date and time as a
%@  @code{datime/6} record of the form
%@  @code{datime(@var{Year},@var{Month},@var{Day},@var{Hour},@var{Min},@var{Sec})}.
%@  All fields are integers.
%@  @item datime(@var{+When},@var{-Datime})
%@  @item datime(@var{-When},@var{+Datime})
%@  Convert a time stamp, as obtained by @code{now/1}, to a
%@  @code{datime/6} record. Can be used in both directions.

datime(Datime) :-
	now(When),
	datime(When, D),
        D = Datime.

datime(When, Datime) :- ground(Datime), !,      % [PM] 4.0 prefer mktime since many Datime map to the same When
        Datime = datime(Year,Month,Day,Hour,Min,Sec),
	c_mktime(Year, Month, Day, Hour, Min, Sec, W),
        integer(W),                             % [] on failure
        When = W.
datime(When, Datime) :- nonvar(When), !,
        Datime = datime(Year,Month,Day,Hour,Min,Sec),
	c_datime(When, Year, Month, Day, Hour, Min, Sec, Res),
        Res == 0.

foreign(sp_datime,  c, c_datime(+term,          % bignum
				-integer, -integer, -integer,
                                -integer, -integer, -integer,
                                [-integer])).

foreign(sp_mktime,  c, c_mktime(+integer, +integer, +integer,
                                +integer, +integer, +integer,
                                -term)).

%@  @item sleep(@var{+Seconds})
%@  @PLXindex {sleep/1 (system)}
%@  Puts the SICStus Prolog process asleep for @var{Second} seconds, where
%@  @var{Seconds} should be a non-negative number.
%@  @c [PM] 4.0 now interruptible sleep using SPIO
%@  @c Under UNIX, the @code{usleep}
%@  @c function will be used if @var{Seconds} is less than one, and @code{sleep}
%@  @c otherwise.  Under MSDOS, Windows or OS/2, the @code{Sleep} function will be used.

sleep(Seconds) :-
   Goal = sleep(Seconds),
   must_be(Seconds, number(>=(0)), Goal, 1),
   TimeoutParam is float(Seconds),
   repeat,
     prolog:'$select'([],_LReadyRaw,
                      [],_CReadyRaw,
                      [],_RReadyRaw,
                      [],_WReadyRaw,
                      TimeoutParam, ECODE),
     %% spio_t_error_code
     ECODE \== -10014,                            % repeat while SPIO_E_INTERRUPTED
   !,
   ( ECODE == -10018 ->           % SPIO_E_TIMEOUT (the expacted case)
       true
   ; ECODE < 0 ->                 % failure
       illarg(system(sleep, ECODE), Goal, 0)
   ; true                       % success (impossible)
   ).


%@  @item environ(@var{?Var}, @var{?Value})
%@  @PLXindex {environ/2 (system)}
%@  @var{Var} is the name of an environment variable, and @var{Value} is its
%@  value.  Both are atoms.  Can be used to enumerate all current
%@  environment variables.

environ(Var, Value) :-
        var(Var), !,
        system_environ(Var, Value).
environ(Var, Value) :-
	c_getenv(Var, Value, 0).

% [PM] Backtrack over a snapshot of the environment.
system_environ(Var, Value) :-
   c_environ(Environ, Res),     % A snapshot of the environment
   Res == 0,                    % consider reporting errors if non-zero
   key_member(Environ, Var, Value).

key_member([KV|KVs], K,V) :-
   key_member1(KVs, K,V, KV).

key_member1([], K,V, (K=V)).
key_member1([_KV|_KVs], K,V, (K=V)).
key_member1([KV|KVs], K,V, _KV) :-
   key_member1(KVs, K,V, KV).

% [PM] Not public yet
%--------------------------------------------------------------------------
%   setenv(+Var, +Value)
%   Var is the name of a UNIX environment variable, and Value is its value.
%   Both are atoms.  Sets the value of the environment variable Var in
%   the current process and in child processes spawned after the call
%   to setenv/2.
%   Note: Will leak memory on most platforms due to general losynes of
%   environment variable API.

setenv(Var, Value) :-
	%% Goal = setenv(Var, Value),
	%% must_be(Var, atom, Goal, 1),
	%% must_be(Value, atom, Goal, 2),
   c_setenv(Var, Value, Res),
   Res = 0.                     % FIXME: Error handling?

foreign(sp_getenv1,    c, c_getenv(+string, -term, [-integer])).
foreign(sp_setenv,    c, c_setenv(+string, +string, [-integer])).
foreign(sp_environ,    c, c_environ(-term, [-integer])).




%-------------------------------------------------------------------------

foreign_resource(system, [
	sp_time,
	sp_datime,
        sp_mktime,
	sp_getenv1,
	sp_setenv,

        sp_environ

			 ]).

:- load_foreign_resource(library(system(system))).

%@  @end table
