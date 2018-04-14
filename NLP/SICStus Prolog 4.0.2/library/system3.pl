/* Copyright(C) 1995-96, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : SYSTEM.PL							     %
%   Authors : Mats Carlsson, Stefan Andersson				     %
%   Updated: 11 May 1996						     %
%   Purpose: Operating system utilities					     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(system3, [
	now/1,
	datime/1,
	datime/2,
	environ/2,
	sleep/1,
	host_name/1,
	host_id/1,
	file_exists/1,
	file_exists/2,
	file_property/2,
	rename_file/2,
	delete_file/1,
	delete_file/2,
	make_directory/1,
	working_directory/2,
	directory_files/2,
	mktemp/2,
	tmpnam/1,
	system/0,
	system/1,
	system/2,
	shell/0,
	shell/1,
	shell/2,
	exec/3,
        popen/3,
	pid/1,
	kill/2,
	wait/2
	]).

:- use_module(library(types), [
	must_be/4
	]).

:- use_module(library(system), [
	now/1,
	datime/1,
	datime/2,
	environ/2,
	sleep/1
	]).

:- use_module(library(file_systems), [
	rename_file/2,
	make_directory/1,
	delete_directory/2,
	directory_members_of_directory/2,
	file_members_of_directory/2
	]).

:- use_module(library(sockets), [
	current_host/1
	]).

:- use_module(library(lists), [
	keys_and_values/3
	]).

:- use_module(library(process)).

host_name(HostName) :-
	current_host(HostName).

host_id(HostName) :-
	current_host(HostName).

file_exists(FileName) :-
	file_exists(FileName, []).

file_exists(FileName, Permissions) :-
	file_systems:file_exists(FileName, Permissions), !.
file_exists(FileName, Permissions) :-
	file_systems:directory_exists(FileName, Permissions), !.

file_property(FileName, Property) :-
	existing_file_or_directory(FileName, What),
	file_property(What, FileName, Property).

file_property(file, FileName, Property) :-
	file_systems:file_property(FileName, Property).
file_property(directory, FileName, Property) :-
	file_systems:directory_property(FileName, Property).

delete_file(Filename) :-
	delete_file(Filename, [recursive]).

delete_file(Filename, Options) :-
	existing_file_or_directory(Filename, What),
	delete_file(What, Filename, Options).

delete_file(directory, Filename, Options) :-
	memberchk(recursive, Options), !,
	delete_directory(Filename, [if_nonempty(delete)]).
delete_file(directory, Filename, _Options) :-
	delete_directory(Filename, [if_nonempty(ignore)]).
delete_file(file, Filename, _) :-
	file_systems:delete_file(Filename).
delete_file(neither, Filename, Options) :-
	nonmember(ignore, Options),
	absolute_file_name(Filename, _, [access(exist),file_errors(error)]).

working_directory(Old, New) :-
	prolog:'$unix_cd'(Old, New).

directory_files(Directory, RelFiles) :-
	directory_members_of_directory(Directory, Set1),
	file_members_of_directory(Directory, Set2),
	append(Set1, Set2, KL),
	keys_and_values(KL, RelFiles, _).


existing_file_or_directory(Filename, What) :-
	(   absolute_file_name(Filename, Abs, [access(exist),file_type(directory),file_errors(fail)]) ->
	    What = directory
	;   absolute_file_name(Filename, Abs, [access(exist),file_errors(fail)]) ->
	    What = file
	;   What = neither
	).

	
proc_call(Binary) :-
	process_create(Binary, [], [process(Pid)]),
	process_wait(Pid, _).

proc_call(Binary, DashC, Cmd, Status) :-
	process_create(Binary, [DashC,Cmd], [process(Pid)]),
	process_wait(Pid, Status).

mktemp(Template, FileName) :-
	atom_codes(Template, FileCodes),
	basename(FileCodes, BaseCodes),
	atom_codes(BaseName, BaseCodes),
	open(temp(BaseName), write, S, [if_exists(generate_unique_name)]),
	current_stream(FileName, _, S),
	close(S).

basename(File, Base) :-
	basename(File, S, S, Base).

basename([], Base, [], Base).
basename([0'/|File], _, _, Base) :- !,
	basename(File, S, S, Base).
basename([C|File], S0, [C|S], Base) :- !,
	basename(File, S0, S, Base).

tmpnam(FileName) :-
	open(temp(sp), write, S, [if_exists(generate_unique_name)]),
	current_stream(FileName, _, S),
	close(S).

system :-
	system_binary(Binary, _),
	proc_call(Binary).

system(Cmd) :-
	system_binary(Binary, DashC),
	proc_call(Binary, DashC, Cmd, exit(0)).

system(Cmd, Status) :-
	system_binary(Binary, DashC),
	proc_call(Binary, DashC, Cmd, exit(Status)).

shell :-
	shell_binary(Binary, _),
	proc_call(Binary).

shell(Cmd) :-
	shell_binary(Binary, DashC),
	proc_call(Binary, DashC, Cmd, exit(0)).

shell(Cmd, Status) :-
	shell_binary(Binary, DashC),
	proc_call(Binary, DashC, Cmd, exit(Status)).

system_binary(Binary, '/C') :-
	environ('COMSPEC', Binary), !.
system_binary('/bin/sh', '-c').

shell_binary(Binary, '-c') :-
	environ('SHELL', Binary), !.
shell_binary(Binary, DashC) :-
	system_binary(Binary, DashC).

exec(Cmd, [Stdin,Stdout,Stderr], Pid) :-
	process_create(Cmd, [], [stdin(Stdin),stdout(Stdout),stderr(Stderr),process(Pid)]).

popen(Cmd, Mode, Stream) :-
	Goal = popen(Cmd,Mode,Stream),
	must_be(Mode, oneof([read,write]), Goal, 2),
	(   Mode==read -> Opt = stdout(pipe(Stream))
	;   Opt = stdin(pipe(Stream))
	),
	shell_binary(Binary, DashC),
	process_create(Binary, [DashC,Cmd], [Opt]).

pid(Pid) :-
	process_id(Pid).

kill(Pid, Sig) :-
	process_kill(Pid, Sig).

wait(Pid, Sig) :-
	process_wait(Pid, exit(Sig)).

