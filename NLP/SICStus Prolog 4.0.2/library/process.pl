/* -*- Mode:Prolog -*- */
/* Copyright(C) 2006, Swedish Institute of Computer Science */

:- module(process,
          [
           process_create/2,
           process_create/3,
           process_wait/2,
           process_wait/3,
           process_id/1,
           process_id/2,
           is_process/1,
           process_release/1,
           process_kill/1,
           process_kill/2
          ]).

%@  This package contains utilities for process creation.
%@  

%@  A process is represented by a @dfn{process reference}, a ground
%@  compound term. Both SICStus and the operating system maintain a
%@  state for each such process reference and they must therefore be
%@  released, either explicitly with @code{process_release/1} or
%@  implicitly by @code{process_wait/[2,3]}. Process references are
%@  created with @code{process_create/[2,3]} if explicitly requested with
%@  the @code{process/1} option. Process references are required in
%@  order to obtain the exit status of a process. Many of the
%@  predicates can accept a numeric operating system process id
%@  (``PID'') but since process ids are subject to re-use by the OS
%@  this is less reliable and does not work if the process has already
%@  exited.

%@  Run @command{ls} on a home directory in a subshell under UNIX:
%@  @example
%@  @group
%@  | ?- @kbd{absolute_file_name('$SHELL', Shell),}
%@       @kbd{absolute_file_name('~/', Dir),}
%@       @kbd{process_create(Shell, ['-c', [ ls, ' ', file(Dir) ]]).}
%@  @end group
%@  @end example

%@  Run @command{notepad.exe} on a file @file{C:/foo.txt} under Windows:
%@  @example
%@  @group
%@  | ?- @kbd{absolute_file_name('$SYSTEMROOT/notepad.exe', Prog),}
%@       @kbd{process_create(Prog, [file('C:/foo.txt')]).}
%@  @end group
%@  @end example


%@  Exported predicates:
%@  
%@  @table @code

%@  @item process_create(@var{+File}, @var{+Args})
%@  @itemx process_create(@var{+File}, @var{+Args}, @var{:Options})
%@  @PLXindex {process_create/[2,3] (process)}

%@  Start a new process running the program identified by @var{File}
%@  and the arguments specified in @var{Args}. The standard streams of
%@  the new process can be redirected to prolog streams. The exit
%@  status of the process can be obtained with @code{process_wait/[2,3]}.

%@

%@  @var{File}, is expanded as if by @code{absolute_file_name/2}
%@  (with argument @code{access(execute)}) and
%@  is used to locate the file to execute.
%@  

%@  The predefined file search path @code{path/1} (@pxref{ref-fdi})
%@  is especially useful here since it makes it easy to look up the
%@  names of an executable in the directories mentioned by the
%@  @code{PATH} environment variable. To run the Windows command shell
%@  @command{cmd} you would simply specify @code{path('cmd.exe')}, to
%@  start the UNIX Bash shell you would specify @code{path(bash)}.

%@

%@  @var{Args} is a list of argument specifications. Each argument
%@  specification is either a simple argument specification, see
%@  below, or a non-empty list of simple argument specifications. The
%@  expanded value of each element of @var{Args} is concatenated to
%@  produce a single argument to the new process. A @dfn{simple
%@  argument specification} can be one of:

%@

%@  @table @asis
%@  @item an atom
%@  The atom name is used as the expanded value. Some operating
%@  systems only support 7-bit ASCII characters here. Even when some
%@  larger subset of Unicode is used it may not work correctly
%@  with all programs.
%@
%@  @item @code{file(@var{File})}
%@  @var{File}, an atom, is treated as a file name and subject to
%@  an operating system specific transformation to ensure file name
%@  syntax and character set is appropriate for the new process. This
%@  is especially important under Windows where it ensures that the full
%@  Windows Unicode character set can be used.

%@  @strong{Please note}: The @var{File} part of
%@  @code{file(@var{File})} is not subject to syntactic rewriting, the
%@  argument specification @code{file/1} only adjusts for differences
%@  in file name syntax and character
%@  encoding between SICStus and the operating system. You
%@  must explicitly call
%@  @code{absolute_file_name/[2,3]} if you want to expand file search
%@  paths etc.
%@  

%@  @end table

%@  @var{Options} is a list of options:
%@

%@  @table @code
%@  @item stdin(@var{Spec})
%@  @itemx stdout(@var{Spec})
%@  @itemx stderr(@var{Spec})

%@  Each @var{Spec} specifies how the corresponding standard stream of
%@  the new process should be created. @var{Spec} can be one of:
%@  @table @code

%@  @item std
%@  The new process shares the (OS level) standard stream with the
%@  Prolog process. This is the default.
%@  Note that, especially under Windows, the Prolog process may not have
%@  any OS level standard streams, or the OS streams may not be
%@  connected to a console or terminal. In such a case you need to use
%@  @code{pipe/1} spec, see below, and explicitly read (write) data
%@  from (to) the process.

%@  @item null
%@  The stream is redirected to a null stream, i.e.@: a stream that
%@  discards written data and that is always at end of file when read.

%@  @item pipe(@var{Stream})
%@  A new Prolog (text) stream is created and connected to the
%@  corresponding stream of the new process. It is currently not
%@  possible to request binary streams or to specify a character set
%@  different from the OS default.
%@  This stream must be closed using @code{close/[1,2]}, it is not
%@  closed automatically when the new process exits.

%@  @end table
%@  

%@  @item process(@var{Proc})
%@  @var{Proc} will be bound to a process reference that can be used
%@  in calls to @code{process_wait/[2,3]} etc.. This process reference
%@  must be released, either explicitly with @code{process_release/1} or
%@  implicitly by @code{process_wait/[2,3]}.
%@
%@  @item detached(@var{Bool})
%@  @var{Bool} is either @code{true} or @code{false}. Specifies
%@  whether the new process should be ``detached'', i.e.@: whether it
%@  should be notified of terminal events such as @kbd{^C}
%@  interrupts. By default a new process is created detached if none
%@  of the standard streams are specified, explicitly or implicitly,
%@  as @code{std}.
%@
%@  @item cwd(@var{CWD})
%@  
%@  @var{CWD} is expanded as if by @code{absolute_file_name/2} and
%@  is used as the working directory for the new process.
%@
%@  By default, the working directory is the same as the Prolog
%@  working directory.
%@  
%@  @item window(@var{Bool})
%@  @var{Bool} is either @code{true} or
%@  @code{false} (the default). Specifies whether the process should
%@  open in its own window.
%@  
%@  Specifying @code{window(true)} may give unexpected results if the
%@  standard stream options @code{stdin/1}, @code{stdout/1} and
%@  @code{stderr/1} are specified with anything but their default
%@  value @code{std}.
%@  
%@  Currently only implemented on Windows.
%@  


%@  @c  @item daemon(@var{Bool})
%@  @c  not documented

%@  @end table

:- meta_predicate process_create(+, +).
process_create(File, Args) :-
        Goal = process_create(File, Args),
        Options = [],
        process_create1(File, Args, Options, Goal).

:- meta_predicate process_create(+, +, :).
process_create(File, Args, Options) :-
        Goal = process_create(File, Args, Options),
        process_create1(File, Args, Options, Goal).

process_create1(File, Args, Options, Goal) :-
        prolog:process_create(File, Args, Options, Goal).

%@  @item process_wait(@var{+Process}, @var{-ExitStatus})
%@  @itemx process_wait(@var{+Process}, @var{-ExitStatus}, @var{+Options})
%@  @PLXindex {process_wait/[2,3] (process)}

%@  Wait for a process to exit and obtain the exit status.
%@

%@  @var{Process} is either a process reference obtained from
%@  @code{process_create/3} or an OS process identifier. Specifying a
%@  process identifier is not reliable. The process identifier may
%@  have been re-used by the operating system. Under Windows, it is not
%@  possible to obtain the exit status using a process identifier if
%@  the process has already exited.
%@

%@  @var{ExitStatus} is one of:
%@  @table @code
%@  @item exit(@var{ExitCode})
%@  The process has exited with exit code @var{ExitCode}. By
%@  convention processes use exit code zero to signify success and a
%@  (positive) non-zero value to specify failure.

%@  @item killed(@var{SignalNumber})
%@  UNIX only, the process was killed by signal @code{SignalNumber} (a
%@  positive integer).
%@
%@  @item timeout
%@  The @code{timeout/1} option was specified and the process did not
%@  exit within the specified interval. In this case the process
%@  reference is not released, even if the @code{release/1} option is
%@  specified.
%@  @end table

%@  @var{Options} is a list of options:

%@  @table @code

%@  @item timeout(@var{Seconds})
%@  Specify a maximum time, in seconds, to wait for the process to
%@  terminate. @var{Seconds} should be an integer or floating point
%@  number or the atom @code{infinite} (the default) to specify 
%@  infinite wait. If the specified timeout interval passes before the
%@  process exits, @code{process_wait/3} exits with @var{ExitStatus}
%@  set to @code{timeout} and the process reference is not released.
%@
%@  Currently the UNIX implementation supports only timeout values
%@  0 (zero) and @code{infinite}.
%@

%@  @item release(@var{Bool})
%@  @var{Bool} is either @code{true} (the default) or
%@  @code{false}. Specifies whether the process reference should be
%@  released when @code{process_wait/3} exits successfully.

%@  @end table

process_wait(Process, ExitStatus) :-
        Goal = process_wait(Process, ExitStatus),
        Options = [],
        prolog:process_wait(Process, ExitStatus, Options, Goal).

process_wait(Process, ExitStatus, Options) :-
        Goal = process_wait(Process, ExitStatus, Options),
        prolog:process_wait(Process, ExitStatus, Options, Goal).

%@  @item process_id(@var{-PID})
%@  @PLXindex {process_id/1 (process)}

%@  Obtain the process identifier of the current (i.e.@: Prolog)
%@  process.

process_id(PID) :-
        prolog:process_self_id(PID).

%@  @item process_id(@var{+Process}, @var{+PID})
%@  @PLXindex {process_id/2 (process)}
%@  Obtain the process identifier of the process reference
%@  @var{Process}.

process_id(Process, PID) :-
        Goal = process_id(Process, PID),
        prolog:process_id(Process, PID, Goal).

%@  @item is_process(@var{+Thing})
%@  @PLXindex {is_process/1 (process)}
%@  Returns true if @var{Thing} is a process reference that has not
%@  been released.
is_process(Thing) :-
	Goal = is_process(Thing),
        prolog:is_process(Thing, Goal).

%@  @item process_release(@var{+Process})
%@  @PLXindex {process_release/1 (process)}
%@  Release a process reference @var{Process} that has previously been
%@  obtained from @code{process_create/3}. This ensures that Prolog
%@  and the operating system can reclaim any resources associated with
%@  the process reference.
%@
%@  Usually you would not call this. Either do not request the process
%@  reference when calling @code{process_create/3} or let
%@  @code{process_wait/[2,3]} reclaim the process reference when the
%@  process terminates.

process_release(Process) :-
        Goal = process_release(Process),
        prolog:process_release(Process, Goal).

%@  @item process_kill(@var{+Process})
%@  @itemx process_kill(@var{+Process}, @var{+SignalSpec})
%@  @PLXindex {process_kill/[1,2] (process)}

%@  Send a signal to the process designated by @var{Process}. The
%@  signal can either be a non-negative integer or a signal name as an
%@  (all uppercase) atom.
%@

%@  The following signal names are accepted under UNIX if the platform
%@  defines them: @code{SIGABRT}, @code{SIGALRM}, @code{SIGBUS},
%@  @code{SIGCHLD}, @code{SIGCONT}, @code{SIGFPE}, @code{SIGHUP},
%@  @code{SIGILL}, @code{SIGINT}, @code{SIGKILL} (the default),
%@  @code{SIGPIPE}, @code{SIGPOLL}, @code{SIGPROF}, @code{SIGQUIT},
%@  @code{SIGSEGV}, @code{SIGSTOP}, @code{SIGSYS}, @code{SIGTERM},
%@  @code{SIGTRAP}, @code{SIGTSTP}, @code{SIGTTIN}, @code{SIGTTOU},
%@  @code{SIGURG}, @code{SIGUSR1}, @code{SIGUSR2}, @code{SIGVTALRM},
%@  @code{SIGXCPU} and @code{SIGXFSZ}. However, many of these do not
%@  make sense to send as signals.
%@

%@  Under Windows, which does not have the signal
%@  concept, the signal name @code{SIGKILL} (the default) is treated
%@  specially and terminates the process with
%@  @code{TerminateProcess(Process, -1)}.
%@

process_kill(Process, SignalSpec) :-
	Goal = process_kill(Process, SignalSpec),
	prolog:process_kill(Process, SignalSpec, Goal).

process_kill(Process) :-
	Goal = process_kill(Process),
	prolog:process_kill(Process, 'SIGKILL', Goal).



%@  @end table
