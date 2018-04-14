/* -*- Mode:Prolog; -*-
/* Copyright (C) 2005 Swedish Institute of Computer Science. */

:- module(sockets, [

%%	socket/2,
%%	socket_bind/2,
%%	socket_connect/3,
%%	socket_listen/2,
%%	socket_accept/2,
%%	socket_accept/3,
%%	socket_select/5,
%%	% socket_select/6,
%%	socket_close/1,
%%	socket_buffering/4,

	current_host/1,

%% [PM] 4.0 Should be replaced with non-determinate hostname_address/3
%%	hostname_address/2

	socket_client_open/3,
	socket_server_open/2,
        socket_server_close/1,
	socket_server_accept/4,
	socket_select/7

		   ]).
:- use_module(library(types), [
	illarg/3,
	must_be/4
	]).


%@  @cindex sockets
%@  @cindex process communication
%@  @cindex communication, process
%@  This library package defines a number of predicates for
%@  communicating over sockets.

%@  To create a (bi-directional) stream connected to a remote server,
%@  use @code{socket_client_open/3}.

%@  To open a port for remote clients to connect to, use
%@  @code{socket_server_open/2} and to open a stream to a connecting
%@  client, use @code{socket_server_accept/4}.

%@  To be able to multiplex input and output from several streams (not
%@  necesessarily socket streams) and incoming connections, use
%@  @code{socket_select/7}.


%@  
%@  All streams below can be read from as well as written on.  All I/O
%@  predicates operating on streams can be used, for example
%@  @code{get_code/2}, @code{get_byte/2},
%@  @code{read/2}, @code{write/2}, @code{format/3},
%@  @code{current_stream/3},
%@  etc.  The predicates that create streams take options similar to
%@  @code{open/4}, e.g.@:, to specify whether the stream is binary
%@  (the default) or text.
%@  
%@  @table @code


%@  @item socket_client_open(@var{+Addr}, @var{-Stream}, @var{+Options})
%@  @PLXindex {socket_client_open/3 (sockets)}
%@  Creates a stream @var{Stream} connected to address @var{Addr}.
%@  @var{Addr} can be:
%@  @table @code
%@  @item @var{Host}:@var{Port}

%@  Connect to the machine with address @var{Host} (a host name or
%@  host address) at port @var{Port} (a port number or service name).
%@  The @var{Host} should be an atom, e.g.@:, @code{'www.sics.se'}. The
%@  @var{Port} is either a port number as an integer or atom, e.g.@:,
%@  @code{80}, or @code{'80'}; alternatively some @dfn{well known
%@  port names} can be used, e.g., @code{'http'}. The set of well
%@  known port names is OS specific, portable code should use integer
%@  port numbers.
%@  @end table
%@  
%@  The stream is created using options from @var{Options}. Supported
%@  options include:
%@  @table @code
%@  @item type(binary)
%@  Create a binary stream (the default).
%@  @item type(text)
%@  Create a text stream. The default encoding is Latin 1.
%@  @item eof_action(@var{Action})
%@  end of file action, as for @code{open/4}.
%@  @c @item flush(auto)
%@  @c As for open/4.
%@  @end table
%@  
%@  To create a binary stream to some web server @code{www.sics.se}, you
%@  would do e.g.@:,
%@  @example
%@  | ?- socket_client_open('www.sics.se':80, Stream, [type(binary)]).
%@  @end example
%@  
%@  @noindent or, to make a text (Latin 1)
%@  stream to a @code{daytime} service in Hong Kong you could do:
%@  
%@  @example
%@  | ?- socket_client_open('stdtime.gov.hk':daytime, S, [type(text)]),
%@       read_line(S, L),
%@       format('~s', [L]).
%@  @end example
%@  
%@  See the source code for @code{library('linda/client')} for a
%@  simple client.
%@  
socket_client_open(Addr, Stream, Options) :-
   Goal = socket_client_open(Addr, Stream, Options),
   ArgNo = 3,
   OptionClass = socket_stream_open,
   socket_option_bits([direction(readwrite)|Options], OptionClass, OptionBits0, Options1, Goal,ArgNo),
   add_default_open_option_mode_bits(OptionBits0, OptionBits),
   %% FIXME: type check Addr
   Addr = (Nodename:Servname),
   prolog:'$socket_open'(Nodename, Servname, OptionBits, Options1, RawStream),
   Stream = '$stream'(RawStream).

socket_option_bits(Options, OptionClass, OptionBits, Options1, Goal,ArgNo) :-
   must_be(Options, proper_list(nonvar), Goal,ArgNo),
   socket_option_bits1(Options, OptionClass, 0,OptionBits, Options1, Goal,ArgNo).

socket_option_bits1([], _OptionClass, OptionBits0,OptionBits, Options1, _Goal,_ArgNo) :-
   OptionBits = OptionBits0,
   Options1 = [].
socket_option_bits1([Option|Options], OptionClass, OptionBits0,OptionBits, Options1, Goal,ArgNo) :-
   ( socket_option_bit(Option, OptionClass, OptionBit, Goal,ArgNo) ->
      OptionBits1 is OptionBits0 \/ OptionBit,
      Options1 = Options2
   ; otherwise ->
     OptionBits1 = OptionBits0,
     Options1 = [Option|Options2]
   ),
   socket_option_bits1(Options, OptionClass, OptionBits1,OptionBits, Options2, Goal,ArgNo).

% Caller ensures Option is nonvar
socket_option_bit(type(X), OptionClass, OptionBit, Goal,ArgNo) :-
   ( mode_type_bit(X, OptionClass, OptionBit0) ->
      OptionBit = OptionBit0
   ; mode_type_bit(_, OptionClass, OptionBit0) ->
      findall(T, mode_type_bit(T, Goal, _), Ts),
      must_be(X, one_of([Ts]), Goal,ArgNo)
   ).
socket_option_bit(eof_action(X), OptionClass, OptionBit, Goal,ArgNo) :-
   ( mode_eof_action_bit(X, OptionClass, OptionBit0) ->
      OptionBit = OptionBit0
   ; mode_eof_action_bit(_, OptionClass, OptionBit0) ->
      findall(T, mode_eof_action_bit(T, OptionClass, _), Ts),
      must_be(X, one_of([Ts]), Goal,ArgNo)
   ).
socket_option_bit(direction(X), OptionClass, OptionBit, Goal,ArgNo) :-
   ( mode_direction_bit(X, OptionClass, OptionBit0) ->
      OptionBit = OptionBit0
   ; mode_direction_bit(_, OptionClass, OptionBit0) ->
      findall(T, mode_direction_bit(T, Goal, _), Ts),
      must_be(X, one_of([Ts]), Goal,ArgNo)
   ).
socket_option_bit(flush(X), OptionClass, OptionBit, Goal,ArgNo) :-
   ( mode_flush_bit(X, OptionClass, OptionBit0) ->
      OptionBit = OptionBit0
   ; mode_flush_bit(_, OptionClass, OptionBit0) ->
      findall(T, mode_flush_bit(T, Goal, _), Ts),
      must_be(X, one_of([Ts]), Goal,ArgNo)
   ).

% FIXME: These option names are ugly
socket_option_bit(loopback,   socket_server_open, 0x0001, _Goal,_ArgNo). % PROLOG_SOCKET_LISTEN_OPTION_LOOPBACK
socket_option_bit(numeric_nodename, socket_server_open, 0x0002, _Goal,_ArgNo). % PROLOG_SOCKET_LISTEN_OPTION_NUMERIC_NODENAME
socket_option_bit(numeric_servname, socket_server_open, 0x0004, _Goal,_ArgNo). % PROLOG_SOCKET_LISTEN_OPTION_NUMERIC_SERVNAME
socket_option_bit(reuseaddr, socket_server_open, 0x0008, _Goal,_ArgNo). % PROLOG_SOCKET_LISTEN_OPTION_REUSEADDR


mode_direction_bit(read,      socket_stream_open, 0x0002). % read modeRead
mode_direction_bit(write,     socket_stream_open, 0x0004). % write modeWrite
mode_direction_bit(readwrite, socket_stream_open, 0x0006). % readwrite modeRead\/modeWrite

%% mode_bit(modeTTY, _OptionClass, 0x0008).

mode_eof_action_bit(eof,      socket_stream_open, 0x0200). % eof_action(eof) modeEofOnEof
mode_eof_action_bit(reset,    socket_stream_open, 0x0400). % eof_action(reset) modeResetOnEof

mode_type_bit(binary, socket_stream_open,         0x0800). % type(binary) modeBinary
mode_type_bit(text,   socket_stream_open,         0x4000). % type(text) modeText

mode_flush_bit(auto,   socket_stream_open,        0x8000). % flush(auto) modeAutoFlush

%@  @item socket_server_open(@var{?Port}, @var{-ServerSocket})
%@  @PLXindex {socket_server_open/2 (sockets)}
%@  Create a server socket @var{ServerSocket} that listens on port
%@  @var{Port}. Port can be either an integer port number or an atomic
%@  service name, see @code{socket_client_open/3} for details. Port can
%@  also be a variable in which case a free port number is used and
%@  @var{Port} is bound to it.
%@  The created server socket should be closed with
%@  @code{socket_server_close/1} eventually. Incoming connection can
%@  be accepted
%@  with @code{socket_server_accept/4} and waited for with
%@  @code{socket_select/7}.
%@  See the source code for @code{library('linda/server')} for a
%@  simple server that uses this predicate.
socket_server_open(Servname, Socket) :-
   Nodename = '',
   Options = [],
   Goal = socket_server_open(Servname, Socket),
   ArgNo = 0,
   socket_server1(Nodename, Servname, Socket, Options, Goal,ArgNo).

socket_server_open(Servname, Socket, Options) :-
   Nodename = '',
   Goal = socket_server_open(Servname, Socket, Options),
   ArgNo = 3,
   socket_server1(Nodename, Servname, Socket, Options, Goal,ArgNo).

socket_server_open(Nodename, Servname, Socket, Options) :-
   Goal = socket_server_open(Nodename, Servname, Socket, Options),
   ArgNo = 4,
   socket_server1(Nodename, Servname, Socket, Options, Goal,ArgNo).
   

socket_server1(Nodename, Servname, Socket, Options, Goal,ArgNo) :-
   OptionClass = socket_server_open,
   socket_option_bits(Options, OptionClass, OptionBits, Options1, Goal,ArgNo),
   prolog:'$socket_listen'(Nodename, Servname, OptionBits, Options1, RawSocket),
   socket_raw(Socket, RawSocket).


%@  @item socket_server_accept(@var{+ServerSocket}, @var{-Client}, @var{-Stream}, @var{+StreamOptions})
%@  @PLXindex {socket_server_accept/4 (sockets)}
%@  The first connection to socket @var{ServerSocket} is extracted,
%@  blocking if necessary.  The stream @var{Stream} is created on this
%@  connection using @var{StreamOptions} as for
%@  @code{socket_client_open/3}. @var{Client} will be unified with an atom containing
%@  the numerical Internet host address of the connecting client.

%@  Note that the stream will be @code{type(binary)} unless
%@  @code{type(text)} is explicitly specified.
socket_server_accept(Socket, ClientNodename, Stream, Options) :-
   Goal = socket_server_accept(Socket, ClientNodename, Stream, Options),
   ArgNo = 4,
   socket_option_bits(Options, socket_server_accept, AcceptOptionBits, Options1, Goal,ArgNo),
   socket_option_bits(Options1, socket_stream_open, OpenOptionBits0, Options2, Goal,ArgNo),
   add_default_open_option_bits(OpenOptionBits0, OpenOptionBits),
   OptionBits is AcceptOptionBits \/ OpenOptionBits,
   socket_raw(Socket, RawSocket),
   prolog:'$socket_accept'(RawSocket, OptionBits, Options2, RawNodename, _RawServname, RawStream),
   ClientNodename = RawNodename,
   Stream = '$stream'(RawStream).


add_default_open_option_bits(OpenOptionBits0, OpenOptionBits) :-
   add_default_open_option_mode_bits(OpenOptionBits0, OpenOptionBits).


% default is modeBinary
add_default_open_option_mode_bits(OpenOptionBits0, OpenOptionBits) :-
   mode_type_bit(binary, socket_stream_open,         ModeBinary),
   OpenOptionBits0 /\ ModeBinary =\= 0, !,
   OpenOptionBits = OpenOptionBits0.
add_default_open_option_mode_bits(OpenOptionBits0, OpenOptionBits) :-
   mode_type_bit(text, socket_stream_open,         ModeText),
   OpenOptionBits0 /\ ModeText =\= 0, !,
   OpenOptionBits = OpenOptionBits0.
add_default_open_option_mode_bits(OpenOptionBits0, OpenOptionBits) :-
   mode_type_bit(binary, socket_stream_open,         ModeBinary), !,
   OpenOptionBits is OpenOptionBits0 \/ ModeBinary.


%@  @item socket_server_close(@var{+ServerSocket})
%@  @PLXindex {socket_server_close/1 (sockets)}
%@  Close the server socket @var{ServerSocket} and stop listening on
%@  its port.
socket_server_close(Socket) :-
   socket_raw(Socket, RawSocket),
   prolog:'$socket_close'(RawSocket).


%@  @item socket_select(@var{+ServerSockets},@var{-SReady}, @var{+ReadStreams},@var{-RReady}, @var{+WriteStreams},@var{-WReady}, @var{+Timeout})
%@  @PLXindex {socket_select/7 (sockets)}
%@  Check for server sockets with incoming connections (i.e.@:, ready
%@  for @code{socket_server_accept/4}), streams on @var{ReadStreams}
%@  ready for input, and streams on @var{WriteStreams}
%@  ready for output. The streams can be any kind of streams, they need
%@  not be socket streams. The ready server sockets are returned (in the same
%@  order) in @var{SReady}, the ready input streams in @var{RReady},
%@  and the ready output streams in @var{WReady}.
%@  
%@  An input (output) stream is ready for input (output) when an
%@  @dfn{item} can be read (written) without blocking. An item
%@  is a character for text streams and a byte for binary streams.
%@  Note that a stream is considered ready for I/O if the
%@  corresponding I/O operation will raise an error (such as if the
%@  stream is past end of stream).
%@  
%@  Each entry in the input lists @var{ServerSockets},
%@  @var{ReadStreams}, and @var{WriteStreams} can be either a server
%@  socket or stream respectively or a term
%@  @code{@var{Term}-@var{Entry}} where @var{Entry} is the server
%@  socket or stream and @var{Term} is some arbitrary term used for
%@  book-keeping. If an entry is associated with a term in this way
%@  then so will the corresponding ready entry.
%@  
%@  If @var{TimeOut} is instantiated to @code{off}, the predicate
%@  waits until something is available.  If @var{TimeOut} is a nonzero
%@  number (integer or floating point), then the predicate waits at
%@  most that number of seconds before returning. For backward
%@  compatibility, if @var{TimeOut} is @var{S:U} the predicate waits
%@  at most @var{S} seconds and @var{U} microseconds. If there is a
%@  timeout, all ready lists are unified with @code{[]}.
%@  
%@  See the source code for @code{library('linda/server')} for a
%@  simple server that uses this predicate.
socket_select(ListenSockets,LReady, ReadStreams,RReady, WriteStreams,WReady, Timeout) :-
   Goal = socket_select(ListenSockets,LReady, ReadStreams,RReady, WriteStreams,WReady, Timeout),
   must_be(ListenSockets, proper_list(ground), Goal,1),
   raw_socket_list(ListenSockets, ListenSocketsRaw),
   ConnectSockets = [],         % FIXME: not yet supported (if ever)
   raw_socket_list(ConnectSockets, ConnectSocketsRaw),
   
   must_be(ReadStreams, proper_list(ground), Goal,3),
   raw_stream_list(ReadStreams, ReadStreamsRaw),
   must_be(WriteStreams, proper_list(ground), Goal,5),
   raw_stream_list(WriteStreams, WriteStreamsRaw),
   ( Timeout == 'off' ->                        % infinite timeout
      TimeoutParam = -1.0
   ; number(Timeout), Timeout >= 0 ->
      TimeoutParam is float(Timeout)            % allow integers too
   ; Timeout = S:M, integer(S), S>=0, integer(M), M>=0 -> % backward compatibility
      TimeoutParam is (float(S) + float(M)/1000000)
   ; otherwise ->
      illarg(domain(term,time_out_spec), Goal, 7)
   ),

   repeat,                                      % repeat while SPIO_E_INTERRUPTED
     prolog:'$select'(ListenSocketsRaw,LReadyRaw,
                      ConnectSocketsRaw,CReadyRaw,
                      ReadStreamsRaw,RReadyRaw,
                      WriteStreamsRaw,WReadyRaw,
                      TimeoutParam, ECODE),
     ECODE \== -10014,                            % repeat while SPIO_E_INTERRUPTED
   !,

   ( ECODE >= 0 ->
      unraw_socket_list(LReadyRaw, ListenSockets, LReady),
      unraw_socket_list(CReadyRaw, ConnectSockets, CReady),
      unraw_stream_list(RReadyRaw, ReadStreams, RReady),
      unraw_stream_list(WReadyRaw, WriteStreams, WReady)
   ; spio_error_name(ECODE, 'SPIO_E_TIMEOUT') ->
      LReady = [],
      CReady = [],
      RReady = [],
      WReady = []
   ; otherwise ->
      illarg(system('socket_select', ECODE), Goal, 0)
   ).

spio_error_name(Code, Name) :-
   spio_error_name1(Code, Name0), !,
   Name = Name0.
spio_error_name(Code, Name) :-
   number_codes(Code, Codes),
   ( Code < 0 ->
      append("SPIO Error: ", Codes, NameCodes)
   ; otherwise ->
      append("SPIO Success: ", Codes, NameCodes)
   ),
   atom_codes(Name, NameCodes).

spio_error_name1(0, 'SPIO_S_NOERR').
spio_error_name1(-10014, 'SPIO_E_INTERRUPTED').
spio_error_name1(-10018, 'SPIO_E_TIMEOUT').
spio_error_name1(-10019, 'SPIO_E_WOULD_BLOCK').

raw_socket_list([], SocketsRaw) :-
   SocketsRaw = [].
raw_socket_list([Entry|Sockets], [SocketRaw|SocketsRaw]) :-
   socket_entry_raw(Entry, SocketRaw),
   raw_socket_list(Sockets, SocketsRaw).

socket_entry_raw(_Key-Socket, SocketRaw) :- !,
  socket_raw(Socket, SocketRaw).
socket_entry_raw(Socket, SocketRaw) :-
  socket_raw(Socket, SocketRaw).

socket_raw('$socket'(Raw), Raw).

unraw_socket_list([], _Keyed, Sockets) :-
   Sockets = [].
unraw_socket_list([SocketRaw|SocketsRaw], [KE|Keyed], Sockets) :-
   socket_entry_raw(KE, KERaw),
   SocketRaw == KERaw, !,
   Sockets = [KE|Sockets1],
   unraw_socket_list(SocketsRaw, Keyed, Sockets1).
unraw_socket_list(SocketsRaw, Keyed, Sockets) :- SocketsRaw = [_|_],
   Keyed = [_KE|Keyed1],
   unraw_socket_list(SocketsRaw, Keyed1, Sockets).

raw_stream_list([], StreamsRaw) :-
   StreamsRaw = [].
raw_stream_list([Entry|Streams], [StreamRaw|StreamsRaw]) :-
   stream_entry_raw(Entry, StreamRaw),
   raw_stream_list(Streams, StreamsRaw).

stream_entry_raw(_Key-Stream, StreamRaw) :- !,
  stream_raw(Stream, StreamRaw).
stream_entry_raw(Stream, StreamRaw) :-
  stream_raw(Stream, StreamRaw).

stream_raw('$stream'(Raw), Raw).

   
unraw_stream_list([], _Keyed, Streams) :-
   Streams = [].
unraw_stream_list([StreamRaw|StreamsRaw], [KE|Keyed], Streams) :-
   stream_entry_raw(KE, KERaw),
   StreamRaw == KERaw, !,
   Streams = [KE|Streams1],
   unraw_stream_list(StreamsRaw, Keyed, Streams1).
unraw_stream_list(StreamsRaw, Keyed, Streams) :- StreamsRaw = [_|_],
   Keyed = [_KE|Keyed1],
   unraw_stream_list(StreamsRaw, Keyed1, Streams).

%@  @item current_host(@var{?HostName})
%@  @PLXindex {current_host/1 (sockets)}
%@  @var{HostName} is unified with the fully qualified name of the
%@  machine that the process is executing on. The call will also succeed if
%@  @var{HostName} is instantiated to the unqualified name of the
%@  machine in lower case.

current_host(Host) :-
  prolog:'$hostname'(HostName, Domain),
  ( Host = HostName ->
     true
  ; atom(Host) ->
     atom_concat(Host, Domain, HostName)
  ).

%@  @end table

