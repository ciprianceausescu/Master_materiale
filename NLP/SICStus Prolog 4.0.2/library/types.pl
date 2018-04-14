% -*- Mode:Prolog -*-
%   Package: types.pl
%   Author : Richard A. O'Keefe
%   Updated: 11 Oct 1991
%   Purpose: More and better type tests.

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1989, Quintus Computer Systems, Inc.  All rights reserved.

/*  This file adds the type tests through the predicates:

	must_be(+Term, +Type, +Goal, +ArgNo)

    Checks whether the the Term in ArgNo of Goal belongs to the
    indicated Type.  If it isn't, there are two cases:
    The Term may not be instantiated enough to tell yet, in which
        case an instantiation_error will be raised
    The Term may be definitely not of the type, in which case a
	type_error is raised.

*/

:- module(types, [
	must_be/4,
	illarg/3,
	illarg/4
   ]).

% avoid a cyclic dependency
% :- use_module(library(lists), [
% 	memberchk/2
%	]).

%@  This library module provides more and better type tests.
%@  For the purposes of this library, we first define an abstract type
%@  @var{typeterm}, as follows:
%@  
%@  @multitable @columnfractions .25 .75
%@  @item @var{typeterm} @tab ::= @code{atom} 
%@  @item @tab | @code{atomic} 
%@  @item @tab | @code{callable}
%@  @item @tab | @code{character}
%@  @item @tab | @code{character_code}
%@  @item @tab | @code{compound}
%@  @item @tab | @code{float}
%@  @item @tab | @code{float(@var{rangeterm})}
%@  @item @tab | @code{ground}
%@  @item @tab | @code{integer}
%@  @item @tab | @code{integer(@var{rangeterm})}
%@  @item @tab | @code{list}
%@  @item @tab | @code{list(@var{Type})}
%@  @item @tab | @code{nonvar}
%@  @item @tab | @code{number}
%@  @item @tab | @code{number(@var{rangeterm})}
%@  @item @tab | @code{oneof(@var{L})}
%@  @item @tab | @code{pair}
%@  @item @tab | @code{proper_list}
%@  @item @tab | @code{proper_list(@var{Type})}
%@  @item @tab | @code{simple}
%@  @item @tab | @code{term}
%@  @item @tab | @code{var}
%@  @item
%@  @item @var{rangeterm} @tab ::= @code{between(L,U)}
%@  @item @tab | @code{>=(L)}
%@  @item @tab | @code{>(L)}
%@  @item @tab | @code{=\=(L)}
%@  @end multitable
%@  
%@  Exported predicates:
%@  
%@  @table @code
%@  @item must_be(@var{+Term}, @var{+Type}, @var{+Goal}, @var{+ArgNo})
%@  @PLXindex {must_be/4 (types)}
%@  checks whether the @var{Term} belongs to the indicated @var{Type},
%@  which should be a @var{typeterm}.  If it isn't, there are two cases:
%@  the @var{Term} may not be instantiated enough to tell yet, in which
%@  case an Instantiation Error will be raised, or the @var{Term} may be
%@  definitely not of the type, in which case a Type Error is raised.  You
%@  should use this in commands with side effects, and should arrange that
%@  if this predicate does not succeed the side effect(s) will not take
%@  place. If an exception is raised, it will pinpoint the line of code in
%@  the scope of which the error occurs, if possible.
%@  
%@  @item illarg(@var{+ErrorTerm}, @var{+Goal}, @var{+ArgNo})
%@  @itemx illarg(@var{+ErrorTerm}, @var{+Goal}, @var{+ArgNo}, @var{+Culprit})
%@  @PLXindex {illarg/[3,4] (types)}
%@  is the way to raise an error exception, if you
%@  would like the exception to pinpoint the line of code in the scope of
%@  which the error occurs.  This is especially useful in the context of
%@  source-linked debugging. @var{Culprit} defaults to argument number
%@  @var{ArgNo} of @var{Goal}. These three arguments are passed to the
%@  exception being raised, if appropriate. @var{ErrorTerm} should be one
%@  of the following.  @xref{ref-ere-err}.
%@  
%@  @table @code
%@  @item var
%@  An Instantiation error is raised.
%@  
%@  @item type(@var{ErrorType})
%@  Same as @code{must_be(@var{Culprit}, @var{ErrorType}, @var{Goal}, @var{ArgNo})}.
%@  
%@  @item domain(@var{ErrorType},@var{ErrorDomain})
%@  First, the type is checked by @code{must_be(@var{Culprit}, @var{ErrorType}, @var{Goal}, @var{ArgNo})}.
%@  If the type is valid, a
%@  Domain Error is raised with the expected domain being
%@  @var{ErrorDomain}.
%@  
%@  @item force_type(@var{ExpType})
%@  A Type Error is raised.
%@  
%@  @item context(@var{ContextType},@var{CommandType})
%@  A Context Error is raised.
%@  
%@  @item existence(@var{ObjType},@var{Culprit},@var{Message})
%@  An Existence Error is raised.
%@  
%@  @item permission(@var{Operation},@var{ObjType},@var{Message})
%@  A Permission Error is raised.
%@  
%@  @item representation(@var{ErrorType})
%@  A Representation Error is raised.
%@  
%@  @item evaluation(@var{ErrorType})
%@  An Evaluation Error is raised.
%@  
%@  @item consistency(@var{Culprit1},@var{Culprit2},@var{Message})
%@  A Consistency Error is raised.
%@  
%@  @item syntax(@var{Pos},@var{Msg},@var{Tokens},@var{AfterError})
%@  A Syntax Error is raised.
%@  
%@  @item resource(@var{Resource})
%@  A Resource Error is raised.
%@  
%@  @item system(@var{Message})
%@  A System Error is raised.
%@  
%@  @end table
%@  @end table


must_be(Term, Type, Goal, ArgNo) :-
	prolog:must_be(Term, Type, Goal, ArgNo).

illarg(ErrorTerm, Goal, ArgNo) :-
	prolog:illarg(ErrorTerm, Goal, ArgNo).

illarg(ErrorTerm, Goal, ArgNo, Culprit) :-
	prolog:illarg(ErrorTerm, Goal, ArgNo, Culprit).

