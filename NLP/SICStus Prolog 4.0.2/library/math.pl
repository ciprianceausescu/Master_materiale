%   Module : math
%   Authors: Evan Tick + Richard A. O'Keefe
%   Updated: 04/15/99
%   Purpose: Interface to Unix Math library

%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(math, [
	% predicate versions of built-in expressions
	abs/2,
	cos/2,
	acos/2,
	cosh/2,
	acosh/2,
	cot/2,
	acot/2,
	coth/2,
	acoth/2,
	sin/2,
	asin/2,
	sinh/2,
	asinh/2,
	tan/2,
	atan/2,
	tanh/2,
	atanh/2,
	exp/2,
	exp/3,
	float_fractional_part/2,
	float_integer_part/2,
	gcd/3,
	float/2,
	integer/2,
	log/2,
	log/3,
	max/3,
	min/3,
	msb/2,
	sign/2,
	sqrt/2,
	acot2/3,
	atan2/3,
	floor/2,
	truncate/2,
	ceiling/2,
	round/2,
	% esoteric functions defined in C
	decode_float/4,
	hypot/3,
	j0/2,
	j1/2,
	jn/3,
	y0/2,
	y1/2,
	yn/3,
	% library(knuth_b_1)
	constant/2,
	% library(order)
	number_order/3
   ]).
:- use_module(library(types), [
	must_be/4,
	illarg/3
	]).

/*  library(math) started as a tiny little demo, showing how you could
    pull in the Unix math(3m) library.  Then we ported the system to
    the Xerox Lisp machines, added a couple of functions, wrote some
    missing inverses, moved the new stuff back to the Sun, and it sort
    of grew from there.  The nasty thing is that when you do
	:- compile(library(math)).
    it uses about 81k of memory in SunOS 3.5.  Sorry about that, but it
    really isn't our fault:  math.o takes about 3.6k, and math.pl is
    about the same size.  It turns out that a program which has no
    code of its own but pulls in all of the functions that we pull in
    from the Unix math library is also about 80k.  That's how big the
    SunOS 3.5 math library is.
*/

%@  This module provides a number of mathematical predicates.  Some of the
%@  exported predicates duplicate the built-in arithmetic evaluables
%@  (@pxref{ref-ari-aex}); others extend that set.  The following all take
%@  some number of numeric arguments and yield a numeric result as their
%@  last argument.  For example, @code{sin(@var{X}, @var{Result})} unifies
%@  @var{Result} with @code{sin(@var{X})}:

%@  @table @code
 

%@  @item abs(@var{+X}, @var{-Value})
%@  @PLXindex {abs/2 (math)}

abs(X, Value) :-
	Value is abs(X).

	
%@  @item cos(@var{+X}, @var{-Value})
%@  @PLXindex {cos/2 (math)}

cos(X, Value) :-
	Value is cos(X).

%@  @item acos(@var{+X}, @var{-Value})
%@  @PLXindex {acos/2 (math)}

acos(X, Value) :-
	Value is acos(X).

%@  @item cosh(@var{+X}, @var{-Value})
%@  @PLXindex {cosh/2 (math)}

cosh(X, Value) :-
	Value is cosh(X).

%@  @item acosh(@var{+X}, @var{-Value})
%@  @PLXindex {acosh/2 (math)}

acosh(X, Value) :-
	Value is acosh(X).

%@  @item cot(@var{+X}, @var{-Value})
%@  @PLXindex {cot/2 (math)}

cot(X, Value) :-
	Value is cot(X).

%@  @item acot(@var{+X}, @var{-Value})
%@  @PLXindex {acot/2 (math)}

acot(X, Value) :-
	Value is acot(X).

%@  @item coth(@var{+X}, @var{-Value})
%@  @PLXindex {coth/2 (math)}

coth(X, Value) :-
	Value is coth(X).

%@  @item acoth(@var{+X}, @var{-Value})
%@  @PLXindex {acoth/2 (math)}

acoth(X, Value) :-
	Value is acoth(X).

%@  @item sin(@var{+X}, @var{-Value})
%@  @PLXindex {sin/2 (math)}

sin(X, Value) :-
	Value is sin(X).

%@  @item asin(@var{+X}, @var{-Value})
%@  @PLXindex {asin/2 (math)}

asin(X, Value) :-
	Value is asin(X).

%@  @item sinh(@var{+X}, @var{-Value})
%@  @PLXindex {sinh/2 (math)}

sinh(X, Value) :-
	Value is sinh(X).

%@  @item asinh(@var{+X}, @var{-Value})
%@  @PLXindex {asinh/2 (math)}

asinh(X, Value) :-
	Value is asinh(X).

%@  @item tan(@var{+X}, @var{-Value})
%@  @PLXindex {tan/2 (math)}

tan(X, Value) :-
	Value is tan(X).

%@  @item atan(@var{+X}, @var{-Value})
%@  @PLXindex {atan/2 (math)}

atan(X, Value) :-
	Value is atan(X).

%@  @item tanh(@var{+X}, @var{-Value})
%@  @PLXindex {tanh/2 (math)}

tanh(X, Value) :-
	Value is tanh(X).

%@  @item atanh(@var{+X}, @var{-Value})
%@  @PLXindex {atanh/2 (math)}

atanh(X, Value) :-
	Value is atanh(X).

%@  @item exp(@var{+X}, @var{-Value})
%@  @PLXindex {exp/[2,3] (math)}

exp(X, Value) :-
	Value is exp(X).

%@  @item exp(@var{+X}, @var{+Y}, @var{-Value})

exp(X, Y, Value) :-
	Value is exp(X,Y).

	
%@  @item float_fractional_part(@var{+X}, @var{-Value})
%@  @PLXindex {float_fractional_part/2 (math)}

float_fractional_part(X, Value) :-
	Value is float_fractional_part(X).

%@  @item float_integer_part(@var{+X}, @var{-Value})
%@  @PLXindex {float_integer_part/2 (math)}

float_integer_part(X, Value) :-
	Value is float_integer_part(X).

%@  @item gcd(@var{+X}, @var{+Y}, @var{-Value})
%@  @PLXindex {gcd/3 (math)}

gcd(X, Y, Value) :-
	Value is gcd(X,Y).

%@  @item float(@var{+X}, @var{-Value})
%@  @PLXindex {float/2 (math)}

float(X, Value) :-
	Value is float(X).

%@  @item integer(@var{+X}, @var{-Value})
%@  @PLXindex {integer/2 (math)}

integer(X, Value) :-
	Value is integer(X).

%@  @item log(@var{+X}, @var{-Value})
%@  @PLXindex {log/2 (math)}

log(X, Value) :-
	Value is log(X).

%@  @item log(+X, @var{+Y}, @var{-Value})
%@  @PLXindex {log/2 (math)}

log(X, Y, Value) :-
	Value is log(X,Y).

%@  @item max(@var{+X}, @var{+Y}, @var{Max})
%@  @PLXindex {max/3 (math)}

max(X, Y, Max) :-
	Max is max(X,Y).

%@  @item min(@var{+X}, @var{+Y}, @var{Min})
%@  @PLXindex {min/3 (math)}

min(X, Y, Min) :-
	Min is min(X,Y).

%@  @item msb(@var{+X}, @var{-Value})
%@  @PLXindex {msb/2 (math)}

msb(X, Value) :-
	Value is msb(X).

%@  @item sign(@var{+X}, @var{Sign})
%@  @PLXindex {sign/2 (math)}

sign(X, Sign) :-
	Sign is sign(X).

%@  @item sqrt(@var{+X}, @var{-Value})
%@  @PLXindex {sqrt/2 (math)}

sqrt(X, Value) :-
	Value is sqrt(X).

%@  @item acot2(@var{+X}, @var{+Y}, @var{-Value})
%@  @PLXindex {acot2/3 (math)}

acot2(X, Y, Value) :-
	Value is acot2(X,Y).

%@  @item atan2(@var{+X}, @var{+Y}, @var{-Value})
%@  @PLXindex {atan2/3 (math)}

atan2(X, Y, Value) :-
	Value is atan2(X,Y).

%@  @item floor(@var{+X}, @var{-Value})
%@  @PLXindex {floor/2 (math)}

floor(X, I) :-
	I is floor(X).

%@  @item truncate(@var{+X}, @var{-Value})
%@  @PLXindex {truncate/2 (math)}

truncate(X, I) :-
	I is truncate(X).

%@  @item ceiling(@var{+X}, @var{-Value})
%@  @PLXindex {ceiling/2 (math)}

ceiling(X, I) :-
	I is ceiling(X).

%@  @item round(@var{+X}, @var{-Value})
%@  @PLXindex {round/2 (math)}

round(X, I) :-
	I is round(X).

%@  @item hypot(@var{+X}, @var{+Y}, @var{-Value})
%@  @PLXindex {hypot/2 (math)}

%@  @item j0(@var{+X}, @var{-Value})
%@  @PLXindex {j0/2 (math)}

%@  @item j1(@var{+X}, @var{-Value})
%@  @PLXindex {j1/2 (math)}

%@  @item jn(@var{+X}, @var{+Y}, @var{-Value})
%@  @PLXindex {jn/3 (math)}

%@  @item y0(@var{+X}, @var{-Value})
%@  @PLXindex {y0/2 (math)}

%@  @item y1(@var{+X}, @var{-Value})
%@  @PLXindex {y1/2 (math)}

%@  @item yn(@var{+X}, @var{+Y}, @var{-Value})
%@  @PLXindex {yn/3 (math)}


%@  @end table

%@  The following predicate decomposes a float:
%@  
%@  @table @code
 
%@  @item decode_float(@var{?Number}, @var{?Sign}, @var{?Significand}, @var{?Exponent})
%@  @PLXindex {decode_float/4 (math)}
%@  is true when @var{Number} and @var{Significand} are the same kind of float,
%@  @var{Sign} and @var{Exponent} are integers, @var{Sign = +1} or @var{Sign = -1}, and
%@  @var{abs(Number) = Significand = 0.0} and @var{Exponent = 0} or
%@  @var{abs(Number) = Sign * Significand * 2.0**Exponent}.

decode_float(Number, Sign, Significand, Exponent) :-
	Goal = decode_float(Number,Sign,Significand,Exponent),
	(   float(Number) ->
	    'QFdecd'(Number, Sign, Significand, Exponent)
	;   var(Number) ->
	    'QFencd'(X, Sign, Significand, Exponent, Status),
	    (	Status =:= 1 -> Number = X
	    ;	Status =:= 2 ->
		illarg(representation(float), Goal, 0)
	    )
	;   must_be(Number, float, Goal, 1)
	).



foreign_resource(math,
    [
	pl_hypot,
	pl_j0,
	pl_j1,
	pl_jn,
	pl_y0,
	pl_y1,
	pl_yn,
	'QFdecd',	% QFdecd(+Num, -Sign, -Frac, -Expt)
	'QFencd'	% QFencd(-Num, +Sign, +Frac +Expt) -> Status
    ]).


/* ----------------------------------------------------------------------
     Declare Prolog predicate names and arguments for each C function
   ---------------------------------------------------------------------- */

foreign(pl_hypot,	hypot(	 +float,+float,[-float])).
foreign(pl_j0,		j0(	 +float,[-float])).
foreign(pl_j1,		j1(	 +float,[-float])).
foreign(pl_jn,		jn(	 +integer,+float,[-float])).
foreign(pl_y0,		y0(	 +float,[-float])).
foreign(pl_y1,		y1(	 +float,[-float])).
foreign(pl_yn,		yn(	 +integer,+float,[-float])).
foreign('QFdecd',	'QFdecd'(+float,-integer,-float,-integer)).
foreign('QFencd',	'QFencd'(-float,+integer,+float,+integer,[-integer])).

:- load_foreign_resource(library(system(math))).


%   Package: knuth_b_1
%   Typist : Richard A. O'Keefe
%   Author : the rules of arithmetic, via Donald Knuth
%   Updated: 30 Apr 1990
%   Purpose: table of constants.

/*  This is table 1 of appendix B of "The Art of Computer Programming"
    volume 1, by Donald E. Knuth.  That table gives 40 decimal places.
    As most of the Prolog systems known to me have trouble with 6
    places, I have rounded to 7 decimal places.  Anyone wanting more
    should consult the original reference.  Note that most of these
    names are compound terms, e.g. sqrt(2), 3^(1/3), cos(1).  This
    was done for clarity.

    This table may be extended with other constants taken from
    Abramowitz & Stegun, "Handbook of Mathematical Functions",
    chapter 1.  Note that they give numbers to 21 or 26 places.
    Which constants would be most useful to you?
*/



%@  @end table

%@  This predicate implements is table 1 of appendix B of "The Art of
%@  Computer Programming" volume 1, by Donald E. Knuth.

%@  @table @code

%@  @item constant(@var{+Expression}, @var{-Value}) 
%@  @PLXindex {constant/2 (math)}

constant(sqrt(2),	1.41421356237309504880).
constant(sqrt(3),	1.73205080756887729353).
constant(sqrt(5),	2.23606797749978969641).
constant(sqrt(10),	3.16227766016837933200).

constant(2^(1/3),	1.25992104989487316477).
constant(3^(1/3),	1.44224957030740838232).
constant(2^(1/4),	1.18920711500272106672).

constant(ln(2),		0.69314718055994530942).
constant(ln(3),		1.09861228866810969140).
constant(ln(10),	2.30258509299404568402).
constant(1/ln(2),	1.44269504088896340736).
constant(1/ln(10),	0.43429448190325182765).
constant(ln(ln(2)),    -0.36651292058166432701).

constant(pi,		3.14159265358979323846).
constant(pi/180,	0.01745329251994329577).
constant(1/pi,		0.31830988618379067154).
constant(pi^2,		9.86960440108935861883).
constant(sqrt(pi),	1.77245385090551602730).
constant(ln(pi),	1.14472988584940017414).

constant(gamma(1/2),	1.77245385090551602730).
constant(gamma(1/3),	2.67893853470774763366).
constant(gamma(2/3),	1.35411793942640041695).

constant(e,		2.71828182845904523536).
constant(1/e,		0.36787944117144232160).
constant(e^2,		7.38905609893065022723).
constant(e^(pi/4),	2.19328005073801545656).

constant(gamma,		0.57721566490153286061).
constant(e^gamma,	1.78107241799019798524).

constant(phi,		1.61803398874989484820).
constant(ln(phi),	0.48121182505960344750).
constant(1/ln(phi),	2.07808692123502753760).

constant(sin(1),	0.84147098480789650665).
constant(cos(1),	0.54030230586813971740).

constant(zeta(3),	1.20205690315959428540).


%@  @end table

%@  The following predicate is analogous to @code{compare/3}, but
%@  compares numbers.

%@  @table @code
 
/*  The usual convention for Prolog operations is INPUTS before OUTPUTS.
    The built-in predicate compare/3 violates this.  We're sorry about
    that, but that's the way it was in DEC-10 Prolog, and Quintus isn't
    in the business of breaking other people's working code.  What we
    *can* do is provide an additional interface.

    We suggest that you reserve predicate names "compare*" for things
    which (perhaps after closure on initial arguments) have the same
    argument order as compare/3.  Names "<type>_order" are recommended
    for predicates following the inputs-before-outputs convention.
    Here are four.  Please suggest more.

    The convention I have adopted here is that if the first two
    arguments are not comparable, the predicate fails.  This is done for
    sets (unordered and ordered), and _should_ be done for floats.
*/

%@  @item number_order(@var{+X}, @var{+Y}, @var{-R})
%@  @PLXindex {number_order/3 (math)}
%@  is true when @var{X} and @var{Y} are numbers and @var{R} is @code{<}, @code{=}, or @code{>} according as
%@  @var{X < Y}, @var{X =:= Y}, or @var{X > Y}.  Note that if @var{X =:= Y}, it does not follow
%@  that @var{X=Y}; one of them might be a float and the other an integer, or
%@  one of them might be +0.0 and the other -0.0.

number_order(X, Y, R) :-
	number(X), number(Y),
	( X < Y -> R = <
	; X > Y -> R = >
	; X=:=Y -> R = =
	).


%@  @end table
