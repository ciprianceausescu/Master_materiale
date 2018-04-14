/*  File   : math.c
    Authors: Evan Tick + Richard A. O'Keefe
    Updated: 04/15/99
    Defines: Interface to Unix Math library

    Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

    The math library is supplied with your Unix system.  It contains a
    number of common mathmetical (mostly transcendental) functions.
    We have added qp_pow(x, n) for calculating x**n when n is integral.
    This saves an integer->float coercion and a fair chunk of
    fiddling around while pow(3m) works out that its second argument
    used to be an integer.  This saving is worth having on workstations
    without floating point hardware, and never does any harm.  For some
    strange reason, the UNIX C library includes trig and arc-trig
    functions, and hyperbolic trig functions, but no arc-hyperbolic trig
    functions.  This is a pity, as it is tricky to get them right.  The
    code here is neither certified nor supported.

    ---------------------------------------------------------------------- */
   
#include <sicstus/sicstus.h>
#include "math_glue.h"
#include <math.h>		/* include math library definitions */
#include <errno.h>

#define COMPILE_TIME_ASSERT_DECLARATION(COND) extern int boobytrap[(COND) ? 1 : -1]


/*  From here on is the work of Richard A. O'Keefe.
    Note that Quintus are not a supplier of mathematical software:
    the routines provided here are not supported, and while they
    have been tested, Quintus make no claim that they are any good
    for anything at all.  There are some magic numbers, which have
    been chosen to suit a purported implementation of IEEE-754
    double-precision arithmetic.  The greatest source of error in
    this package is thought to be the truncation which takes place
    when floating-point numbers are represented as Prolog terms,
    but we repeat that you should really get routines like this
    from your C or Fortran vendor, or from a library such as NAG
    or IMSL or FUNPACK.  The only precautions which have been
    taken are precautions against overflow and underflow, and even
    that isn't guaranteed either.  This is not supported code!
*/


#ifdef	vax
#error "[PM] 4.0 not supported"
/* VAX D-format is used for 'double' */
#define	SignBit  0x00008000
#define	SignWord 0
#define	ExptMask 0x00007F80
#else
#ifdef	u370
#error "[PM] 4.0 not supported"
/* IBM System/370 "long" format is used for 'double' */
#define	SignBit	 0x80000000
#define	SignWord 0
#define ExptMask 0x7F000000
#else
/* All other machines are close enough to IEEE 754 "double" format */
#define	SignBit  0x80000000
#define SignWord 0
#define	ExptMask 0x7FF00000
#endif
#endif

union pun { double d; int L[2]; };
COMPILE_TIME_ASSERT_DECLARATION(sizeof(union pun) == sizeof(double));
COMPILE_TIME_ASSERT_DECLARATION(sizeof(int[2]) == sizeof(double));

#if 0                           /* [PM] 4.0 not needed */
   #if MULTI_SP_AWARE

   /* [PM] 3.9b4 ensures local.foo works. Also avoids need for SP_CONTEXT_SWITCH_HOOK. */
   #define local (*(void **)*SP_foreign_stash())

   #else  /* !MULTI_SP_AWARE */

   static void *local;

   #endif /* !MULTI_SP_aware */
#endif  /* 0 */

/*  QFdecd(+X, -S, -F, -E)
    takes a double X and breaks it into three pieces S, F, and E
    such that
	S = 1 or S = -1
	F = 0.0 or 0.5 <= F < 1.0
	X = S.F.2**E (E is an integer)
    We have to be careful about this in order to get the result
    right for IEEE -0.0.
*/
void SPCDECL QFdecd(SPAPI_ARG_PROTO_DECL 
    double X,
    long *S,
    double *F,
    long *E)
{
  union pun ux;
  int evalue;
  SPAPI_ARG_IGNORE;

  ux.d = X;
  *S = ux.L[SignWord]&SignBit ? -1 : 1;
  ux.L[SignWord] &=~ SignBit;
  *F = frexp(ux.d, &evalue);
  *E = evalue;
}


long SPCDECL QFencd(SPAPI_ARG_PROTO_DECL 
    double *X,
    long S,
    double F,
    long E)
{
  SPAPI_ARG_IGNORE;
  if (S != 1 && S != -1) return 0;
  if (F >= 1.0 || (F < 0.5 && (F != 0.0 || E != 0))) return 0;
  errno = 0;
  F = ldexp(F, E);
  if (errno) return 2;
  *X = S < 0 ? -F : F;
  return 1;
}

double SPCDECL pl_j0(SPAPI_ARG_PROTO_DECL double x)
{
  SPAPI_ARG_IGNORE;
  return j0(x);
}

double SPCDECL pl_j1(SPAPI_ARG_PROTO_DECL double x)
{
  SPAPI_ARG_IGNORE;
  return j1(x);
}

double SPCDECL pl_jn(SPAPI_ARG_PROTO_DECL long n,double x)
{
  SPAPI_ARG_IGNORE;
  return jn(n,x);
}

double SPCDECL pl_y0(SPAPI_ARG_PROTO_DECL double x)
{
  SPAPI_ARG_IGNORE;
  return y0(x);
}

double SPCDECL pl_y1(SPAPI_ARG_PROTO_DECL double x)
{
  SPAPI_ARG_IGNORE;
  return y1(x);
}

double SPCDECL pl_yn(SPAPI_ARG_PROTO_DECL long n,double x)
{
  SPAPI_ARG_IGNORE;
  return yn(n,x);
}

double SPCDECL pl_hypot(SPAPI_ARG_PROTO_DECL double x,double y)
{
  SPAPI_ARG_IGNORE;
  return hypot(x,y);
}

