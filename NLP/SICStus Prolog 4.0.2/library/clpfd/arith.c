/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

#if MULTI_SP_AWARE
#define mul_lt_internal(A1,A2) mul_lt_internal(HIDDEN_ARG, A1,A2)
#define mul_ltd_internal(A1,A2,A3) mul_ltd_internal(HIDDEN_ARG, A1,A2,A3)
#define scalar_product_init(A1,A2) scalar_product_init(HIDDEN_ARG, A1,A2)
#define scalar_product_le(A1,A2) scalar_product_le(HIDDEN_ARG, A1,A2)
#define scalar_product_ge(A1,A2) scalar_product_ge(HIDDEN_ARG, A1,A2)
#endif /* MULTI_SP_AWARE */

#define NegateLB(LB) ((LB)==Inf ? Sup : Tminus(LB))

#define NegateUB(UB) ((UB)==Sup ? Inf : Tminus(UB))

#define ABS(G) ((G)>=0 ? (G) : -(G))

static INLINE long gcd (long c1,long c2)
{
  if (c1 > 1 && c2 > 1) {
    while (c1 != 0 && c2 != 0) {
      c1 %= c2;
      if (c1 != 0)
	c2 %= c1;
    }
    return c1+c2;
  } else {
    return 1;
  }
}

#define HALFSHIFT (4<<LogSizeOfWord)
#ifdef __MSVC_RUNTIME_CHECKS
/* [PM] 3.9.1 .NET "/RTCc Reports when a value is assigned to a
        smaller data type that results in a data loss." */
#define LOWER_UHALF(X) ((UHALF)((X) & ((UHALF)~0)))
#else
#define LOWER_UHALF(X) ((UHALF)(X))
#endif

/* returns TRUE on overflow */
/* ripped off from wamfunc.c */
static SP_BOOL long_mult(SP_WORD x, SP_WORD y, SP_WORD *productp)
{
  SP_UWORD lower, tmp;
  UHALF x0, x1, y0, y1;
  int negated;
  
  if (x>>HALFSHIFT==x>>(HALFSHIFT-1) &&
      y>>HALFSHIFT==y>>(HALFSHIFT-1)) {
    *productp = x*y;
    return FALSE;
  }

  negated = 0;
  if (x<0)
    x = -x, negated ^= 1;
  if (y<0)
    y = -y, negated ^= 1;

  x0 = LOWER_UHALF(x);
  x1 = (UHALF)(x>>HALFSHIFT);
  y0 = LOWER_UHALF(y);
  y1 = (UHALF)(y>>HALFSHIFT);

  lower = 0L;
  if (x0 && y0) {
    lower = (SP_UWORD)x0*(SP_UWORD)y0;
  }
  if (x1 && y1) {
    return TRUE;
  } else if (x0 && y1) {
    tmp = (SP_UWORD)x0*(SP_UWORD)y1 + (lower>>HALFSHIFT);
    if (tmp>>HALFSHIFT)
      return TRUE;
    else
      lower = (tmp<<HALFSHIFT) + LOWER_UHALF(lower);
  } else if (x1 && y0) {
    tmp = (SP_UWORD)x1*(SP_UWORD)y0 + (lower>>HALFSHIFT);
    if (tmp>>HALFSHIFT)
      return TRUE;
    else
      lower = (tmp<<HALFSHIFT) + LOWER_UHALF(lower);
  }
  if ((SP_WORD)lower<0L && (lower<<1L || !negated))
    return TRUE;
  if (negated)
    lower = -lower;
  *productp = lower;
  return FALSE;
}

/* precond: t1 and t2 are tagged integers */
static TAGGED safe_mul_val(TAGGED t1, TAGGED t2)
{
  long st = GetSmall(t1);
  long su = GetSmall(t2);
  long prod;
  
  if (!long_mult(st,su,&prod) && IntIsSmall(prod))
    return MakeSmall(prod);
  else
    return (((st<0)^(su<0)) ? Inf : Sup);
}

/* operations exported to indexical.c */

TAGGED safe_mul(TAGGED t1, TAGGED t2)
{
  if (TagIsSmall(t1)) {
    if (TagIsSmall(t2))
      t1 = safe_mul_val(t1,t2);
    else if (Teqz(t1))
      return ERRORTAG;	/* 0*inf -> give up */
    else
      t1 = (Tltz(t1)^(t2==Inf) ? Inf : Sup);
  } else if (TagIsSmall(t2)) {
    if (Teqz(t2))
      t1 = ERRORTAG;		/* inf*0 -> give up */
    else
      t1 = (Tltz(t2)^(t1==Inf) ? Inf : Sup);
  } else
    t1 = (t1!=t2 ? Inf : Sup);
  return t1;
}

/* ceiling of quotient, SmallxSmall */
static TAGGED safe_divu_val(TAGGED t1, TAGGED t2)
{
  if (Tnez(t2)) {
    SP_WORD x = t1-TaggedZero;
    SP_WORD y = t2-TaggedZero;
    SP_WORD q = (  x==0 ? 0
		: (x<0)^(y<0) ? x/y 
		: x>0 ? (x+y-1)/y
		: (x+y+1)/y
		);
    t1 = MakeSmall(q);
  } else if (Tltz(t1))
    t1 = Inf;
  else if (Tgtz(t1))
    t1 = Sup;
  else
    t1 = ERRORTAG;	/* 0/0 -> give up */
  return t1;
}


/* ceiling of quotient */
TAGGED safe_divu(TAGGED t1, TAGGED t2)
{
  if (TagIsSmall(t1)) {
    if (TagIsSmall(t2))
      t1 = safe_divu_val(t1,t2);
    else if (Tnez(t1))	/* +/sup, -/inf -> 1 */
      t1 = (Tltz(t1)^(t2==Inf) ? TaggedZero : TaggedOne);
  } else if (TagIsSmall(t2))
    t1 = (Tltz(t2)^(t1==Inf) ? Inf : Sup);
  else
    t1 = ERRORTAG;		/* inf/inf -> give up */
  return t1;
}

/* floor of quotient, SmallxSmall */
static TAGGED safe_divd_val(TAGGED t1, TAGGED t2)
{
  if (Tnez(t2)) {
    SP_WORD x = t1-TaggedZero;
    SP_WORD y = t2-TaggedZero;
    SP_WORD q = ( x==0 ? 0
	       : (x<0)^(y>0) ? x/y 
	       : x>0 ? (x-y-1)/y
	       : (x-y+1)/y
	       );
	      
    t1 = MakeSmall(q);
  } else if (Tltz(t1))
    t1 = Inf;
  else if (Tgtz(t1))
    t1 = Sup;
  else
    t1 = ERRORTAG;	/* 0/0 -> give up */
  return t1;
}

/* floor of quotient */
TAGGED safe_divd(TAGGED t1, TAGGED t2)
{
  if (TagIsSmall(t1)) {
    if (TagIsSmall(t2)) {
      t1 = safe_divd_val(t1,t2);
    } else if (Tnez(t1))	/* -/sup, +/inf -> -1 */
      t1 = (Tltz(t1)^(t2==Sup) ? TaggedZero : TaggedMinusOne);
  } else if (TagIsSmall(t2))
    t1 = (Tltz(t2)^(t1==Inf) ? Inf : Sup);
  else
    t1 = ERRORTAG;		/* inf/inf -> give up */
  return t1;
}

TAGGED safe_plus(TAGGED t1, TAGGED t2)
{
  if (TagIsSmall(t1)) {
    if (TagIsSmall(t2)) {
      TADDCHK(t1,t2);
    } else
      t1 = t2;
  }
  return t1;
}

TAGGED safe_minus(TAGGED t1, TAGGED t2)
{
  if (TagIsSmall(t1)) {
    if (TagIsSmall(t2)) {
      TSUBCHK(t1,t2);
    } else
      t1 = (t2==Inf ? Sup : Inf);
  }
  return t1;
}

static TAGGED safe_negate_val(TAGGED t2)
{
  t2 = Tminus(t2);
  return (TagIsSmall(t2) ? t2 : Sup);
}

static TAGGED safe_negate(TAGGED t2)
{
  TAGGED t1;
  
  if (TagIsSmall(t2)) {
    t1 = Tminus(t2);
    if (!TagIsSmall(t1))
      t1 = Sup;
  } else
    t1 = (t2==Inf ? Sup : Inf);
  return t1;
}

static TAGGED safe_min(TAGGED t1,TAGGED t2)
{
  return (FDlt(t1,t2) ? t1 : t2);
}

static TAGGED safe_max(TAGGED t1,TAGGED t2)
{
  return (FDgt(t1,t2) ? t1 : t2);
}

#if (LogSizeOfWord==2)

#ifdef __MSVC_RUNTIME_CHECKS
/* [PM] 4.0 MS C compiler (with /RTCc) will barf when it detect
   truncation. Instead explicitly mask the value. (It would probably
   not hurt to leave this in, any half-decent compiler will realize it
   is a no-op. */
#define MASK_32(X) ((long)((X) & 0xFFFFFFFF))
#else  /* !__MSVC_RUNTIME_CHECKS */
#define MASK_32(X) ((long)(X))
#endif  /* !__MSVC_RUNTIME_CHECKS */


#define giant SP_int64

#define let_giant(X) SP_int64 X

#define field_giant(X) SP_int64 X

#define init_giant(X)

#define mem_giant SP_int64

#define ref_mem_giant(X) (*(X))

#define store_giant(X,Y) ((Y)=(X))

#define ASLONG(DW) (MASK_32(DW))

#define ASLONGL(DW) (DW)

#define ASULONG(DW) ((unsigned long)MASK_32(DW))

#define ASUMSW(DW) ((unsigned long)MASK_32(((DW)>>32)))

#define issmall_d(X) ((X)>=-HighInt && (X)<HighInt)

/*TODO: assuming signed shift*/
#define islong_d(X) (((X)>>31)==((X)>>32))

#define cmp_deqd(X,Y) ((X)==(Y))
#define cmp_dltd(X,Y) ((X)<(Y))
#define cmp_dgtd(X,Y) ((X)>(Y))

#define cmp_deqz(X) ((X)==0)
#define cmp_dnez(X) ((X)!=0)
#define cmp_dltz(X) ((X)< 0)
#define cmp_dlez(X) ((X)<=0)
#define cmp_dgtz(X) ((X)> 0)
#define cmp_dgez(X) ((X)>=0)

#define add_zdd(X,Y) ((Y)=(X))
#define add_zld(X,Y) ((Y)=(X))

#define add_ztd(X,Y) ((Y)=GetLong(X))

#define add_ddd(X,Y,Z) ((Z)=(X)+(Y))
#define add_dud(X,Y,Z) ((Z)=(X)+(Y))

#define sub_ddd(X,Y,Z) ((Z)=(X)-(Y))
#define sub_dud(X,Y,Z) ((Z)=(X)-(Y))
#define sub_dld(X,Y,Z) ((Z)=(X)-(Y))

#define sub_zdd(X,Y) ((Y) = -(X))

#define mul_uud(X,Y,Z) ((Z)=(X)*(Y))

#define mul_ltd(X,Y,Z) ((Z)=mul_lt_internal((X),(Y)))

static giant
mul_lt_internal MAGIC (HIDDEN_PROTO
		       SP_WORD x,TAGGED ty)
{
  if (ty==Inf) {
    return (giant)(-x)<<32;
  } else if (ty==Sup) {
    return (giant)(x)<<32;
  } else {
    SP_WORD y = GetSmall(ty);
    giant z = (giant)x*(giant)y;

    if (!islong_d(z))
      fd.fd_overflow = TRUE;
    return z;
  }
}

#define div_dll(X,Y) ((X)/(Y))
#define div_dld(X,Y,Z) ((Z)=(X)/(Y))
#define div_dud_internal(X,Y,Z) ((Z)=(X)/(Y))

#define CEILDIVUU(X,Y) (((X)-1)/(Y)+1)

#define div_dldf(X,Y,Z) ((Z)=((X)>=0 ? ((Y)>=0 ? (X)/(Y)          : -CEILDIVUU(X,-(Y)))	\
			             : ((Y)>=0 ? -CEILDIVUU(-(X),Y) : (X)/(Y))))	\


#define div_dldc(X,Y,Z) ((Z)=((X)>=0 ? ((Y)>=0 ? CEILDIVUU(X,Y) : (X)/(Y))		\
			             : ((Y)>=0 ? (X)/(Y)      : CEILDIVUU(-(X),-(Y)))))	\


#define mod_duu(X,Y) ((SP_UWORD)((X)>=0 ? (X)%(Y) : (((X)%(Y))+(Y))%(Y)))

#else /* LogSizeOfWord==2 */

#define giant Dword

#define let_giant(X) struct dword X ## _mem; giant X = &X ## _mem

#define field_giant(X) struct dword X ## _mem; giant X

#define init_giant(X) (X = &X ## _mem)

#define mem_giant struct dword

#define ref_mem_giant(X) (X)

#define store_giant(X,Y)

/* operations on double word entities, for scalar_product */

struct dword {
  union {
    SP_UWORD asuword;
    SP_WORD asword;
  } msw;
  union {
    SP_UWORD asuword;
    SP_WORD asword;
  } lsw;
};

typedef struct dword *Dword;

#define ASLONG(X) ((X)->lsw.asword)

#define ASLONGL(X) ((X)->lsw.asword)

#define ASULONG(X) ((X)->lsw.asuword)

#define ASUMSW(X) ((X)->msw.asuword)

#define issmall_d(x)								\
    (((x)->msw.asword==0  && (x)->lsw.asword>=0 && (x)->lsw.asword<HighInt) ||	\
     ((x)->msw.asword==-1 && (x)->lsw.asword<0  && (x)->lsw.asword>=-HighInt))	\


#define islong_d(X)					\
    (((X)->msw.asword==0  && (X)->lsw.asword>=0) ||	\
     ((X)->msw.asword==-1 && (X)->lsw.asword<0))	\


#define cmp_deqd(X,Y) (((X)->lsw.asword==(Y)->lsw.asword) && ((X)->msw.asword==(Y)->msw.asword))

#define cmp_dltd(x,y) (((x)->msw.asword==(y)->msw.asword) ? ((x)->lsw.asuword < (y)->lsw.asuword) : ((x)->msw.asword < (y)->msw.asword))

#define cmp_dgtd(x,y) (((x)->msw.asword==(y)->msw.asword) ? ((x)->lsw.asuword > (y)->lsw.asuword) : ((x)->msw.asword > (y)->msw.asword))

#define cmp_deqz(x) ((x)->msw.asword==0 && (x)->lsw.asword==0)

#define cmp_dnez(x) (!cmp_deqz(x))

#define cmp_dltz(x) ((x)->msw.asword<0)

#define cmp_dgez(x) (!cmp_dltz(x))

#define cmp_dlez(x) (cmp_dltz(x) || cmp_deqz(x))

#define cmp_dgtz(x) (!cmp_dlez(x))

#define add_zdd(X,Y) (*(Y) = *(X))

static void 
add_ztd(TAGGED x,Dword y)
{
  if (TagIsSmall(x)) {
    y->lsw.asword = GetSmall(x);
    y->msw.asword = (Tltz(x) ? -1 : 0);
  } else {
    TAGGED *s = TagToSTR(x);
    y->lsw.asuword = s[1];
    if (s[0]==BIGNUM_HEADER)
      y->msw.asword = ((SP_WORD)s[1]<0 ? -1 : 0);
    else
      y->msw.asuword = s[2];
  }
}

static void 
add_zld(long x,Dword y)
{
  y->lsw.asword = x;
  y->msw.asword = (x<0 ? -1 : 0);
}

static void 
add_ddd(Dword x,Dword y,Dword z)
{
  SP_UWORD lsw = x->lsw.asuword + y->lsw.asuword;
  SP_BOOL carry = (lsw < x->lsw.asuword);

  z->lsw.asuword = lsw;
  z->msw.asuword = x->msw.asuword + y->msw.asuword + carry;
}

static void 
sub_ddd(Dword x,Dword y,Dword z)
{
  SP_UWORD lsw = x->lsw.asuword - y->lsw.asuword;
  SP_BOOL borrow = (lsw > x->lsw.asuword);

  z->lsw.asuword = lsw;
  z->msw.asuword = x->msw.asuword - y->msw.asuword - borrow;
}

static void 
sub_dld(Dword x,long y,Dword z)
{
  SP_UWORD lsw = x->lsw.asuword - (SP_UWORD)y;
  SP_BOOL borrow = (lsw > x->lsw.asuword) - (y<0);

  z->lsw.asuword = lsw;
  z->msw.asuword = x->msw.asuword - borrow;
}

static void 
sub_zdd(Dword x,Dword y)
{
  SP_UWORD lsw;
  
  y->lsw.asuword = lsw = - x->lsw.asuword;
  y->msw.asuword = - x->msw.asuword;
  if (lsw!=0)
    y->msw.asuword--;
}

static void 
add_dud(Dword x,SP_UWORD y,Dword z)
{
  SP_UWORD lsw = x->lsw.asuword + y;
  int carry = (lsw < x->lsw.asuword);
  
  z->lsw.asuword = lsw;
  z->msw.asuword = x->msw.asuword + carry;
}

static void 
sub_dud(Dword x,SP_UWORD y,Dword z)
{
  SP_UWORD lsw = x->lsw.asuword - y;
  int carry = (lsw < x->lsw.asuword) - (y>0);
  
  z->lsw.asuword = lsw;
  z->msw.asuword = x->msw.asuword + carry;
}


static void 
mul_uud(SP_UWORD x,SP_UWORD y,Dword z)
{
  int carry = 0;
  SP_UWORD lsw;
  SP_UWORD x0 = LOWER_UHALF(x);
  SP_UWORD x1 = (UHALF)(x>>HALFSHIFT);
  SP_UWORD y0 = LOWER_UHALF(y);
  SP_UWORD y1 = (UHALF)(y>>HALFSHIFT);
  SP_UWORD p01 = (SP_UWORD)x0*(SP_UWORD)y1;
  SP_UWORD p10 = (SP_UWORD)x1*(SP_UWORD)y0;
  
  z->lsw.asuword = (SP_UWORD)x0*(SP_UWORD)y0;
  z->msw.asuword = (SP_UWORD)x1*(SP_UWORD)y1;
  lsw = z->lsw.asuword + (p01<<HALFSHIFT);
  if (lsw<z->lsw.asuword)
    carry++;
  z->lsw.asuword = lsw;
  lsw = z->lsw.asuword + (p10<<HALFSHIFT);
  if (lsw<z->lsw.asuword)
    carry++;
  z->lsw.asuword = lsw;
  z->msw.asuword += (p01>>HALFSHIFT) + (p10>>HALFSHIFT) + carry;
}

#define mul_ltd(X,Y,Z) mul_ltd_internal((X),(Y),(Z))

static void 
mul_ltd_internal MAGIC (HIDDEN_PROTO
			SP_WORD x,TAGGED ty,Dword z)
{
  if (ty==Inf) {
    z->msw.asword = -x;
    z->lsw.asword = 0;
  } else if (ty==Sup) {
    z->msw.asword = x;
    z->lsw.asword = 0;
  } else {
    SP_WORD y = GetSmall(ty);
  
    if (x>>HALFSHIFT==x>>(HALFSHIFT-1) &&
	y>>HALFSHIFT==y>>(HALFSHIFT-1)) {
      z->lsw.asword = x*y;
      z->msw.asword = (z->lsw.asword<0 ? -1 : 0);
    } else {
      int negated = 0;
      
      if (x<0)
	x = -x, negated ^= 1;
      if (y<0)
	y = -y, negated ^= 1;

      mul_uud(x,y,z);
      if (negated)
	sub_zdd(z,z);
      if (!islong_d(z))
	fd.fd_overflow = TRUE;
    }
  }
}

/* Roll-your-own division and remainder algorithms.
   Assume everything is unsigned.

   Consider X/Y, X = (A<<32)+B, |X|<=63, |Y|<=25.

   Let:    Q1 = (1<<32)/Y       |Q1|<=31
           R1 = (1<<32)%Y       |R1|<=25
	   Q2 = B/Y             |Q2|<=6
	   R2 = B%Y             |R2|<=25

   Case 1 (A=0): X/Y = Q2, X%Y = R2.

   Case 2 (A>0): X/Y = A*Q1 + Q2 + (A*R1 + R2)/Y
                 X%Y = (A*R1 + R2)%Y
*/

/* x is nonnegative! */
static void 
div_dud_internal(Dword x,SP_UWORD y,Dword z)
{
  struct dword dividend = *x;
  struct dword term;
  SP_UWORD q1, r1, q2, r2;

  if (y==1) {
    z->lsw.asuword = x->lsw.asuword;
    z->msw.asuword = x->msw.asuword;
  } else {
    z->lsw.asuword = 0;
    z->msw.asuword = 0;
    /* note: dividend is two times too small */
    q1 = (((SP_UWORD)((-1L)<<(WORDSIZE-1)))/y)<<1;
    r1 = (((SP_UWORD)((-1L)<<(WORDSIZE-1)))%y)<<1;
    if (r1>=y) {
      q1++;
      r1 -= y;
    }
    q2 = dividend.lsw.asuword/y;
    r2 = dividend.lsw.asuword%y;
    while (dividend.msw.asuword>0) {
      mul_uud(q1,dividend.msw.asuword,&term);
      add_ddd(z,&term,z);
      add_dud(z,q2,z);
      mul_uud(r1,dividend.msw.asuword,&dividend);
      add_dud(&dividend,r2,&dividend);
      q2 = dividend.lsw.asuword/y;
      r2 = dividend.lsw.asuword%y;
    }
    add_dud(z,q2,z);
  }
}

/* round towards zero */
static void 
div_dld(giant x,SP_WORD y,giant z)
{
  if (y==1) {
    add_zdd(x,z);
  } else {
    int negx=0, negy=0;
    if (cmp_dltz(x)) {
      negx=1;
      sub_zdd(x,x);
    }
    if (y<0) {
      negy=1;
      y = -y;
    }
    div_dud_internal(x,y,z);
    if (negx)
      sub_zdd(x,x);
    if (negx^negy)
      sub_zdd(z,z);
  }
}

static SP_WORD 
div_dll(giant x,SP_WORD y)
{
  let_giant(z);

  div_dld(x,y,z);
  return z->lsw.asword;
}

/* round towards -INF (floor) */
static void 
div_dldf(giant x,SP_WORD y,giant z)
{
  if (y==1) {
    add_zdd(x,z);
  } else {
    int negx=0, negy=0;
    if (cmp_dltz(x)) {
      negx=1;
      sub_zdd(x,x);
    }
    if (y<0) {
      negy=1;
      y = -y;
    }
    if (negx^negy)
      sub_dld(x,1-y,x);
    div_dud_internal(x,y,z);
    if (negx^negy)
      sub_dld(x,y-1,x);
    if (negx)
      sub_zdd(x,x);
    if (negx^negy)
      sub_zdd(z,z);
  }
}

/* round towards +INF (ceiling) */
static void 
div_dldc(giant x,SP_WORD y,giant z)
{
  if (y==1) {
    add_zdd(x,z);
  } else {
    int negx=0, negy=0;
    if (cmp_dltz(x)) {
      negx=1;
      sub_zdd(x,x);
    }
    if (y<0) {
      negy=1;
      y = -y;
    }
    if (negx==negy)
      sub_dld(x,1-y,x);
    div_dud_internal(x,y,z);
    if (negx==negy)
      sub_dld(x,y-1,x);
    if (negx)
      sub_zdd(x,x);
    if (negx^negy)
      sub_zdd(z,z);
  }
}

/* x is nonnegative! */
static SP_UWORD 
mod_duu_internal(Dword x,SP_UWORD y)
{
  struct dword dividend = *x;
  SP_UWORD r1, r2;

  if (y==1)
    return 0;
  /* note: dividend is two times too small */
  r1 = (((SP_UWORD)((-1L)<<(WORDSIZE-1)))%y)<<1;
  if (r1>=y) {
    r1 -= y;
  }
  r2 = dividend.lsw.asuword%y;
  while (dividend.msw.asuword>0) {
    mul_uud(r1,dividend.msw.asuword,&dividend);
    add_dud(&dividend,r2,&dividend);
    r2 = dividend.lsw.asuword%y;
  }
  return r2;
}

/* see adjust_bounds_gcd for x<0 case! */
static SP_UWORD 
mod_duu(giant x,SP_UWORD y)
{
  SP_UWORD m;

  if (cmp_dgez(x))
    return mod_duu_internal(x,y);
  sub_zdd(x,x);
  m = mod_duu_internal(x,y);
  sub_zdd(x,x);
  return (m==0 ? 0 : y-m);	/* modulo, not remainder */
}

#endif /* LogSizeOfWord==2 */

typedef long TERM;

struct linear_data {
  void (SPCDECL *destructor)(void *);
  void (SPCDECL *daemon)(HIDDEN_PROTO
			 Argtype,void *,SP_globref,TAGGED); /* (w,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  SP_globref refbase;
  SP_BOOL fast;
  int op;			/* 1(#>=), 2(#>=), 3(#=), 4(#\=) */
  long stamp;			/* increases up to backtracking */
  int units;			/* maintained incrementally */
  int nonunits;			/* maintained incrementally */
  int gcdall;			/* valid during GCD rules */
  field_giant(rhs);		/* RHS - valid up to backtracking */
  int prunings;			/* counter */
  int nvars;			/* #terms */
  int ntargets;			/* #terms that may be targets */
  int overflow;			/* set by the daemon */
  field_giant(bige);
  field_giant(bigf);
  TERM *heap;
  int *vheap;
  int heapsize;
  TERM *target;
  Dvar dvar;
  struct {
    mem_giant *cmin;		/* min(ai*xi) */
    mem_giant *cmax;		/* max(ai*xi) */
    mem_giant *interval;	/* cmax-cmin */
    long *coeff;		/* ai */
    long *abscoeff;		/* |ai| */
    long *gcd;			/* valid during GCD rules */
  } term;
};


#define SV(I)   (pdata->target[I])
#define COEFF(t) (pdata->term.coeff[t])
#define ABSCOEFF(t) (pdata->term.abscoeff[t])
#define GCD(t) (pdata->term.gcd[t])
#define DVAR(t) (pdata->dvar+(t))
#define RefAttr(T) (pdata->refbase + 2*(T))
#define RefVar(T) (pdata->refbase + 2*(T) + 1)
#define CMIN(t) ref_mem_giant(pdata->term.cmin+(t))
#define CMAX(t) ref_mem_giant(pdata->term.cmax+(t))
#define INTERVAL(t) ref_mem_giant(pdata->term.interval+(t))

static void SPCDECL linear_destructor(void *pdata_v)
{
  struct linear_data *pdata = (struct linear_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nvars<<1);
  SP_free(pdata);
}

#define SWAP(I,J)				\
{						\
  TERM vi = pdata->heap[I];			\
  TERM vj = pdata->heap[J];			\
  pdata->heap[I] = vj;				\
  pdata->heap[J] = vi;				\
  pdata->vheap[vi] = (J);			\
  pdata->vheap[vj] = (I);			\
}

static void 
spheapify(struct linear_data *pdata,
	  int i)
{
  TERM *heap = pdata->heap;
  mem_giant *key = pdata->term.interval;
  
  for (;;) {
    int l = (i<<1)+1;
    int topmost = i;
    if (l<pdata->heapsize && cmp_dgtd(ref_mem_giant(key+heap[l]),ref_mem_giant(key+heap[topmost])))
      topmost = l;
    if (l+1<pdata->heapsize && cmp_dgtd(ref_mem_giant(key+heap[l+1]),ref_mem_giant(key+heap[topmost])))
      topmost = l+1;
    if (topmost==i)
      break;
    SWAP(i,topmost);
    i = topmost;
  }
}

static void 
refresh_gcd(struct linear_data *pdata)
{
  int ntargets = pdata->ntargets;
  int units = pdata->units;
  int nonunits = pdata->nonunits;
  long g;
  int i;

  if (nonunits==0)
    pdata->gcdall = 1;
  else if (units>1) {
    pdata->gcdall = 1;
    for (i=ntargets-1; i>=0; i--)
      GCD(SV(i)) = 1;
  } else if (units==1) { /* GCD(i)=gcdall=1 except for one i */
    int ix=0;			/* avoid false alarm */
    pdata->gcdall = 1;
    g = -1;
    for (i=0; i<ntargets; i++) {
      TERM elt = SV(i);
      long ac = ABSCOEFF(elt);

      GCD(elt) = 1;
      if (cmp_deqz(INTERVAL(elt)))
	;
      else if (ac==1)
	ix = elt;
      else if (g== -1)
	g = ac;
      else
	g = gcd(g,ac);
    }
    GCD(ix) = g;
  } else {	/* for each i, compute GCD of all coefficients except i */
    int i0;
    for (i0=0; i0<ntargets && cmp_deqz(INTERVAL(SV(i0))); i0++)
      ;
    g = ABSCOEFF(SV(i0));
    for (i=i0+1; i<ntargets; i++) {
      TERM elt = SV(i);
      
      if (cmp_dgtz(INTERVAL(elt))) {
	GCD(elt) = g;
	g = gcd(g,ABSCOEFF(elt));
      }
    }
    pdata->gcdall = g;
				/* now, SV(i)->gcd = gcd(a[0] ... a[i-1]) */
    for (i=ntargets-1; i>=0 && cmp_deqz(INTERVAL(SV(i))); i--)
      ;
    g = ABSCOEFF(SV(i));
    for (i--; i>i0; i--) {
      TERM elt = SV(i);
      
      if (cmp_dgtz(INTERVAL(elt))) {
	GCD(elt) = gcd(g,GCD(elt));
	g = gcd(g,ABSCOEFF(elt));
      }
    }
    GCD(SV(i0)) = g;
				/* now, SV(i)->gcd = gcd({a[j] | i!=j}) */
  }
}

static void SPCDECL 
linear_daemon MAGIC (HIDDEN_PROTO
		     Argdecl,
		     void *vdata,
		     SP_globref attr_ref,
		     TAGGED global)
{
  struct linear_data *pdata = (struct linear_data *)vdata;
  TAGGED tstate;
  int ar, state_stamp;

  tstate = RefMutable(CTagToArg(global,1));
  ar = Arity(TagToHeadfunctor(tstate));
  state_stamp = GetSmall(CTagToArg(tstate,ar));
  if (pdata->stamp==state_stamp) { /* incremental */
    int j = (attr_ref - pdata->refbase)>>1;
    long c = COEFF(j);
    long ac = ABSCOEFF(j);
    Dvar dv = DVAR(j);
    let_giant(cminj);
    let_giant(cmaxj);
    TERM elt;

    dvar_init(dv, attr_ref, attr_ref+1);
    if (pdata->fast) {
      if (c>0) {
	add_zld(c*dvar_min_l(dv),cminj);
	add_zld(c*dvar_max_l(dv),cmaxj);
      } else {
	add_zld(c*dvar_min_l(dv),cmaxj);
	add_zld(c*dvar_max_l(dv),cminj);
      }
      if (ASLONG(cminj)==ASLONG(CMIN(j)) && ASLONG(cmaxj)==ASLONG(CMAX(j)))
	return;			/* self-invocation */
    } else {
      if (c>0) {
	mul_ltd(c,dvar_min_t(dv),cminj);
	mul_ltd(c,dvar_max_t(dv),cmaxj);
      } else {
	mul_ltd(c,dvar_max_t(dv),cminj);
	mul_ltd(c,dvar_min_t(dv),cmaxj);
      }
      if (cmp_deqd(cminj,CMIN(j)) && cmp_deqd(cmaxj,CMAX(j)))
	return;			/* self-invocation */
    }
    (void)daemon_copy_state(w,&global);
    pdata->stamp++;
    add_ddd(pdata->bigf,CMIN(j),pdata->bigf);
    sub_ddd(pdata->bige,CMAX(j),pdata->bige);
    add_zdd(cminj,CMIN(j));
    add_zdd(cmaxj,CMAX(j));
    sub_ddd(cmaxj,cminj,INTERVAL(j));
    sub_ddd(pdata->bigf,cminj,pdata->bigf);
    add_ddd(pdata->bige,cmaxj,pdata->bige);
    if (dvar_is_integer(dv)) {
      if (ac==1)
	pdata->units--;
      else
	pdata->nonunits--;
    }
    if (fd.fd_overflow) {
      pdata->overflow = TRUE;
      goto push;
    }
    if (pdata->units+pdata->nonunits==0)
      goto push;
    switch (pdata->op) {
    case 1:
      spheapify(pdata,pdata->vheap[j]);
      elt = pdata->heap[0];
      if (cmp_dltd(pdata->bigf,INTERVAL(elt)))
	goto push;
      return;
    case 2:
      spheapify(pdata,pdata->vheap[j]);
      elt = pdata->heap[0];
      if (cmp_dltd(pdata->bige,INTERVAL(elt)))
	goto push;
      return;
    case 3:
      spheapify(pdata,pdata->vheap[j]);
      elt = pdata->heap[0];
      if (cmp_dltd(pdata->bigf,INTERVAL(elt)) ||
	  cmp_dltd(pdata->bige,INTERVAL(elt)))
	goto push;
      return;
    case 4:
      if (pdata->units+pdata->nonunits <= 1)
	goto push;
      return;
    }
  }
 push:
  fd_enqueue_global(w, global, 0x5/* MINMAX, append*/);   
}

static SP_BOOL
scalar_product_init MAGIC (HIDDEN_PROTO
			   struct linear_data *pdata,
			   SP_BOOL incremental)
{
  int op = pdata->op;
  int i;
  /* Phase 1: compute:
     bigf = rhs - sum{min(a_i x_i)}
     bige = sum{max(a_i x_i)} - rhs

     I_i = max(a_i x_i) - min(a_i x_i)
  */
  if (incremental) {
    for (i=0; i<pdata->ntargets; i++) {
      int j = SV(i);
      Dvar dv = DVAR(j);
      dvar_init(dv, RefAttr(j), RefVar(j));
      if (dvar_is_integer(dv))
	sub_ddd(pdata->rhs,CMIN(j),pdata->rhs);
#if DBG
      {
	long c = COEFF(j);
	let_giant(cminj);
	let_giant(cmaxj);
	TAGGED mint = dvar_min_t(dv);
	TAGGED maxt = dvar_max_t(dv);
	if (c>0) {
	  mul_ltd(c,mint,cminj);
	  mul_ltd(c,maxt,cmaxj);
	  if ((TagIsSmall(mint) && !cmp_deqd(cminj,CMIN(j))) ||
	      (TagIsSmall(maxt) && !cmp_deqd(cmaxj,CMAX(j)))) {
	    fprintf(stderr, "! OUT OF SYNC\n");
	    return FALSE;
	  }
	} else {
	  mul_ltd(c,maxt,cminj);
	  mul_ltd(c,mint,cmaxj);
	  if ((TagIsSmall(maxt) && !cmp_deqd(cminj,CMIN(j))) ||
	      (TagIsSmall(mint) && !cmp_deqd(cmaxj,CMAX(j)))) {
	    fprintf(stderr, "! OUT OF SYNC\n");
	    return FALSE;
	  }
	}
      }
#endif
    }
  } else {
    pdata->fast = TRUE;
    add_zdd(pdata->rhs,pdata->bigf);
    sub_zdd(pdata->bigf,pdata->bige);
    pdata->units = 0;
    pdata->nonunits = 0;
    for (i=0; i<pdata->ntargets; i++) {
      int j = SV(i);
      long c = COEFF(j);
      long ac = ABSCOEFF(j);
      Dvar dv = DVAR(j);
      giant cminj = CMIN(j);
      giant cmaxj = CMAX(j);
      giant intervalj = INTERVAL(j);

      dvar_init(dv, RefAttr(j), RefVar(j));
      if (c>0) {
	mul_ltd(c,dvar_min_t(dv),cminj);
	mul_ltd(c,dvar_max_t(dv),cmaxj);
      } else {
	mul_ltd(c,dvar_max_t(dv),cminj);
	mul_ltd(c,dvar_min_t(dv),cmaxj);
      }
      sub_ddd(cmaxj,cminj,intervalj);
      store_giant(cminj,CMIN(j));
      store_giant(cmaxj,CMAX(j));
      store_giant(intervalj,INTERVAL(j));
      if (op<4) {
	pdata->heap[i] = j;
	pdata->vheap[j] = i;
      }
      sub_ddd(pdata->bigf,cminj,pdata->bigf);
      add_ddd(pdata->bige,cmaxj,pdata->bige);
      if (pdata->fast &&
	  !(islong_d(cminj) && islong_d(cmaxj) && islong_d(intervalj)))
	pdata->fast = FALSE;
      if (dvar_is_integer(dv)) {
	sub_ddd(pdata->rhs,cminj,pdata->rhs);
      } else if (ac==1)
	pdata->units++;
      else
	pdata->nonunits++;
    }
    pdata->heapsize = i;
    if (op<4)
      for (i=(pdata->heapsize-2)>>1; i>=0; i--)
	spheapify(pdata,i);
    if (pdata->fast &&
	!(islong_d(pdata->bige) && islong_d(pdata->bigf) && islong_d(pdata->rhs)))
      pdata->fast = FALSE;
  }
  return TRUE;
}


static SP_BOOL
scalar_product_le MAGIC (HIDDEN_PROTO
			 struct linear_data *pdata,
			 TERM elt)
{
  long c = COEFF(elt);
  Dvar dv = DVAR(elt);
  let_giant(decr);
  let_giant(cmax0);

  add_zdd(CMAX(elt),cmax0);

  /*
    Phase 2:

    For <=
    ******
    bigf>=0 is a necessary condition.
    bige<=0 is a sufficient condition.

    enforce:
    bigf >= I_i for all i

    rules:
    x_i <=  floor(F / a_i) + min(x_i)  if a_i>0
    x_i >= -floor(F /-a_i) + max(x_i)  if a_i<0

  */

  if (cmp_dltz(pdata->bigf))
    return FALSE;
  if (c>0) {
    let_giant(tmp);
    let_giant(ub);

    add_ddd(pdata->bigf,CMIN(elt),tmp); /* [MC] 3.11.3 */
    div_dldf(tmp,c,ub);
    if (islong_d(tmp) && issmall_d(ub)) {/* representable ub */
      if (dvar_fix_max_l(dv,ASLONG(ub))<0)
	return FALSE;
      mul_ltd(c,dvar_max_t(dv),CMAX(elt));
    } else if (cmp_dgtz(ub)) { /* ub overflow - can't prune the variable */
      long modc = mod_duu(pdata->bigf,c);
      
      add_ddd(pdata->bigf,CMIN(elt),CMAX(elt)); /* ensure bigf >= I_i */
      if (modc>0)
	sub_dud(CMAX(elt),modc,CMAX(elt));
    } else {			/* ub underflow - signal int overflow */
      fd.fd_overflow = TRUE;
      return FALSE;
    }
  } else {
    let_giant(tmp);
    let_giant(lb);

    add_ddd(pdata->bigf,CMIN(elt),tmp); /* [MC] 3.11.3 */
    div_dldc(tmp,c,lb);
    if (islong_d(tmp) && issmall_d(lb)) {/* representable lb */
      if (dvar_fix_min_l(dv,ASLONG(lb))<0)
	return FALSE;
      mul_ltd(c,dvar_min_t(dv),CMAX(elt));
    } else if (cmp_dltz(lb)) {	/* lb underflow - can't prune the variable */
      long modc = mod_duu(pdata->bigf,-c);
      
      add_ddd(pdata->bigf,CMIN(elt),CMAX(elt)); /* ensure bigf >= I_i */
      if (modc>0)
	sub_dud(CMAX(elt),modc,CMAX(elt));
    } else {			/* lb overflow - signal int overflow */
      fd.fd_overflow = TRUE;
      return FALSE;
    }
  }
  sub_ddd(cmax0,CMAX(elt),decr);
  sub_ddd(INTERVAL(elt),decr,INTERVAL(elt));
  sub_ddd(pdata->bige,decr,pdata->bige);
  spheapify(pdata,0);
  if (dvar_is_integer(dv)) {
    sub_ddd(pdata->rhs,CMAX(elt),pdata->rhs);
    if (ABSCOEFF(elt)==1)
      pdata->units--;
    else
      pdata->nonunits--;
  }
  return TRUE;
}


static SP_BOOL
scalar_product_le_fast(struct linear_data *pdata,
		       TERM elt)
{
  long c = COEFF(elt);
  Dvar dv = DVAR(elt);
  long decr;
  long cmax0 = ASLONG(CMAX(elt));
  long cmax;

  if (ASLONG(pdata->bigf)<0)
    return FALSE;
  if (c>0) {
    long ub = ASLONG(pdata->bigf)/c + dvar_min_l(dv);
    if (dvar_fix_max_l(dv,ub)<0)
      return FALSE;
    cmax = c*dvar_max_l(dv);
  } else {
    long lb = -(ASLONG(pdata->bigf)/(-c)) + dvar_max_l(dv);
    if (dvar_fix_min_l(dv,lb)<0)
      return FALSE;
    cmax = c*dvar_min_l(dv);
  }
  add_zld(cmax,CMAX(elt));
  decr = cmax0-cmax;
  ASLONGL(INTERVAL(elt)) -= decr;
  spheapify(pdata,0);
  sub_dld(pdata->bige,decr,pdata->bige);
  if (dvar_is_integer(dv)) {
    sub_dld(pdata->rhs,cmax,pdata->rhs);
    if (ABSCOEFF(elt)==1)
      pdata->units--;
    else
      pdata->nonunits--;
  }
  return TRUE;
}

static SP_BOOL
scalar_product_ge MAGIC(HIDDEN_PROTO
			struct linear_data *pdata,
			TERM elt)
{
  long c = COEFF(elt);
  Dvar dv = DVAR(elt);
  let_giant(decr);
  let_giant(cmin0);

  add_zdd(CMIN(elt),cmin0);
  
  /*
    Phase 2:

    For >=
    ******
    bige>=0 is a necessary condition.
    bigf<=0 is a sufficient condition.

    enforce:
    bige >= I_i for all i

    rules:
    x_i >= -floor(E / a_i) + max(x_i)  if a_i>0
    x_i <=  floor(E /-a_i) + min(x_i)  if a_i<0
  */

  if (cmp_dltz(pdata->bige))
    return FALSE;
  if (c>0) {
    let_giant(tmp);
    let_giant(lb);

    sub_ddd(pdata->bige,CMAX(elt),tmp); /* [MC] 3.11.3 */
    div_dldc(tmp,-c,lb);
    if (islong_d(tmp) && issmall_d(lb)) {/* representable lb */
      if (dvar_fix_min_l(dv,ASLONG(lb))<0)
	return FALSE;
      mul_ltd(c,dvar_min_t(dv),CMIN(elt));
    } else if (cmp_dltz(lb)) {	/* lb underflow - can't prune the variable */
      long modc = mod_duu(pdata->bige,c);
      
      sub_ddd(CMAX(elt),pdata->bige,CMIN(elt)); /* ensure bige >= I_i */
      if (modc>0)
	add_dud(CMIN(elt),modc,CMIN(elt));
    } else {			/* lb overflow - signal int overflow */
      fd.fd_overflow = TRUE;
      return FALSE;
    }
  } else {
    let_giant(tmp);
    let_giant(ub);

    sub_ddd(pdata->bige,CMAX(elt),tmp); /* [MC] 3.11.3 */
    div_dldf(tmp,-c,ub);
    if (islong_d(tmp) && issmall_d(ub)) {/* representable ub */
      if (dvar_fix_max_l(dv,ASLONG(ub))<0)
	return FALSE;
      mul_ltd(c,dvar_max_t(dv),CMIN(elt));
    } else if (cmp_dgtz(ub)) {	/* ub overflow - can't prune the variable */
      long modc = mod_duu(pdata->bige,-c);
      
      sub_ddd(CMAX(elt),pdata->bige,CMIN(elt)); /* ensure bige >= I_i */
      if (modc>0)
	add_dud(CMIN(elt),modc,CMIN(elt));
    } else {			/* ub underflow - signal int overflow */
      fd.fd_overflow = TRUE;
      return FALSE;
    }
  }
  sub_ddd(CMIN(elt),cmin0,decr);
  sub_ddd(INTERVAL(elt),decr,INTERVAL(elt));
  sub_ddd(pdata->bigf,decr,pdata->bigf);
  spheapify(pdata,0);
  if (dvar_is_integer(dv)) {
    sub_ddd(pdata->rhs,CMIN(elt),pdata->rhs);
    if (ABSCOEFF(elt)==1)
      pdata->units--;
    else
      pdata->nonunits--;
  }
  return TRUE;
}

static SP_BOOL
scalar_product_ge_fast(struct linear_data *pdata,
		       TERM elt)
{
  long c = COEFF(elt);
  Dvar dv = DVAR(elt);
  long decr;
  long cmin0 = ASLONG(CMIN(elt));
  long cmin;

  if (ASLONG(pdata->bige)<0)
    return FALSE;
  if (c>0) {
    long lb = -(ASLONG(pdata->bige)/c) + dvar_max_l(dv);
    if (dvar_fix_min_l(dv,lb)<0)
      return FALSE;
    cmin = c*dvar_min_l(dv);
  } else {
    long ub = ASLONG(pdata->bige)/(-c) + dvar_min_l(dv);
    if (dvar_fix_max_l(dv,ub)<0)
      return FALSE;
    cmin = c*dvar_max_l(dv);
  }
  add_zld(cmin,CMIN(elt));
  decr = cmin-cmin0;
  ASLONGL(INTERVAL(elt)) -= decr;
  spheapify(pdata,0);
  sub_dld(pdata->bigf,decr,pdata->bigf);
  if (dvar_is_integer(dv)) {
    sub_dld(pdata->rhs,cmin,pdata->rhs);
    if (ABSCOEFF(elt)==1)
      pdata->units--;
    else
      pdata->nonunits--;
  }
  return TRUE;
}

/* Preconditions:
   0<coeff<mod, 0=<rhs<mod, 0<mod

   Solve min X such that coeff*X = rhs (modulo mod)
*/
static long solve_gcd(long coeff, long rhs, long mod)
{
  if (rhs==0)
    return 0;
  else {
    long rhs1 = rhs%coeff;
    if (rhs1==0)
      return rhs/coeff;
    else
      return (rhs + mod*solve_gcd(mod%coeff, coeff-rhs1, coeff)) / coeff;
  }
}


/* Preconditions: 
   0<mod, gcdall = gcd(coeff,mod)

   Adjusts minx up and maxx down s.t.
   Returns smallest s.t. coeff*minx = coeff*maxx = rhs (modulo mod)
*/
static void adjust_bounds_gcd(long coeff, giant rhs,
			      long mod, long gcdall,
			      long *minx, long *maxx)
{
  long minx0 = *minx, maxx0 = *maxx;
  long q = mod/gcdall;
  long r, x, s;
  let_giant(rhslocal);

  if (coeff>0) {
    add_zdd(rhs,rhslocal);
  } else {
    sub_zdd(rhs,rhslocal);
    coeff = -coeff;
  }
  coeff = coeff % mod;
  s = solve_gcd(coeff, mod_duu(rhslocal,mod), mod);
  r = minx0 % q;
  if (r<0)
    r += q;
  x = minx0 - r + s;
  if (x<minx0)
    x += q;
  *minx = x;
  r = maxx0 % q;
  if (r<0)
    r += q;
  x = maxx0 - r + s;
  if (x>maxx0)
    x -= q;
  *maxx = x;
}

/*
  '$fd_linear'(+State0, -State, -Actions).
  State = state(CX,Op,RHS,Nground,Handle,Stamp) where CX are all non-ground
  Op in [1,2,3,4] means [#=<,#>=,#=,#\=].
*/
void SPCDECL
prolog_fd_linear MAGIC (HIDDEN_PROTO
			SP_term_ref State0,
			SP_term_ref State,
			SP_term_ref Actions)
{
  WAMENV;
  TAGGED tvec, telt, handle, t1;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  int nvars, i;
  long state_stamp;
  int op, ent, total_size;
  int nonground = 0;
  char *ptr;
  struct linear_data *pdata;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;
/*    X(0) = RefTerm(State0); */
  dvar_export_start(w);
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct linear_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get CX0 */
    nvars = list_length(tvec);	/* count terms */
    total_size = 
      nvars*sizeof(struct dvar) +
      3*nvars*sizeof(mem_giant) +
      3*nvars*sizeof(long) +
      2*nvars*sizeof(TERM) +
      nvars*sizeof(int);
  
    pdata = Palloc(struct linear_data, total_size, handle);
    init_giant(pdata->rhs);
    init_giant(pdata->bige);
    init_giant(pdata->bigf);
    ptr = (char *)(pdata+1);
    pdata->term.cmin = (mem_giant *)ptr;
    ptr += nvars*sizeof(mem_giant);
    pdata->term.cmax = (mem_giant *)ptr;
    ptr += nvars*sizeof(mem_giant);
    pdata->term.interval = (mem_giant *)ptr;
    ptr += nvars*sizeof(mem_giant);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->term.coeff = (long *)ptr;
    ptr += nvars*sizeof(long);
    pdata->term.abscoeff = (long *)ptr;
    ptr += nvars*sizeof(long);
    pdata->term.gcd = (long *)ptr;
    ptr += nvars*sizeof(long);
    pdata->target = (TERM *)ptr;
    ptr += nvars*sizeof(TERM);
    pdata->heap = (TERM *)ptr;
    ptr += nvars*sizeof(TERM);
    pdata->vheap = (int *)ptr;
    ptr += nvars*sizeof(int);
#if DBG
    if (ptr != (char *)(pdata+1)+total_size)
      printf("SHOULDN'T HAPPEN: expected ptr=%p, got %p\n",
	     (char *)(pdata+1)+total_size, ptr);
#endif
    pdata->destructor = linear_destructor;
    pdata->daemon = linear_daemon;
    pdata->overflow = FALSE;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    pdata->stamp = state_stamp-1; /* don't trust initially */
    DerefArg(telt,X(0),2);
    pdata->op = op = GetSmall(telt);
    pdata->nvars = nvars;
    pdata->ntargets = nvars;
    DerefArg(tvec,X(0),1);	/* get CX0 */
    DerefArg(telt,X(0),3);	/* get RHS */
    add_ztd(telt,pdata->rhs);
				/* xfer all info to the struct linear_terms */
    for (i=0; i<nvars; i++) {
      TERM elt = i;
      long c;
      
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(t1,telt,1);
      COEFF(elt) = c = GetSmall(t1);
      ABSCOEFF(elt) = ABS(c);
      get_var_and_attr(telt+WD(1),RefAttr(elt));
      SV(i) = elt;
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    for (i=0; i<pdata->ntargets; i++) {
      int j = SV(i);
      Dvar dv = DVAR(j);
      TAGGED functor;
      dvar_init(dv, RefAttr(j), RefVar(j));
      switch (op) {
      case 4:
	functor = fd.functor_val;
	break;
      default:
	functor = fd.functor_minmax;
	break;
      }
      dvar_attach_daemon(w, dv, pdata, X(1), functor);
    }
  }

				/* RESUME HERE */
  fd.fd_overflow = pdata->overflow;
  if (state_stamp != pdata->stamp ||
      !scalar_product_init(pdata,TRUE)) {
    DerefArg(telt,X(0),3);
    add_ztd(telt,pdata->rhs);
    DerefArg(telt,X(0),4);
    pdata->ntargets = nvars-GetSmall(telt);
    scalar_product_init(pdata,FALSE);
  }
  pdata->stamp = state_stamp+1;
  op = pdata->op;
  ent = -1;
				/* (dis)entailment tests based on GCD */
  switch (op) {
  case 3:
    refresh_gcd(pdata);	/* compute gcdall, GCD() */
    if (pdata->gcdall>1 && mod_duu(pdata->rhs,pdata->gcdall)!=0)
      goto ret;
    break;
  case 4:
    refresh_gcd(pdata);	/* compute gcdall, GCD() */
    if (pdata->gcdall>1 && mod_duu(pdata->rhs,pdata->gcdall)!=0) {
      ent = 1;
      goto ret;
    }
    break;
  }
 loop:
  switch (op) {
  case 1:			/* #=< */
    if (pdata->heapsize>0) {
      TERM elt = pdata->heap[0]; 
      if (cmp_dltd(pdata->bigf,INTERVAL(elt))) {
	if (pdata->fast ?
	    !scalar_product_le_fast(pdata,elt) :
	    !scalar_product_le(pdata,elt))
	  goto ret;
	goto loop;
      }
    }
    break;
  case 2:			/* #>= */
    if (pdata->heapsize>0) {
      TERM elt = pdata->heap[0]; 
      if (cmp_dltd(pdata->bige,INTERVAL(elt))) {
	if (pdata->fast ?
	    !scalar_product_ge_fast(pdata,elt) :
	    !scalar_product_ge(pdata,elt))
	  goto ret;
	goto loop;
      }
    }
    break;
  case 3:			/* #= */
    if (pdata->heapsize>0) {
      TERM elt = pdata->heap[0]; 
      if (cmp_dltd(pdata->bigf,INTERVAL(elt))) {
	if (pdata->fast ?
	    !scalar_product_le_fast(pdata,elt) :
	    !scalar_product_le(pdata,elt))
	  goto ret;
	goto loop;
      }
      if (cmp_dltd(pdata->bige,INTERVAL(elt))) {
	if (pdata->fast ?
	    !scalar_product_ge_fast(pdata,elt) :
	    !scalar_product_ge(pdata,elt))
	  goto ret;
	goto loop;
      }
      refresh_gcd(pdata);	/* compute gcdall, GCD() */
      if (mod_duu(pdata->rhs,pdata->gcdall) != 0)
	goto ret;
      if (pdata->units<=1)
	for (i=0; i<pdata->ntargets; i++) {
	  TERM elt = SV(i);
	  Dvar dv = DVAR(elt);
	  TAGGED tmin = dvar_min_t(dv);
	  TAGGED tmax = dvar_max_t(dv);

	  if (AreSmall(tmin,tmax) && tmin!=tmax && GCD(elt)>pdata->gcdall) {
	    /* Ensure that:

	       Ai * min(Xi) - RHS = Ai * max(Xi) - RHS = 0 (modulo G)

	       where G is the gcd of all coefficients excepts Ai.
	       Fails if an empty domain is produced.
	    */
	    long imin = GetSmall(tmin);
	    long imax = GetSmall(tmax);
	    int rc;

	    adjust_bounds_gcd(COEFF(elt),
				  pdata->rhs,
				  GCD(elt),
				  pdata->gcdall,
				  &imin, &imax);
	    rc = dvar_fix_interval_l(dv, imin, imax);
	    if (rc<0)
	      goto ret;
	    if (rc>0) {
	      let_giant(cmin0);
	      let_giant(cmax0);
	      let_giant(decr);
	      long c = COEFF(elt);

	      add_zdd(CMIN(elt),cmin0);
	      add_zdd(CMAX(elt),cmax0);

	      if (c>0) {
		mul_ltd(c,dvar_min_t(dv),CMIN(elt));
		mul_ltd(c,dvar_max_t(dv),CMAX(elt));
	      } else {
		mul_ltd(c,dvar_max_t(dv),CMIN(elt));
		mul_ltd(c,dvar_min_t(dv),CMAX(elt));
	      }
	      sub_ddd(CMAX(elt),CMIN(elt),INTERVAL(elt));
	      sub_ddd(cmax0,CMAX(elt),decr);
	      sub_ddd(pdata->bige,decr,pdata->bige);
	      sub_ddd(CMIN(elt),cmin0,decr);
	      sub_ddd(pdata->bigf,decr,pdata->bigf);
	      spheapify(pdata,pdata->vheap[elt]);
	      if (dvar_is_integer(dv)) {
		sub_ddd(pdata->rhs,CMIN(elt),pdata->rhs);
		if (ABSCOEFF(elt)==1)
		  pdata->units--;
		else
		  pdata->nonunits--;
	      }
	      goto loop;
	    }
	  }
	}
    }
    break;
  case 4:			/* #\= */
    nonground = pdata->units+pdata->nonunits;
    if (nonground==0 && cmp_deqz(pdata->rhs))
      goto ret;
    else if (nonground==1) {
      for (i=0; cmp_deqz(INTERVAL(SV(i))); i++)
	;
      {
	TERM elt = SV(i);
	Dvar dv = DVAR(elt);

	if (mod_duu(pdata->rhs,ABSCOEFF(elt))==0) { /* RHS a multiple of coefficient */
	  dvar_prune_value_l(dv,div_dll(pdata->rhs,COEFF(elt)),w);
	}
      }
    }
    break;
  }
  switch (op) {
  case 1:
    ent = (cmp_dltz(pdata->bigf) ? -1 : cmp_dlez(pdata->bige));
    break;
  case 2:
    ent = (cmp_dltz(pdata->bige) ? -1 : cmp_dlez(pdata->bigf));
    break;
  case 3:
    ent = (cmp_dltz(pdata->bigf) ? -1 :
	   cmp_dltz(pdata->bige) ? -1 : 
	   cmp_dgtz(pdata->bigf) ? 0 :
	   cmp_dgtz(pdata->bige) ? 0 : 1);
    break;
  case 4:
    ent = (nonground<=1);
    break;
  }

  if (ent>=0) {
    int inf = 0;
    int sup = pdata->ntargets-1;
    TERM held = SV(sup); /* sup is the hole */
    TERM current = SV(inf);
    
    while (inf<=sup) {
      Dvar dv = DVAR(current);
      
      dvar_pruning_done(w,dv);
      dvar_export(w,dv);
      if (!dvar_is_integer(dv)) {
	SV(inf) = current;
	inf++;
	current = (inf>=sup ? held : SV(inf));
      } else {
	SV(sup) = current;
	sup--;
	current = (inf>=sup ? held : SV(sup));
      }
    }
    pdata->ntargets = inf;
  }
  if (ent==0 && op==3 && pdata->ntargets==2 && cmp_deqz(pdata->rhs)) {
    TERM elt0 = SV(0);
    TERM elt1 = SV(1);

    if (COEFF(elt0) == -COEFF(elt1)) {
      dvar_export_equal(w,DVAR(elt0), DVAR(elt1));
      ent = 1;
    }
  }
  if (islong_d(pdata->rhs))
    CTagToArg(X(0),3) = MakeInteger(ASLONG(pdata->rhs));
  else {			/* make a 64-bit bignum */
    TAGGED *h;
    RequireHeap(4,EVAL_ARITY);
    h = w->global_top;
    h[0] = BIGNUM_HEADER-LStep(1);
    h[1] = ASULONG(pdata->rhs);
    h[2] = ASUMSW(pdata->rhs);
    h[3] = BIGNUM_HEADER-LStep(1);
    w->global_top = h+4;
    CTagToArg(X(0),3) = MakeStructure(h);
  }
  CTagToArg(X(0),4) = MakeSmall(nvars-pdata->ntargets);
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
  if (!fd_check_overflow(CTagToArg(X(1),2)))
    SP_fail();
}


struct arith_data {
  void (SPCDECL *destructor)(void *);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  Dvar dvar;
  int nrefs;
};

#define xmin dvar_min_t(dvx)
#define xmax dvar_max_t(dvx)
#define ymin dvar_min_t(dvy)
#define ymax dvar_max_t(dvy)
#define zmin dvar_min_t(dvz)
#define zmax dvar_max_t(dvz)

static void SPCDECL arith_destructor(void *pdata_v)
{
  struct arith_data *pdata = (struct arith_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

#define CEILSQRT \
  { \
    long m = GetSmall(ymin); \
    if (m>=3) \
      {			/* m = ceiling(sqrt(m)) */ \
	long n1 = m>>1, n2 = m; \
	while (n1<m) \
	  m = n1, \
	  n1 = (n1 + n2/n1)>>1; \
	if (m<n1 || m*m < n2) \
	  m++; \
      } \
    sqrt_ymin = MakeSmall(m); \
    ymin_align = MakeSmall(m*m); \
  }

#define FLOORSQRT \
    { \
      long n = GetSmall(ymax); \
      if (n>=2) \
	{			/* n = floor(sqrt(n)) */ \
	  long n1 = n>>1, n2 = n; \
 \
	  while (n1<n) \
	    n = n1, \
	    n1 = (n1 + n2/n1)>>1; \
	} \
      sqrt_ymax = MakeSmall(n); \
    }

/*
  '$fd_square'(State0, State, -Actions).
  State = state(X, XMut, Y, YMut)
*/
void SPCDECL
prolog_fd_square MAGIC (HIDDEN_PROTO
			SP_term_ref State,
			SP_term_ref NewState,
			SP_term_ref Actions)
{
  WAMENV;
  Dvar dvx, dvy;
  TAGGED newmin=0, newmax=0;
  TAGGED sqrt_ymin, sqrt_ymax, ymin_align;
  TAGGED handle;
  SP_BOOL committed;
  struct arith_data *pdata;
  int ent = -1;			/* disentailed unless otherwise */

  (void)State;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;
  dvar_export_start(w);
  RefTerm(NewState) = static_output_state(w,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct arith_data,handle);
  } else {			/* build persistent state */
    pdata = Palloc(struct arith_data, 2*sizeof(struct dvar), handle);
    pdata->destructor = arith_destructor;
    pdata->nrefs = 4;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(4);
    pdata->dvar = (Dvar)(pdata+1);
    get_var_and_attr(X(0),pdata->refbase);
    get_var_and_attr(X(0)+WD(2),pdata->refbase+2);
  }

  /* RESUME HERE */
  dvx = pdata->dvar;
  dvy = dvx+1;
  dvar_init(dvx, pdata->refbase,   pdata->refbase+1);
  dvar_init(dvy, pdata->refbase+2, pdata->refbase+3);
  sqrt_ymin = ymin;
  sqrt_ymax = ymax;
    
  CEILSQRT;
  if (TagIsSmall(ymax))
    FLOORSQRT;

  switch ((!TagIsSmall(xmin) ? 0x0 : Tltz(xmin) ? 0x4 : 0x8)+
	  (!TagIsSmall(xmax) ? 0x0 : Tltz(xmax) ? 0x1 : 0x2)) {
  case 0x0:		/* inf..sup */
  case 0x2:		/* inf..+b */
  case 0x4:		/*  -a..sup */
    newmin = ymin;
    newmax = ymax;
    break;
  case 0x1:		/* inf..-b => b^2..sup */
    newmin = safe_mul_val(xmax,xmax);
    newmax = ymax;
    break;
  case 0x5:		/*  -a..-b => b^2..a^2 */
    newmin = safe_mul_val(xmax,xmax);
    newmax = safe_mul_val(xmin,xmin);
    break;
  case 0x6:		/*  -a..b => 0..max(a^2,b^2) */
    if (Tgt(Tminus(xmin),xmax))
      newmax = safe_mul_val(xmin,xmin);
    else
      newmax = safe_mul_val(xmax,xmax);
    break;
  case 0x8:		/* a..sup => a^2..sup */
    newmin = safe_mul_val(xmin,xmin);
    newmax = ymax;
    break;
  case 0xa:		/*  a..b => a^2..b^2 */
    newmin = safe_mul_val(xmin,xmin);
    newmax = safe_mul_val(xmax,xmax);
    break;
  }
  if (FDgt(ymin_align,newmin))
    newmin = ymin_align;
  if (dvar_fix_interval_t(dvy, newmin, newmax)<0)
    goto fail_or_oflo;

  newmin = NegateUB(sqrt_ymax);
  newmax = NegateLB(sqrt_ymin);
  if (dvar_compare_interval_t(dvx, newmin, newmax,w)==FDI_DISJOINT) {
    newmin = sqrt_ymin;
    newmax = sqrt_ymax;
  } else if (dvar_compare_interval_t(dvx, sqrt_ymin, sqrt_ymax,w)!=FDI_DISJOINT) {
    newmax = sqrt_ymax;
  }
  if (dvar_fix_interval_t(dvx, newmin, newmax)<0)
    goto fail_or_oflo;
  
  ent = dvar_is_integer_first(dvx);
  
  dvar_export(w,dvx);
  dvar_export(w,dvy);
  goto ret;
  
fail_or_oflo:
  if (newmin==Sup || newmax==Inf) /* can't represent bounds */
    fd.fd_overflow = TRUE;
 ret:
  if (!fd_check_overflow(CTagToArg(X(1),2)))
    SP_fail();
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}


static SP_BOOL 
is_supported_divider(long d,
		     TAGGED prod_min,
		     TAGGED prod_max,
		     TAGGED fact_min,
		     TAGGED fact_max)
{
  long pmin = GetSmall(prod_min);
  long pmax = GetSmall(prod_max);
  int sign = 0;

  if (pmin<0) {
    sign++;
    pmax = -GetSmall(prod_min);
    pmin = -GetSmall(prod_max);
  }
  if (d<0) {
    sign++;
    d = -d;
  }
  if ((pmin-1)/d==pmax/d)
    return FALSE;
  pmin = (pmin-1)/d+1;
  pmax /= d;
  if (sign!=1) {
    prod_min = MakeSmall(pmin);
    prod_max = MakeSmall(pmax);
  } else {
    prod_max = MakeSmall(-pmin);
    prod_min = MakeSmall(-pmax);
  }
  if (fd_compare_intervals(prod_min,prod_max,fact_min,fact_max)==FDI_DISJOINT)
    return FALSE;
  return TRUE;
}

#define ARITH_DISPATCH(xmin,xmax,ymin,ymax)			\
         ((!TagIsSmall(xmin) ? 0x00 :				\
	   Tltz(xmin) ? 0x40 : Teqz(xmin) ? 0x80 : 0xc0)+	\
	  (!TagIsSmall(xmax) ? 0x30 :				\
	   Tltz(xmax) ? 0x00 : Teqz(xmax) ? 0x10 : 0x20)+	\
	  (!TagIsSmall(ymin) ? 0x00 :				\
	   Tltz(ymin) ? 0x04 : Teqz(ymin) ? 0x08 : 0x0c)+	\
	  (!TagIsSmall(ymax) ? 0x03 :				\
	   Tltz(ymax) ? 0x00 : Teqz(ymax) ? 0x01 : 0x02))	\

/* -1=overflow, 0=fail, 2=no pruning, 1=pruning */
static int fd_product_inverse_dispatch(Dvar dvx,
				       Dvar dvy,
				       Dvar dvz)
{
  TAGGED newmin, newmax;
  int x_is_mon=0;
  
  switch (ARITH_DISPATCH(xmin,xmax,ymin,ymax)) {
				/* automatically generated table! */
  case 0x90: /* 0-0 / inf- -(d) => 0-0 */
  case 0x94: /* 0-0 / -(c)- -(d) => 0-0 */
  case 0x9e: /* 0-0 / +(c)- +(d) => 0-0 */
  case 0x9f: /* 0-0 / +(c)-sup => 0-0 */
    newmin = TaggedZero;
    newmax = TaggedZero;
    break;
  case 0xeb: /* +(a)- +(b) / 0-sup => 1-b */
    x_is_mon = 1;
    newmin = TaggedOne;
    newmax = xmax;
    break;
  case 0x0: /* inf- -(b) / inf- -(d) => 1-sup */
  case 0x1: /* inf- -(b) / inf-0 => 1-sup */
  case 0xfb: /* +(a)-sup / 0-sup => 1-sup */
  case 0xff: /* +(a)-sup / +(c)-sup => 1-sup */
    newmin = TaggedOne;
    newmax = zmax;
    break;
  case 0x10: /* inf-0 / inf- -(d) => 0-sup */
  case 0x14: /* inf-0 / -(c)- -(d) => 0-sup */
  case 0xbe: /* 0-sup / +(c)- +(d) => 0-sup */
  case 0xbf: /* 0-sup / +(c)-sup => 0-sup */
    newmin = TaggedZero;
    newmax = zmax;
    break;
  case 0x41: /* -(a)- -(b) / inf-0 => 1- -(a) */
    x_is_mon = 1;
    newmin = TaggedOne;
    newmax = Tminus(xmin);
    break;
  case 0x40: /* -(a)- -(b) / inf- -(d) => 1-a/d */
    x_is_mon = 1;
    newmin = TaggedOne;
    newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymax)));
    break;
  case 0x50: /* -(a)-0 / inf- -(d) => 0-a/d */
  case 0x54: /* -(a)-0 / -(c)- -(d) => 0-a/d */
    newmin = TaggedZero;
    newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymax)));
    break;
  case 0xae: /* 0- +(b) / +(c)- +(d) => 0-b/c */
  case 0xaf: /* 0- +(b) / +(c)-sup => 0-b/c */
    newmin = TaggedZero;
    newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymin)));
    break;
  case 0xef: /* +(a)- +(b) / +(c)-sup => 1-b/c */
    x_is_mon = 1;
    newmin = TaggedOne;
    newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymin)));
    break;
  case 0x9: /* inf- -(b) / 0-0 => 1-0 */
  case 0x49: /* -(a)- -(b) / 0-0 => 1-0 */
  case 0xe9: /* +(a)- +(b) / 0-0 => 1-0 */
  case 0xf9: /* +(a)-sup / 0-0 => 1-0 */
    return 0;
  case 0x4b: /* -(a)- -(b) / 0-sup => a- -1 */
    x_is_mon = 1;
    newmin = xmin;
    newmax = TaggedMinusOne;
    break;
  case 0x42: /* -(a)- -(b) / inf- +(d) => a- -(a) */
  case 0x43: /* -(a)- -(b) / inf-sup => a- -(a) */
  case 0x46: /* -(a)- -(b) / -(c)- +(d) => a- -(a) */
  case 0x47: /* -(a)- -(b) / -(c)-sup => a- -(a) */
    x_is_mon = 1;
    newmin = xmin;
    newmax = Tminus(xmin);
    break;
  case 0x4a: /* -(a)- -(b) / 0- +(d) => a-b/d */
    x_is_mon = 1;
    newmin = xmin;
    newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymax)));
    break;
  case 0xf: /* inf- -(b) / +(c)-sup => inf- -1 */
  case 0xf0: /* +(a)-sup / inf- -(d) => inf- -1 */
    newmin = zmin;
    newmax = TaggedMinusOne;
    break;
  case 0xb: /* inf- -(b) / 0-sup => inf- -1 */
  case 0xf1: /* +(a)-sup / inf-0 => inf- -1 */
    newmin = zmin;
    newmax = TaggedMinusOne;
    break;
  case 0x1e: /* inf-0 / +(c)- +(d) => inf-0 */
  case 0x1f: /* inf-0 / +(c)-sup => inf-0 */
  case 0xb0: /* 0-sup / inf- -(d) => inf-0 */
  case 0xb4: /* 0-sup / -(c)- -(d) => inf-0 */
    newmin = zmin;
    newmax = TaggedZero;
    break;
  case 0xf4: /* +(a)-sup / -(c)- -(d) => inf-a/c */
  case 0xf5: /* +(a)-sup / -(c)-0 => inf-a/c */
    newmin = zmin;
    newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymin)));
    break;
  case 0x70: /* -(a)-sup / inf- -(d) => inf-a/d */
  case 0x74: /* -(a)-sup / -(c)- -(d) => inf-a/d */
    newmin = zmin;
    newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymax)));
    break;
  case 0x2e: /* inf- +(b) / +(c)- +(d) => inf-b/c */
  case 0x2f: /* inf- +(b) / +(c)-sup => inf-b/c */
    newmin = zmin;
    newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymin)));
    break;
  case 0xa: /* inf- -(b) / 0- +(d) => inf-b/d */
  case 0xe: /* inf- -(b) / +(c)- +(d) => inf-b/d */
    newmin = zmin;
    newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymax)));
    break;
  case 0xe1: /* +(a)- +(b) / inf-0 => -(b)- -1 */
    x_is_mon = 1;
    newmin = Tminus(xmax);
    newmax = TaggedMinusOne;
    break;
  case 0xe2: /* +(a)- +(b) / inf- +(d) => -(b)-b */
  case 0xe3: /* +(a)- +(b) / inf-sup => -(b)-b */
  case 0xe6: /* +(a)- +(b) / -(c)- +(d) => -(b)-b */
  case 0xe7: /* +(a)- +(b) / -(c)-sup => -(b)-b */
    x_is_mon = 1;
    newmin = Tminus(xmax);
    newmax = xmax;
    break;
  case 0xe5: /* +(a)- +(b) / -(c)-0 => -(b)-a/c */
    x_is_mon = 1;
    newmin = Tminus(xmax);
    newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymin)));
    break;
  case 0x4f: /* -(a)- -(b) / +(c)-sup => a/c- -1 */
    x_is_mon = 1;
    newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymin)));
    newmax = TaggedMinusOne;
    break;
  case 0x5e: /* -(a)-0 / +(c)- +(d) => a/c-0 */
  case 0x5f: /* -(a)-0 / +(c)-sup => a/c-0 */
    newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymin)));
    newmax = TaggedZero;
    break;
  case 0x7e: /* -(a)-sup / +(c)- +(d) => a/c-sup */
  case 0x7f: /* -(a)-sup / +(c)-sup => a/c-sup */
    newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymin)));
    newmax = zmax;
    break;
  case 0x6e: /* -(a)- +(b) / +(c)- +(d) => a/c-b/c */
  case 0x6f: /* -(a)- +(b) / +(c)-sup => a/c-b/c */
    newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymin)));
    newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymin)));
    break;
  case 0x4e: /* -(a)- -(b) / +(c)- +(d) => a/c-b/d */
    x_is_mon = 1;
    newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymin)));
    newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymax)));
    break;
  case 0xea: /* +(a)- +(b) / 0- +(d) => a/d-b */
    x_is_mon = 1;
    newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymax)));
    newmax = xmax;
    break;
  case 0xfa: /* +(a)-sup / 0- +(d) => a/d-sup */
  case 0xfe: /* +(a)-sup / +(c)- +(d) => a/d-sup */
    newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymax)));
    newmax = zmax;
    break;
  case 0xee: /* +(a)- +(b) / +(c)- +(d) => a/d-b/c */
    x_is_mon = 1;
    newmin = MakeSmall(CEILDIV(GetSmall(xmin),GetSmall(ymax)));
    newmax = MakeSmall(FLOORDIV(GetSmall(xmax),GetSmall(ymin)));
    break;
  case 0x4: /* inf- -(b) / -(c)- -(d) => b/c-sup */
  case 0x5: /* inf- -(b) / -(c)-0 => b/c-sup */
    newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymin)));
    newmax = zmax;
    break;
  case 0x45: /* -(a)- -(b) / -(c)-0 => b/c- -(a) */
    x_is_mon = 1;
    newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymin)));
    newmax = Tminus(xmin);
    break;
  case 0x44: /* -(a)- -(b) / -(c)- -(d) => b/c-a/d */
    x_is_mon = 1;
    newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymin)));
    newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymax)));
    break;
  case 0xa0: /* 0- +(b) / inf- -(d) => b/d-0 */
  case 0xa4: /* 0- +(b) / -(c)- -(d) => b/d-0 */
    newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymax)));
    newmax = TaggedZero;
    break;
  case 0xe0: /* +(a)- +(b) / inf- -(d) => b/d- -1 */
    x_is_mon = 1;
    newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymax)));
    newmax = TaggedMinusOne;
    break;
  case 0x20: /* inf- +(b) / inf- -(d) => b/d-sup */
  case 0x24: /* inf- +(b) / -(c)- -(d) => b/d-sup */
    newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymax)));
    newmax = zmax;
    break;
  case 0xe4: /* +(a)- +(b) / -(c)- -(d) => b/d-a/c */
    x_is_mon = 1;
    newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymax)));
    newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymin)));
    break;
  case 0x60: /* -(a)- +(b) / inf- -(d) => b/d-a/d */
  case 0x64: /* -(a)- +(b) / -(c)- -(d) => b/d-a/d */
    newmin = MakeSmall(-FLOORDIV(GetSmall(xmax),-GetSmall(ymax)));
    newmax = MakeSmall(-CEILDIV(GetSmall(xmin),-GetSmall(ymax)));
    break;
  default:
    return 2;
  }
  if (FDlt(newmin,zmin))
    newmin = zmin;
  if (FDgt(newmax,zmax))
    newmax = zmax;
  if (x_is_mon) {
    /* adjust newmin to ensure that it divides some X value,
       and similarly for newmax */
    long m = GetSmall(newmin);
    long n = GetSmall(newmax);
	
    while (m<=n && (m==0 || !is_supported_divider(m,xmin,xmax,ymin,ymax)))
      m++;
    while (m<=n && (n==0 || !is_supported_divider(n,xmin,xmax,ymin,ymax)))
      n--;
	
    newmin = MakeSmall(m);
    newmax = MakeSmall(n);
  }
  if (dvar_fix_interval_t(dvz, newmin, newmax)<0)
    return (newmin==Sup || newmax==Inf) ? -1 : 0;
  else
    return 1;
}

/*
  '$fd_product'(State0, State, Actions).
  State = state(X, XMut, Y, YMut, Z, ZMut)
*/
void SPCDECL
prolog_fd_product MAGIC (HIDDEN_PROTO
			 SP_term_ref State,
			 SP_term_ref NewState,
			 SP_term_ref Actions)
{
  WAMENV;
  Dvar dvx, dvy, dvz;
  TAGGED newmin, newmax;
  TAGGED handle;
  SP_BOOL committed;
  struct arith_data *pdata;
  int ent = -1;			/* disentailed unless otherwise */
  int iter;

  (void)State;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;
  dvar_export_start(w);
  RefTerm(NewState) = static_output_state(w,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct arith_data,handle);
  } else {			/* build persistent state */
    pdata = Palloc(struct arith_data, 3*sizeof(struct dvar), handle);
    pdata->destructor = arith_destructor;
    pdata->nrefs = 6;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(6);
    pdata->dvar = (Dvar)(pdata+1);
    get_var_and_attr(X(0),pdata->refbase);
    get_var_and_attr(X(0)+WD(2),pdata->refbase+2);
    get_var_and_attr(X(0)+WD(4),pdata->refbase+4);
  }

  /* RESUME HERE */
  dvx = pdata->dvar;
  dvy = dvx+1;
  dvz = dvx+2;
  dvar_init(dvx, pdata->refbase,   pdata->refbase+1);
  dvar_init(dvy, pdata->refbase+2, pdata->refbase+3);
  dvar_init(dvz, pdata->refbase+4, pdata->refbase+5);

  for (iter=0; iter<3; iter++)
    switch (iter) {
    case 0:			/* (X,Y) => Z */
      switch (ARITH_DISPATCH(xmin,xmax,ymin,ymax)) {
	/* automatically generated table! */
      case 0x9: /* inf- -(b) * 0-0 => 0-0 */
      case 0x19: /* inf-0 * 0-0 => 0-0 */
      case 0x29: /* inf- +(b) * 0-0 => 0-0 */
      case 0x39: /* inf-sup * 0-0 => 0-0 */
      case 0x49: /* -(a)- -(b) * 0-0 => 0-0 */
      case 0x59: /* -(a)-0 * 0-0 => 0-0 */
      case 0x69: /* -(a)- +(b) * 0-0 => 0-0 */
      case 0x79: /* -(a)-sup * 0-0 => 0-0 */
      case 0x90: /* 0-0 * inf- -(d) => 0-0 */
      case 0x91: /* 0-0 * inf-0 => 0-0 */
      case 0x92: /* 0-0 * inf- +(d) => 0-0 */
      case 0x93: /* 0-0 * inf-sup => 0-0 */
      case 0x94: /* 0-0 * -(c)- -(d) => 0-0 */
      case 0x95: /* 0-0 * -(c)-0 => 0-0 */
      case 0x96: /* 0-0 * -(c)- +(d) => 0-0 */
      case 0x97: /* 0-0 * -(c)-sup => 0-0 */
      case 0x99: /* 0-0 * 0-0 => 0-0 */
      case 0x9a: /* 0-0 * 0- +(d) => 0-0 */
      case 0x9b: /* 0-0 * 0-sup => 0-0 */
      case 0x9e: /* 0-0 * +(c)- +(d) => 0-0 */
      case 0x9f: /* 0-0 * +(c)-sup => 0-0 */
      case 0xa9: /* 0- +(b) * 0-0 => 0-0 */
      case 0xb9: /* 0-sup * 0-0 => 0-0 */
      case 0xe9: /* +(a)- +(b) * 0-0 => 0-0 */
      case 0xf9: /* +(a)-sup * 0-0 => 0-0 */
	newmin = TaggedZero;
	newmax = TaggedZero;
	break;
      case 0x1: /* inf- -(b) * inf-0 => 0-sup */
      case 0x5: /* inf- -(b) * -(c)-0 => 0-sup */
      case 0x10: /* inf-0 * inf- -(d) => 0-sup */
      case 0x11: /* inf-0 * inf-0 => 0-sup */
      case 0x14: /* inf-0 * -(c)- -(d) => 0-sup */
      case 0x15: /* inf-0 * -(c)-0 => 0-sup */
      case 0x41: /* -(a)- -(b) * inf-0 => 0-sup */
      case 0x50: /* -(a)-0 * inf- -(d) => 0-sup */
      case 0x51: /* -(a)-0 * inf-0 => 0-sup */
      case 0xab: /* 0- +(b) * 0-sup => 0-sup */
      case 0xaf: /* 0- +(b) * +(c)-sup => 0-sup */
      case 0xba: /* 0-sup * 0- +(d) => 0-sup */
      case 0xbb: /* 0-sup * 0-sup => 0-sup */
      case 0xbe: /* 0-sup * +(c)- +(d) => 0-sup */
      case 0xbf: /* 0-sup * +(c)-sup => 0-sup */
      case 0xeb: /* +(a)- +(b) * 0-sup => 0-sup */
      case 0xfa: /* +(a)-sup * 0- +(d) => 0-sup */
      case 0xfb: /* +(a)-sup * 0-sup => 0-sup */
	newmin = TaggedZero;
	newmax = zmax;
	break;
      case 0xaa: /* 0- +(b) * 0- +(d) => 0- +(b)* +(d) */
      case 0xae: /* 0- +(b) * +(c)- +(d) => 0- +(b)* +(d) */
      case 0xea: /* +(a)- +(b) * 0- +(d) => 0- +(b)* +(d) */
	newmin = TaggedZero;
	newmax = safe_mul_val(xmax,ymax);
	break;
      case 0x45: /* -(a)- -(b) * -(c)-0 => 0- -(a)* -(c) */
      case 0x54: /* -(a)-0 * -(c)- -(d) => 0- -(a)* -(c) */
      case 0x55: /* -(a)-0 * -(c)-0 => 0- -(a)* -(c) */
	newmin = TaggedZero;
	newmax = safe_mul_val(xmin,ymin);
	break;
      case 0xa: /* inf- -(b) * 0- +(d) => inf-0 */
      case 0xb: /* inf- -(b) * 0-sup => inf-0 */
      case 0x1a: /* inf-0 * 0- +(d) => inf-0 */
      case 0x1b: /* inf-0 * 0-sup => inf-0 */
      case 0x1e: /* inf-0 * +(c)- +(d) => inf-0 */
      case 0x1f: /* inf-0 * +(c)-sup => inf-0 */
      case 0x4b: /* -(a)- -(b) * 0-sup => inf-0 */
      case 0x5b: /* -(a)-0 * 0-sup => inf-0 */
      case 0x5f: /* -(a)-0 * +(c)-sup => inf-0 */
      case 0xa0: /* 0- +(b) * inf- -(d) => inf-0 */
      case 0xa1: /* 0- +(b) * inf-0 => inf-0 */
      case 0xb0: /* 0-sup * inf- -(d) => inf-0 */
      case 0xb1: /* 0-sup * inf-0 => inf-0 */
      case 0xb4: /* 0-sup * -(c)- -(d) => inf-0 */
      case 0xb5: /* 0-sup * -(c)-0 => inf-0 */
      case 0xe1: /* +(a)- +(b) * inf-0 => inf-0 */
      case 0xf1: /* +(a)-sup * inf-0 => inf-0 */
      case 0xf5: /* +(a)-sup * -(c)-0 => inf-0 */
	newmin = zmin;
	newmax = TaggedZero;
	break;
      case 0xe0: /* +(a)- +(b) * inf- -(d) => inf- +(a)* -(d) */
      case 0xf0: /* +(a)-sup * inf- -(d) => inf- +(a)* -(d) */
      case 0xf4: /* +(a)-sup * -(c)- -(d) => inf- +(a)* -(d) */
	newmin = zmin;
	newmax = safe_mul_val(xmin,ymax);
	break;
      case 0x2a: /* inf- +(b) * 0- +(d) => inf- +(b)* +(d) */
      case 0x2e: /* inf- +(b) * +(c)- +(d) => inf- +(b)* +(d) */
      case 0xa2: /* 0- +(b) * inf- +(d) => inf- +(b)* +(d) */
      case 0xe2: /* +(a)- +(b) * inf- +(d) => inf- +(b)* +(d) */
	newmin = zmin;
	newmax = safe_mul_val(xmax,ymax);
	break;
      case 0x47: /* -(a)- -(b) * -(c)-sup => inf- -(a)* -(c) */
      case 0x57: /* -(a)-0 * -(c)-sup => inf- -(a)* -(c) */
      case 0x74: /* -(a)-sup * -(c)- -(d) => inf- -(a)* -(c) */
      case 0x75: /* -(a)-sup * -(c)-0 => inf- -(a)* -(c) */
	newmin = zmin;
	newmax = safe_mul_val(xmin,ymin);
	break;
      case 0xe: /* inf- -(b) * +(c)- +(d) => inf- -(b)* +(c) */
      case 0xf: /* inf- -(b) * +(c)-sup => inf- -(b)* +(c) */
      case 0x4f: /* -(a)- -(b) * +(c)-sup => inf- -(b)* +(c) */
	newmin = zmin;
	newmax = safe_mul_val(xmax,ymin);
	break;
      case 0xef: /* +(a)- +(b) * +(c)-sup => +(a)* +(c)-sup */
      case 0xfe: /* +(a)-sup * +(c)- +(d) => +(a)* +(c)-sup */
      case 0xff: /* +(a)-sup * +(c)-sup => +(a)* +(c)-sup */
	newmin = safe_mul_val(xmin,ymin);
	newmax = zmax;
	break;
      case 0xee: /* +(a)- +(b) * +(c)- +(d) => +(a)* +(c)- +(b)* +(d) */
	newmin = safe_mul_val(xmin,ymin);
	newmax = safe_mul_val(xmax,ymax);
	break;
      case 0xa4: /* 0- +(b) * -(c)- -(d) => +(b)* -(c)-0 */
      case 0xa5: /* 0- +(b) * -(c)-0 => +(b)* -(c)-0 */
      case 0xe5: /* +(a)- +(b) * -(c)-0 => +(b)* -(c)-0 */
	newmin = safe_mul_val(xmax,ymin);
	newmax = TaggedZero;
	break;
      case 0x24: /* inf- +(b) * -(c)- -(d) => +(b)* -(c)-sup */
      case 0x25: /* inf- +(b) * -(c)-0 => +(b)* -(c)-sup */
      case 0xa7: /* 0- +(b) * -(c)-sup => +(b)* -(c)-sup */
      case 0xe7: /* +(a)- +(b) * -(c)-sup => +(b)* -(c)-sup */
	newmin = safe_mul_val(xmax,ymin);
	newmax = zmax;
	break;
      case 0xe4: /* +(a)- +(b) * -(c)- -(d) => +(b)* -(c)- +(a)* -(d) */
	newmin = safe_mul_val(xmax,ymin);
	newmax = safe_mul_val(xmin,ymax);
	break;
      case 0xa6: /* 0- +(b) * -(c)- +(d) => +(b)* -(c)- +(b)* +(d) */
      case 0xe6: /* +(a)- +(b) * -(c)- +(d) => +(b)* -(c)- +(b)* +(d) */
	newmin = safe_mul_val(xmax,ymin);
	newmax = safe_mul_val(xmax,ymax);
	break;
      case 0x64: /* -(a)- +(b) * -(c)- -(d) => +(b)* -(c)- -(a)* -(c) */
      case 0x65: /* -(a)- +(b) * -(c)-0 => +(b)* -(c)- -(a)* -(c) */
	newmin = safe_mul_val(xmax,ymin);
	newmax = safe_mul_val(xmin,ymin);
	break;
      case 0x4a: /* -(a)- -(b) * 0- +(d) => -(a)* +(d)-0 */
      case 0x5a: /* -(a)-0 * 0- +(d) => -(a)* +(d)-0 */
      case 0x5e: /* -(a)-0 * +(c)- +(d) => -(a)* +(d)-0 */
	newmin = safe_mul_val(xmin,ymax);
	newmax = TaggedZero;
	break;
      case 0x42: /* -(a)- -(b) * inf- +(d) => -(a)* +(d)-sup */
      case 0x52: /* -(a)-0 * inf- +(d) => -(a)* +(d)-sup */
      case 0x7a: /* -(a)-sup * 0- +(d) => -(a)* +(d)-sup */
      case 0x7e: /* -(a)-sup * +(c)- +(d) => -(a)* +(d)-sup */
	newmin = safe_mul_val(xmin,ymax);
	newmax = zmax;
	break;
      case 0x6a: /* -(a)- +(b) * 0- +(d) => -(a)* +(d)- +(b)* +(d) */
      case 0x6e: /* -(a)- +(b) * +(c)- +(d) => -(a)* +(d)- +(b)* +(d) */
	newmin = safe_mul_val(xmin,ymax);
	newmax = safe_mul_val(xmax,ymax);
	break;
      case 0x46: /* -(a)- -(b) * -(c)- +(d) => -(a)* +(d)- -(a)* -(c) */
      case 0x56: /* -(a)-0 * -(c)- +(d) => -(a)* +(d)- -(a)* -(c) */
	newmin = safe_mul_val(xmin,ymax);
	newmax = safe_mul_val(xmin,ymin);
	break;
      case 0x4e: /* -(a)- -(b) * +(c)- +(d) => -(a)* +(d)- -(b)* +(c) */
	newmin = safe_mul_val(xmin,ymax);
	newmax = safe_mul_val(xmax,ymin);
	break;
      case 0x0: /* inf- -(b) * inf- -(d) => -(b)* -(d)-sup */
      case 0x4: /* inf- -(b) * -(c)- -(d) => -(b)* -(d)-sup */
      case 0x40: /* -(a)- -(b) * inf- -(d) => -(b)* -(d)-sup */
	newmin = safe_mul_val(xmax,ymax);
	newmax = zmax;
	break;
      case 0x44: /* -(a)- -(b) * -(c)- -(d) => -(b)* -(d)- -(a)* -(c) */
	newmin = safe_mul_val(xmax,ymax);
	newmax = safe_mul_val(xmin,ymin);
	break;
      case 0x66: /* -(a)- +(b) * -(c)- +(d) => min(-(a)* +(d),+(b)* -(c))-max(-(a)* -(c),+(b)* +(d)) */
	{
	  TAGGED minmax = safe_mul_val(xmin,ymax);
	  TAGGED maxmin = safe_mul_val(xmax,ymin);
	  TAGGED minmin = safe_mul_val(xmin,ymin);
	  TAGGED maxmax = safe_mul_val(xmax,ymax);

	  newmin = FDgt(minmax,maxmin) ? maxmin : minmax;
	  newmax = FDgt(minmin,maxmax) ? minmin : maxmax;
	}
	break;
      default:
	continue;
      }
      if (dvar_fix_interval_t(dvz, newmin, newmax)<0) {
	if (newmin==Sup || newmax==Inf) /* can't represent bounds */
	  goto oflo;
	goto ret;
      }
      break;
    case 1:			/* (X,Y) => X */
      switch (fd_product_inverse_dispatch(dvz, dvy, dvx)) {
      case -1: goto oflo;
      case 0: goto ret;
      }
      break;
    case 2:		/* (Z,X) => Y */
      switch (fd_product_inverse_dispatch(dvz, dvx, dvy)) {
      case -1: goto oflo;
      case 0: goto ret;
      }
      break;
    }

  dvar_export(w,dvx);
  dvar_export(w,dvy);
  dvar_export(w,dvz);
  ent = (dvar_is_integer(dvx) &&
 	 dvar_is_integer(dvy) &&
 	 dvar_is_integer(dvz));
/*   ent = (!dvar_is_integer_first(dvx) + */
/* 	 !dvar_is_integer_first(dvy) + */
/* 	 !dvar_is_integer_first(dvz) <= 1); */
  goto ret;
 oflo:
  fd.fd_overflow = TRUE;
 ret:
  if (!fd_check_overflow(CTagToArg(X(1),2)))
    SP_fail();
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}


static SP_BOOL
is_feasible_quotient(TAGGED x1,
		     TAGGED x2,
		     TAGGED y1,
		     TAGGED y2,
		     TAGGED z)
{
  if (!TagIsSmall(z) || Teqz(z))
    return TRUE;
  if (Tgtz(z)) {
    y1 = safe_mul(z,y1);
    y2 = safe_mul(z,y2);
  } else {
    TAGGED tmp;
    tmp = safe_mul(z,y2);
    y2  = safe_mul(z,y1);
    y1  = tmp;
  }
  return (fd_interval_cmp(y1,y2,x1,x2)!=FD_CONTAINS);
}

/*
  '$fd_quotient'(State0, State, Actions).
  State = state(X, XMut, Y, YMut, Z, ZMut)
*/
void SPCDECL
prolog_fd_quotient MAGIC (HIDDEN_PROTO
			  SP_term_ref State,
			  SP_term_ref NewState,
			  SP_term_ref Actions)
{
  WAMENV;
  Dvar dvx, dvy, dvz;
  TAGGED newmin, newmax, axmin, axmax, aymin, aymax, azmin, azmax;
  int iter;
  SP_BOOL compl;
  TAGGED handle;
  SP_BOOL committed;
  struct arith_data *pdata;
  int ent = -1;			/* disentailed unless otherwise */

  (void)State;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;
  dvar_export_start(w);
  RefTerm(NewState) = static_output_state(w,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct arith_data,handle);
  } else {			/* build persistent state */
    pdata = Palloc(struct arith_data, 3*sizeof(struct dvar), handle);
    pdata->destructor = arith_destructor;
    pdata->nrefs = 6;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(6);
    pdata->dvar = (Dvar)(pdata+1);
    get_var_and_attr(X(0),pdata->refbase);
    get_var_and_attr(X(0)+WD(2),pdata->refbase+2);
    get_var_and_attr(X(0)+WD(4),pdata->refbase+4);
  }

  /* RESUME HERE */
  dvx = pdata->dvar;
  dvy = dvx+1;
  dvz = dvx+2;
  dvar_init(dvx, pdata->refbase,   pdata->refbase+1);
  dvar_init(dvy, pdata->refbase+2, pdata->refbase+3);
  dvar_init(dvz, pdata->refbase+4, pdata->refbase+5);

  for (iter=0; iter<3; iter++)
    switch (iter) {
    case 0:			/* (X,Y) => Z */
      axmin = (Tgez(xmin) ? xmin : Tminus(xmin));
      axmax = (Tgez(xmax) ? xmax : Tminus(xmax));
      aymin = (Tgez(ymin) ? ymin : Tminus(ymin));
      aymax = (Tgez(ymax) ? ymax : Tminus(ymax));
      switch (ARITH_DISPATCH(xmin,xmax,ymin,ymax)) {
      case 0x9: /* inf..-(axmax) * 0..0 */
      case 0x49: /* -(axmin)..-(axmax) * 0..0 */
      case 0x19: /* inf..0 * 0..0 */
      case 0x59: /* -(axmin)..0 * 0..0 */
      case 0x29: /* inf..+(axmax) * 0..0 */
      case 0x39: /* inf..sup * 0..0 */
      case 0x69: /* -(axmin)..+(axmax) * 0..0 */
      case 0x79: /* -(axmin)..sup * 0..0 */
      case 0x99: /* 0..0 * 0..0 */
      case 0xa9: /* 0..+(axmax) * 0..0 */
      case 0xb9: /* 0..sup * 0..0 */
      case 0xe9: /* +(axmin)..+(axmax) * 0..0 */
      case 0xf9: /* +(axmin)..sup * 0..0 */
	goto ret;
      case 0x90: /* 0..0 * inf..-(aymax) */
      case 0x94: /* 0..0 * -(aymin)..-(aymax) */
      case 0x91: /* 0..0 * inf..0 */
      case 0x95: /* 0..0 * -(aymin)..0 */
      case 0x92: /* 0..0 * inf..+(aymax) */
      case 0x93: /* 0..0 * inf..sup */
      case 0x96: /* 0..0 * -(aymin)..+(aymax) */
      case 0x97: /* 0..0 * -(aymin)..sup */
      case 0x9a: /* 0..0 * 0..+(aymax) */
      case 0x9b: /* 0..0 * 0..sup */
      case 0x9e: /* 0..0 * +(aymin)..+(aymax) */
      case 0x9f: /* 0..0 * +(aymin)..sup */
	newmin = TaggedZero;
	newmax = TaggedZero;
	break;
      case 0xaa: /* 0..+(axmax) * 0..+(aymax) */
      case 0xab: /* 0..+(axmax) * 0..sup */
      case 0xeb: /* +(axmin)..+(axmax) * 0..sup */
	newmin = TaggedZero;
	newmax = axmax;
	break;
      case 0x41: /* -(axmin)..-(axmax) * inf..0 */
      case 0x51: /* -(axmin)..0 * inf..0 */
      case 0x55: /* -(axmin)..0 * -(aymin)..0 */
	newmin = TaggedZero;
	newmax = axmin;
	break;
      case 0x0: /* inf..-(axmax) * inf..-(aymax) */
      case 0x1: /* inf..-(axmax) * inf..0 */
      case 0x10: /* inf..0 * inf..-(aymax) */
      case 0x14: /* inf..0 * -(aymin)..-(aymax) */
      case 0x11: /* inf..0 * inf..0 */
      case 0x15: /* inf..0 * -(aymin)..0 */
      case 0xba: /* 0..sup * 0..+(aymax) */
      case 0xbb: /* 0..sup * 0..sup */
      case 0xbe: /* 0..sup * +(aymin)..+(aymax) */
      case 0xbf: /* 0..sup * +(aymin)..sup */
      case 0xfb: /* +(axmin)..sup * 0..sup */
      case 0xff: /* +(axmin)..sup * +(aymin)..sup */
	newmin = TaggedZero;
	newmax = Sup;
	break;
      case 0xae: /* 0..+(axmax) * +(aymin)..+(aymax) */
      case 0xaf: /* 0..+(axmax) * +(aymin)..sup */
      case 0xef: /* +(axmin)..+(axmax) * +(aymin)..sup */
	newmin = TaggedZero;
	newmax = safe_divd_val(axmax,aymin);
	break;
      case 0x40: /* -(axmin)..-(axmax) * inf..-(aymax) */
      case 0x50: /* -(axmin)..0 * inf..-(aymax) */
      case 0x54: /* -(axmin)..0 * -(aymin)..-(aymax) */
	newmin = TaggedZero;
	newmax = safe_divd_val(axmin,aymax);
	break;
      case 0x45: /* -(axmin)..-(axmax) * -(aymin)..0 */
	newmin = safe_divd_val(axmax,aymin);
	newmax = axmin;
	break;
      case 0x4: /* inf..-(axmax) * -(aymin)..-(aymax) */
      case 0x5: /* inf..-(axmax) * -(aymin)..0 */
	newmin = safe_divd_val(axmax,aymin);
	newmax = Sup;
	break;
      case 0x44: /* -(axmin)..-(axmax) * -(aymin)..-(aymax) */
	newmin = safe_divd_val(axmax,aymin);
	newmax = safe_divd_val(axmin,aymax);
	break;
      case 0xea: /* +(axmin)..+(axmax) * 0..+(aymax) */
	newmin = safe_divd_val(axmin,aymax);
	newmax = axmax;
	break;
      case 0xfa: /* +(axmin)..sup * 0..+(aymax) */
      case 0xfe: /* +(axmin)..sup * +(aymin)..+(aymax) */
	newmin = safe_divd_val(axmin,aymax);
	newmax = Sup;
	break;
      case 0xee: /* +(axmin)..+(axmax) * +(aymin)..+(aymax) */
	newmin = safe_divd_val(axmin,aymax);
	newmax = safe_divd_val(axmax,aymin);
	break;
      case 0xa1: /* 0..+(axmax) * inf..0 */
      case 0xa5: /* 0..+(axmax) * -(aymin)..0 */
      case 0xe1: /* +(axmin)..+(axmax) * inf..0 */
	newmin = safe_negate_val(axmax);
	newmax = TaggedZero;
	break;
      case 0xa2: /* 0..+(axmax) * inf..+(aymax) */
      case 0xa3: /* 0..+(axmax) * inf..sup */
      case 0xa6: /* 0..+(axmax) * -(aymin)..+(aymax) */
      case 0xa7: /* 0..+(axmax) * -(aymin)..sup */
	newmax = axmax;
	newmin = safe_negate(newmax);
	break;
      case 0x61: /* -(axmin)..+(axmax) * inf..0 */
      case 0x65: /* -(axmin)..+(axmax) * -(aymin)..0 */
	newmin = safe_negate_val(axmax);
	newmax = axmin;
	break;
      case 0x21: /* inf..+(axmax) * inf..0 */
      case 0x25: /* inf..+(axmax) * -(aymin)..0 */
	newmin = safe_negate_val(axmax);
	newmax = Sup;
	break;
      case 0xe5: /* +(axmin)..+(axmax) * -(aymin)..0 */
	newmin = safe_negate_val(axmax);
	newmax = safe_negate(safe_divd_val(axmin,aymin));
	break;
      case 0x4b: /* -(axmin)..-(axmax) * 0..sup */
      case 0x5a: /* -(axmin)..0 * 0..+(aymax) */
      case 0x5b: /* -(axmin)..0 * 0..sup */
	newmin = safe_negate_val(axmin);
	newmax = TaggedZero;
	break;
      case 0x6a: /* -(axmin)..+(axmax) * 0..+(aymax) */
      case 0x6b: /* -(axmin)..+(axmax) * 0..sup */
	newmin = safe_negate_val(axmin);
	newmax = axmax;
	break;
      case 0x52: /* -(axmin)..0 * inf..+(aymax) */
      case 0x53: /* -(axmin)..0 * inf..sup */
      case 0x56: /* -(axmin)..0 * -(aymin)..+(aymax) */
      case 0x57: /* -(axmin)..0 * -(aymin)..sup */
	newmax = axmin;
	newmin = safe_negate(newmax);
	break;
      case 0x7a: /* -(axmin)..sup * 0..+(aymax) */
      case 0x7b: /* -(axmin)..sup * 0..sup */
	newmin = safe_negate_val(axmin);
	newmax = Sup;
	break;
      case 0x4a: /* -(axmin)..-(axmax) * 0..+(aymax) */
	newmin = safe_negate_val(axmin);
	newmax = safe_negate(safe_divd_val(axmax,aymax));
	break;
      case 0xb: /* inf..-(axmax) * 0..sup */
      case 0xf: /* inf..-(axmax) * +(aymin)..sup */
      case 0x1a: /* inf..0 * 0..+(aymax) */
      case 0x1b: /* inf..0 * 0..sup */
      case 0x1e: /* inf..0 * +(aymin)..+(aymax) */
      case 0x1f: /* inf..0 * +(aymin)..sup */
      case 0xb0: /* 0..sup * inf..-(aymax) */
      case 0xb4: /* 0..sup * -(aymin)..-(aymax) */
      case 0xb1: /* 0..sup * inf..0 */
      case 0xb5: /* 0..sup * -(aymin)..0 */
      case 0xf0: /* +(axmin)..sup * inf..-(aymax) */
      case 0xf1: /* +(axmin)..sup * inf..0 */
	newmin = Inf;
	newmax = TaggedZero;
	break;
      case 0x2a: /* inf..+(axmax) * 0..+(aymax) */
      case 0x2b: /* inf..+(axmax) * 0..sup */
	newmin = Inf;
	newmax = axmax;
	break;
      case 0x71: /* -(axmin)..sup * inf..0 */
      case 0x75: /* -(axmin)..sup * -(aymin)..0 */
	newmin = Inf;
	newmax = axmin;
	break;
      case 0x2e: /* inf..+(axmax) * +(aymin)..+(aymax) */
      case 0x2f: /* inf..+(axmax) * +(aymin)..sup */
	newmin = Inf;
	newmax = safe_divd_val(axmax,aymin);
	break;
      case 0x70: /* -(axmin)..sup * inf..-(aymax) */
      case 0x74: /* -(axmin)..sup * -(aymin)..-(aymax) */
	newmin = Inf;
	newmax = safe_divd_val(axmin,aymax);
	break;
      case 0xa: /* inf..-(axmax) * 0..+(aymax) */
      case 0xe: /* inf..-(axmax) * +(aymin)..+(aymax) */
	newmin = Inf;
	newmax = safe_negate(safe_divd_val(axmax,aymax));
	break;
      case 0xf4: /* +(axmin)..sup * -(aymin)..-(aymax) */
      case 0xf5: /* +(axmin)..sup * -(aymin)..0 */
	newmin = Inf;
	newmax = safe_negate(safe_divd_val(axmin,aymin));
	break;
      case 0xa0: /* 0..+(axmax) * inf..-(aymax) */
      case 0xa4: /* 0..+(axmax) * -(aymin)..-(aymax) */
      case 0xe0: /* +(axmin)..+(axmax) * inf..-(aymax) */
	newmin = safe_negate(safe_divd_val(axmax,aymax));
	newmax = TaggedZero;
	break;
      case 0x20: /* inf..+(axmax) * inf..-(aymax) */
      case 0x24: /* inf..+(axmax) * -(aymin)..-(aymax) */
	newmin = safe_negate(safe_divd_val(axmax,aymax));
	newmax = Sup;
	break;
      case 0x60: /* -(axmin)..+(axmax) * inf..-(aymax) */
      case 0x64: /* -(axmin)..+(axmax) * -(aymin)..-(aymax) */
	newmin = safe_negate(safe_divd_val(axmax,aymax));
	newmax = safe_divd_val(axmin,aymax);
	break;
      case 0xe4: /* +(axmin)..+(axmax) * -(aymin)..-(aymax) */
	newmin = safe_negate(safe_divd_val(axmax,aymax));
	newmax = safe_negate(safe_divd_val(axmin,aymin));
	break;
      case 0x4f: /* -(axmin)..-(axmax) * +(aymin)..sup */
      case 0x5e: /* -(axmin)..0 * +(aymin)..+(aymax) */
      case 0x5f: /* -(axmin)..0 * +(aymin)..sup */
	newmin = safe_negate(safe_divd_val(axmin,aymin));
	newmax = TaggedZero;
	break;
      case 0x7e: /* -(axmin)..sup * +(aymin)..+(aymax) */
      case 0x7f: /* -(axmin)..sup * +(aymin)..sup */
	newmin = safe_negate(safe_divd_val(axmin,aymin));
	newmax = Sup;
	break;
      case 0x6e: /* -(axmin)..+(axmax) * +(aymin)..+(aymax) */
      case 0x6f: /* -(axmin)..+(axmax) * +(aymin)..sup */
	newmin = safe_negate(safe_divd_val(axmin,aymin));
	newmax = safe_divd_val(axmax,aymin);
	break;
      case 0x4e: /* -(axmin)..-(axmax) * +(aymin)..+(aymax) */
	newmin = safe_negate(safe_divd_val(axmin,aymin));
	newmax = safe_negate(safe_divd_val(axmax,aymax));
	break;
      case 0x42: /* -(axmin)..-(axmax) * inf..+(aymax) */
      case 0x43: /* -(axmin)..-(axmax) * inf..sup */
      case 0x46: /* -(axmin)..-(axmax) * -(aymin)..+(aymax) */
      case 0x47: /* -(axmin)..-(axmax) * -(aymin)..sup */
      case 0x62: /* -(axmin)..+(axmax) * inf..+(aymax) */
      case 0x63: /* -(axmin)..+(axmax) * inf..sup */
      case 0x66: /* -(axmin)..+(axmax) * -(aymin)..+(aymax) */
      case 0x67: /* -(axmin)..+(axmax) * -(aymin)..sup */
      case 0xe2: /* +(axmin)..+(axmax) * inf..+(aymax) */
      case 0xe3: /* +(axmin)..+(axmax) * inf..sup */
      case 0xe6: /* +(axmin)..+(axmax) * -(aymin)..+(aymax) */
      case 0xe7: /* +(axmin)..+(axmax) * -(aymin)..sup */
	newmax = safe_max(axmin,axmax);
	newmin = safe_negate(newmax);
	break;
      default:
	continue;
      }
      if (dvar_fix_interval_t(dvz, newmin, newmax)<0) {
	if (newmin==Sup || newmax==Inf) /* can't represent bounds */
	  goto oflo;
	goto ret;
      }
      break;
    case 1:			/* (X,Z) => Y */
      axmin = (Tgez(xmin) ? xmin : Tminus(xmin));
      axmax = (Tgez(xmax) ? xmax : Tminus(xmax));
      azmin = (Tgez(zmin) ? zmin : Tminus(zmin));
      azmax = (Tgez(zmax) ? zmax : Tminus(zmax));
      compl = FALSE;
      switch (ARITH_DISPATCH(xmin,xmax,zmin,zmax)) {
      case 0x90: /* 0..0 * inf..-(azmax) */
      case 0x94: /* 0..0 * -(azmin)..-(azmax) */
      case 0x9e: /* 0..0 * +(azmin)..+(azmax) */
      case 0x9f: /* 0..0 * +(azmin)..sup */
        goto ret;
      case 0x3: /* inf..-(axmax) * inf..sup */
      case 0x43: /* -(axmin)..-(axmax) * inf..sup */
      case 0xe3: /* +(axmin)..+(axmax) * inf..sup */
      case 0xf3: /* +(axmin)..sup * inf..sup */
        compl = TRUE;
        newmin = MakeSmall(1);
        newmax = MakeSmall(-1);
        break;
      case 0xb: /* inf..-(axmax) * 0..sup */
      case 0x4b: /* -(axmin)..-(axmax) * 0..sup */
        compl = TRUE;
        newmin = MakeSmall(1);
        newmax = axmax;
        break;
      case 0xe1: /* +(axmin)..+(axmax) * inf..0 */
      case 0xf1: /* +(axmin)..sup * inf..0 */
        compl = TRUE;
        newmin = MakeSmall(1);
        newmax = axmin;
        break;
      case 0x7: /* inf..-(axmax) * -(azmin)..sup */
      case 0x47: /* -(axmin)..-(axmax) * -(azmin)..sup */
        compl = TRUE;
        newmin = MakeSmall(1);
        newmax = safe_minus(safe_divu(safe_plus(axmax,MakeSmall(1)),safe_plus(azmin,MakeSmall(1))),MakeSmall(1));
        break;
      case 0xe2: /* +(axmin)..+(axmax) * inf..+(azmax) */
      case 0xf2: /* +(axmin)..sup * inf..+(azmax) */
        compl = TRUE;
        newmin = MakeSmall(1);
        newmax = safe_minus(safe_divu(safe_plus(axmin,MakeSmall(1)),safe_plus(azmax,MakeSmall(1))),MakeSmall(1));
        break;
      case 0x1: /* inf..-(axmax) * inf..0 */
      case 0x41: /* -(axmin)..-(axmax) * inf..0 */
        compl = TRUE;
        newmin = safe_negate_val(axmax);
        newmax = MakeSmall(-1);
        break;
      case 0x9: /* inf..-(axmax) * 0..0 */
      case 0x49: /* -(axmin)..-(axmax) * 0..0 */
        compl = TRUE;
        newmin = safe_negate_val(axmax);
        newmax = axmax;
        break;
      case 0x5: /* inf..-(axmax) * -(azmin)..0 */
      case 0x45: /* -(axmin)..-(axmax) * -(azmin)..0 */
        compl = TRUE;
        newmin = safe_negate_val(axmax);
        newmax = safe_minus(safe_divu(safe_plus(axmax,MakeSmall(1)),safe_plus(azmin,MakeSmall(1))),MakeSmall(1));
        break;
      case 0xeb: /* +(axmin)..+(axmax) * 0..sup */
      case 0xfb: /* +(axmin)..sup * 0..sup */
        compl = TRUE;
        newmin = safe_negate_val(axmin);
        newmax = MakeSmall(-1);
        break;
      case 0xe9: /* +(axmin)..+(axmax) * 0..0 */
      case 0xf9: /* +(axmin)..sup * 0..0 */
        compl = TRUE;
        newmin = safe_negate_val(axmin);
        newmax = axmin;
        break;
      case 0xea: /* +(axmin)..+(axmax) * 0..+(azmax) */
      case 0xfa: /* +(axmin)..sup * 0..+(azmax) */
        compl = TRUE;
        newmin = safe_negate_val(axmin);
        newmax = safe_minus(safe_divu(safe_plus(axmin,MakeSmall(1)),safe_plus(azmax,MakeSmall(1))),MakeSmall(1));
        break;
      case 0x2: /* inf..-(axmax) * inf..+(azmax) */
      case 0x42: /* -(axmin)..-(axmax) * inf..+(azmax) */
        compl = TRUE;
        newmin = safe_minus(MakeSmall(1),safe_divu(safe_plus(axmax,MakeSmall(1)),safe_plus(azmax,MakeSmall(1))));
        newmax = MakeSmall(-1);
        break;
      case 0xa: /* inf..-(axmax) * 0..+(azmax) */
      case 0x4a: /* -(axmin)..-(axmax) * 0..+(azmax) */
        compl = TRUE;
        newmin = safe_minus(MakeSmall(1),safe_divu(safe_plus(axmax,MakeSmall(1)),safe_plus(azmax,MakeSmall(1))));
        newmax = axmax;
        break;
      case 0x6: /* inf..-(axmax) * -(azmin)..+(azmax) */
      case 0x46: /* -(axmin)..-(axmax) * -(azmin)..+(azmax) */
        compl = TRUE;
        newmin = safe_minus(MakeSmall(1),safe_divu(safe_plus(axmax,MakeSmall(1)),safe_plus(azmax,MakeSmall(1))));
        newmax = safe_minus(safe_divu(safe_plus(axmax,MakeSmall(1)),safe_plus(azmin,MakeSmall(1))),MakeSmall(1));
        break;
      case 0xe7: /* +(axmin)..+(axmax) * -(azmin)..sup */
      case 0xf7: /* +(axmin)..sup * -(azmin)..sup */
        compl = TRUE;
        newmin = safe_minus(MakeSmall(1),safe_divu(safe_plus(axmin,MakeSmall(1)),safe_plus(azmin,MakeSmall(1))));
        newmax = MakeSmall(-1);
        break;
      case 0xe5: /* +(axmin)..+(axmax) * -(azmin)..0 */
      case 0xf5: /* +(axmin)..sup * -(azmin)..0 */
        compl = TRUE;
        newmin = safe_minus(MakeSmall(1),safe_divu(safe_plus(axmin,MakeSmall(1)),safe_plus(azmin,MakeSmall(1))));
        newmax = axmin;
        break;
      case 0xe6: /* +(axmin)..+(axmax) * -(azmin)..+(azmax) */
      case 0xf6: /* +(axmin)..sup * -(azmin)..+(azmax) */
        compl = TRUE;
        newmin = safe_minus(MakeSmall(1),safe_divu(safe_plus(axmin,MakeSmall(1)),safe_plus(azmin,MakeSmall(1))));
        newmax = safe_minus(safe_divu(safe_plus(axmin,MakeSmall(1)),safe_plus(azmax,MakeSmall(1))),MakeSmall(1));
        break;
      case 0x0: /* inf..-(axmax) * inf..-(azmax) */
      case 0x10: /* inf..0 * inf..-(azmax) */
      case 0x14: /* inf..0 * -(azmin)..-(azmax) */
      case 0xbe: /* 0..sup * +(azmin)..+(azmax) */
      case 0xbf: /* 0..sup * +(azmin)..sup */
      case 0xff: /* +(axmin)..sup * +(azmin)..sup */
        newmin = TaggedZero;
        newmax = Sup;
        break;
      case 0xae: /* 0..+(axmax) * +(azmin)..+(azmax) */
      case 0xaf: /* 0..+(axmax) * +(azmin)..sup */
      case 0xef: /* +(axmin)..+(axmax) * +(azmin)..sup */
        newmin = TaggedZero;
        newmax = safe_divd_val(axmax,azmin);
        break;
      case 0x40: /* -(axmin)..-(axmax) * inf..-(azmax) */
      case 0x50: /* -(axmin)..0 * inf..-(azmax) */
      case 0x54: /* -(axmin)..0 * -(azmin)..-(azmax) */
        newmin = TaggedZero;
        newmax = safe_divd_val(axmin,azmax);
        break;
      case 0x4: /* inf..-(axmax) * -(azmin)..-(azmax) */
        newmin = safe_divu(safe_plus(axmax,MakeSmall(1)),safe_plus(azmin,MakeSmall(1)));
        newmax = Sup;
        break;
      case 0x44: /* -(axmin)..-(axmax) * -(azmin)..-(azmax) */
        newmin = safe_divu(safe_plus(axmax,MakeSmall(1)),safe_plus(azmin,MakeSmall(1)));
        newmax = safe_divd_val(axmin,azmax);
        break;
      case 0xfe: /* +(axmin)..sup * +(azmin)..+(azmax) */
        newmin = safe_divu(safe_plus(axmin,MakeSmall(1)),safe_plus(azmax,MakeSmall(1)));
        newmax = Sup;
        break;
      case 0xee: /* +(axmin)..+(axmax) * +(azmin)..+(azmax) */
        newmin = safe_divu(safe_plus(axmin,MakeSmall(1)),safe_plus(azmax,MakeSmall(1)));
        newmax = safe_divd_val(axmax,azmin);
        break;
      case 0xf: /* inf..-(axmax) * +(azmin)..sup */
      case 0x1e: /* inf..0 * +(azmin)..+(azmax) */
      case 0x1f: /* inf..0 * +(azmin)..sup */
      case 0xb0: /* 0..sup * inf..-(azmax) */
      case 0xb4: /* 0..sup * -(azmin)..-(azmax) */
      case 0xf0: /* +(axmin)..sup * inf..-(azmax) */
        newmin = Inf;
        newmax = TaggedZero;
        break;
      case 0x2e: /* inf..+(axmax) * +(azmin)..+(azmax) */
      case 0x2f: /* inf..+(axmax) * +(azmin)..sup */
        newmin = Inf;
        newmax = safe_divd_val(axmax,azmin);
        break;
      case 0x70: /* -(axmin)..sup * inf..-(azmax) */
      case 0x74: /* -(axmin)..sup * -(azmin)..-(azmax) */
        newmin = Inf;
        newmax = safe_divd_val(axmin,azmax);
        break;
      case 0xe: /* inf..-(axmax) * +(azmin)..+(azmax) */
        newmin = Inf;
        newmax = safe_negate(safe_divu(safe_plus(axmax,MakeSmall(1)),safe_plus(azmax,MakeSmall(1))));
        break;
      case 0xf4: /* +(axmin)..sup * -(azmin)..-(azmax) */
        newmin = Inf;
        newmax = safe_negate(safe_divu(safe_plus(axmin,MakeSmall(1)),safe_plus(azmin,MakeSmall(1))));
        break;
      case 0xa0: /* 0..+(axmax) * inf..-(azmax) */
      case 0xa4: /* 0..+(axmax) * -(azmin)..-(azmax) */
      case 0xe0: /* +(axmin)..+(axmax) * inf..-(azmax) */
        newmin = safe_negate(safe_divd_val(axmax,azmax));
        newmax = TaggedZero;
        break;
      case 0x20: /* inf..+(axmax) * inf..-(azmax) */
      case 0x24: /* inf..+(axmax) * -(azmin)..-(azmax) */
        newmin = safe_negate(safe_divd_val(axmax,azmax));
        newmax = Sup;
        break;
      case 0x60: /* -(axmin)..+(axmax) * inf..-(azmax) */
      case 0x64: /* -(axmin)..+(axmax) * -(azmin)..-(azmax) */
        newmin = safe_negate(safe_divd_val(axmax,azmax));
        newmax = safe_divd_val(axmin,azmax);
        break;
      case 0xe4: /* +(axmin)..+(axmax) * -(azmin)..-(azmax) */
        newmin = safe_negate(safe_divd_val(axmax,azmax));
        newmax = safe_negate(safe_divu(safe_plus(axmin,MakeSmall(1)),safe_plus(azmin,MakeSmall(1))));
        break;
      case 0x4f: /* -(axmin)..-(axmax) * +(azmin)..sup */
      case 0x5e: /* -(axmin)..0 * +(azmin)..+(azmax) */
      case 0x5f: /* -(axmin)..0 * +(azmin)..sup */
        newmin = safe_negate(safe_divd_val(axmin,azmin));
        newmax = TaggedZero;
        break;
      case 0x7e: /* -(axmin)..sup * +(azmin)..+(azmax) */
      case 0x7f: /* -(axmin)..sup * +(azmin)..sup */
        newmin = safe_negate(safe_divd_val(axmin,azmin));
        newmax = Sup;
        break;
      case 0x6e: /* -(axmin)..+(axmax) * +(azmin)..+(azmax) */
      case 0x6f: /* -(axmin)..+(axmax) * +(azmin)..sup */
        newmin = safe_negate(safe_divd_val(axmin,azmin));
        newmax = safe_divd_val(axmax,azmin);
        break;
      case 0x4e: /* -(axmin)..-(axmax) * +(azmin)..+(azmax) */
        newmin = safe_negate(safe_divd_val(axmin,azmin));
        newmax = safe_negate(safe_divu(safe_plus(axmax,MakeSmall(1)),safe_plus(azmax,MakeSmall(1))));
        break;
      default:
	continue;
      }
      if ((compl
	   ? dvar_prune_interval_t(dvy, newmin, newmax,w)
	   : dvar_fix_interval_t(dvy, newmin, newmax))<0) {
	if (newmin==Sup || newmax==Inf) /* can't represent bounds */
	  goto oflo;
	goto ret;
      }
      break;
    case 2:			/* (Y,Z) => X */
      aymin = (Tgez(ymin) ? ymin : Tminus(ymin));
      aymax = (Tgez(ymax) ? ymax : Tminus(ymax));
      azmin = (Tgez(zmin) ? zmin : Tminus(zmin));
      azmax = (Tgez(zmax) ? zmax : Tminus(zmax));
      switch (ARITH_DISPATCH(ymin,ymax,zmin,zmax)) {
      case 0x90: /* 0..0 * inf..-(azmax) */
      case 0x94: /* 0..0 * -(azmin)..-(azmax) */
      case 0x91: /* 0..0 * inf..0 */
      case 0x95: /* 0..0 * -(azmin)..0 */
      case 0x92: /* 0..0 * inf..+(azmax) */
      case 0x93: /* 0..0 * inf..sup */
      case 0x96: /* 0..0 * -(azmin)..+(azmax) */
      case 0x97: /* 0..0 * -(azmin)..sup */
      case 0x99: /* 0..0 * 0..0 */
      case 0x9a: /* 0..0 * 0..+(azmax) */
      case 0x9b: /* 0..0 * 0..sup */
      case 0x9e: /* 0..0 * +(azmin)..+(azmax) */
      case 0x9f: /* 0..0 * +(azmin)..sup */
	goto ret;
      case 0x10: /* inf..0 * inf..-(azmax) */
      case 0x14: /* inf..0 * -(azmin)..-(azmax) */
      case 0x50: /* -(aymin)..0 * inf..-(azmax) */
	newmin = azmax;
	newmax = Sup;
	break;
      case 0x54: /* -(aymin)..0 * -(azmin)..-(azmax) */
	newmin = azmax;
	newmax = safe_minus(safe_plus(safe_mul_val(aymin,azmin),aymin),MakeSmall(1));
	break;
      case 0xaf: /* 0..+(aymax) * +(azmin)..sup */
      case 0xbe: /* 0..sup * +(azmin)..+(azmax) */
      case 0xbf: /* 0..sup * +(azmin)..sup */
	newmin = azmin;
	newmax = Sup;
	break;
      case 0xae: /* 0..+(aymax) * +(azmin)..+(azmax) */
	newmin = azmin;
	newmax = safe_minus(safe_plus(safe_mul_val(aymax,azmax),aymax),MakeSmall(1));
	break;
      case 0x0: /* inf..-(aymax) * inf..-(azmax) */
      case 0x4: /* inf..-(aymax) * -(azmin)..-(azmax) */
      case 0x40: /* -(aymin)..-(aymax) * inf..-(azmax) */
	newmin = safe_mul_val(aymax,azmax);
	newmax = Sup;
	break;
      case 0x44: /* -(aymin)..-(aymax) * -(azmin)..-(azmax) */
	newmin = safe_mul_val(aymax,azmax);
	newmax = safe_minus(safe_plus(safe_mul_val(aymin,azmin),aymin),MakeSmall(1));
	break;
      case 0xef: /* +(aymin)..+(aymax) * +(azmin)..sup */
      case 0xfe: /* +(aymin)..sup * +(azmin)..+(azmax) */
      case 0xff: /* +(aymin)..sup * +(azmin)..sup */
	newmin = safe_mul_val(aymin,azmin);
	newmax = Sup;
	break;
      case 0xee: /* +(aymin)..+(aymax) * +(azmin)..+(azmax) */
	newmin = safe_mul_val(aymin,azmin);
	newmax = safe_minus(safe_plus(safe_mul_val(aymax,azmax),aymax),MakeSmall(1));
	break;
      case 0xab: /* 0..+(aymax) * 0..sup */
      case 0xeb: /* +(aymin)..+(aymax) * 0..sup */
	newmin = safe_minus(MakeSmall(1),aymax);
	newmax = Sup;
	break;
      case 0xa9: /* 0..+(aymax) * 0..0 */
      case 0xe9: /* +(aymin)..+(aymax) * 0..0 */
	newmax = safe_minus(aymax,MakeSmall(1));
	newmin = safe_negate(newmax);
	break;
      case 0xaa: /* 0..+(aymax) * 0..+(azmax) */
      case 0xea: /* +(aymin)..+(aymax) * 0..+(azmax) */
	newmin = safe_minus(MakeSmall(1),aymax);
	newmax = safe_minus(safe_plus(safe_mul_val(aymax,azmax),aymax),MakeSmall(1));
	break;
      case 0x41: /* -(aymin)..-(aymax) * inf..0 */
      case 0x51: /* -(aymin)..0 * inf..0 */
	newmin = safe_minus(MakeSmall(1),aymin);
	newmax = Sup;
	break;
      case 0x49: /* -(aymin)..-(aymax) * 0..0 */
      case 0x59: /* -(aymin)..0 * 0..0 */
	newmax = safe_minus(aymin,MakeSmall(1));
	newmin = safe_negate(newmax);
	break;
      case 0x45: /* -(aymin)..-(aymax) * -(azmin)..0 */
      case 0x55: /* -(aymin)..0 * -(azmin)..0 */
	newmin = safe_minus(MakeSmall(1),aymin);
	newmax = safe_minus(safe_plus(safe_mul_val(aymin,azmin),aymin),MakeSmall(1));
	break;
      case 0x24: /* inf..+(aymax) * -(azmin)..-(azmax) */
      case 0x25: /* inf..+(aymax) * -(azmin)..0 */
      case 0xa7: /* 0..+(aymax) * -(azmin)..sup */
      case 0xe7: /* +(aymin)..+(aymax) * -(azmin)..sup */
	newmin = safe_minus(MakeSmall(1),safe_plus(safe_mul_val(aymax,azmin),aymax));
	newmax = Sup;
	break;
      case 0xa5: /* 0..+(aymax) * -(azmin)..0 */
      case 0xe5: /* +(aymin)..+(aymax) * -(azmin)..0 */
	newmin = safe_minus(MakeSmall(1),safe_plus(safe_mul_val(aymax,azmin),aymax));
	newmax = safe_minus(aymax,MakeSmall(1));
	break;
      case 0xa6: /* 0..+(aymax) * -(azmin)..+(azmax) */
      case 0xe6: /* +(aymin)..+(aymax) * -(azmin)..+(azmax) */
	newmin = safe_minus(MakeSmall(1),safe_plus(safe_mul_val(aymax,azmin),aymax));
	newmax = safe_minus(safe_plus(safe_mul_val(aymax,azmax),aymax),MakeSmall(1));
	break;
      case 0x64: /* -(aymin)..+(aymax) * -(azmin)..-(azmax) */
	newmin = safe_minus(MakeSmall(1),safe_plus(safe_mul_val(aymax,azmin),aymax));
	newmax = safe_minus(safe_plus(safe_mul_val(aymin,azmin),aymin),MakeSmall(1));
	break;
      case 0x65: /* -(aymin)..+(aymax) * -(azmin)..0 */
	newmin = safe_negate(safe_max(safe_minus(safe_plus(safe_mul_val(aymax,azmin),aymax),MakeSmall(1)),safe_minus(aymin,MakeSmall(1))));
	newmax = safe_max(safe_minus(safe_plus(safe_mul_val(aymin,azmin),aymin),MakeSmall(1)),safe_minus(aymax,MakeSmall(1)));
	break;
      case 0xa4: /* 0..+(aymax) * -(azmin)..-(azmax) */
	newmin = safe_minus(MakeSmall(1),safe_plus(safe_mul_val(aymax,azmin),aymax));
	newmax = safe_negate_val(azmax);
	break;
      case 0xe4: /* +(aymin)..+(aymax) * -(azmin)..-(azmax) */
	newmin = safe_minus(MakeSmall(1),safe_plus(safe_mul_val(aymax,azmin),aymax));
	newmax = safe_negate(safe_mul_val(aymin,azmax));
	break;
      case 0x42: /* -(aymin)..-(aymax) * inf..+(azmax) */
      case 0x52: /* -(aymin)..0 * inf..+(azmax) */
      case 0x7a: /* -(aymin)..sup * 0..+(azmax) */
      case 0x7e: /* -(aymin)..sup * +(azmin)..+(azmax) */
	newmin = safe_minus(MakeSmall(1),safe_plus(safe_mul_val(aymin,azmax),aymin));
	newmax = Sup;
	break;
      case 0x4a: /* -(aymin)..-(aymax) * 0..+(azmax) */
      case 0x5a: /* -(aymin)..0 * 0..+(azmax) */
	newmin = safe_minus(MakeSmall(1),safe_plus(safe_mul_val(aymin,azmax),aymin));
	newmax = safe_minus(aymin,MakeSmall(1));
	break;
      case 0x6a: /* -(aymin)..+(aymax) * 0..+(azmax) */
	newmin = safe_negate(safe_max(safe_minus(aymax,MakeSmall(1)),safe_minus(safe_plus(safe_mul_val(aymin,azmax),aymin),MakeSmall(1))));
	newmax = safe_max(safe_minus(aymin,MakeSmall(1)),safe_minus(safe_plus(safe_mul_val(aymax,azmax),aymax),MakeSmall(1)));
	break;
      case 0x6e: /* -(aymin)..+(aymax) * +(azmin)..+(azmax) */
	newmin = safe_minus(MakeSmall(1),safe_plus(safe_mul_val(aymin,azmax),aymin));
	newmax = safe_minus(safe_plus(safe_mul_val(aymax,azmax),aymax),MakeSmall(1));
	break;
      case 0x46: /* -(aymin)..-(aymax) * -(azmin)..+(azmax) */
      case 0x56: /* -(aymin)..0 * -(azmin)..+(azmax) */
	newmin = safe_minus(MakeSmall(1),safe_plus(safe_mul_val(aymin,azmax),aymin));
	newmax = safe_minus(safe_plus(safe_mul_val(aymin,azmin),aymin),MakeSmall(1));
	break;
      case 0x5e: /* -(aymin)..0 * +(azmin)..+(azmax) */
	newmin = safe_minus(MakeSmall(1),safe_plus(safe_mul_val(aymin,azmax),aymin));
	newmax = safe_negate_val(azmin);
	break;
      case 0x4e: /* -(aymin)..-(aymax) * +(azmin)..+(azmax) */
	newmin = safe_minus(MakeSmall(1),safe_plus(safe_mul_val(aymin,azmax),aymin));
	newmax = safe_negate(safe_mul_val(aymax,azmin));
	break;
      case 0xa1: /* 0..+(aymax) * inf..0 */
      case 0xe1: /* +(aymin)..+(aymax) * inf..0 */
	newmin = Inf;
	newmax = safe_minus(aymax,MakeSmall(1));
	break;
      case 0x4b: /* -(aymin)..-(aymax) * 0..sup */
      case 0x5b: /* -(aymin)..0 * 0..sup */
	newmin = Inf;
	newmax = safe_minus(aymin,MakeSmall(1));
	break;
      case 0x2a: /* inf..+(aymax) * 0..+(azmax) */
      case 0x2e: /* inf..+(aymax) * +(azmin)..+(azmax) */
      case 0xa2: /* 0..+(aymax) * inf..+(azmax) */
      case 0xe2: /* +(aymin)..+(aymax) * inf..+(azmax) */
	newmin = Inf;
	newmax = safe_minus(safe_plus(safe_mul_val(aymax,azmax),aymax),MakeSmall(1));
	break;
      case 0x47: /* -(aymin)..-(aymax) * -(azmin)..sup */
      case 0x57: /* -(aymin)..0 * -(azmin)..sup */
      case 0x74: /* -(aymin)..sup * -(azmin)..-(azmax) */
      case 0x75: /* -(aymin)..sup * -(azmin)..0 */
	newmin = Inf;
	newmax = safe_minus(safe_plus(safe_mul_val(aymin,azmin),aymin),MakeSmall(1));
	break;
      case 0xa0: /* 0..+(aymax) * inf..-(azmax) */
      case 0xb0: /* 0..sup * inf..-(azmax) */
      case 0xb4: /* 0..sup * -(azmin)..-(azmax) */
	newmin = Inf;
	newmax = safe_negate_val(azmax);
	break;
      case 0x1e: /* inf..0 * +(azmin)..+(azmax) */
      case 0x1f: /* inf..0 * +(azmin)..sup */
      case 0x5f: /* -(aymin)..0 * +(azmin)..sup */
	newmin = Inf;
	newmax = safe_negate_val(azmin);
	break;
      case 0xe: /* inf..-(aymax) * +(azmin)..+(azmax) */
      case 0xf: /* inf..-(aymax) * +(azmin)..sup */
      case 0x4f: /* -(aymin)..-(aymax) * +(azmin)..sup */
	newmin = Inf;
	newmax = safe_negate(safe_mul_val(aymax,azmin));
	break;
      case 0xe0: /* +(aymin)..+(aymax) * inf..-(azmax) */
      case 0xf0: /* +(aymin)..sup * inf..-(azmax) */
      case 0xf4: /* +(aymin)..sup * -(azmin)..-(azmax) */
	newmin = Inf;
	newmax = safe_negate(safe_mul_val(aymin,azmax));
	break;
      case 0x69: /* -(aymin)..+(aymax) * 0..0 */
	newmax = safe_max(safe_minus(aymin,MakeSmall(1)),safe_minus(aymax,MakeSmall(1)));
	newmin = safe_negate(newmax);
	break;
      case 0x66: /* -(aymin)..+(aymax) * -(azmin)..+(azmax) */
	newmin = safe_negate(safe_max(safe_minus(safe_plus(safe_mul_val(aymax,azmin),aymax),MakeSmall(1)),safe_minus(safe_plus(safe_mul_val(aymin,azmax),aymin),MakeSmall(1))));
	newmax = safe_max(safe_minus(safe_plus(safe_mul_val(aymin,azmin),aymin),MakeSmall(1)),safe_minus(safe_plus(safe_mul_val(aymax,azmax),aymax),MakeSmall(1)));
	break;
      default:
	continue;
      }
      if (dvar_fix_interval_t(dvx, newmin, newmax)<0) {
	if (newmin==Sup || newmax==Inf) /* can't represent bounds */
	  goto oflo;
	goto ret;
      }
      break;
    }
  /* Valid rule:

     Z is invalid if:
     Z>0 and dom(X) \subseteq (ZA,ZB)
     Z<0 and dom(X) \subseteq (ZB,ZA)

     where A = max(Y | Y<0), B = min(Y | Y>0)
   */
  if (TagIsSmall(xmin) && TagIsSmall(xmax)) {
    TAGGED y1 = dvar_predecessor_t(dvy,TaggedZero);
    TAGGED y2 = dvar_successor_t(dvy,TaggedZero);
    
    while (!is_feasible_quotient(xmin,xmax,y1,y2,zmin)) {
      if (dvar_prune_value_t(dvz,zmin,w)<0)
	goto ret;
    }
    while (!is_feasible_quotient(xmin,xmax,y1,y2,zmax)) {
      if (dvar_prune_value_t(dvz,zmax,w)<0)
	goto ret;
    }
  }
  dvar_export(w,dvx);
  dvar_export(w,dvy);
  dvar_export(w,dvz);
  ent = (dvar_is_integer(dvx) &&
 	 dvar_is_integer(dvy) &&
 	 dvar_is_integer(dvz));
/*   ent = (!dvar_is_integer_first(dvx) + */
/* 	 !dvar_is_integer_first(dvy) + */
/* 	 !dvar_is_integer_first(dvz) <= 1); */
  goto ret;
 oflo:
  fd.fd_overflow = TRUE;
 ret:
  if (!fd_check_overflow(CTagToArg(X(1),2)))
    SP_fail();
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}

/*
  '$fd_modulo'(State0, State, Actions).
  State = state(X, XMut, Y, YMut, Z, ZMut)
*/
void SPCDECL
prolog_fd_modulo MAGIC (HIDDEN_PROTO
			SP_term_ref State,
			SP_term_ref NewState,
			SP_term_ref Actions)
{
  WAMENV;
  Dvar dvx, dvy, dvz;
  TAGGED newmin, newmax, axmin, axmax, aymin, aymax, azmin, azmax;
  int iter;
  SP_BOOL compl;
  TAGGED handle;
  SP_BOOL committed;
  struct arith_data *pdata;
  int ent = -1;			/* disentailed unless otherwise */

  (void)State;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;
  dvar_export_start(w);
  RefTerm(NewState) = static_output_state(w,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct arith_data,handle);
  } else {			/* build persistent state */
    pdata = Palloc(struct arith_data, 3*sizeof(struct dvar), handle);
    pdata->destructor = arith_destructor;
    pdata->nrefs = 6;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(6);
    pdata->dvar = (Dvar)(pdata+1);
    get_var_and_attr(X(0),pdata->refbase);
    get_var_and_attr(X(0)+WD(2),pdata->refbase+2);
    get_var_and_attr(X(0)+WD(4),pdata->refbase+4);
  }

  /* RESUME HERE */
  dvx = pdata->dvar;
  dvy = dvx+1;
  dvz = dvx+2;
  dvar_init(dvx, pdata->refbase,   pdata->refbase+1);
  dvar_init(dvy, pdata->refbase+2, pdata->refbase+3);
  dvar_init(dvz, pdata->refbase+4, pdata->refbase+5);

  for (iter=0; iter<3; iter++)
    switch (iter) {
    case 0:			/* (X,Y) => Z */
      axmin = (Tgez(xmin) ? xmin : Tminus(xmin));
      axmax = (Tgez(xmax) ? xmax : Tminus(xmax));
      aymin = (Tgez(ymin) ? ymin : Tminus(ymin));
      aymax = (Tgez(ymax) ? ymax : Tminus(ymax));
      switch (ARITH_DISPATCH(xmin,xmax,ymin,ymax)) {
      case 0x9: /* inf..-(axmax) * 0..0 */
      case 0x49: /* -(axmin)..-(axmax) * 0..0 */
      case 0x19: /* inf..0 * 0..0 */
      case 0x59: /* -(axmin)..0 * 0..0 */
      case 0x29: /* inf..+(axmax) * 0..0 */
      case 0x39: /* inf..sup * 0..0 */
      case 0x69: /* -(axmin)..+(axmax) * 0..0 */
      case 0x79: /* -(axmin)..sup * 0..0 */
      case 0x99: /* 0..0 * 0..0 */
      case 0xa9: /* 0..+(axmax) * 0..0 */
      case 0xb9: /* 0..sup * 0..0 */
      case 0xe9: /* +(axmin)..+(axmax) * 0..0 */
      case 0xf9: /* +(axmin)..sup * 0..0 */
        goto ret;
      case 0x90: /* 0..0 * inf..-(aymax) */
      case 0x94: /* 0..0 * -(aymin)..-(aymax) */
      case 0x91: /* 0..0 * inf..0 */
      case 0x95: /* 0..0 * -(aymin)..0 */
      case 0x92: /* 0..0 * inf..+(aymax) */
      case 0x93: /* 0..0 * inf..sup */
      case 0x96: /* 0..0 * -(aymin)..+(aymax) */
      case 0x97: /* 0..0 * -(aymin)..sup */
      case 0x9a: /* 0..0 * 0..+(aymax) */
      case 0x9b: /* 0..0 * 0..sup */
      case 0x9e: /* 0..0 * +(aymin)..+(aymax) */
      case 0x9f: /* 0..0 * +(aymin)..sup */
        newmin = TaggedZero;
        newmax = TaggedZero;
        break;
      case 0xa0: /* 0..+(axmax) * inf..-(aymax) */
      case 0xa1: /* 0..+(axmax) * inf..0 */
      case 0xa2: /* 0..+(axmax) * inf..+(aymax) */
      case 0xa3: /* 0..+(axmax) * inf..sup */
      case 0xa7: /* 0..+(axmax) * -(aymin)..sup */
      case 0xab: /* 0..+(axmax) * 0..sup */
      case 0xaf: /* 0..+(axmax) * +(aymin)..sup */
      case 0xe0: /* +(axmin)..+(axmax) * inf..-(aymax) */
      case 0xe1: /* +(axmin)..+(axmax) * inf..0 */
      case 0xe2: /* +(axmin)..+(axmax) * inf..+(aymax) */
      case 0xe3: /* +(axmin)..+(axmax) * inf..sup */
      case 0xe7: /* +(axmin)..+(axmax) * -(aymin)..sup */
      case 0xeb: /* +(axmin)..+(axmax) * 0..sup */
      case 0xef: /* +(axmin)..+(axmax) * +(aymin)..sup */
        newmin = TaggedZero;
        newmax = axmax;
        break;
      case 0xb0: /* 0..sup * inf..-(aymax) */
      case 0xb1: /* 0..sup * inf..0 */
      case 0xb2: /* 0..sup * inf..+(aymax) */
      case 0xb3: /* 0..sup * inf..sup */
      case 0xb7: /* 0..sup * -(aymin)..sup */
      case 0xbb: /* 0..sup * 0..sup */
      case 0xbf: /* 0..sup * +(aymin)..sup */
      case 0xf0: /* +(axmin)..sup * inf..-(aymax) */
      case 0xf1: /* +(axmin)..sup * inf..0 */
      case 0xf2: /* +(axmin)..sup * inf..+(aymax) */
      case 0xf3: /* +(axmin)..sup * inf..sup */
      case 0xf7: /* +(axmin)..sup * -(aymin)..sup */
      case 0xfb: /* +(axmin)..sup * 0..sup */
      case 0xff: /* +(axmin)..sup * +(aymin)..sup */
        newmin = TaggedZero;
        newmax = Sup;
        break;
      case 0xba: /* 0..sup * 0..+(aymax) */
      case 0xbe: /* 0..sup * +(aymin)..+(aymax) */
      case 0xfa: /* +(axmin)..sup * 0..+(aymax) */
      case 0xfe: /* +(axmin)..sup * +(aymin)..+(aymax) */
        newmin = TaggedZero;
        newmax = safe_minus(aymax,MakeSmall(1));
        break;
      case 0xb4: /* 0..sup * -(aymin)..-(aymax) */
      case 0xb5: /* 0..sup * -(aymin)..0 */
      case 0xf4: /* +(axmin)..sup * -(aymin)..-(aymax) */
      case 0xf5: /* +(axmin)..sup * -(aymin)..0 */
        newmin = TaggedZero;
        newmax = safe_minus(aymin,MakeSmall(1));
        break;
      case 0xb6: /* 0..sup * -(aymin)..+(aymax) */
      case 0xf6: /* +(axmin)..sup * -(aymin)..+(aymax) */
        newmin = TaggedZero;
        newmax = safe_minus(safe_max(aymin,aymax),MakeSmall(1));
        break;
      case 0xaa: /* 0..+(axmax) * 0..+(aymax) */
      case 0xae: /* 0..+(axmax) * +(aymin)..+(aymax) */
      case 0xea: /* +(axmin)..+(axmax) * 0..+(aymax) */
      case 0xee: /* +(axmin)..+(axmax) * +(aymin)..+(aymax) */
        newmin = TaggedZero;
        newmax = safe_min(axmax,safe_minus(aymax,MakeSmall(1)));
        break;
      case 0xa4: /* 0..+(axmax) * -(aymin)..-(aymax) */
      case 0xa5: /* 0..+(axmax) * -(aymin)..0 */
      case 0xe4: /* +(axmin)..+(axmax) * -(aymin)..-(aymax) */
      case 0xe5: /* +(axmin)..+(axmax) * -(aymin)..0 */
        newmin = TaggedZero;
        newmax = safe_min(axmax,safe_minus(aymin,MakeSmall(1)));
        break;
      case 0xa6: /* 0..+(axmax) * -(aymin)..+(aymax) */
      case 0xe6: /* +(axmin)..+(axmax) * -(aymin)..+(aymax) */
        newmin = TaggedZero;
        newmax = safe_min(axmax,safe_minus(safe_max(aymin,aymax),MakeSmall(1)));
        break;
      case 0xa: /* inf..-(axmax) * 0..+(aymax) */
      case 0xe: /* inf..-(axmax) * +(aymin)..+(aymax) */
      case 0x1a: /* inf..0 * 0..+(aymax) */
      case 0x1e: /* inf..0 * +(aymin)..+(aymax) */
        newmin = safe_minus(MakeSmall(1),aymax);
        newmax = TaggedZero;
        break;
      case 0x3a: /* inf..sup * 0..+(aymax) */
      case 0x3e: /* inf..sup * +(aymin)..+(aymax) */
        newmax = safe_minus(aymax,MakeSmall(1));
        newmin = safe_negate(newmax);
        break;
      case 0x2a: /* inf..+(axmax) * 0..+(aymax) */
      case 0x2e: /* inf..+(axmax) * +(aymin)..+(aymax) */
        newmin = safe_minus(MakeSmall(1),aymax);
        newmax = safe_min(axmax,safe_minus(aymax,MakeSmall(1)));
        break;
      case 0x4: /* inf..-(axmax) * -(aymin)..-(aymax) */
      case 0x5: /* inf..-(axmax) * -(aymin)..0 */
      case 0x14: /* inf..0 * -(aymin)..-(aymax) */
      case 0x15: /* inf..0 * -(aymin)..0 */
        newmin = safe_minus(MakeSmall(1),aymin);
        newmax = TaggedZero;
        break;
      case 0x34: /* inf..sup * -(aymin)..-(aymax) */
      case 0x35: /* inf..sup * -(aymin)..0 */
        newmax = safe_minus(aymin,MakeSmall(1));
        newmin = safe_negate(newmax);
        break;
      case 0x24: /* inf..+(axmax) * -(aymin)..-(aymax) */
      case 0x25: /* inf..+(axmax) * -(aymin)..0 */
        newmin = safe_minus(MakeSmall(1),aymin);
        newmax = safe_min(axmax,safe_minus(aymin,MakeSmall(1)));
        break;
      case 0x6: /* inf..-(axmax) * -(aymin)..+(aymax) */
      case 0x16: /* inf..0 * -(aymin)..+(aymax) */
        newmin = safe_minus(MakeSmall(1),safe_max(aymin,aymax));
        newmax = TaggedZero;
        break;
      case 0x36: /* inf..sup * -(aymin)..+(aymax) */
        newmax = safe_minus(safe_max(aymin,aymax),MakeSmall(1));
        newmin = safe_negate(newmax);
        break;
      case 0x26: /* inf..+(axmax) * -(aymin)..+(aymax) */
        newmin = safe_minus(MakeSmall(1),safe_max(aymin,aymax));
        newmax = safe_min(axmax,safe_minus(safe_max(aymin,aymax),MakeSmall(1)));
        break;
      case 0x40: /* -(axmin)..-(axmax) * inf..-(aymax) */
      case 0x41: /* -(axmin)..-(axmax) * inf..0 */
      case 0x42: /* -(axmin)..-(axmax) * inf..+(aymax) */
      case 0x43: /* -(axmin)..-(axmax) * inf..sup */
      case 0x47: /* -(axmin)..-(axmax) * -(aymin)..sup */
      case 0x4b: /* -(axmin)..-(axmax) * 0..sup */
      case 0x4f: /* -(axmin)..-(axmax) * +(aymin)..sup */
      case 0x50: /* -(axmin)..0 * inf..-(aymax) */
      case 0x51: /* -(axmin)..0 * inf..0 */
      case 0x52: /* -(axmin)..0 * inf..+(aymax) */
      case 0x53: /* -(axmin)..0 * inf..sup */
      case 0x57: /* -(axmin)..0 * -(aymin)..sup */
      case 0x5b: /* -(axmin)..0 * 0..sup */
      case 0x5f: /* -(axmin)..0 * +(aymin)..sup */
        newmin = safe_negate_val(axmin);
        newmax = TaggedZero;
        break;
      case 0x60: /* -(axmin)..+(axmax) * inf..-(aymax) */
      case 0x61: /* -(axmin)..+(axmax) * inf..0 */
      case 0x62: /* -(axmin)..+(axmax) * inf..+(aymax) */
      case 0x63: /* -(axmin)..+(axmax) * inf..sup */
      case 0x67: /* -(axmin)..+(axmax) * -(aymin)..sup */
      case 0x6b: /* -(axmin)..+(axmax) * 0..sup */
      case 0x6f: /* -(axmin)..+(axmax) * +(aymin)..sup */
        newmin = safe_negate_val(axmin);
        newmax = axmax;
        break;
      case 0x70: /* -(axmin)..sup * inf..-(aymax) */
      case 0x71: /* -(axmin)..sup * inf..0 */
      case 0x72: /* -(axmin)..sup * inf..+(aymax) */
      case 0x73: /* -(axmin)..sup * inf..sup */
      case 0x77: /* -(axmin)..sup * -(aymin)..sup */
      case 0x7b: /* -(axmin)..sup * 0..sup */
      case 0x7f: /* -(axmin)..sup * +(aymin)..sup */
        newmin = safe_negate_val(axmin);
        newmax = Sup;
        break;
      case 0x0: /* inf..-(axmax) * inf..-(aymax) */
      case 0x1: /* inf..-(axmax) * inf..0 */
      case 0x2: /* inf..-(axmax) * inf..+(aymax) */
      case 0x3: /* inf..-(axmax) * inf..sup */
      case 0x7: /* inf..-(axmax) * -(aymin)..sup */
      case 0xb: /* inf..-(axmax) * 0..sup */
      case 0xf: /* inf..-(axmax) * +(aymin)..sup */
      case 0x10: /* inf..0 * inf..-(aymax) */
      case 0x11: /* inf..0 * inf..0 */
      case 0x12: /* inf..0 * inf..+(aymax) */
      case 0x13: /* inf..0 * inf..sup */
      case 0x17: /* inf..0 * -(aymin)..sup */
      case 0x1b: /* inf..0 * 0..sup */
      case 0x1f: /* inf..0 * +(aymin)..sup */
        newmin = Inf;
        newmax = TaggedZero;
        break;
      case 0x20: /* inf..+(axmax) * inf..-(aymax) */
      case 0x21: /* inf..+(axmax) * inf..0 */
      case 0x22: /* inf..+(axmax) * inf..+(aymax) */
      case 0x23: /* inf..+(axmax) * inf..sup */
      case 0x27: /* inf..+(axmax) * -(aymin)..sup */
      case 0x2b: /* inf..+(axmax) * 0..sup */
      case 0x2f: /* inf..+(axmax) * +(aymin)..sup */
        newmin = Inf;
        newmax = axmax;
        break;
      case 0x4a: /* -(axmin)..-(axmax) * 0..+(aymax) */
      case 0x4e: /* -(axmin)..-(axmax) * +(aymin)..+(aymax) */
      case 0x5a: /* -(axmin)..0 * 0..+(aymax) */
      case 0x5e: /* -(axmin)..0 * +(aymin)..+(aymax) */
        newmin = safe_negate(safe_min(axmin,safe_minus(aymax,MakeSmall(1))));
        newmax = TaggedZero;
        break;
      case 0x7a: /* -(axmin)..sup * 0..+(aymax) */
      case 0x7e: /* -(axmin)..sup * +(aymin)..+(aymax) */
        newmin = safe_negate(safe_min(axmin,safe_minus(aymax,MakeSmall(1))));
        newmax = safe_minus(aymax,MakeSmall(1));
        break;
      case 0x6a: /* -(axmin)..+(axmax) * 0..+(aymax) */
      case 0x6e: /* -(axmin)..+(axmax) * +(aymin)..+(aymax) */
        newmin = safe_negate(safe_min(axmin,safe_minus(aymax,MakeSmall(1))));
        newmax = safe_min(axmax,safe_minus(aymax,MakeSmall(1)));
        break;
      case 0x44: /* -(axmin)..-(axmax) * -(aymin)..-(aymax) */
      case 0x45: /* -(axmin)..-(axmax) * -(aymin)..0 */
      case 0x54: /* -(axmin)..0 * -(aymin)..-(aymax) */
      case 0x55: /* -(axmin)..0 * -(aymin)..0 */
        newmin = safe_negate(safe_min(axmin,safe_minus(aymin,MakeSmall(1))));
        newmax = TaggedZero;
        break;
      case 0x74: /* -(axmin)..sup * -(aymin)..-(aymax) */
      case 0x75: /* -(axmin)..sup * -(aymin)..0 */
        newmin = safe_negate(safe_min(axmin,safe_minus(aymin,MakeSmall(1))));
        newmax = safe_minus(aymin,MakeSmall(1));
        break;
      case 0x64: /* -(axmin)..+(axmax) * -(aymin)..-(aymax) */
      case 0x65: /* -(axmin)..+(axmax) * -(aymin)..0 */
        newmin = safe_negate(safe_min(axmin,safe_minus(aymin,MakeSmall(1))));
        newmax = safe_min(axmax,safe_minus(aymin,MakeSmall(1)));
        break;
      case 0x46: /* -(axmin)..-(axmax) * -(aymin)..+(aymax) */
      case 0x56: /* -(axmin)..0 * -(aymin)..+(aymax) */
        newmin = safe_negate(safe_min(axmin,safe_minus(safe_max(aymin,aymax),MakeSmall(1))));
        newmax = TaggedZero;
        break;
      case 0x76: /* -(axmin)..sup * -(aymin)..+(aymax) */
        newmin = safe_negate(safe_min(axmin,safe_minus(safe_max(aymin,aymax),MakeSmall(1))));
        newmax = safe_minus(safe_max(aymin,aymax),MakeSmall(1));
        break;
      case 0x66: /* -(axmin)..+(axmax) * -(aymin)..+(aymax) */
        newmin = safe_negate(safe_min(axmin,safe_minus(safe_max(aymin,aymax),MakeSmall(1))));
        newmax = safe_min(axmax,safe_minus(safe_max(aymin,aymax),MakeSmall(1)));
        break;
      default:
        continue;
      }
      if (dvar_fix_interval_t(dvz, newmin, newmax)<0) {
	if (newmin==Sup || newmax==Inf) /* can't represent bounds */
	  goto oflo;
	goto ret;
      }
      break;
    case 1:			/* (X,Z) => Y */
      axmin = (Tgez(xmin) ? xmin : Tminus(xmin));
      axmax = (Tgez(xmax) ? xmax : Tminus(xmax));
      azmin = (Tgez(zmin) ? zmin : Tminus(zmin));
      azmax = (Tgez(zmax) ? zmax : Tminus(zmax));
      compl = FALSE;
      switch (ARITH_DISPATCH(xmin,xmax,zmin,zmax)) {
      case 0xe: /* inf..-(axmax) * +(azmin)..+(azmax) */
      case 0xf: /* inf..-(axmax) * +(azmin)..sup */
      case 0x4e: /* -(axmin)..-(axmax) * +(azmin)..+(azmax) */
      case 0x4f: /* -(axmin)..-(axmax) * +(azmin)..sup */
      case 0x1e: /* inf..0 * +(azmin)..+(azmax) */
      case 0x1f: /* inf..0 * +(azmin)..sup */
      case 0x5e: /* -(axmin)..0 * +(azmin)..+(azmax) */
      case 0x5f: /* -(axmin)..0 * +(azmin)..sup */
      case 0x90: /* 0..0 * inf..-(azmax) */
      case 0x94: /* 0..0 * -(azmin)..-(azmax) */
      case 0x92: /* 0..0 * inf..+(azmax) */
      case 0x93: /* 0..0 * inf..sup */
      case 0x96: /* 0..0 * -(azmin)..+(azmax) */
      case 0x97: /* 0..0 * -(azmin)..sup */
      case 0x9e: /* 0..0 * +(azmin)..+(azmax) */
      case 0x9f: /* 0..0 * +(azmin)..sup */
      case 0xa0: /* 0..+(axmax) * inf..-(azmax) */
      case 0xa4: /* 0..+(axmax) * -(azmin)..-(azmax) */
      case 0xb0: /* 0..sup * inf..-(azmax) */
      case 0xb4: /* 0..sup * -(azmin)..-(azmax) */
      case 0xe0: /* +(axmin)..+(axmax) * inf..-(azmax) */
      case 0xe4: /* +(axmin)..+(axmax) * -(azmin)..-(azmax) */
      case 0xf0: /* +(axmin)..sup * inf..-(azmax) */
      case 0xf4: /* +(axmin)..sup * -(azmin)..-(azmax) */
        goto ret;
      case 0x0: /* inf..-(axmax) * inf..-(azmax) */
      case 0x4: /* inf..-(axmax) * -(azmin)..-(azmax) */
      case 0x40: /* -(axmin)..-(axmax) * inf..-(azmax) */
      case 0x44: /* -(axmin)..-(axmax) * -(azmin)..-(azmax) */
      case 0x10: /* inf..0 * inf..-(azmax) */
      case 0x14: /* inf..0 * -(azmin)..-(azmax) */
      case 0x50: /* -(axmin)..0 * inf..-(azmax) */
      case 0x54: /* -(axmin)..0 * -(azmin)..-(azmax) */
      case 0x20: /* inf..+(axmax) * inf..-(azmax) */
      case 0x24: /* inf..+(axmax) * -(azmin)..-(azmax) */
      case 0x30: /* inf..sup * inf..-(azmax) */
      case 0x34: /* inf..sup * -(azmin)..-(azmax) */
      case 0x60: /* -(axmin)..+(axmax) * inf..-(azmax) */
      case 0x64: /* -(axmin)..+(axmax) * -(azmin)..-(azmax) */
      case 0x70: /* -(axmin)..sup * inf..-(azmax) */
      case 0x74: /* -(axmin)..sup * -(azmin)..-(azmax) */
        compl = TRUE;
        newmax = azmax;
        newmin = safe_negate(newmax);
        break;
      case 0x2e: /* inf..+(axmax) * +(azmin)..+(azmax) */
      case 0x2f: /* inf..+(axmax) * +(azmin)..sup */
      case 0x3e: /* inf..sup * +(azmin)..+(azmax) */
      case 0x3f: /* inf..sup * +(azmin)..sup */
      case 0x6e: /* -(axmin)..+(axmax) * +(azmin)..+(azmax) */
      case 0x6f: /* -(axmin)..+(axmax) * +(azmin)..sup */
      case 0x7e: /* -(axmin)..sup * +(azmin)..+(azmax) */
      case 0x7f: /* -(axmin)..sup * +(azmin)..sup */
      case 0xae: /* 0..+(axmax) * +(azmin)..+(azmax) */
      case 0xaf: /* 0..+(axmax) * +(azmin)..sup */
      case 0xbe: /* 0..sup * +(azmin)..+(azmax) */
      case 0xbf: /* 0..sup * +(azmin)..sup */
      case 0xee: /* +(axmin)..+(axmax) * +(azmin)..+(azmax) */
      case 0xef: /* +(axmin)..+(axmax) * +(azmin)..sup */
      case 0xfe: /* +(axmin)..sup * +(azmin)..+(azmax) */
      case 0xff: /* +(axmin)..sup * +(azmin)..sup */
        compl = TRUE;
        newmax = azmin;
        newmin = safe_negate(newmax);
        break;
      default:
        continue;
      }
      if ((compl
	   ? dvar_prune_interval_t(dvy, newmin, newmax,w)
	   : dvar_fix_interval_t(dvy, newmin, newmax))<0) {
	if (newmin==Sup || newmax==Inf) /* can't represent bounds */
	  goto oflo;
	goto ret;
      }
      break;
    case 2:			/* (Y,Z) => X */
      if (FDlt(zmax,TaggedZero)) {
	if (dvar_fix_interval_t(dvx, Inf, TaggedZero)<0)
	  goto ret;
      } else if (FDgt(zmin,TaggedZero)) {
	if (dvar_fix_interval_t(dvx, TaggedZero, Sup)<0)
	  goto ret;
      }
      break;
    }
  if (dvar_is_integer(dvx) && dvar_is_integer(dvy)) {
    newmin = MakeSmall(GetSmall(xmin) % GetSmall(ymin));
    if (dvar_fix_value_t(dvz,newmin)<0)
      goto ret;
  } else if (dvar_is_integer(dvy)) {
    long k = GetSmall(ymin);
    if (TagIsSmall(xmin))
      while (!dvar_contains_value_t(dvz,MakeSmall(GetSmall(xmin) % k)))
	if (dvar_prune_value_t(dvx,xmin,w)<0)
	  goto ret;
    if (TagIsSmall(xmax))
      while (!dvar_contains_value_t(dvz,MakeSmall(GetSmall(xmax) % k)))
	if (dvar_prune_value_t(dvx,xmax,w)<0)
	  goto ret;
    if (TagIsSmall(xmin) && TagIsSmall(xmax) && (Tgtz(xmin) || Tltz(xmax))) {
      TAGGED amk = MakeSmall(GetSmall(xmin) % k);
      TAGGED bmk = MakeSmall(GetSmall(xmax) % k);
      TAGGED kmax = k>0 ? MakeSmall(k-1) : MakeSmall(-k-1);
      if (Tlt(xmax-xmin+TaggedZero,kmax) &&
	  Tle(amk,bmk) &&
	  dvar_fix_interval_t(dvz, amk, bmk)<0)
	goto ret;
    }
  }
  dvar_export(w,dvx);
  dvar_export(w,dvy);
  dvar_export(w,dvz);
  ent = (dvar_is_integer(dvx) &&
	 dvar_is_integer(dvy) &&
	 dvar_is_integer(dvz));
  goto ret;
 oflo:
  fd.fd_overflow = TRUE;
 ret:
  if (!fd_check_overflow(CTagToArg(X(1),2)))
    SP_fail();
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}
