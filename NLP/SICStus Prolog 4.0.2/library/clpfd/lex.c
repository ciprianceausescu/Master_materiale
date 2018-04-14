/* Copyright(C) 2002, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

#undef alpha                    /* [PM] 3.10 used as variable name in this file but a pre-processor macro on ALPHA platforms... */

#if MULTI_SP_AWARE
#define lex_chain_pair(A1,A2,A3,A4) lex_chain_pair(HIDDEN_ARG, A1,A2,A3,A4)
#define repair_atleast(A1,A2,A3,A4,A5,A6,A7,A8,A9) repair_atleast(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7,A8,A9)
#define repair_among(A1,A2,A3,A4,A5,A6) repair_among(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define among_case(A1,A2,A3,A4,A5,A6) among_case(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define prune_among(A1,A2,A3,A4,A5,A6,A7) prune_among(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7)
#define prune_among_det(A1,A2,A3,A4,A5,A6,A7) prune_among_det(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7)
#define prune_among_force(A1,A2,A3,A4,A5) prune_among_force(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define prune_between(A1,A2,A3,A4,A5,A6,A7) prune_between(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7)
#define entailed_between(A1,A2,A3,A4,A5) entailed_between(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define entailed_among(A1,A2,A3,A4) entailed_among(HIDDEN_ARG, A1,A2,A3,A4)
#define prunable_among(A1,A2,A3,A4,A5) prunable_among(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define compute_limit(A1,A2,A3,A4,A5,A6,A7) compute_limit(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7)
#define adjust_vector_up(A1,A2,A3,A4,A5,A6) adjust_vector_up(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define adjust_vector_down(A1,A2,A3,A4,A5,A6) adjust_vector_down(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#endif /* MULTI_SP_AWARE */

struct lexc_data {
  void (SPCDECL *destructor)(void *);
  void (SPCDECL *daemon)(HIDDEN_PROTO
			 Argtype,void *,SP_globref,TAGGED); /* (w,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  long stamp;			/* increases up to backtracking */
  int flags;
  int ntuples;
  int nvars;
  Dvar xvar;
  TAGGED among_spec;
  long *lb;
  long *ub;
  long *min_l;
  long *max_l;
  int *state;			/* 0x1 = tuple[i] OP tuple[i+1] entailed */
  int *stack;
  int *limit;			/* cached value of compute_limit */
  int top;
};

#define RefCAttr(T,V) (pdata->refbase + ((nvars*(T) + (V))<<1))
#define CVAR(T,V)     (pdata->xvar    + nvars*(T) + (V))
#define LB(T,V)       (pdata->lb[       nvars*(T) + (V)])
#define UB(T,V)       (pdata->ub[       nvars*(T) + (V)])

static void SPCDECL lexc_destructor(void *pdata_v)
{
  struct lexc_data *pdata = (struct lexc_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->ntuples*pdata->nvars<<1);
  SP_free(pdata);
}

enum cmpenum {
  cmp_l=1,
  cmp_e,
  cmp_le,
  cmp_g,
#if EAGER_ENTAILMENT
  cmp_lg,			/* too expensive */
#endif
  cmp_ge,
  cmp_lge
};

#define DECODE_SPEC(SPEC,L,U,SET)		\
DerefArg((SET),(SPEC),1);			\
(L) = GetSmall(SET);				\
DerefArg((SET),(SPEC),2);			\
(U) = GetSmall(SET);				\
DerefArg((SET),(SPEC),3);			\


static enum cmpenum dvar_cmp(Dvar dv1,Dvar dv2)
{
  long max_min = dvar_max_l(dv1) - dvar_min_l(dv2);
  long min_max = dvar_min_l(dv1) - dvar_max_l(dv2);

  if (dvar_is_integer(dv1) && dvar_is_integer(dv2))
    return (max_min<0 ? cmp_l : max_min>0 ? cmp_g : cmp_e);
  else if (dvar_is_alias(dv1,dv2))
    return cmp_e;
  else if (max_min<=0)		/* l,e,le */
    return (max_min<0 ? cmp_l :
	    min_max<0 ? cmp_le : cmp_e);
  else				/* g,ge,lge */
    return (min_max>0 ? cmp_g :
	    min_max==0 ? cmp_ge : cmp_lge);
}

static SP_BOOL lex_chain_leqc(struct lexc_data *pdata,
			   Dvar xv, Dvar yv, int c)
{
  int rc1 = dvar_fix_min_t(yv,dvar_min_t(xv)+IStep(c));
  int rc2 = dvar_fix_max_t(xv,dvar_max_t(yv)-IStep(c));

  if (rc1<0 || rc2<0)
    return FALSE;
  if (rc1+rc2>0)
    pdata->stack[pdata->top++] = xv-pdata->xvar;
  return TRUE;
}

static int lex_chain_pair MAGIC (HIDDEN_PROTO
				 Argdecl,
				 struct lexc_data *pdata,
				 int i,
				 int mode)
{
  int ent=-1, iptr=0, jptr, kptr, c=0;
  int nvars = pdata->nvars;
  Dvar xvar = CVAR(i,0);
  Dvar yvar = CVAR(i+1,0);
  SP_globref xref = RefCAttr(i,0);
  SP_globref yref = RefCAttr(i+1,0);
  Dvar xv, yv;
  
 state1:
  if (iptr==nvars) {
    if (mode&1)
      goto ret_fail;
    else
      goto ret_succeed;
  }
  xv = xvar+iptr;
  yv = yvar+iptr;
  dvar_init(xv, xref+(iptr<<1), xref+(iptr<<1)+1);
  dvar_init(yv, yref+(iptr<<1), yref+(iptr<<1)+1);
  switch (dvar_cmp(xv,yv)) {
  case cmp_g:
    goto ret_fail;
  case cmp_ge:
    if (!lex_chain_leqc(pdata,xv,yv,0))
      goto ret_fail;
  case cmp_e:
    iptr++;
    goto state1;
#if EAGER_ENTAILMENT
  case cmp_lg:
    goto ret_xi_lt_yi;
#endif
  case cmp_l:
    goto ret_succeed;
  case cmp_lge:
    if (!lex_chain_leqc(pdata,xv,yv,0))
      goto ret_fail;
  case cmp_le:
    goto state2;
  }
 state2:
  jptr = iptr+1;
 state2b:
  if (jptr==nvars) {
    if (mode&1)
      goto ret_xi_lt_yi;
    else
      goto ret_xi_le_yi;
  }
  xv = xvar+jptr;
  yv = yvar+jptr;
  dvar_init(xv, xref+(jptr<<1), xref+(jptr<<1)+1);
  dvar_init(yv, yref+(jptr<<1), yref+(jptr<<1)+1);
  switch (dvar_cmp(xv,yv)) {
  case cmp_g:
    goto ret_xi_lt_yi;
  case cmp_l:
    goto ret_xi_le_yi;
  case cmp_e:
    jptr++;
    goto state2b;
  case cmp_le:
    goto state3;
  case cmp_ge:
    goto state4;
  default: /* cmp_lg, cmp_lge */;
    mode = (mode&0x1) + 2;
    ent = 0;
    goto ret;
  }
 state3:
  kptr = jptr+1;
 state3b:
  if (kptr==nvars) {
    if (mode&1)
      goto delay3;
    else
      goto ret_xi_le_yi;
  }
  xv = xvar+kptr;
  yv = yvar+kptr;
  dvar_init(xv, xref+(kptr<<1), xref+(kptr<<1)+1);
  dvar_init(yv, yref+(kptr<<1), yref+(kptr<<1)+1);
  switch (dvar_cmp(xv,yv)) {
  case cmp_l:
    goto ret_xi_le_yi;
  case cmp_e:
  case cmp_le:
    kptr++;
    goto state3b;
  default: /* cmp_g, cmp_ge, cmp_lg, cmp_lge */;
  delay3:
    mode = (mode&0x1) + 4;
    ent = 0;
    goto ret;
  }
 state4:
  kptr = jptr+1;
 state4b:
  if (kptr==nvars) {
    if (mode&1)
      goto ret_xi_lt_yi;
    else
      goto delay4;
  }
  xv = xvar+kptr;
  yv = yvar+kptr;
  dvar_init(xv, xref+(kptr<<1), xref+(kptr<<1)+1);
  dvar_init(yv, yref+(kptr<<1), yref+(kptr<<1)+1);
  switch (dvar_cmp(xv,yv)) {
  case cmp_g:
    goto ret_xi_lt_yi;
  case cmp_e:
  case cmp_ge:
    kptr++;
    goto state4b;
  default: /* cmp_l, cmp_le, cmp_lg, cmp_lge */;
  delay4:
    mode = (mode&0x1) + 6;
    ent = 0;
    goto ret;
  }
 ret_xi_lt_yi:
  c = -1;
 ret_xi_le_yi:
  xv = xvar+iptr;
  yv = yvar+iptr;
  if (!lex_chain_leqc(pdata,xv,yv,c))
    goto ret_fail;
  if (dvar_max_l(xv) > dvar_min_l(yv)+c)
    dvar_export_leqc(w,xv,yv,c);
 ret_succeed:
  ent = 1;
 ret:
 ret_fail:
  return ent;
}

static SP_BOOL need_bounds(struct lexc_data *pdata, int i, int flags)
{
  if (flags&0x6)
    return TRUE;
  else if (pdata->state[i]>0)
    return (i>=2 && pdata->state[i-2]==0 && pdata->state[i-1]==0);
  else
    return ((i+1<pdata->ntuples && pdata->state[i+1]==0) ||
	    (i>=1 && pdata->state[i-1]==0));
}

static int
repair_atleast MAGIC (HIDDEN_PROTO
		      struct lexc_data *pdata,
		      Argdecl,
		      int l, int u,
		      int countval,
		      TAGGED valueset,
		      SP_BOOL ubp,
		      long *vec,
		      Dvar var)
{
  int j, countvar=0, alpha, slack, nforced;
  int nvars = pdata->nvars;
  int rc = 0;
      
  for (j=nvars-1; j>=0; j--) {
    TAGGED elt = MakeSmall(vec[j]);
    int dom_intersects =
      (dvar_compare_set(var+j,valueset,w) != FDI_DISJOINT);
    
    if (fd_member(elt,valueset))
      countval--;
    if (dom_intersects)
      countvar++;
    if (countval+countvar>=l &&
	(ubp ? Tgt(elt,dvar_min_t(var+j)) : Tlt(elt,dvar_max_t(var+j)))) {
      if (countval+countvar==l && dom_intersects) {
	if (fd_compare((ubp ? fd_intersection_interval(w,dvar_set(var+j,w),Inf,FDdecr(elt))
			    : fd_intersection_interval(w,dvar_set(var+j,w),FDincr(elt),Sup)),
		       valueset) != FDI_DISJOINT)
	  break;
      } else
	break;
    }
  }
  if (j<0)
    return 2;
  alpha = j;
  slack = countval+countvar-l;
  nforced = 0;
  for (j=nvars-1; j>=alpha; j--) {
    pdata->stack[j] = nforced;
    switch (dvar_compare_set(var+j,valueset,w)) {
    case FDI_EQUAL:
    case FDI_SUBSET:
      nforced++;
    }
  }
  for (j=alpha; j<nvars; j++) {
    TAGGED elt = MakeSmall(vec[j]);
    int dom_intersects =
      (dvar_compare_set(var+j,valueset,w) != FDI_DISJOINT);
    
    if (j==alpha) {
      if (slack==0 && dom_intersects)
	elt = ubp ? fd_intersection_max(fd_intersection_interval(w,dvar_set(var+j,w),Inf,FDdecr(elt)),
			       valueset)
	          : fd_intersection_min(fd_intersection_interval(w,dvar_set(var+j,w),FDincr(elt),Sup),
			       valueset);
      else
	elt = ubp ? dvar_predecessor_t(var+j,elt)
	          : dvar_successor_t(var+j,elt);
    } else {
      if (slack==0 && dom_intersects)
	elt = ubp ? fd_intersection_max(dvar_set(var+j,w),valueset)
	          : fd_intersection_min(dvar_set(var+j,w),valueset);
      else if (countval+pdata->stack[j]==u)
	/* TODO: fd_subtract_{max,min} */
	elt = ubp ? fd_intersection_max(dvar_set(var+j,w),fd_complement(w,valueset))
	          : fd_intersection_min(dvar_set(var+j,w),fd_complement(w,valueset));
      else
	elt = ubp ? dvar_max_t(var+j)
	          : dvar_min_t(var+j);
    }
    if (vec[j] != GetSmall(elt))
      rc |= 1;
    vec[j] = GetSmall(elt);
    if (fd_member(elt,valueset))
      countval++;
    else if (dom_intersects)
      slack--;
  }
  return rc;
}

static SP_BOOL
repair_among MAGIC (HIDDEN_PROTO
		    struct lexc_data *pdata,
		    Argdecl,
		    int flags,
		    SP_BOOL ubp,
		    long *vec,
		    Dvar var)
{
  int nvars = pdata->nvars;
  int rc;
  int j, k;

  do {
    rc = 0;
    if (flags&0x2) {
      int l, u, countval=0;
      TAGGED valueset;

      DECODE_SPEC(pdata->among_spec,l,u,valueset);
      for (j=nvars-1; j>=0; j--) {
	TAGGED elt = MakeSmall(vec[j]);
	if (fd_member(elt,valueset))
	  countval++;
      }
      if (countval<l)
	rc |= repair_atleast(pdata,w,l,u,countval,valueset,ubp,vec,var);
      else if (countval>u)
	rc |= repair_atleast(pdata,w,nvars-u,nvars-l,nvars-countval,
			     fd_complement(w,valueset),ubp,vec,var);
    }
    if (flags&0x4) {	/* propagate chain of #< */
      long *min_l = pdata->min_l;
      long *max_l = pdata->max_l;

      for (j=0; j<nvars; j++) {
	min_l[j] = dvar_min_l(var+j);
	if (j>0 && min_l[j]<=min_l[j-1])
	  min_l[j] = dvar_successor_l(var+j,min_l[j-1]);
      }
      for (j=nvars-1; j>=0; j--) {
	max_l[j] = dvar_max_l(var+j);
	if (j<nvars-1 && max_l[j]>=max_l[j+1])
	  max_l[j] = dvar_predecessor_l(var+j,max_l[j+1]);
      }
      if (ubp) {
	for (j=0; j<nvars && rc<2; ) {
	  if (j==0) {
	    if (vec[j]>=min_l[j])
	      j++;
	    else
	      rc |= 2;
	  } else {
	    if (vec[j]>=min_l[j] && vec[j]>vec[j-1]) {
	      j++;
	    } else {
	      j--;
	      rc |= 1;
	      vec[j] = dvar_predecessor_l(var+j,vec[j]);
	      for (k=j+1; k<nvars; k++)
		vec[k] = max_l[k];
	    }
	  }
	}
      } else {
	for (j=0; j<nvars && rc<2; ) {
	  if (j==0) {
	    if (vec[j]<=max_l[j])
	      j++;
	    else
	      rc |= 2;
	  } else {
	    if (vec[j]<=vec[j-1]) {
	      rc |= 1;
	      vec[j] = dvar_successor_l(var+j,vec[j-1]);
	      for (k=j+1; k<nvars; k++)
		vec[k] = min_l[k];
	    }
	    if (vec[j]<=max_l[j]) {
	      j++;
	    } else {
	      j--;
	      rc |= 1;
	      vec[j] = dvar_successor_l(var+j,vec[j]);
	      for (k=j+1; k<nvars; k++)
		vec[k] = min_l[k];
	    }
	  }
	}
      }
    }
  } while (rc==1 && (flags&0x6)==0x6);
  return (rc<2);
}

/* Compute d as the lexicographically smallest vector d such that
   lex_le(c,d) and d[i] in dom(X[i]) for all i and among(...) holds.
   If lt!=0, lex_lt(c,d) must hold.
   Returns -1 for failure, 0 for no change, 1 for change.
*/
static int
adjust_vector_up MAGIC (HIDDEN_PROTO
			struct lexc_data *pdata,
			Argdecl,
			int flags,
			long *c,
			long *d,
			Dvar var)
{
  int j, alpha = -1;
  int nvars = pdata->nvars;
  long prevlb;
      
  /* compute alpha = msp that we have to increment */
  j = 0;
  prevlb = c[j];
  while (j<nvars && dvar_contains_value_l(var+j,prevlb)) {
    if (prevlb < dvar_max_l(var+j))
      alpha = j;
    j++;
    prevlb = c[j];
  }
  if ((j<nvars && prevlb < dvar_max_l(var+j)) || (j==nvars && !(flags&0x1)))
    alpha = j;
  if (alpha<0)
    return -1;
  for (j=0; j<nvars; j++)
    d[j] = (j<alpha ? c[j] :
	    j>alpha ? dvar_min_l(var+j) :
	    dvar_successor_l(var+j,c[j]));
  if ((flags&0x6) &&
      !repair_among(pdata,w,flags,FALSE,d,var))
    return -1;
  return (alpha<nvars);
}

/* Compute d as the lexicographically greatest vector d such that
   lex_le(d,c) and d[i] in dom(X[i]) for all i and among(...) holds.
   If lt!=0, lex_lt(d,c) must hold.
   Returns -1 for failure, 0 for no change, 1 for change.
*/
static int
adjust_vector_down MAGIC (HIDDEN_PROTO
			  struct lexc_data *pdata,
			  Argdecl,
			  int flags,
			  long *c,
			  long *d,
			  Dvar var)
{
  int j, alpha = -1;
  int nvars = pdata->nvars;
  long prevub;
      
  /* compute alpha = msp that we have to increment */
  j = 0;
  prevub = c[j];
  while (j<nvars && dvar_contains_value_l(var+j,prevub)) {
    if (prevub > dvar_min_l(var+j))
      alpha = j;
    j++;
    prevub = c[j];
  }
  if ((j<nvars && prevub > dvar_min_l(var+j)) || (j==nvars && !(flags&0x1)))
    alpha = j;
  if (alpha<0)
    return -1;
  for (j=0; j<nvars; j++)
    d[j] = (j<alpha ? c[j] :
	    j>alpha ? dvar_max_l(var+j) :
	    dvar_predecessor_l(var+j,c[j]));
  if ((flags&0x6) &&
      !repair_among(pdata,w,flags,TRUE,d,var))
    return -1;
  return (alpha<nvars);
}

/* Compute largest limit such that either X[0,limit) = LB[0,limit) or
   X[0,limit) = UB[0,limit) must hold.
*/
static int 
compute_limit MAGIC (HIDDEN_PROTO
		     struct lexc_data *pdata,
		     Argdecl,
		     long *lbrow,
		     long *ubrow,
		     int inf,
		     int flags,
		     Dvar var)
{
  int nvars = pdata->nvars;
  int sup = nvars;
  int j, mid;

  if (!(flags&0x2)) {
    if (inf==nvars || dvar_successor_l(var+inf,lbrow[inf])<ubrow[inf])
      return inf;
    inf++;
    while (inf<nvars &&
	   dvar_min_l(var+inf)==ubrow[inf] &&
	   dvar_max_l(var+inf)==lbrow[inf])
      inf++;
  } else {			/* among(L,U,Set) */
    long *c = Malloc(nvars,long);
    while (inf<sup) {
      mid = (inf+sup)>>1;
      for (j=0; j<=mid; j++)
	c[j] = lbrow[j];
      c[mid]++;			/* increment last pos. by one */
      for (j=mid+1; j<nvars; j++)
	c[j] = dvar_min_l(var+j);
      adjust_vector_up(pdata,w,(flags&0x6),c,c,var); /* LT off */
      for (j=0; j<=mid; j++)
	if (c[j]!=ubrow[j])
	  break;
      if (j>mid)
	inf = mid+1;
      else
	sup = mid;
    }
    Free(c);
  }
  return inf;
}

#define CASE_MAYBE_GE 0
#define CASE_MAYBE_LE 1
#define CASE_MAYBE_GT 2
#define CASE_MAYBE_LT 3
#define CASE_MAYBE_ANY 4
#define CASE_INOUT_GE 5
#define CASE_INOUT_LE 6
#define CASE_INOUT_ANY 7

/* Case analysis for Prop. 1 & 2 */
static int
among_case MAGIC (HIDDEN_PROTO
		  Argdecl,
		  int i,
		  long *bound,
		  SP_BOOL ubp,
		  TAGGED valueset,
		  Dvar var)
{
  long min_of_intersection;
  long max_of_intersection;
  
  if (ubp) {
    switch (dvar_compare_set(var+i,valueset,w)) {
    case FDI_DISJOINT:
    case FDI_EQUAL:
    case FDI_SUBSET:
      if (dvar_min_l(var+i)==bound[i])
	return CASE_INOUT_GE;
      else
	return CASE_INOUT_ANY;
    }
    min_of_intersection = GetSmall(fd_intersection_min(dvar_set(var+i,w),valueset));
    if (min_of_intersection==bound[i])
      return CASE_MAYBE_GE;
    else if (min_of_intersection>bound[i])
      return CASE_MAYBE_GT;
    else
      return CASE_MAYBE_ANY;
  } else {
    switch (dvar_compare_set(var+i,valueset,w)) {
    case FDI_DISJOINT:
    case FDI_EQUAL:
    case FDI_SUBSET:
      if (dvar_max_l(var+i)==bound[i])
	return CASE_INOUT_LE;
      else
	return CASE_INOUT_ANY;
    }
    max_of_intersection = GetSmall(fd_intersection_max(dvar_set(var+i,w),valueset));
    if (max_of_intersection==bound[i])
      return CASE_MAYBE_LE;
    else if (max_of_intersection<bound[i])
      return CASE_MAYBE_LT;
    else
      return CASE_MAYBE_ANY;
  }
}

static SP_BOOL
prune_among_force MAGIC (HIDDEN_PROTO
			 struct lexc_data *pdata,
			 Argdecl,
			 int limit,
			 TAGGED valueset,
			 Dvar var)
{
  int nvars = pdata->nvars;
  SP_BOOL pruned = FALSE;
  int j;

  for (j=limit; j<nvars; j++) {
    switch (dvar_compare_set(var+j,valueset,w)) {
    case FDI_DISJOINT:
    case FDI_EQUAL:
    case FDI_SUBSET:
      break;
    default:
      dvar_fix_set(var+j,valueset,w);
      pruned = TRUE;
    }
  }
  return pruned;
}

static void
prune_among_det MAGIC (HIDDEN_PROTO
		       struct lexc_data *pdata,
		       Argdecl,
		       long *lbrow,
		       long *ubrow,
		       int limit, /* first nonground */
		       int flags,
		       Dvar var)
{
  int least, most, j;
  TAGGED valueset, complement;
  int nvars = pdata->nvars;
  int nb_in[2];
  int nb_out[2];
  int can_force_in, can_force_out;
  /* [0] - counts dom(X[i]) vs. valueset, i<limit
   * [1] - counts dom(X[i]) vs. valueset, i>=limit
   */

  DECODE_SPEC(pdata->among_spec,least,most,valueset);
  complement = fd_complement(w,valueset);
  nb_in[0] = nb_out[0] = 0;

  for (j=0; j<limit; j++) {
    int lbin = fd_member(MakeSmall(lbrow[j]),valueset);
    
    nb_in[0] += lbin;
    nb_out[0] += 1-lbin;
  }
 restart:
  nb_in[1] = nb_out[1] = 0;
  for (j=limit; j<nvars; j++) {
    switch (dvar_compare_set(var+j,valueset,w)) {
    case FDI_EQUAL:
    case FDI_SUBSET:
      nb_in[1]++;
      break;
    case FDI_DISJOINT:
      nb_out[1]++;
      break;
    }
  }

  can_force_in = (nb_out[0]+nb_out[1] == nvars-least-1);
  can_force_out = (nb_in[0]+nb_in[1] == most-1);
  if (nb_in[1]+nb_out[1] == nvars-limit) { /* no further pruning possible */
  } else if (nb_out[0]+nb_out[1] == nvars-least) {
    /* force all "maybe" to be "in" */
    prune_among_force(pdata,w,limit,valueset,var);
  } else if (nb_in[0]+nb_in[1] == most) {
    /* force all "maybe" to be "out" */
    prune_among_force(pdata,w,limit,complement,var);
  } else if (can_force_in||can_force_out) {
    int l, rule;

    /* Proposition 1&2&3, four variants */
    for (rule=0; rule<4; rule++) {
      long *bound = (rule&2) ? lbrow : ubrow;
      TAGGED set =  (rule&1) ? complement : valueset;
      SP_BOOL cond =   (rule&1) ? can_force_out : can_force_in;
      int gtpos = -1;
      int token = CASE_MAYBE_GE; /* [PM] 3.10 shut up compiler, init value will not be used */
      
      if (cond) {
	for (l=limit; l+1<nvars; l++) {
	  token = among_case(w,l,bound,(rule<2),set,var);
	  switch (token) {
	  case CASE_MAYBE_GE:
	  case CASE_MAYBE_LE:
	  case CASE_INOUT_GE:
	  case CASE_INOUT_LE:
	    continue;
	  case CASE_MAYBE_GT:
	  case CASE_MAYBE_LT:
	    gtpos = l;
	  }
	  break;
	}
	if (l+1<nvars && (token==CASE_MAYBE_GT || token==CASE_MAYBE_LT)) {
	  if (prune_among_force(pdata,w,l+1,set,var))
	    goto restart;
	}
	for (j=limit; j<=l; j++) {
	  TAGGED feasible;
	  
	  switch (dvar_compare_set(var+j,set,w)) {
	  case FDI_DISJOINT:
	  case FDI_EQUAL:
	  case FDI_SUBSET:
	    continue;
	  }
	  feasible =
	    (rule&2) ? fd_union_interval(w,set,MakeSmall(lbrow[j]+(j<gtpos)),Sup) :
	               fd_union_interval(w,set,Inf,MakeSmall(ubrow[j]-(j<gtpos)));
	  switch (dvar_compare_set(var+j,feasible,w)) {
	  case FDI_EQUAL:
	  case FDI_SUBSET:
	    continue;
	  }
	  dvar_fix_set(var+j,feasible,w);
	  goto restart;
	}
      }
    }
    /* Proposition 4, two variants */
    if (limit+1>=nvars || lbrow[limit+1]<=ubrow[limit+1]+1)
      goto prop5;
    switch (dvar_compare_set(var+limit,valueset,w)) {
    case FDI_DISJOINT:
    case FDI_EQUAL:
    case FDI_SUBSET:
      goto prop5;
    }
    switch (dvar_compare_set(var+limit+1,valueset,w)) {
    case FDI_DISJOINT:
    case FDI_EQUAL:
    case FDI_SUBSET:
      goto prop5;
    }
    for (rule=0; rule<2; rule++) {
      TAGGED set =  (rule&1) ? complement : valueset;
      TAGGED cset = (rule&1) ? valueset : complement;
      SP_BOOL cond =   (rule&1) ? can_force_out : can_force_in;
      TAGGED subset;

      if (cond) {
	subset = fd_subtract(w,dvar_set(var+limit,w),
			     fd_pair(w,MakeSmall(lbrow[limit]),
				     MakeSmall(ubrow[limit])));
	if (fd_compare(subset,set)==FDI_DISJOINT &&
	    dvar_prune_set(var+limit+1,
			   fd_intersection_interval(w,cset,MakeSmall(ubrow[limit+1]+1),
					   MakeSmall(lbrow[limit+1]-1)),w)!=0)
	  goto restart;
      }
    }
  prop5:
    /* if forcing all but one maybes to IN (OUT) causes contradiction,
       then at least one of the maybes must be OUT (IN) */
    for (rule=0; rule<2; rule++) {
      TAGGED set =  (rule&1) ? complement : valueset;
      SP_BOOL cond =   (rule&1) ? can_force_out : can_force_in;

      if (cond) {
	Dvar var2 = Malloc(nvars,struct dvar);
	long *bound = Malloc(nvars,long);
	int maybes = nvars-nb_out[0]-nb_out[1]-nb_in[0]-nb_in[1];
	int pos = 0;
        SP_BOOL force = FALSE;

	for (j=0; j<limit; j++) {
	  TAGGED t = MakeSmall(lbrow[j]);
	  
	  (var2+j)->min = t;
	  (var2+j)->max = t;
	  (var2+j)->set = fd_pair(w,t,t);
	  (var2+j)->flags = DV_PRUNED_VAL|DV_INTERVAL|DV_SET_OK;
	}
	for (j=limit; j<nvars; j++) {
	  dvar_assign(var2+j,var+j,w);
	  switch (dvar_compare_set(var2+j,set,w)) {
	  case FDI_DISJOINT:
	  case FDI_EQUAL:
	  case FDI_SUBSET:
	    continue;
	  }
	  if (maybes>1)
	    dvar_fix_set(var2+j,set,w);
	  else
	    pos = j;
	  maybes--;
	}
	if (adjust_vector_up(pdata,w,(flags&0x4),lbrow,bound,var2)<0) /* LT off, AMONG off */
	  force = TRUE;
	for (j=limit; j<nvars && !force; j++) {
	  if (bound[j]<ubrow[j])
	    break;
	  else if (bound[j]>ubrow[j])
	    force = TRUE;
	}
	Free(var2);
	Free(bound);
	if (force) {
	  dvar_fix_set(var+pos,set,w);
	  goto restart;
	}
      }
    }
  }
}

/* Filtering for among(L,U,Set) applied to row Row.
   We also know that either X[0,limit) = LB[0,limit)
   or X[0,limit) = UB[0,limit) holds.
*/
static void
prune_among MAGIC (HIDDEN_PROTO
		   struct lexc_data *pdata,
		   Argdecl,
		   long *lbrow,
		   long *ubrow,
		   int limit,
		   int flags,
		   Dvar var)
{
  int j, k, rc;
  int l;			/* first nonground pos. */
  int nvars = pdata->nvars;

  do {
    rc = 0;
    if (flags&0x2) {
      for (l=0; l<nvars; l++)
	if (!dvar_is_integer(var+l))
	  break;
      if (limit<=l) {
	prune_among_det(pdata,w,lbrow,ubrow,l,flags,var);
      } else if (limit<nvars) {
	Dvar var_a = Malloc(2*nvars,struct dvar);
	Dvar var_b = var_a+nvars;
	long *bound = Malloc(nvars,long);
	int l;

	for (j=0; j<limit; j++) {
	  dvar_assign(var_a+j,var+j,w);
	  dvar_fix_value_l(var_a+j,lbrow[j]);
	  bound[j] = lbrow[j];
	}
	l = limit;
	for (j=limit; j<nvars; j++) {
	  dvar_assign(var_a+j,var+j,w);
	  if (j==l) {
	    dvar_fix_interval_t(var_a+j,MakeSmall(lbrow[j]),Sup);
	    if (dvar_is_integer(var_a+j))
	      l++;
	  }
	  bound[j] = dvar_max_l(var+j);
	}
	repair_among(pdata,w,flags,TRUE,bound,var_a);
	prune_among_det(pdata,w,lbrow,bound,l,flags,var_a);
	for (j=0; j<limit; j++) {
	  dvar_assign(var_b+j,var+j,w);
	  dvar_fix_value_l(var_b+j,lbrow[j]);
	  bound[j] = ubrow[j];
	}
	l = limit;
	for (j=limit; j<nvars; j++) {
	  dvar_assign(var_b+j,var+j,w);
	  if (j==l) {
	    dvar_fix_interval_t(var_b+j,Inf,MakeSmall(ubrow[j]));
	    if (dvar_is_integer(var_b+j))
	      l++;
	  }
	  bound[j] = dvar_min_l(var+j);
	}
	repair_among(pdata,w,flags,FALSE,bound,var_b);
	prune_among_det(pdata,w,bound,ubrow,l,flags,var_b);
	for (j=limit; j<nvars; j++)
	  dvar_fix_set(var+j,fd_union(w,dvar_set(var_a+j,w),dvar_set(var_b+j,w)),w);
	Free(var_a);
	Free(bound);
      }
    }
    if (flags&0x4) {	/* propagate chain of #< */
      long *min_l = pdata->min_l;
      for (j=nvars-2; j>=0; j--) {
	TAGGED b = dvar_max_t(var+j+1);
	rc |= dvar_fix_max_t(var+j,FDdecr(b));
      }
      for (j=1; j<nvars; j++) {
	TAGGED b = dvar_min_t(var+j-1);
	rc |= dvar_fix_min_t(var+j,FDincr(b));
      }
      /* special rule based on "either take LB or must increase" */
      for (j=nvars-1; j>=0; j--)
	min_l[j] = lbrow[j];
      for (j=0; j<nvars-1; j++) {
	if (!dvar_is_integer(var+j)) { /* assume we increase var+j */
	  long lb = -CLPFD_MAXINT;
	  for (k=0; k<nvars; k++) {
	    if (k<j)
	      lb = dvar_min_l(var+k);
	    else if (k==j)
	      lb = dvar_successor_l(var+k,dvar_min_l(var+k));
	    else
	      lb = dvar_successor_l(var+k,lb);
	    if (min_l[k] > lb)
	      min_l[k] = lb;
	  }
	}
      }
      for (j=nvars-1; j>=1; j--) {
	rc |= dvar_fix_min_l(var+j,min_l[j]);
      }
    }
  } while (rc && (flags&0x6)==0x6);
}


/* Require: lbrow[*] and ubrow[*] are _feasible_ bounds. */
static void
prune_between MAGIC (HIDDEN_PROTO
		     struct lexc_data *pdata,
		     Argdecl,
		     long *lbrow,
		     long *ubrow,
		     int flags,
		     int row,
		     Dvar var)
{
  int nvars   = pdata->nvars;
  int j;

  for (j=0; j<nvars && lbrow[j]==ubrow[j]; j++)
    dvar_fix_value_l(var+j,lbrow[j]);
  if (j<nvars)
    dvar_fix_interval_l(var+j,lbrow[j],ubrow[j]);
  pdata->limit[row] = compute_limit(pdata,w,lbrow,ubrow,j,flags,var);
  for (; j<pdata->limit[row]; j++)
    dvar_fix_set(var+j,fd_pair(w,MakeSmall(lbrow[j]),MakeSmall(ubrow[j])),w);
  if (j<nvars && ubrow[j]+1<=lbrow[j]-1)
    dvar_prune_interval_l(var+j,ubrow[j]+1,lbrow[j]-1,w);
  if (flags&0x6)
    prune_among(pdata,w,lbrow,ubrow,pdata->limit[row],flags,var);
}

static SP_BOOL
entailed_among MAGIC (HIDDEN_PROTO
		      struct lexc_data *pdata,
		      Argdecl,
		      int flags,
		      Dvar var)
{
  int nvars = pdata->nvars;
  int j;
  SP_BOOL ent = TRUE;

  if (flags&0x2) {
    int nb_in=0;
    int nb_out=0;
    int least, most;
    TAGGED valueset;
    DECODE_SPEC(pdata->among_spec,least,most,valueset);
    for (j=0; j<nvars; j++)
      switch (dvar_compare_set(var+j,valueset,w)) {
      case FDI_EQUAL:
      case FDI_SUBSET:
	nb_in++;
	break;
      case FDI_DISJOINT:
	nb_out++;
	break;
      }
    ent &= (nb_in>=least && nvars-nb_out<=most);
  }
  if (flags&0x4) {
    for (j=0; j<nvars-1 && ent; j++)
      if (Tge(dvar_max_t(var+j),dvar_min_t(var+j+1)))
	ent = FALSE;
  }
  return ent;
}

static SP_BOOL
prunable_among MAGIC (HIDDEN_PROTO
		      struct lexc_data *pdata,
		      Argdecl,
		      int limit,
		      int flags,
		      Dvar var)
{
  int least, most, j, l;
  TAGGED valueset;
  int nvars = pdata->nvars;
  int nb_in=0, nb_out=0;
  SP_BOOL prunable = FALSE;

  if (flags&0x2) {
    for (l=0; l<nvars; l++)
      if (!dvar_is_integer(var+l))
	break;
    if (limit>l)
      return TRUE;
    DECODE_SPEC(pdata->among_spec,least,most,valueset);
    for (j=limit; j<nvars; j++) {
      switch (dvar_compare_set(var+j,valueset,w)) {
      case FDI_EQUAL:
      case FDI_SUBSET:
	nb_in++;
	break;
      case FDI_DISJOINT:
	nb_out++;
	break;
      }
    }
    prunable |= (nb_out>=nvars-least-1 || nb_in>=most-1);
  }
  if (flags&0x4) {
    for (j=0; j<nvars-1 && !prunable; j++)
      if (Tge(dvar_min_t(var+j),dvar_min_t(var+j+1)) ||
	  Tge(dvar_max_t(var+j),dvar_max_t(var+j+1)))
	prunable = TRUE;
  }
  return prunable;
}

static void SPCDECL 
lexc_daemon MAGIC (HIDDEN_PROTO
		   Argdecl,
		   void *vdata,
		   SP_globref attr_ref,
		   TAGGED global)
{
  struct lexc_data *pdata = (struct lexc_data *)vdata;
  TAGGED tstate;
  int ar, state_stamp;

  if (RefMutable(CTagToArg(global,3)) & IStep(1)) /* STATUS: enqueued */
    return;
  tstate = RefMutable(CTagToArg(global,1));
  ar = Arity(TagToHeadfunctor(tstate));
  state_stamp = GetSmall(CTagToArg(tstate,ar));
  if (pdata->stamp==state_stamp) { /* incremental */
    /* valid: state, lb, ub */
    int ntuples = pdata->ntuples;
    int nvars = pdata->nvars;
    int refoffset = (attr_ref-pdata->refbase)>>1;
    int row = refoffset/nvars;
    int elt = refoffset%nvars;
    int flags = pdata->flags;
    int j;

    pdata->among_spec = CTagToArg(tstate,5);

    /* no incremental handling of lex_chain_pair yet */

    if (need_bounds(pdata,row,flags)) {
      Dvar var = CVAR(row,0);
      SP_globref ref = RefCAttr(row,0);
      
      dvar_init(var+elt, attr_ref, attr_ref+1);
      if (!dvar_contains_value_l(var+elt,LB(row,elt)) ||
	  !dvar_contains_value_l(var+elt,UB(row,elt)))
	goto push;
      if (flags&0x6) {
	for (j=0; j<nvars; j++) {
	  if (j!=elt)
	    dvar_init(var+j, ref, ref+1);
	  ref += 2;
	}
	if (prunable_among(pdata,w,pdata->limit[row],flags,var))
	  goto push;
      }
    } else if ((row<ntuples-1 &&
		pdata->state[row]==0 && pdata->state[row+1]>0) ||
	       (row>0 &&
		pdata->state[row-1]==0 && pdata->state[row]>0))
      goto push;
    return;
  }
 push:
  fd_enqueue_global(w, global, 0x7/* DOM, append*/);   
}

/*
  '$fd_lex_chain'(+State0, +State, -Actions).
  State = state(Tuples,NT,NV,Flags,Among,EntFlags,Handle,Stamp)
  |EntFlags| = NT
  EntFlags[i]==1 iff Tuples[i] OP Tuples[i+1] is entailed.
  If EntFlags[i]==1, Tuples[i] is _separated_ from Tuples[i+1].
  (Flags & 0x1) means lexicographic (strict) less than.
  (Flags & 0x2), Among=among(L,U,Vals) means each vector must take
  at least L and at most U values out of Vals.
  (Flags & 0x4) means each vector strictly increasing.

  We handle three cases:
  1) Tuple i is separated from both i-1 and i+1.
     Then tuple i is ignored altogether.
  2) Tuples i and i+1 are not separated, but are separated from i-1 and i+2.
     Then tuples i and i+1 are handled by the tuple-pair algorithm.
  3) Groups of 3 or more nonseparated tuples are handled by:
     a) compute feasible upper bounds
     b) compute feasible lower bounds
     c) prune wrt. both bounds
*/
void SPCDECL
prolog_fd_lex_chain MAGIC (HIDDEN_PROTO
			   SP_term_ref State0,
			   SP_term_ref State,
			   SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, trow, tmat;
  int ntuples, nvars, nelts, i, j, flags;
  struct lexc_data *pdata;
  SP_BOOL committed, post;
  long state_stamp;
  char *ptr;
  WAMENV;

/*    X(0) = RefTerm(State0); */
  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */
  w->numstack_end = NULL;
  dvar_export_start(w);
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    post = FALSE;
    pdata = Pdata(struct lexc_data,handle);
    ntuples = pdata->ntuples;
    nvars = pdata->nvars;
    nelts = ntuples*nvars;
  } else {			/* build persistent state */
    post = TRUE;
    DerefArg(telt,X(0),2);	/* get Ntuples */
    ntuples = GetSmall(telt);
    DerefArg(telt,X(0),3);	/* get Nvars */
    nvars = GetSmall(telt);
    nelts = ntuples*nvars;
    pdata = Palloc(struct lexc_data,
		   nelts*(sizeof(struct dvar)+2*sizeof(long)+sizeof(int)) +
		   2*nvars*sizeof(long) +
		   2*ntuples*sizeof(int),
		   handle);
    pdata->destructor = lexc_destructor;
    pdata->daemon = lexc_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nelts<<1);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    DerefArg(telt,X(0),4);	/* get Flags */
    pdata->flags = GetSmall(telt);
    pdata->ntuples = ntuples;
    pdata->nvars = nvars;
    ptr = (char *)(pdata+1);
    pdata->xvar = (Dvar)ptr;
    ptr += nelts*sizeof(struct dvar);
    pdata->lb = (long *)ptr;
    ptr += nelts*sizeof(long);
    pdata->ub = (long *)ptr;
    ptr += nelts*sizeof(long);
    pdata->min_l = (long *)ptr;
    ptr += nvars*sizeof(long);
    pdata->max_l = (long *)ptr;
    ptr += nvars*sizeof(long);
    pdata->stack = (int *)ptr;
    ptr += nelts*sizeof(int);
    pdata->state = (int *)ptr;
    ptr += ntuples*sizeof(int);
    pdata->limit = (int *)ptr;
    ptr += ntuples*sizeof(int);
    {
      SP_globref ref = pdata->refbase;
      
      DerefArg(tmat,X(0),1);	/* get X */
      for (i=0; i<ntuples; i++) {
	DerefCar(trow,tmat);
	DerefCdr(tmat,tmat);
	for (j=0; j<nvars; j++) {
	  DerefCar(telt,trow);
	  DerefCdr(trow,trow);
	  get_var_and_attr(telt,ref);
	  ref += 2;
	}
      }
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */

  flags = pdata->flags;
  pdata->top = 0;
  pdata->stamp = state_stamp+1;
  DerefArg(trow,X(0),6);	/* get EntFlags */
  for (i=0; i<ntuples-1; i++) {
    DerefCar(telt,trow);
    DerefCdr(trow,trow);
    pdata->state[i] = !IsVar(telt);
  }
  pdata->state[i] = 1;
  pdata->among_spec = CTagToArg(X(0),5);

  for (i=0; i<ntuples; i++)
    if (need_bounds(pdata,i,flags)) {
      SP_globref ref = RefCAttr(i,0);
      Dvar xv = CVAR(i,0);
      for (j=0; j<nvars; j++) {
	dvar_init(xv+j, ref, ref+1);
	ref += 2;
      }
    }

  /* compute all upper bounds */
  
  for (i=ntuples-1; i>=0; i--) {
    if (!need_bounds(pdata,i,flags)) {
    } else if (pdata->state[i]>0) {
      for (j=nvars-1; j>=0; j--)
	UB(i,j) = dvar_max_l(CVAR(i,j));
      if ((flags&0x6) &&
	  !repair_among(pdata,w,flags,TRUE,&UB(i,0),CVAR(i,0)))
	goto ret_fail;
    } else if (adjust_vector_down(pdata,w,flags,&UB(i+1,0),&UB(i,0),CVAR(i,0))<0)
      goto ret_fail;
  }

  /* compute all lower bounds */
  
  for (i=0; i<ntuples; i++) {
    if (!need_bounds(pdata,i,flags)) {
    } else if (i==0 || pdata->state[i-1]>0) {
      for (j=nvars-1; j>=0; j--)
	LB(i,j) = dvar_min_l(CVAR(i,j));
      if ((flags&0x6) &&
	  !repair_among(pdata,w,flags,FALSE,&LB(i,0),CVAR(i,0)))
	goto ret_fail;
    } else if (adjust_vector_up(pdata,w,flags,&LB(i-1,0),&LB(i,0),CVAR(i,0))<0)
      goto ret_fail;
  }
  
  /* prune all tuples wrt. lower and upper bounds */

  for (i=0; i<ntuples; i++)
    if (need_bounds(pdata,i,flags)) {
      Dvar xvar = pdata->xvar + i*nvars;
      long *lbrow = pdata->lb + i*nvars;
      long *ubrow = pdata->ub + i*nvars;
      int l=nvars;		/* first nonground pos. */

      prune_between(pdata,w,lbrow,ubrow,flags,i,xvar);
      for (j=0; j<nvars; j++) {
	if (l==nvars && !dvar_is_integer(xvar+j))
	  l = j;
      }
      for (j=nvars-1; j>=0; j--)
	dvar_pruning_done(w,xvar+j);
      continue;
    }
    else if (pdata->state[i]==0) {
      switch (lex_chain_pair(w,pdata,i,(flags&0x1))) {
      case -1:
	goto ret_fail;
      case  1:
	pdata->state[i] = 1;
      }
    }

  /* export prunings */
  
  for (i=0; i<ntuples; i++)
    if (need_bounds(pdata,i,flags)) {
      Dvar xv = CVAR(i,0);
      for (j=nvars-1; j>=0; j--)
	dvar_export(w,xv+j);
    }
  i = pdata->top;
  while (i>0) {
    j = pdata->stack[--i];
    dvar_export(w,pdata->xvar+j);
    dvar_export(w,pdata->xvar+nvars+j);
  }

  /* update states */

  ent = 1;
  DerefArg(trow,X(0),6);	/* get Flags */
  for (i=0; i<ntuples-1; i++) {
    DerefCar(telt,trow);
    DerefCdr(trow,trow);
    if (pdata->state[i]==0) {
      for (j=0; j<nvars; j++) {
	long cmp = dvar_max_l(CVAR(i,j))-dvar_min_l(CVAR(i+1,j));
	if (cmp<0)
	  goto sep;
	if (cmp>0)
	  goto nosep;
      }
      if (!(flags&0x1))
	goto sep;
    nosep:			/* tuple i can be lex gt tuple i+1  */
      ent = 0;
      continue;
    }
  sep:				/* tuple i can't be lex gt tuple i+1  */
    if (IsVar(telt)) {
      LetShadowGlobal;
      BindHVA(telt,TaggedOne);
    }
  }
  if (ent==1 && (flags&0x6)) {	/* check entailment of among as well */
    for (i=0; i<ntuples && ent==1; i++)
      ent = entailed_among(pdata,w,flags,CVAR(i,0));
  }
  if (ent==0 && post) {
    SP_globref ref = pdata->refbase;
    Dvar var = pdata->xvar;
    for (j=0; j<nelts; j++) {
      dvar_init(var+j, ref, ref+1);
      dvar_attach_daemon(w, var+j, pdata, X(1), fd.functor_dom);
      ref += 2;
    }
  }

 ret_fail:
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}

