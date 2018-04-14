/* Copyright(C) 2000, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

#if MULTI_SP_AWARE
#define eval(A1,A2,A3,A4) eval(HIDDEN_ARG, A1,A2,A3,A4)
#define eval_interval(A1,A2,A3,A4,A5) eval_interval(HIDDEN_ARG, A1,A2,A3,A4,A5)
#endif /* MULTI_SP_AWARE */

struct ac_linear_data {
  void (SPCDECL *destructor)(void*);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  SP_globref refbase;
  long stamp;			/* increases up to backtracking */
  int nvars;
  int nrelevant;		/* 1+index of last not fully supported var */
  long rhs;
  TAGGED singleton_zero;
  struct {
    Dvar dvar;
    long *coeff;
    TAGGED *min;	       /* min feasible sum up to and including this */
    TAGGED *max;	       /* max feasible sum up to and including this */
    TAGGED *failed;
    TAGGED *succeeded;
    TAGGED *unsupported;
  } term;
};

#define ATTRIBUTE_LOC(X) (pdata->refbase+4*(X))
#define FAILED_GLOBAL(X) RefGlob(pdata->refbase+4*(X)+2)
#define SUCCEEDED_GLOBAL(X) RefGlob(pdata->refbase+4*(X)+3)
#define DVAR(X) (pdata->term.dvar+(X))
#define COEFF(X) (pdata->term.coeff[X])
#define TMIN(X) (pdata->term.min[X])
#define TMAX(X) (pdata->term.max[X])
#define FAILED(X) (pdata->term.failed[X])
#define SUCCEEDED(X) (pdata->term.succeeded[X])
#define UNSUPPORTED(X) (pdata->term.unsupported[X])

static void SPCDECL ac_linear_destructor(void *pdata_v)
{
  struct ac_linear_data *pdata = (struct ac_linear_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  SP_free_globrefs(pdata->refbase,4*pdata->nvars);
  SP_free(pdata);
}

#if 0
static int
eval MAGIC (HIDDEN_PROTO
	    struct ac_linear_data *pdata,
	    Argdecl,
	    int term,
	    long sofar)
{
  int nvars = pdata->nvars;
  TAGGED tsofar = MakeSmall(sofar);
  Dvar dv = DVAR(term);
  long c = COEFF(term);
  long xmin, xmax;
  TAGGED tmin, tmax, new;
  
  if (term==nvars || fd_member(tsofar, SUCCEEDED(term)))
    return TRUE;
  if (fd_member(tsofar, FAILED(term)))
    return FALSE;
  
  if (c>0) {
    xmin = CEILDIV(GetSmall(TMIN(term))-sofar,c);
    xmax = FLOORDIV(GetSmall(TMAX(term))-sofar,c);
  } else {
    xmin = CEILDIV(-(GetSmall(TMAX(term))-sofar),-c);
    xmax = FLOORDIV(-(GetSmall(TMIN(term))-sofar),-c);
  }
  tmin = MakeSmall(xmin);
  tmax = MakeSmall(xmax);
  if (Tgt(tmin,tmax) ||
      dvar_compare_interval_t(dv,tmin,tmax,w)==FDI_DISJOINT) {
    goto fail;
  } else if (term==nvars-1) {	/* then we must have tmin==tmax */
    new = fd_interval(w,tmin,tmax);
  } else {
    FDITER it;
    FDCONS cons;
    fdcons_init(&cons);
    fditer_init(&it, dvar_set(dv,w));
    fditer_skip(&it, tmin-IStep(1));
    while (!fditer_empty(&it)) {
      TAGGED y = fditer_next(&it);
      if (Tgt(y,tmax))
	break;
      if (eval(pdata,w,term+1,sofar+c*GetSmall(y))) {
	fdcons_add(&cons,w,y);
	if (term >= pdata->nrelevant)
	  break;
      }
    }
    new = fdcons_set(&cons);
  }
  if (new==EmptySet) {
  fail:
    FAILED(term) = fd_union_interval(w,FAILED(term),tsofar,tsofar);
    return FALSE;
  } else {
    if (new!=EmptySet && fd_compare(UNSUPPORTED(term),new)!=FDI_DISJOINT) {
      TAGGED uns = fd_subtract(w,UNSUPPORTED(term),new);
      if (uns==EmptySet && term+1==pdata->nrelevant) {
	int i;
	pdata->nrelevant = 0;
	for (i=0; i<term; i++)
	  if (UNSUPPORTED(i)!=EmptySet)
	    pdata->nrelevant = i+1;
      }
      UNSUPPORTED(term) = uns;
    }
    SUCCEEDED(term) = fd_union_interval(w,SUCCEEDED(term),tsofar,tsofar);
    return TRUE;
  }
}
#endif

static TAGGED
eval_interval MAGIC (HIDDEN_PROTO
		     struct ac_linear_data *pdata,
		     Argdecl,
		     int term,
		     TAGGED b1,
		     TAGGED e1)
{
  Dvar dv = DVAR(term);
  long c = COEFF(term);
  long xmin, xmax;
  TAGGED tmin, tmax, new, values, set1, set2;

  if (term==pdata->nvars)
    return pdata->singleton_zero;
  else if (term>0 && term >= pdata->nrelevant && TMIN(term-1)==TMAX(term-1))
    return fd_interval(w,TMIN(term-1),TMIN(term-1));
  else if (dvar_is_integer(dv)) {
    TAGGED shift = MakeSmall(c*dvar_min_l(dv));
    return fd_lsh(w,eval_interval(pdata,w,term+1,TADD(b1,shift),TADD(e1,shift)),Tminus(shift));
  }
  set1 = fd_interval(w,b1,e1);
  new = EmptySet;
  values = fd_intersection(w,SUCCEEDED(term),set1);
  set2 = fd_subtract(w,set1,values);
  set2 = fd_subtract(w,set2,FAILED(term));

  if (TMIN(term)==TMAX(term) && c == 1) {
    new = fd_intersection(w,dvar_set(dv,w),fd_neg_offset(w,set2,TMIN(term)));
    values = fd_union(w,values,fd_neg_offset(w,new,TMIN(term)));
  } else if (TMIN(term)==TMAX(term) && c == -1) {
    new = fd_intersection(w,dvar_set(dv,w),fd_lsh(w,set2,Tminus(TMIN(term))));
    values = fd_union(w,values,fd_lsh(w,new,TMIN(term)));
  } else {
    while (set2!=EmptySet) {
      TAGGED r2 = CTagToCar(set2);
      TAGGED b2 = RangeMin(r2);
      TAGGED e2 = RangeMax(r2);
      set2 = CTagToCdr(set2);
      if (c>0) {
	xmin = CEILDIV(GetSmall(TMIN(term))-GetSmall(e2),c);
	xmax = FLOORDIV(GetSmall(TMAX(term))-GetSmall(b2),c);
      } else {
	xmin = CEILDIV(-(GetSmall(TMAX(term))-GetSmall(b2)),-c);
	xmax = FLOORDIV(-(GetSmall(TMIN(term))-GetSmall(e2)),-c);
      }
      tmin = MakeSmall(xmin);
      tmax = MakeSmall(xmax);
      if (Tle(tmin,tmax) &&
	  dvar_compare_interval_t(dv,tmin,tmax,w)!=FDI_DISJOINT) {
	if (c == 1) {
	  TAGGED set3 = fd_intersection_interval(w,dvar_set(dv,w),tmin,tmax);
	  while (set3!=EmptySet) {
	    TAGGED r3 = CTagToCar(set3);
	    TAGGED b3 = RangeMin(r3);
	    TAGGED e3 = RangeMax(r3);
	    TAGGED b5 = Tgt(TADD(b2,b3),TMIN(term)) ? TADD(b2,b3) : TMIN(term);
	    TAGGED e5 = Tlt(TADD(e2,e3),TMAX(term)) ? TADD(e2,e3) : TMAX(term);
	    TAGGED set4;
	    set3 = CTagToCdr(set3);
	    if (term >= pdata->nrelevant &&
		fd_compare_interval(values,b2,e2)<=FDI_SUPERSET) /* b2..e2 included in values */
	      break;
	    set4 = eval_interval(pdata,w,term+1, b5, e5); 
	    while (set4!=EmptySet) {
	      TAGGED r4 = CTagToCar(set4);
	      TAGGED b4 = RangeMin(r4);
	      TAGGED e4 = RangeMax(r4);
	      TAGGED a = Tgt(TSUB(b4,e3),b2) ? TSUB(b4,e3) : b2;
	      TAGGED b = Tlt(TSUB(e4,b3),e2) ? TSUB(e4,b3) : e2;
	      TAGGED c = Tgt(TSUB(b4,e2),b3) ? TSUB(b4,e2) : b3;
	      TAGGED d = Tlt(TSUB(e4,b2),e3) ? TSUB(e4,b2) : e3;
	      set4 = CTagToCdr(set4);
	      if (Tle(a,b))
		values = fd_union_interval(w,values,a,b);
	      if (Tle(c,d))
		new = fd_union_interval(w,new,c,d);
	    }
	  }
	} else if (c == -1) {
	  TAGGED set3 = fd_intersection_interval(w,dvar_set(dv,w),tmin,tmax);
	  while (set3!=EmptySet) {
	    TAGGED r3 = CTagToCar(set3);
	    TAGGED b3 = RangeMin(r3);
	    TAGGED e3 = RangeMax(r3);
	    TAGGED b5 = Tgt(TSUB(b2,e3),TMIN(term)) ? TSUB(b2,e3) : TMIN(term);
	    TAGGED e5 = Tlt(TSUB(e2,b3),TMAX(term)) ? TSUB(e2,b3) : TMAX(term);
	    TAGGED set4;
	    set3 = CTagToCdr(set3);
	    if (term >= pdata->nrelevant &&
		fd_compare_interval(values,b2,e2)<=FDI_SUPERSET) /* b2..e2 included in values */
	      break;
	    set4 = eval_interval(pdata,w,term+1, b5, e5);
	    while (set4!=EmptySet) {
	      TAGGED r4 = CTagToCar(set4);
	      TAGGED b4 = RangeMin(r4);
	      TAGGED e4 = RangeMax(r4);
	      TAGGED a = Tgt(TADD(b4,b3),b2) ? TADD(b4,b3) : b2;
	      TAGGED b = Tlt(TADD(e4,e3),e2) ? TADD(e4,e3) : e2;
	      TAGGED c = Tgt(TSUB(b2,e4),b3) ? TSUB(b2,e4) : b3;
	      TAGGED d = Tlt(TSUB(e2,b4),e3) ? TSUB(e2,b4) : e3;
	      set4 = CTagToCdr(set4);
	      if (Tle(a,b))
		values = fd_union_interval(w,values,a,b);
	      if (Tle(c,d))
		new = fd_union_interval(w,new,c,d);
	    }
	  }
	} else {
	  FDITER it;
	  fditer_init(&it, dvar_set(dv,w));
	  fditer_skip(&it, tmin-IStep(1));
	  while (!fditer_empty(&it)) {
	    TAGGED y = fditer_next(&it);
	    TAGGED tciy = MakeSmall(c*GetSmall(y));
	    if (Tgt(y,tmax)) {
	      break;
	    } else if (term >= pdata->nrelevant &&
		       fd_compare_interval(values,b2,e2)<=FDI_SUPERSET) { /* b2..e2 included in values */
	      break;
	    } else if (TMIN(term) == TMAX(term)) {
	      TAGGED v = TSUB(TMIN(term),tciy);
	      new = fd_insert_into(w,y,new);
	      values = fd_insert_into(w,v,values);
	    } else {
	      TAGGED b5 = Tgt(TADD(b2,tciy),TMIN(term)) ? TADD(b2,tciy) : TMIN(term);
	      TAGGED e5 = Tlt(TADD(e2,tciy),TMAX(term)) ? TADD(e2,tciy) : TMAX(term);
	      TAGGED set4 = eval_interval(pdata,w,term+1, b5, e5);
	      if (set4!=EmptySet)
		new = fd_insert_into(w,y,new);
	      while (set4!=EmptySet) {
		TAGGED r4 = CTagToCar(set4);
		TAGGED b4 = RangeMin(r4);
		TAGGED e4 = RangeMax(r4);
		set4 = CTagToCdr(set4);
		values = fd_union_interval(w,values,TSUB(b4,tciy),TSUB(e4,tciy));
	      }
	    }
	  }
	}
      }
    }
  }
  if (new!=EmptySet && fd_compare(UNSUPPORTED(term),new)!=FDI_DISJOINT) {
    TAGGED uns = fd_subtract(w,UNSUPPORTED(term),new);
    if (uns==EmptySet && term+1==pdata->nrelevant) {
      int i;
      pdata->nrelevant = 0;
      for (i=0; i<term; i++)
	if (UNSUPPORTED(i)!=EmptySet)
	  pdata->nrelevant = i+1;
    }
    UNSUPPORTED(term) = uns;
  }
  SUCCEEDED(term) = fd_union(w,SUCCEEDED(term),values);
  FAILED(term) = fd_union(w,FAILED(term),fd_subtract(w,set1,values));
  return values;
}

/*
   '$fd_ac_linear'(+State0, -State, -Actions) :-
   State0 = State = state([f(Coeff,Xvar,Xattr,FailedMutable)...],NVars,RHS,NTargets,_Handle,Stamp)
 */
void SPCDECL
prolog_fd_ac_linear MAGIC (HIDDEN_PROTO
			   SP_term_ref State0,
			   SP_term_ref State,
			   SP_term_ref Actions)
{
  WAMENV;
  struct ac_linear_data *pdata;
  int i, nvars;
  int ent = -1;
  TAGGED list, item, tmp, handle;
  SP_BOOL committed;
  char *ptr;
  long state_stamp, min, max;
  
  w->numstack_end = NULL;

  /*    X(0) = RefTerm(State0); */
  (void)State0;
  dvar_export_start(w);
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct ac_linear_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tmp,X(0),2);
    nvars = GetSmall(tmp);
    pdata = Palloc(struct ac_linear_data,
		   nvars*sizeof(struct dvar)+
		   6*nvars*sizeof(long),
		   handle);
    pdata->destructor = ac_linear_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(4*nvars);
    pdata->nvars = nvars;
    ptr = (char *)(pdata+1);
    pdata->term.dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->term.coeff = (long *)ptr;
    ptr += nvars*sizeof(long);
    pdata->term.min = (TAGGED *)ptr;
    ptr += nvars*sizeof(TAGGED);
    pdata->term.max = (TAGGED *)ptr;
    ptr += nvars*sizeof(TAGGED);
    pdata->term.failed = (TAGGED *)ptr;
    ptr += nvars*sizeof(TAGGED);
    pdata->term.succeeded = (TAGGED *)ptr;
    ptr += nvars*sizeof(TAGGED);
    pdata->term.unsupported = (TAGGED *)ptr;
    ptr += nvars*sizeof(TAGGED);

    DerefArg(tmp,X(0),3);
    pdata->rhs = GetSmall(tmp);
    DerefArg(list,X(0),1);
    for (i=0; i<nvars; i++) {
      SP_globref ref = ATTRIBUTE_LOC(i);
      DerefCar(item,list);
      DerefCdr(list,list);
      DerefArg(tmp,item,1);
      COEFF(i) = GetSmall(tmp);
      get_var_and_attr(item+WD(1), ref);
      DerefArg(tmp,item,4);
      FAILED_GLOBAL(i) = tmp;
    }
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
  }
  
  /* RESUME */
  pdata->stamp = state_stamp+1;
  pdata->nrelevant = 0;
  for (i=0; i < nvars; i++) {
    SP_globref ref = ATTRIBUTE_LOC(i);
    Dvar dv = DVAR(i);
    
    dvar_init(dv, ref, ref+1);
    if (!dvar_is_integer(dv)) {
      FAILED(i) = RefMutable(FAILED_GLOBAL(i));
      SUCCEEDED(i) = EmptySet;
      UNSUPPORTED(i) = dvar_set(dv,w);
      pdata->nrelevant = i+1;
    }
  }
  min = max = 0;
  for (i=nvars-1; i>=0; i--) {
    Dvar dv = DVAR(i);
    long c = COEFF(i);
    TMIN(i) = MakeSmall(min);
    TMAX(i) = MakeSmall(max);
    if (c>0) {
      min -= c*dvar_max_l(dv);
      max -= c*dvar_min_l(dv);
    } else {
      min -= c*dvar_min_l(dv);
      max -= c*dvar_max_l(dv);
    }
  }
  if (min>-pdata->rhs || max<-pdata->rhs)
    goto ret;
  min = max = -pdata->rhs;
  for (i=0; i<nvars; i++) {
    Dvar dv = DVAR(i);
    long c = COEFF(i);
    TAGGED tmin, tmax;
    if (c<0) {
      min += c*dvar_max_l(dv);
      max += c*dvar_min_l(dv);
    } else {
      min += c*dvar_min_l(dv);
      max += c*dvar_max_l(dv);
    }
    tmin = MakeSmall(min);
    tmax = MakeSmall(max);
    if (Tlt(TMIN(i),tmin))
      TMIN(i) = tmin;
    if (Tgt(TMAX(i),tmax))
      TMAX(i) = tmax;
  }
/*   if (!eval(pdata,w,0,-pdata->rhs)) */
/*     goto ret; */
  pdata->singleton_zero = fd_interval(w,TaggedZero,TaggedZero);
  if (eval_interval(pdata,w,0,MakeSmall(-pdata->rhs),MakeSmall(-pdata->rhs))==EmptySet)
    goto ret;

  /* compute pruning acc. to generalization; get rid of heap ptrs */
  for (i=0; i<nvars; i++) {
    Dvar dv = DVAR(i);
    if (!dvar_is_integer(dv)) {
      TAGGED f = FAILED(i);
      if (f!=EmptySet && !OnHeap(f))
	FAILED(i) = fd_localize(w,f);
      else
	FAILED(i) = EmptySet;
      if (i < pdata->nrelevant) {
	dvar_prune_set(dv, UNSUPPORTED(i),w);
	dvar_pruning_done(w,dv);
      }
    }
  }

  /* export pruning */
  ent = 0;
  for (i=0; i < nvars; i++) {
    Dvar dv = DVAR(i);
    if (!dvar_is_integer(dv)) {
      ent++;
      if (FAILED(i)!=EmptySet) {
	tmp = fd_globalize(w,FAILED(i),0,3);
	update_mutable(w, tmp, FAILED_GLOBAL(i));
      }
    }
    if (i < pdata->nrelevant)
      dvar_export(w,dv);
  }
  ent = (ent<=1);
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}
