/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

#if MULTI_SP_AWARE
#define tell_eq(A1,A2,A3,A4) tell_eq(HIDDEN_ARG, A1,A2,A3,A4)
#define negdom(A1,A2) negdom(HIDDEN_ARG, A1,A2)
#endif /* MULTI_SP_AWARE */

static int tell_eq MAGIC (HIDDEN_PROTO
			  Argdecl,
			  Dvar dvx, Dvar dvy,
			  int intersecting)
{
  TAGGED inters;
  TAGGED x = RefGlob(dvx->var_ref);
  TAGGED y = RefGlob(dvy->var_ref);
  
  switch (intersecting) {
  case FDI_DISJOINT:
    return -1;
  case FDI_EQUAL:
    if (!IsVar(x))
      return 1;
    else {
      goto equate;
    }
  case FDI_SUBSET:
    if (!IsVar(x)) {
      dvar_fix_value_t(dvy, x);
      return 1;
    } else {
      goto equate;
    }
  case FDI_SUPERSET:
    if (!IsVar(y)) {
      dvar_fix_value_t(dvx, y);
      return 1;
    } else {
      goto equate;
    }
  default:
    inters = fd_intersection(w,dvar_set(dvx,w),dvar_set(dvy,w));
    if (fd_singleton(inters)) {
      TAGGED min = fd_min(inters);
      if (IsVar(x))
	dvar_fix_value_t(dvx, min);
      if (IsVar(y))
	dvar_fix_value_t(dvy, min);
      return 1;
    } else {
    equate:
      dvar_export_equal(w,dvx, dvy);
      return 1;
    }
  }
}

struct arith_data {
  void (SPCDECL *destructor)(void *);
  void (SPCDECL *daemon)(HIDDEN_PROTO
			 Argtype,void *,SP_globref,TAGGED); /* (w,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  Dvar dvar;
  int nrefs;
};

static void SPCDECL arith_destructor(void *pdata_v)
{
  struct arith_data *pdata = (struct arith_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

/* '$fd_in_set_iff'(+State, -NewState, -Actions) :- X in_set Set iff B.
   State is f(X,XMut,Set,B,BMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_in_set_iff MAGIC (HIDDEN_PROTO
			    SP_term_ref State,
			    SP_term_ref NewState,
			    SP_term_ref Actions)
{
  WAMENV;
  TAGGED set;
  int intersecting;
  TAGGED handle;
  SP_BOOL committed;
  struct arith_data *pdata;
  Dvar dvx, dvb;
  int ent = 0;		/* neither entailed nor dis- */

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
    pdata->nrefs = 5;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(5);
    pdata->dvar = (Dvar)(pdata+1);
    get_var_and_attr(X(0),pdata->refbase); /* get X */
    get_var_and_attr(X(0)+WD(3),pdata->refbase+2); /* get B */
    DerefArg(set,X(0),3);	/* get Set */
    RefGlob(pdata->refbase+4) = set;
  }

  /* RESUME HERE */
  dvx = pdata->dvar;
  dvb = dvx+1;
  dvar_init(dvx, pdata->refbase,   pdata->refbase+1);
  dvar_init(dvb, pdata->refbase+2, pdata->refbase+3);
  set = RefGlob(pdata->refbase+4);
  if (set==EmptySet)
    intersecting = FDI_DISJOINT;
  else
    intersecting = dvar_compare_set(dvx,set,w);
  if (!dvar_is_integer(dvb)) {
    switch (intersecting) {
    case FDI_SUBSET:		/* [B=1,exit] */
    case FDI_EQUAL:		/* [B=1,exit] */
      ent = 1;
      dvar_fix_value_t(dvb, TaggedOne);
      break;
    case FDI_DISJOINT:		/* [B=0,exit] */
      ent = 1;
      dvar_fix_value_t(dvb, TaggedZero);
      break;
    }
  } else if (Teqz(dvar_min_t(dvb))) {
    if (!dvar_is_integer(dvx)) { /* [X in_set \set,exit] */
      switch (intersecting) {
      case FDI_SUBSET:
      case FDI_EQUAL:
	ent = -1; break;
      case FDI_DISJOINT:
	ent = 1; break;
      default:
	ent = 1;
	dvar_prune_set(dvx, set,w);
      }
    } else if (fd_member(dvar_min_t(dvx),set)) {
      ent = -1;			/* [fail] */
    } else {
      ent = 1;			/* [exit] */
    }
  } else {
    if (!dvar_is_integer(dvx)) { /* [X in_set set,exit] */
      switch (intersecting) {
      case FDI_SUBSET:
      case FDI_EQUAL:
	ent = 1; break;
      case FDI_DISJOINT:
	ent = -1; break;
      default:
	ent = 1;
	dvar_fix_set(dvx, set,w);
      }
    } else if (fd_member(dvar_min_t(dvx),set)) {
      ent = 1; /* [exit] */
    } else {
      ent = -1; /* [fail] */
    }
  }
  dvar_pruning_done(w,dvx);
  dvar_pruning_done(w,dvb);
  dvar_export(w,dvx);
  dvar_export(w,dvb);
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}

static void SPCDECL 
eq_iff_daemon MAGIC (HIDDEN_PROTO
		     Argdecl,
		     void *vdata,
		     SP_globref attr_ref,
		     TAGGED global)
{
  struct arith_data *pdata = (struct arith_data *)vdata;
  SP_globref refbase = pdata->refbase;
  int mask = 0;
  TAGGED bvar;

  (void)attr_ref;
  bvar = RefGlob(refbase+1);
  DerefSwitch(bvar,mask|=0x1;);	/* is X var? */
  bvar = RefGlob(refbase+3);
  DerefSwitch(bvar,mask|=0x2;);	/* is Y var? */
  bvar = RefGlob(refbase+5);
  DerefSwitch(bvar,mask|=0x4;);	/* is B var? */
  
  if (mask & 0x4) {
    if ((mask & 0x3)==0x0) {	/* integer(X), integer(Y) */
      goto push;
    } else {
      TAGGED xdom, ydom;
      xdom = RefGlob(refbase);
      DerefAttribute(xdom,xdom); /* get dom/4 term */
      ydom = RefGlob(refbase+2);
      DerefAttribute(ydom,ydom); /* get dom/4 term */
      if (fd_compare(DomainSet(xdom),DomainSet(ydom))==FDI_DISJOINT)
	goto push;
    }
  } else if (Teqz(bvar)) {
    if ((mask & 0x3)!=0x3)	/* integer(X); integer(Y) */
      goto push;
  } else {
    goto push;
  }
  return;
 push:
  fd_enqueue_global(w, global, 0x2/* VAL, prepend*/);   
}

/* '$fd_eq_iff'(+State, -Actions) :- X#=Y iff B.
   State is f(X,XMut,Y,YMut,B,BMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_eq_iff MAGIC (HIDDEN_PROTO
			SP_term_ref State,
			SP_term_ref NewState,
			SP_term_ref Actions)
{
  WAMENV;
  TAGGED handle;
  SP_BOOL committed, post;
  struct arith_data *pdata;
  Dvar dvx, dvy, dvb;
  SP_globref refbase;
  int ent = 0;		/* neither entailed nor dis- */

  (void)State;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;
  dvar_export_start(w);
  RefTerm(NewState) = static_output_state(w,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    post = FALSE;
    pdata = Pdata(struct arith_data,handle);
  } else {			/* build persistent state */
    post = TRUE;
    pdata = Palloc(struct arith_data, 3*sizeof(struct dvar), handle);
    pdata->destructor = arith_destructor;
    pdata->nrefs = 6;
    pdata->daemon = eq_iff_daemon;
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
  dvb = dvx+2;
  refbase = pdata->refbase;
  dvar_init(dvx, refbase,   refbase+1);
  dvar_init(dvy, refbase+2, refbase+3);
  dvar_init(dvb, refbase+4, refbase+5);

  if (!dvar_is_integer(dvb)) {
    if (dvar_is_integer(dvx) && dvar_is_integer(dvy)) {
      dvar_fix_value_l(dvb, (dvar_min_t(dvx)==dvar_min_t(dvy)));
      ent = 1;
    } else if (fd_compare(dvar_set(dvx,w),dvar_set(dvy,w))==FDI_DISJOINT) {
      dvar_fix_value_t(dvb, TaggedZero);
      ent = 1;
    }
  } else if (Teqz(dvar_min_t(dvb))) {
    if (dvar_is_integer(dvy)) {
      ent = (dvar_prune_value_t(dvx, dvar_min_t(dvy),w)>=0 ? 1 : -1);
    } else if (dvar_is_integer(dvx)) {
      ent = (dvar_prune_value_t(dvy, dvar_min_t(dvx),w)>=0 ? 1 : -1);
    } else if (fd_compare(dvar_set(dvx,w),dvar_set(dvy,w))==FDI_DISJOINT)
      ent = 1;
  } else {
    if (dvar_is_integer(dvy)) {
      ent = (dvar_fix_value_t(dvx, dvar_min_t(dvy))>=0 ? 1 : -1);
    } else if (dvar_is_integer(dvx)) {
      ent = (dvar_fix_value_t(dvy, dvar_min_t(dvx))>=0 ? 1 : -1);
    } else {
      ent = tell_eq(w,dvx,dvy,fd_compare(dvar_set(dvx,w),dvar_set(dvy,w)));
    }
  }
  dvar_pruning_done(w,dvx);
  dvar_pruning_done(w,dvy);
  dvar_pruning_done(w,dvb);
  dvar_export(w,dvx);
  dvar_export(w,dvy);
  dvar_export(w,dvb);
  if (ent==0 && post) {
    if (dvar_is_integer(dvb)) {
      dvar_attach_daemon(w, dvx, pdata, X(1), fd.functor_val);
      dvar_attach_daemon(w, dvy, pdata, X(1), fd.functor_val);
    } else {			      
      dvar_attach_daemon(w, dvx, pdata, X(1), fd.functor_dom);
      dvar_attach_daemon(w, dvy, pdata, X(1), fd.functor_dom);
      dvar_attach_daemon(w, dvb, pdata, X(1), fd.functor_val);
    }
  }
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}


/* '$fd_le_iff'(+State, -Actions) :- X #=< Y iff B.
   State is f(X,XMut,Y,YMut,B,BMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_le_iff MAGIC (HIDDEN_PROTO
			SP_term_ref State,
			SP_term_ref NewState,
			SP_term_ref Actions)
{
  WAMENV;
  TAGGED xmin, xmax, ymin, ymax;
  TAGGED handle;
  SP_BOOL committed;
  struct arith_data *pdata;
  Dvar dvx, dvy, dvb;
  int ent = 0;		/* neither entailed nor dis- */

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
  dvb = dvx+2;
  dvar_init(dvx, pdata->refbase,   pdata->refbase+1);
  dvar_init(dvy, pdata->refbase+2, pdata->refbase+3);
  dvar_init(dvb, pdata->refbase+4, pdata->refbase+5);
  xmin = dvar_min_t(dvx);
  xmax = dvar_max_t(dvx);
  ymin = dvar_min_t(dvy);
  ymax = dvar_max_t(dvy);
  if (!dvar_is_integer(dvb)) {
    if (FDlt(ymax,xmin)) {	/* [B=0,exit] */
      ent = 1;
      dvar_fix_value_t(dvb, TaggedZero);
    } else if (!FDgt(xmax,ymin)) { /* [B=1,exit] */
      ent = 1;
      dvar_fix_value_t(dvb, TaggedOne);
    }
  } else if (Teqz(dvar_min_t(dvb))) {		/* enforce X #> Y */
    if (FDlt(ymax,xmin))
      ent = 1;
    else if (!FDgt(xmax,ymin))
      ent = -1;
    else {
      TAGGED ymin1 = FDincr(ymin);
      TAGGED xmax1 = FDdecr(xmax);
      
      if (TagIsSmall(ymin) && FDlt(xmin,ymin1))
	dvar_fix_min_t(dvx, ymin1);
      if (TagIsSmall(xmax) && FDgt(ymax,xmax1))
	dvar_fix_max_t(dvy, xmax1);
      ent = dvar_is_integer(dvx) || dvar_is_integer(dvy) || xmax==ymin1;
    }
  } else {			/* enforce X #=< Y */
    if (FDlt(ymax,xmin))
      ent = -1;
    else if (!FDgt(xmax,ymin))
      ent = 1;
    else {
      if (TagIsSmall(ymax) && FDgt(xmax,ymax))
	dvar_fix_max_t(dvx, ymax);
      if (TagIsSmall(xmin) && FDlt(ymin,xmin))
	dvar_fix_min_t(dvy, xmin);
      ent = dvar_is_integer(dvx) || dvar_is_integer(dvy) || ymax==xmin;
    }
  }
  dvar_pruning_done(w,dvx);
  dvar_pruning_done(w,dvy);
  dvar_pruning_done(w,dvb);
  dvar_export(w,dvx);
  dvar_export(w,dvy);
  dvar_export(w,dvb);
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}



/* '$fd_oneof'(+State, -Actions) :- X#=Z #\/ Y#=Z.
   State is f(X,XMut,Y,YMut,Z,ZMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_oneof MAGIC (HIDDEN_PROTO
		       SP_term_ref State,
		       SP_term_ref NewState,
		       SP_term_ref Actions)
{
  WAMENV;
  int xcapz, ycapz;
  TAGGED xdom, ydom, zdom;
  TAGGED handle;
  SP_BOOL committed;
  struct arith_data *pdata;
  Dvar dvx, dvy, dvz;
  int ent = 0;		/* neither entailed nor dis- */

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

  xdom = dvar_set(dvx,w);
  ydom = dvar_set(dvy,w);
  zdom = dvar_set(dvz,w);
  xcapz = fd_compare(xdom,zdom);
  ycapz = fd_compare(ydom,zdom);
  if (xcapz==FDI_DISJOINT)
    ent = tell_eq(w,dvy,dvz,ycapz);
  else if (ycapz==FDI_DISJOINT)
    ent = tell_eq(w,dvx,dvz,xcapz);
  else {
    TAGGED xcupy = fd_union(w,xdom,ydom);

    switch (fd_compare(zdom,xcupy)) {
    case FDI_INTERSECT:
    case FDI_SUPERSET:
      dvar_fix_set(dvz, xcupy,w);
      /* entailed if at most one variable left */
      ent = (!dvar_is_integer(dvx) + !dvar_is_integer(dvy) + (!dvar_is_integer(dvz)) <= 1);
      break;
    case FDI_DISJOINT:
      ent = -1;
      break;
    }
  }
  dvar_pruning_done(w,dvx);
  dvar_pruning_done(w,dvy);
  dvar_pruning_done(w,dvz);
  dvar_export(w,dvx);
  dvar_export(w,dvy);
  dvar_export(w,dvz);
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}


/* computes -dom(d2) */
/* ripped off from indexical.c */
static TAGGED negdom MAGIC (HIDDEN_PROTO
			    Argdecl,
			    TAGGED d2)
{
  int j;
  TAGGED t2, r2, tail, *h, *array;

  for (j=0, t2=d2; t2!=EmptySet; j++)
    t2 = CTagToCdr(t2);

				/* create j intervals, then merge */

  NumstackAlloc(4*j,array);
  h = array + 4*j;
  tail = EmptySet;
  for (t2=d2; t2!=EmptySet;) {
    h -= 4;
    r2 = CTagToCar(t2); t2 = CTagToCdr(t2);
    h[0] = MakeList(h+2);
    h[1] = tail;
    h[2] = safe_minus(TaggedZero,RangeMax(r2));
    h[3] = safe_minus(TaggedZero,RangeMin(r2));
    tail = MakeList(h);
  }
  return tail;
}


/* '$fd_abs'(+State, -Actions) :- abs(X) #= Y.
   State is f(X,XMut,Y,YMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_abs MAGIC (HIDDEN_PROTO
		     SP_term_ref State,
		     SP_term_ref NewState,
		     SP_term_ref Actions)
{
  WAMENV;
  TAGGED xdom, ydom, xmin;
  TAGGED handle;
  SP_BOOL committed;
  struct arith_data *pdata;
  Dvar dvx, dvy;
  int ent = 0;		/* neither entailed nor dis- */

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

  xdom = dvar_set(dvx,w);
  ydom = dvar_set(dvy,w);
  xmin = fd_min(xdom);
  if (!FDlt(xmin,TaggedZero))
    ent = tell_eq(w,dvx,dvy,fd_compare(xdom,ydom));
  else {
    TAGGED ydom0 = ydom;
    TAGGED xndom, yndom, xdom1, ydom1;
    TAGGED ymin = fd_min(ydom);
    
    if (FDlt(ymin,TaggedZero))
      ydom0 = fd_intersection_interval(w,ydom0,TaggedZero,Sup);
    xndom = negdom(w,xdom);
    yndom = negdom(w,ydom0);
    xdom1 = fd_union(w,ydom0,yndom);
    ydom1 = fd_union(w,xdom,xndom);
    if (FDlt(ymin,TaggedZero))
      ydom1 = fd_intersection_interval(w,ydom1,TaggedZero,Sup);
    switch (fd_compare(xdom,xdom1)) {
    case FDI_SUBSET:
    case FDI_EQUAL:
      break;
    case FDI_DISJOINT:
      ent = -1;
      goto ret;
    case FDI_INTERSECT:
      xdom1 = fd_intersection(w,xdom,xdom1);
    case FDI_SUPERSET:
      dvar_fix_set(dvx, xdom1,w);
    }
    switch (fd_compare(ydom,ydom1)) {
    case FDI_SUBSET:
    case FDI_EQUAL:
      ent = fd_singleton(ydom1);
      break;
    case FDI_DISJOINT:
      ent = -1;
      break;
    case FDI_INTERSECT:
      ydom1 = fd_intersection(w,ydom,ydom1);
    case FDI_SUPERSET:
      ent = fd_singleton(ydom1);
      dvar_fix_set(dvy, ydom1,w);
    }
  }
  dvar_pruning_done(w,dvx);
  dvar_pruning_done(w,dvy);
  dvar_export(w,dvx);
  dvar_export(w,dvy);
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}

