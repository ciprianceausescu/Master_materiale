/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

#if MULTI_SP_AWARE
#define request_tell_fast(A2,A3,A4) request_tell_fast(HIDDEN_ARG, A2,A3,A4)
#define request_tell_interval_fast(A2,A3,A4,A5,A6) request_tell_interval_fast(HIDDEN_ARG, A2,A3,A4,A5,A6)
#define request_tell_value_fast(A2,A3,A4) request_tell_value_fast(HIDDEN_ARG, A2,A3,A4)
#define request_tell(A2,A3) request_tell(HIDDEN_ARG, A2,A3)
#define request_tell_interval(A2,A3,A4) request_tell_interval(HIDDEN_ARG, A2,A3,A4)
#define request_tell_value(A2,A3) request_tell_value(HIDDEN_ARG, A2,A3)
#define request_rewrite_eq(A2,A3,A4) request_rewrite_eq(HIDDEN_ARG, A2,A3,A4)
#define request_rewrite_leqc(A2,A3,A4,A5) request_rewrite_leqc(HIDDEN_ARG, A2,A3,A4,A5)
#endif /* MULTI_SP_AWARE */

/* initialize dvar */

void
dvar_init(Dvar dvar,
	  SP_globref attr_ref,
	  SP_globref var_ref)
{
  TAGGED tmp = RefGlob(attr_ref);

  dvar->attr_ref = attr_ref;
  dvar->var_ref = var_ref;
  DerefAttribute(tmp,tmp); /* get dom/4 term */
  dvar->min = DomainMin(tmp);
  dvar->max = DomainMax(tmp);
  dvar->set = DomainSet(tmp);
  dvar->cookie = DomainSize(tmp);
  dvar->flags = DV_SET_OK;
  if (dvar->cookie==TaggedOne)
    dvar->flags |= DV_PRUNED_VAL;
  if (CTagToCdr(dvar->set)==EmptySet)
    dvar->flags |= DV_INTERVAL;
}

/* initialize dvar, special for indexicals */

void
dvar_init_ix(Dvar dvar,
	     TAGGED attr,
	     TAGGED var)
{
  *(TAGGED *)&dvar->attr_ref = attr;
  *(TAGGED *)&dvar->var_ref = var;
  DerefAttribute(attr,attr); /* get dom/4 term */
  dvar->min = DomainMin(attr);
  dvar->max = DomainMax(attr);
  dvar->set = DomainSet(attr);
  dvar->cookie = DomainSize(attr);
  dvar->flags = DV_SET_OK;
  if (dvar->cookie==TaggedOne)
    dvar->flags |= DV_PRUNED_VAL;
  if (CTagToCdr(dvar->set)==EmptySet)
    dvar->flags |= DV_INTERVAL;
}

TAGGED
dvar_get(Dvar dvar)
{
  TAGGED var = RefGlob(dvar->var_ref);

  DerefHeapSwitch(var,;);
  return var;
}

void
dvar_assign MAGIC (HIDDEN_PROTO
		   Dvar dest,
		   Dvar source,
		   Argdecl)
{
  dest->min = source->min;
  dest->max = source->max;
  dest->set = dvar_set(source,w);
  dest->flags = (source->flags&(DV_PRUNED_VAL|DV_INTERVAL))|DV_SET_OK;
}

/* access primitives */

SP_BOOL
dvar_is_alias(Dvar dv1,Dvar dv2)
{
  TAGGED v1 = RefGlob(dv1->var_ref);
  TAGGED v2 = RefGlob(dv2->var_ref);

  DerefSwitch(v1,;);
  DerefSwitch(v2,;);
  return (v1==v2);
}

TAGGED
dvar_set MAGIC (HIDDEN_PROTO
		Dvar dvar,
		Argdecl)
{
  if (dvar->flags & DV_EXPORTED) {
#if DBG
    fprintf(stderr, "! dvar_export() followed by dvar_set()\n");
#endif
    return ERRORTAG;
  } else if (!(dvar->flags & DV_SET_OK)) {
    dvar->flags |= DV_SET_OK;
    if (dvar->flags & DV_INTERVAL)
      dvar->set = fd_interval(w,dvar->min, dvar->max);
    else
      dvar->set = fd_intersection_interval(w,dvar->set, dvar->min, dvar->max);
  }
  return dvar->set;
}

/* set is assumed to be finite */
long
dvar_value_count(Dvar dvar)
{
  long count;
  TAGGED set, range, tail, a, b;

  if (dvar->flags & DV_INTERVAL)
    return GetSmall(dvar->max - dvar->min + TaggedOne);

  if (dvar->flags & DV_EXPORTED) {
#if DBG
    fprintf(stderr, "! dvar_export() followed by dvar_value_count()\n");
#endif
    return 0;
  }
  count = 0;
  for (set=dvar->set; set!=EmptySet; set=tail) {
    range = CTagToCar(set);
    tail = CTagToCdr(set);
    a = RangeMin(range);
    b = RangeMax(range);
    if (!(dvar->flags & DV_SET_OK)) {
      switch (val_vs_range(dvar->min,range)) {
      case CMP_INSIDE:
	a = dvar->min;
      case CMP_BEFORE:
	break;
      default:
	continue;
      }
      switch (val_vs_range(dvar->max,range)) {
      case CMP_INSIDE:
	b = dvar->max;
	tail = EmptySet;
      case CMP_AFTER:
	break;
      default:
	return count;
      }
    }
    count += GetSmall(b-a+TaggedOne);
  }
  return count;
}

long
dvar_interval_count(Dvar dvar)
{
  long count;
  TAGGED set, range, tail;

  if (dvar->flags & DV_INTERVAL)
    return 1;

  if (dvar->flags & DV_EXPORTED) {
#if DBG
    fprintf(stderr, "! dvar_export() followed by dvar_interval_count()\n");
#endif
    return 0;
  }
  count = 0;
  for (set=dvar->set; set!=EmptySet; set=tail) {
    range = CTagToCar(set);
    tail = CTagToCdr(set);
    if (!(dvar->flags & DV_SET_OK)) {
      switch (val_vs_range(dvar->min,range)) {
      case CMP_INSIDE:
      case CMP_BEFORE:
	break;
      default:
	continue;
      }
      switch (val_vs_range(dvar->max,range)) {
      case CMP_INSIDE:
	tail = EmptySet;
      case CMP_AFTER:
	break;
      default:
	return count;
      }
    }
    count++;
  }
  return count;
}


/* comparisons */

int
dvar_compare_set MAGIC (HIDDEN_PROTO
			Dvar dvar,
			TAGGED set,
			Argdecl)
{
  SP_BOOL ok = (dvar->flags & DV_SET_OK);
  int rc;

  if (dvar->flags & DV_INTERVAL) {
    switch ((rc=fd_compare_interval(set,dvar->min,dvar->max))) {
    case FDI_SUPERSET:
      return FDI_SUBSET;
    case FDI_SUBSET:
      return FDI_SUPERSET;
    default:
      return rc;
    }
  }

  switch (fd_compare(dvar->set,set)) {
  case FDI_EQUAL:
    if (ok)
      return FDI_EQUAL;
    switch (fd_compare_interval(set,dvar->min,dvar->max)) {
      /* "wrong" order */
    case FDI_SUPERSET:
    case FDI_INTERSECT:
      return FDI_SUBSET;
    case FDI_EQUAL:
    case FDI_SUBSET:
    case FDI_DISJOINT:
    default:
      return FDI_EQUAL;
    }
    
  case FDI_SUBSET:
    return FDI_SUBSET;
    
  case FDI_DISJOINT:
    return FDI_DISJOINT;

  case FDI_SUPERSET:
    if (ok)
      return FDI_SUPERSET;
    switch (fd_compare_interval(set,dvar->min,dvar->max)) {
      /* "wrong" order */
    case FDI_EQUAL:
      return FDI_EQUAL;
    case FDI_SUPERSET:
      return FDI_SUBSET;
    case FDI_DISJOINT:
      return FDI_DISJOINT;
    case FDI_SUBSET:
    case FDI_INTERSECT:
    default:
      goto general;
    }
    
  case FDI_INTERSECT:
    if (ok)
      return FDI_INTERSECT;
    switch (fd_compare_interval(set,dvar->min,dvar->max)) {
      /* "wrong" order */
    case FDI_EQUAL:
    case FDI_SUPERSET:
      return FDI_SUBSET;
    case FDI_DISJOINT:
      return FDI_DISJOINT;
    case FDI_SUBSET:
    case FDI_INTERSECT:
    default:
      goto general;
    }
  }
 general:
  dvar->flags |= DV_SET_OK;
  dvar->set = fd_intersection_interval(w,dvar->set, dvar->min, dvar->max);
  return fd_compare(dvar->set, set);
}

int
dvar_compare_interval_t MAGIC (HIDDEN_PROTO
			       Dvar dvar,
			       TAGGED tmin,
			       TAGGED tmax,
			       Argdecl)
{
  SP_BOOL ok = (dvar->flags & DV_SET_OK);

  if (dvar->flags & DV_INTERVAL)
    return fd_compare_intervals(dvar->min,dvar->max,tmin,tmax);

  switch (fd_compare_interval(dvar->set,tmin,tmax)) {
  case FDI_EQUAL:
    if (ok)
      return FDI_EQUAL;
    switch (fd_compare_intervals(tmin,tmax,dvar->min,dvar->max)) {
      /* "wrong" order */
    case FDI_SUPERSET:
    case FDI_INTERSECT:
      return FDI_SUBSET;
    case FDI_EQUAL:
    case FDI_SUBSET:
    case FDI_DISJOINT:
    default:
      return FDI_EQUAL;
    }
    
  case FDI_SUBSET:
    return FDI_SUBSET;
    
  case FDI_DISJOINT:
    return FDI_DISJOINT;

  case FDI_SUPERSET:
    if (ok)
      return FDI_SUPERSET;
    switch (fd_compare_intervals(tmin,tmax,dvar->min,dvar->max)) {
      /* "wrong" order */
    case FDI_EQUAL:
      return FDI_EQUAL;
    case FDI_SUPERSET:
      return FDI_SUBSET;
    case FDI_DISJOINT:
      return FDI_DISJOINT;
    case FDI_SUBSET:
    case FDI_INTERSECT:
    default:
      goto general;
    }
    
  case FDI_INTERSECT:
    if (ok)
      return FDI_INTERSECT;
    switch (fd_compare_intervals(tmin,tmax,dvar->min,dvar->max)) {
      /* "wrong" order */
    case FDI_EQUAL:
    case FDI_SUPERSET:
      return FDI_SUBSET;
    case FDI_DISJOINT:
      return FDI_DISJOINT;
    case FDI_SUBSET:
    case FDI_INTERSECT:
    default:
      goto general;
    }
  }
 general:
  dvar->flags |= DV_SET_OK;
  dvar->set = fd_intersection_interval(w,dvar->set, dvar->min, dvar->max);
  return fd_compare_interval(dvar->set, tmin, tmax);
}

SP_BOOL
dvar_contains_value_t(Dvar dvar, TAGGED tvalue)
{
  return
    InInterval(tvalue,dvar->min,dvar->max) &&
    ((dvar->flags & DV_INTERVAL) || fd_member(tvalue,dvar->set));
}

/* iterators etc. */

/* -CLPFD_MAXINT means no predecessor */
/* if no successor, CLPFD_MAXINT is returned */
long
dvar_successor_l(Dvar dvar,long val)
{
  TAGGED tval =
    dvar_successor_t(dvar, val==-CLPFD_MAXINT ? Inf : MakeSmall(val));

  return (tval==Sup ? CLPFD_MAXINT : GetSmall(tval));
}

/* Inf means no predecessor */
/* if no successor, Sup is returned */
TAGGED
dvar_successor_t(Dvar dvar,TAGGED tval)
{
  if (FDlt(tval,dvar->min))
    return dvar->min;
  if (FDge(tval,dvar->max))
    return Sup;
  tval = FDincr(tval);
  if (!(dvar->flags & DV_INTERVAL)) {
    TAGGED set = dvar->set;
    TAGGED range;

    while (set!=EmptySet) {
      range = CTagToCar(set); 
      set = CTagToCdr(set);   
      switch (val_vs_range(tval,range)) {
      case CMP_BEFORE:
	tval = RangeMin(range);
      case CMP_INSIDE:
	set = EmptySet;
      }
    }
  }
  return tval;
}


/* CLPFD_MAXINT means no successor */
/* if no predecessor, -CLPFD_MAXINT is returned */
long
dvar_predecessor_l(Dvar dvar,long val)
{
  TAGGED tval =
    dvar_predecessor_t(dvar, val==CLPFD_MAXINT ? Sup : MakeSmall(val));

  return (tval==Inf ? -CLPFD_MAXINT : GetSmall(tval));
}

/* Inf means no successor */
/* if no predecessor, Inf is returned */
TAGGED
dvar_predecessor_t(Dvar dvar,TAGGED tval)
{
  if (FDgt(tval,dvar->max))
    return dvar->max;
  if (FDle(tval,dvar->min))
    return Inf;
  tval = FDdecr(tval);
  if (!(dvar->flags & DV_INTERVAL)) {
    TAGGED set = dvar->set;
    TAGGED range;
    TAGGED prevmax = Inf;

    while (set!=EmptySet) {
      range = CTagToCar(set); 
      set = CTagToCdr(set);   
      switch (val_vs_range(tval,range)) {
      case CMP_BEFORE:
	tval = prevmax;
      case CMP_INSIDE:
	set = EmptySet;
      }
      prevmax = RangeMax(range);
    }
  }
  return tval;
}


void 
dviter_init(DVITER *it, Dvar dvar)
{
  TAGGED min = dvar->min;
  TAGGED max = dvar->max;
  TAGGED fdset = 0;
  
  if (!(dvar->flags & DV_INTERVAL)) {
    fdset = dvar->set;
    /* skip intervals that are < min */
    while (TagIsLST(fdset)) {
      TAGGED r = CTagToCar(fdset);
      if (FDle(min,RangeMax(r))) {
	if (FDlt(min,RangeMin(r)))
	  min = RangeMin(r);
	break;
      }
      fdset = CTagToCdr(fdset);
    }
  }
  it->min = min;
  it->max = max;
  it->fdset = fdset;
}

void
dviter_next_interval_l(DVITER *it, long *min, long *max)
{
  TAGGED tmin, tmax;

  dviter_next_interval_t(it,&tmin,&tmax);
  *min = GetSmall(tmin);
  *max = GetSmall(tmax);
}

void
dviter_next_interval_t(DVITER *it, TAGGED *tmin, TAGGED *tmax)
{
  TAGGED fdset = it->fdset;
    
  if (!fdset) {			/* (dvar->flags & DV_INTERVAL) */
    *tmin = it->min;
    *tmax = it->max;
    fdset = EmptySet;
  } else {
    TAGGED r = CTagToCar(fdset);
    
    *tmin = it->min;
    *tmax = FDlt(RangeMax(r),it->max) ? RangeMax(r) : it->max;
    fdset = CTagToCdr(fdset);
    if (TagIsLST(fdset)) {
      r = CTagToCar(fdset);
      it->min = RangeMin(r);
      if (FDgt(it->min,it->max)) /* detect end */
	fdset = EmptySet;
    }
  }
  it->fdset = fdset;
}

TAGGED
dviter_next_value_t(DVITER *it)
{
  TAGGED fdset = it->fdset;
  TAGGED tnext = it->min;
    
  it->min = FDincr(tnext);
  if (it->max==tnext)
    it->fdset = EmptySet;
  else if (fdset) {		/* !(dvar->flags & DV_INTERVAL) */
    TAGGED r = CTagToCar(fdset);
    
    if (RangeMax(r)==tnext) {
      fdset = CTagToCdr(fdset);
      if (TagIsLST(fdset)) {
	r = CTagToCar(fdset);
	it->min = RangeMin(r);
	if (FDgt(it->min,it->max)) /* detect end */
	  fdset = EmptySet;
      }
      it->fdset = fdset;
    }
  }
  return tnext;
}

/* ensure next elt will be > t */
void
dviter_skip_t(DVITER *it, TAGGED t)
{
  TAGGED t1 = FDincr(t);
  if (FDlt(it->min,t1))
    it->min = t1;
  if (it->fdset) {		/* !(dvar->flags & DV_INTERVAL) */
    TAGGED r = CTagToCar(it->fdset);
    
    while (FDgt(it->min,RangeMax(r))) {
      it->fdset = CTagToCdr(it->fdset);
      if (dviter_empty(it))
	return;
      r = CTagToCar(it->fdset);
    }
    if (FDlt(it->min,RangeMin(r)))
      it->min = RangeMin(r);
  }
  if (FDgt(it->min,it->max))
    it->fdset = EmptySet;
}

/* Precondition:
     Domain not empty.
     tmin, tmax, tset to become new dvar fields, if successful.
   Postconditions:
     dvar->min, dvar->max adjusted to existing domain elements.
     dvar->flags & DV_INTERVAL set to its correct value.
     dvar->flags & DV_SET_OK   set to its correct value.
     Value, i.e. rc and inferred values, or:ed into dvar->flags.
     On failure, dvar fields are not written.
*/
static int
dvar_adjust_bounds(Dvar dvar,int rc,TAGGED tmin,TAGGED tmax,TAGGED tset)
{
  TAGGED d, r, rprev=0;
  int cmp;
  SP_BOOL ok = TRUE;
  
  r = CTagToCar(tset);
  d = CTagToCdr(tset);
  cmp = point_vs_range(tmin,r);
  while (d!=EmptySet && cmp==CMP_AFTER) {
    tset = d;			/* optimization */
    r = CTagToCar(d);
    d = CTagToCdr(d);
    cmp = point_vs_range(tmin,r);
  }
  if (cmp==CMP_BEFORE) {
    rc |= DV_PRUNED_MIN;
    tmin = RangeMin(r);
  }
  if (tmin!=RangeMin(r))
    ok = FALSE;
  if (FDgt(tmin,tmax))		/* e.g. min=0, max=2, set=3..3 */
    return -1;
  cmp = point_vs_range(tmax,r);
  while (d!=EmptySet && cmp==CMP_AFTER) {
    rprev = r;
    r = CTagToCar(d);
    d = CTagToCdr(d);
    cmp = point_vs_range(tmax,r);
  }
  switch (cmp) {
  case CMP_BEFORE:
    r = rprev;
    ok = FALSE;
  case CMP_AFTER:
    rc |= DV_PRUNED_MAX;
    tmax = RangeMax(r);
  }
  if (d!=EmptySet || tmax!=RangeMax(r))
    ok = FALSE;
				/* succeess - store dvar fields */
  dvar->flags &= ~(DV_SET_OK|DV_INTERVAL);
  if (point_vs_range(tmin,r)==CMP_INSIDE)
    dvar->flags |= DV_INTERVAL;
  if (ok)
    dvar->flags |= DV_SET_OK;
  if (tmin==tmax)
    rc |= DV_PRUNED_VAL;
  dvar->flags |= (rc & ~DV_PRUNED_DOM);
  dvar->min = tmin;
  dvar->max = tmax;
  dvar->set = tset;
  return rc;
}

/* pruning: remove the arg. from the domain */

int
dvar_prune_interval_t MAGIC (HIDDEN_PROTO
			     Dvar dvar,
			     TAGGED lbt,
			     TAGGED ubt,
			     Argdecl)
{
  int rc = 0;
  
  if (FDgt(lbt,ubt))
    return 0;

  switch_fd_interval_cmp(lbt,ubt,dvar->min,dvar->max,
			 /*BEFORE*/ return 0;,
			 /*MEETS*/ return 0;,
			 /*OVERLAPS*/ goto starts;,
			 /*FINISHED_BY*/ return -1;,
			 /*CONTAINS*/ return -1;,
			 /*STARTS*/ {
			 starts:
			   if (ubt == TaggedHigh) {
			     fd.fd_overflow = TRUE;
			     return -1;
			   }
			   rc = DV_PRUNED_DOM|DV_PRUNED_MIN;
			   if (dvar->flags & DV_INTERVAL) {
			     dvar->flags &= ~DV_SET_OK;
			     dvar->min = ubt+IStep(1);
			   } else
			     return dvar_adjust_bounds(dvar,rc,ubt+IStep(1),dvar->max,dvar->set);
			 },
			 /*EQUALS*/ return -1;,
			 /*STARTED_BY*/ return -1;,
			 /*DURING*/ {
			   if ((dvar->flags & DV_INTERVAL) ||
			       fd_compare_interval(dvar->set,lbt,ubt)!=FDI_DISJOINT) {
			     dvar->flags |= DV_PRUNED_DOM;
			     dvar->flags &= ~DV_INTERVAL;
			     dvar->set = fd_subtract_interval(w,dvar->set,lbt,ubt);
			     rc = DV_PRUNED_DOM;
			   }
			 },
			 /*FINISHES*/ goto overlapped_by;,
			 /*OVERLAPPED_BY*/ {
			 overlapped_by:
			   if (ubt == TaggedLow) {
			     fd.fd_overflow = TRUE;
			     return -1;
			   }
			   rc = DV_PRUNED_DOM|DV_PRUNED_MAX;
			   if (dvar->flags & DV_INTERVAL) {
			     dvar->flags &= ~DV_SET_OK;
			     dvar->max = lbt-IStep(1);
			   } else
			     return dvar_adjust_bounds(dvar,rc,dvar->min,lbt-IStep(1),dvar->set);
			 },
			 /*MET_BY*/ return 0;,
			 /*AFTER*/ return 0;
			 );
  if (dvar->min==dvar->max)
    rc |= DV_PRUNED_VAL;
  dvar->flags |= (rc & ~DV_PRUNED_DOM);
  return rc;
}

int
dvar_prune_set MAGIC (HIDDEN_PROTO
		      Dvar dvar,
		      TAGGED set,
		      Argdecl)
{
  if (set==EmptySet)
    return 0;
  switch ((dvar->flags & DV_INTERVAL)
	  ? fd_compare_interval(set,dvar->min,dvar->max)
	  : fd_compare(set,dvar->set)) {
  case FDI_EQUAL:
  case FDI_SUPERSET:
    return -1;
  case FDI_DISJOINT:
    return 0;
  case FDI_SUBSET:
  case FDI_INTERSECT:
  default:
    dvar->flags |= DV_PRUNED_DOM;
    return dvar_adjust_bounds(dvar,DV_PRUNED_DOM,
			      dvar->min,dvar->max,fd_subtract(w,dvar->set,set));
  }
}


/* fixing: assign the arg. to the domain */

int
dvar_fix_interval_t(Dvar dvar,TAGGED lbt,TAGGED ubt)
{
  int rc = 0;
  TAGGED tmin = dvar->min;
  TAGGED tmax = dvar->max;
  
  if (lbt!=tmin && FDgt(lbt,tmin)) {
    rc |= DV_PRUNED_MIN|DV_PRUNED_DOM;
    tmin = lbt;
  }
  if (ubt!=tmax && FDlt(ubt,tmax)) {
    rc |= DV_PRUNED_MAX|DV_PRUNED_DOM;
    tmax = ubt;
  }
  if (rc) {
    if (FDgt(tmin,tmax))
      return -1;
    else if (dvar->flags & DV_INTERVAL)
      dvar->flags &= ~DV_SET_OK;
    else
      return dvar_adjust_bounds(dvar,rc,tmin,tmax,dvar->set);
    if (tmin==tmax)
      rc |= DV_PRUNED_VAL;
    dvar->flags |= (rc & ~DV_PRUNED_DOM);
    dvar->min = tmin;
    dvar->max = tmax;
  }
  return rc;
}

int
dvar_fix_set MAGIC (HIDDEN_PROTO
		    Dvar dvar,
		    TAGGED set,
		    Argdecl)
{
  if (set==EmptySet)
    return -1;
  switch ((dvar->flags & DV_INTERVAL)
	  ? fd_compare_interval(set,dvar->min,dvar->max)
	  : fd_compare(set,dvar->set)) {
  case FDI_EQUAL:
  case FDI_SUPERSET:
    return 0;
  case FDI_DISJOINT:
    return -1;
  case FDI_SUBSET:
    dvar->flags |= DV_PRUNED_DOM;
    return dvar_adjust_bounds(dvar,DV_PRUNED_DOM,dvar->min,dvar->max,set);
  case FDI_INTERSECT:
  default:
    dvar->flags |= DV_PRUNED_DOM;
    return dvar_adjust_bounds(dvar,DV_PRUNED_DOM,dvar->min,dvar->max,
			      (dvar->flags & DV_INTERVAL) ? set :
			      fd_intersection(w,dvar->set,set));
  }
}


/* pruning done */

void
dvar_pruning_done MAGIC (HIDDEN_PROTO
			 Argdecl,
			 Dvar dvar)
{
  if ((dvar->flags & (DV_PRUNED_DOM|DV_INTERVAL))==DV_PRUNED_DOM) {
    if (!(dvar->flags & DV_SET_OK)) {
      dvar->flags |= DV_SET_OK;
      dvar->set = fd_intersection_interval(w,dvar->set, dvar->min, dvar->max);
    }
    dvar->set = fd_localize(w,dvar->set);
  }
}

/* exporting bindings, fast case */

/* Assuming not debugging. */
/* Assuming fdset is localized or interval. */
/* Assuming fdset is subset of old domain. */
static void 
request_tell_value_fast MAGIC (HIDDEN_PROTO
			       Argdecl,
			       TAGGED value,
			       TAGGED filter)
{
  int why;

  why = fd_tell_value(w,value); /* GC */
  fd_told(w,why&7,filter);
}

static void 
request_tell_interval_fast MAGIC (HIDDEN_PROTO
				  Argdecl,
				  TAGGED min, TAGGED max,
				  int flags,
				  TAGGED filter)
{
  TAGGED dset;
  int why = (flags&(DV_PRUNED_MIN|DV_PRUNED_MAX))|MASK_DOM|MASK_MINMAX;

  DerefAttribute(dset,X(EVAL_ARITY));
  dset = DomainSet(dset);
  if (CTagToCdr(dset)!=EmptySet && fd_compare_interval(dset,min,max)==FDI_INTERSECT) {
    why = fd_tell_unsafe(w, fd_intersection_interval(w,dset,min,max)); /* GC */
  } else {
    why = fd_tell_interval(w,min,max,why); /* GC */
  }
  fd_told(w,why&7,filter);
}

static void 
request_tell_fast MAGIC (HIDDEN_PROTO
			 Argdecl,
			 TAGGED fdset,
			 TAGGED filter)
{
  int why;
  
  why = fd_tell(w,fdset); /* GC */
  fd_told(w,why&7,filter);
}

/* exporting bindings, general case */

/* Assuming fdset is localized or interval. */
/* Assumption that fdset neither contains old domain nor that they are disjoint
   is unsafe in the context of co-references.
   X(2)=action list, X(3)=attribute term, X(4)=domain variable.
*/
static void 
request_tell_value MAGIC (HIDDEN_PROTO
			  Argdecl,
			  TAGGED value)
{
  TAGGED *h, old, dom;
  int why = 0;

  DerefAttribute(dom,X(EVAL_ARITY));
  if (fd.debugging) {
    RequireHeap(5,EVAL_ARITY+2);	/* GC */
    h = w->global_top;
    h[0] = fd.functor_eq;
    h[1] = X(EVAL_ARITY+1);
    h[2] = value;
    h[3] = MakeStructure(h);
    h[4] = X(EVAL_ARITY-1);
    w->global_top = h+5;
    X(EVAL_ARITY-1) = MakeList(h+3);
  } else {
    old = DomainSet(dom);
    if (!fd_member(value,old)) {
      why = -1;
    } else if (DomainSize(dom)==MakeSmall(1)) {
    } else {
      why = fd_tell_value(w,value); /* GC */
    }
    if (why<0) {
      RequireHeap(2,EVAL_ARITY+2);	/* GC */
      h = w->global_top;
      h[0] = atom_fail;
      h[1] = X(EVAL_ARITY-1);
      w->global_top = h+2;
      X(EVAL_ARITY-1) = MakeList(h);
      return;
    } else if (why==0) {
      return;
    } else {
      DerefSwitch(X(EVAL_ARITY+1),fd_told(w,why&7,atom_nil););
      /* otherwise, co-reference already dealt with */
    }
  }
}

static void 
request_tell_interval MAGIC (HIDDEN_PROTO
			     Argdecl,
			     TAGGED min, TAGGED max)
{
  TAGGED *h, old, min1, max1, dset;
  int why=0;

  DerefAttribute(old,X(EVAL_ARITY));
  dset = DomainSet(old);
  if (fd.debugging) {
    dset = fd_globalize(w,fd_interval(w,min,max),5,EVAL_ARITY+2);
    h = w->global_top;
    h[0] = fd.functor_in_set2;
    h[1] = X(EVAL_ARITY+1);
    h[2] = dset;
    h[3] = MakeStructure(h);
    h[4] = X(EVAL_ARITY-1);
    w->global_top = h+5;
    X(EVAL_ARITY-1) = MakeList(h+3);
  } else {
    switch (fd_compare_interval(dset,min,max)) {
    case FDI_SUBSET:
    case FDI_EQUAL:
      break;
    case FDI_DISJOINT:
      why = -1;
      break;
    case FDI_INTERSECT:
      if (CTagToCdr(dset)!=EmptySet) {
	why = fd_tell_unsafe(w, fd_intersection_interval(w,dset,min,max)); /* GC */
	break;
      }
    case FDI_SUPERSET:
      min1 = DomainMin(old);
      max1 = DomainMax(old);
      if (min1!=min) {
	if (!EmptyInterval(min1,min))
	  why |= MASK_MIN+MASK_MINMAX+MASK_DOM;
	else
	  min = min1;
      }
      if (max!=max1) {
	if (!EmptyInterval(max,max1))
	  why |= MASK_MAX+MASK_MINMAX+MASK_DOM;
	else
	  max = max1;
      }
      why = fd_tell_interval(w,min,max,why); /* GC */
    }
    if (why<0) {
      RequireHeap(2,EVAL_ARITY+2);	/* GC */
      h = w->global_top;
      h[0] = atom_fail;
      h[1] = X(EVAL_ARITY-1);
      w->global_top = h+2;
      X(EVAL_ARITY-1) = MakeList(h);
      return;
    } else if (why==0) {
      return;
    } else {
      DerefSwitch(X(EVAL_ARITY+1),fd_told(w,why&7,atom_nil););
      /* otherwise, co-reference already dealt with */
    }
  }
}

static void 
request_tell MAGIC (HIDDEN_PROTO
		    Argdecl,
		    TAGGED fdset)
{
  TAGGED *h, old;
  int why=0;

  if (fd.debugging) {
    fdset = fd_globalize(w,fdset,5,EVAL_ARITY+2);
    h = w->global_top;
    h[0] = fd.functor_in_set2;
    h[1] = X(EVAL_ARITY+1);
    h[2] = fdset;
    h[3] = MakeStructure(h);
    h[4] = X(EVAL_ARITY-1);
    w->global_top = h+5;
    X(EVAL_ARITY-1) = MakeList(h+3);
  } else {
    DerefAttribute(old,X(EVAL_ARITY));
    old = DomainSet(old);
    switch (fd_compare(old,fdset)) {
    case FDI_SUBSET:
    case FDI_EQUAL:
      break;
    case FDI_DISJOINT:
      why = -1;
      break;
    case FDI_SUPERSET:
      why = fd_tell(w,fdset); /* GC */
      break;
    case FDI_INTERSECT:
      if (CTagToCdr(old)==EmptySet) {
	TAGGED range = CTagToCar(old);
	TAGGED min = RangeMin(range);
	TAGGED max = RangeMax(range);

	why = fd_tell(w,fd_intersection_interval(w,fdset,min,max)); /* GC */
      } else
	why = fd_tell_unsafe(w,fd_intersection(w,old,fdset)); /* GC */
    }
    if (why<0) {
      RequireHeap(2,EVAL_ARITY+2);	/* GC */
      h = w->global_top;
      h[0] = atom_fail;
      h[1] = X(EVAL_ARITY-1);
      w->global_top = h+2;
      X(EVAL_ARITY-1) = MakeList(h);
      return;
    } else if (why==0) {
      return;
    } else {
      DerefSwitch(X(EVAL_ARITY+1),fd_told(w,why&7,atom_nil););
      /* otherwise, co-reference already dealt with */
    }
  }
}

static void 
request_rewrite_eq MAGIC (HIDDEN_PROTO
			  Argdecl, TAGGED x_var, TAGGED y_var)
{
  TAGGED *h;
  
  if (x_var==y_var) {
  } else if (!fd.debugging) {
    (void)cunify(w,x_var,y_var);
  } else {
    RequireHeap2(7,x_var,y_var,EVAL_ARITY); /* GC */
    h = w->global_top;
    h[0] = fd.functor_eq;
    h[1] = x_var;
    h[2] = y_var;
    h[3] = fd.functor_call;
    h[4] = MakeStructure(h);
    h[5] = MakeStructure(h+3);
    h[6] = X(EVAL_ARITY-1);
    w->global_top = h+7;
    X(EVAL_ARITY-1) = MakeList(h+5);
  }
}

static void 
request_rewrite_leqc MAGIC (HIDDEN_PROTO
			    Argdecl, TAGGED x_var, TAGGED y_var, int c)
{
  TAGGED *h;
  
  RequireHeap2(11,x_var,y_var,EVAL_ARITY);
  h = w->global_top;
  h[0] = fd.functor_leqc;
  h[1] = x_var;
  h[2] = y_var;
  h[3] = MakeSmall(c);
  h[4] = functor_module;
  h[5] = fd.fd_module->name;
  h[6] = MakeStructure(h);
  h[7] = fd.functor_call;
  h[8] = MakeStructure(h+4);
  h[9] = MakeStructure(h+7);
  h[10] = X(EVAL_ARITY-1);
  w->global_top = h+11;
  X(EVAL_ARITY-1) = MakeList(h+9);
}

void
dvar_export_do MAGIC (HIDDEN_PROTO
		      Argdecl,
		      Dvar dvar)
{
  int flags = dvar->flags;

  dvar->flags |= DV_EXPORTED;
  X(EVAL_ARITY) = RefGlob(dvar->attr_ref);	/* preserve over GC */
  X(EVAL_ARITY+1) = RefGlob(dvar->var_ref);
  if (dvar->cookie!=Sup && !fd.debugging) {
    TAGGED tmp = RefGlob(dvar->attr_ref);
    
    DerefAttribute(tmp,tmp); /* get dom/4 term */
    if (dvar->cookie==DomainSize(tmp)) {
      goto fast;
    }
  }
      
  if (flags & DV_PRUNED_VAL)
    request_tell_value(w, dvar->min);
  else if ((flags & (DV_PRUNED_DOM|DV_INTERVAL))==DV_PRUNED_DOM)
    request_tell(w, dvar->set);
  else
    request_tell_interval(w, dvar->min, dvar->max);
  return;
 fast:
  if (flags & DV_PRUNED_VAL)
    request_tell_value_fast(w, dvar->min, atom_nil);
  else if ((flags & (DV_PRUNED_DOM|DV_INTERVAL))==DV_PRUNED_DOM)
    request_tell_fast(w, dvar->set, atom_nil);
  else
    request_tell_interval_fast(w, dvar->min, dvar->max, dvar->flags, atom_nil);
}

/* special for indexicals---dvar_pruning_done NOT called */
void
dvar_export_ix MAGIC (HIDDEN_PROTO
		      Argdecl,
		      Dvar dvar,
		      TAGGED filter)
{
  int flags = dvar->flags;
  int why;

  X(EVAL_ARITY) = *(TAGGED *)&dvar->attr_ref;
  X(EVAL_ARITY+1) = *(TAGGED *)&dvar->var_ref;
  if (flags & DV_PRUNED_VAL) {
    request_tell_value_fast(w, dvar->min, filter);
  } else if ((flags & (DV_PRUNED_DOM|DV_INTERVAL))==DV_PRUNED_DOM) {
    if (!(dvar->flags & DV_SET_OK)) {
      dvar->flags |= DV_SET_OK;
      dvar->set = fd_intersection_interval(w,dvar->set, dvar->min, dvar->max);
    }
    why = fd_tell_unsafe(w, dvar->set);
    fd_told(w, why&7, atom_nil);
  } else {
    request_tell_interval_fast(w, dvar->min, dvar->max, dvar->flags, filter);
  }
}

void
dvar_export_equal MAGIC (HIDDEN_PROTO
			 Argdecl,
			 Dvar dv1, Dvar dv2)
{
  request_rewrite_eq(w, RefGlob(dv1->var_ref), RefGlob(dv2->var_ref));
}

void
dvar_export_leqc MAGIC (HIDDEN_PROTO
			Argdecl,
			Dvar dv1, Dvar dv2, int c)
{
  request_rewrite_leqc(w, RefGlob(dv1->var_ref), RefGlob(dv2->var_ref), c);
}

/* Support functions for new global API */
/* Preconditions:
   - All new domains localized, or protected in term refs.
   - ar X regs live; X(ar-1) is the tail of the action list 
   - CONTPAD heap words guaranteed
   Postconditions:
   - CONTPAD heap words guaranteed
*/
/* ent: -1 = fail, 0 = suspend, 1 = exit */
void
dvar_export_done MAGIC (HIDDEN_PROTO
			Argdecl,
			SP_term_ref Actions,
			int ent)
{
  TAGGED *h;
  TAGGED action = atom_exit;
  
  switch (ent) {
  case -1:
    action = atom_fail;
  case 1:
    RequireHeap(2,EVAL_ARITY);	/* GC */
    h = w->global_top;
    h[0] = action;
    h[1] = X(EVAL_ARITY-1);
    w->global_top = h+2;
    X(EVAL_ARITY-1) = MakeList(h);
  }
  RefTerm(Actions) = X(EVAL_ARITY-1);
}


/* attaching a daemon */
/* Term = daemon(Global,AttrRef,StatusM,Ent,RawHandle) */
void 
dvar_attach_daemon MAGIC (HIDDEN_PROTO
			  Argdecl,
			  Dvar dv,
			  void *handle,
			  TAGGED global,
			  TAGGED list_functor)
{
  TAGGED *h, thandle, var;

  if (dvar_is_integer(dv))
    return;
  RequireHeap1(256+38+9,global,EVAL_ARITY);	/* see prolog_fd_global_told */
  thandle = PointerToTerm(handle);
  h = w->global_top;
  h[0] = functor_daemon5;
  h[1] = global;
  h[2] = PointerToTerm(dv->attr_ref);
  h[3] = CTagToArg(global,3);	/* status mutable */
  h[4] = CTagToArg(global,4);	/* entailment variable */
  h[5] = thandle;
  w->global_top = h+6;
  var = RefGlob(dv->var_ref);
  DerefSwitch(var, fd_link(w,var,list_functor,MakeStructure(h)););
}


#if DBG
/* Dump a dvar */
void
dvar_dump(Dvar dv)
{
  char mins[32], maxs[32], flags[100];

  strcpy(flags, "");
  if (dv->flags&DV_PRUNED_DOM)
    strcat(flags, "+PRUNED_DOM");
  if (dv->flags&DV_PRUNED_MIN)
    strcat(flags, "+PRUNED_MIN");
  if (dv->flags&DV_PRUNED_MAX)
    strcat(flags, "+PRUNED_MAX");
  if (dv->flags&DV_PRUNED_VAL)
    strcat(flags, "+PRUNED_VAL");
  if (dv->flags&DV_SET_OK)
    strcat(flags, "+SET_OK");
  if (dv->flags&DV_INTERVAL)
    strcat(flags, "+INTERVAL");
  if (dv->flags&DV_EXPORTED)
    strcat(flags, "+EXPORTED");

  if (TagIsSmall(dv->min))
    sprintf(mins, "%ld", GetSmall(dv->min)); /* buffer is large enough */
  else
    strcpy(mins, "inf");
  if (TagIsSmall(dv->max))
    sprintf(maxs, "%ld", GetSmall(dv->max)); /* buffer is large enough */
  else
    strcpy(maxs, "sup"); /* buffer is large enough */
  
  printf("DVAR address=%p attr_ref=%p var_ref=%p\n",
	 dv, dv->attr_ref, dv->var_ref);
  printf("     flags=%s\n", flags+1);
  printf("     min=%s max=%s\n", mins, maxs);
  if (!(dv->flags&DV_INTERVAL)) {
    char *sep = "{";
    DVITER it;
    
    printf("     dom=");
    dviter_init(&it,dv);
    while (!dviter_empty(&it)) {
      TAGGED tmin, tmax;
      dviter_next_interval_t(&it, &tmin, &tmax);
      if (TagIsSmall(tmin))
	sprintf(mins, "%ld", GetSmall(tmin)); /* buffer is large enough */
      else
	strcpy(mins, "inf");
      if (TagIsSmall(tmax))
	sprintf(maxs, "%ld", GetSmall(tmax)); /* buffer is large enough */
      else
	strcpy(maxs, "sup"); /* buffer is large enough */
      printf("%s%s..%s", sep, mins, maxs);
      sep = ",";
    }
    printf("}\n");
  }
}

/* test all cases of dvar_fix_set */
void
dvar_validate MAGIC (HIDDEN_PROTO_VOID)
{
  WAMENV;
  unsigned int oldmask, newmask, intersection;
  int oldarray[5], newarray[5], gotarray[5];
  int i, lb, ub, gotmin, gotmax, gothole, chk_set_ok, chk_interval;
  struct dvar dv;
  TAGGED oldset, newset, expected, got;
  FDCONS cons;
  FDITER iter;

  for (oldmask=1; oldmask<31; oldmask++)
    for (newmask=1; newmask<31; newmask++) {
      intersection = (oldmask&newmask);
      if (intersection!=oldmask) {
	w->numstack_end = NULL;
	for (i=0; i<5; i++) {
	  oldarray[i] = ((1<<i)&oldmask) ? 1 : 0;
	  newarray[i] = ((1<<i)&newmask) ? 1 : 0;
	}
	for (lb=0; lb<5; lb++)
	  if (oldarray[lb])
	    for (ub=lb; ub<5; ub++)
	      if (oldarray[ub]) {
		/* test instance now determined by oldarray,lb,ub, newarray */
		w->numstack_end = NULL;
		fdcons_init(&cons);
		for (i=0; i<5; i++)
		  if (oldarray[i])
		    fdcons_add(&cons,w,MakeSmall(i));
		oldset = fdcons_set(&cons);
		dv.set = oldset;
		dv.min = fd_min(dv.set);
		dv.max = fd_max(dv.set);
		dv.flags = DV_SET_OK;
		if (dv.min==dv.max)
		  dv.flags |= DV_PRUNED_VAL;
		if (CTagToCdr(dv.set)==EmptySet)
		  dv.flags |= DV_INTERVAL;
		fdcons_init(&cons);
		for (i=0; i<5; i++)
		  if (newarray[i])
		    fdcons_add(&cons,w,MakeSmall(i));
		newset = fdcons_set(&cons);
		
		expected = fd_intersection(w,fd_intersection_interval(w,dv.set,MakeSmall(lb),MakeSmall(ub)),
				  newset);
		for (i=0; i<5; i++)
		  gotarray[i] = 0;
		dvar_fix_interval_l(&dv, lb, ub);
		gotmin = 5;
		gotmax = 0;
		gothole = 0;
		if (dvar_fix_set(&dv, newset,w)<0)
		  got = EmptySet;
		else {
		  got = fd_intersection_interval(w,dv.set,dv.min,dv.max);
		  fditer_init(&iter, dv.set);
		  while (!fditer_empty(&iter))
		    gotarray[GetSmall(fditer_next(&iter))] = 1;
		  for (i=0; i<5; i++)
		    if (gotarray[i]) {
		      if (gotmin>i)
			gotmin = i;
		      gotmax = i;
		    }
		  for (i=dvar_min_l(&dv); i<=dvar_max_l(&dv); i++)
		    if (!gotarray[i])
		      gothole = 1;
		}
		chk_set_ok =
		  (got==EmptySet ||
		   !(dv.flags & DV_SET_OK) ==
		   (dvar_min_l(&dv)!=gotmin || dvar_max_l(&dv)!=gotmax));
		chk_interval =
		  (got==EmptySet || !(dv.flags & DV_INTERVAL) == gothole);
		if (fd_compare(expected,got)!=FDI_EQUAL ||
		    !chk_set_ok ||
		    !chk_interval) {
		  printf("! wrong result in dvar_fix_set\n");
		  printf("OLD SET = [%d %d %d %d %d]\n",
			 oldarray[0], oldarray[1], oldarray[2], oldarray[3], oldarray[4]);
		  printf("NEW SET = [%d %d %d %d %d]\n",
			 newarray[0], newarray[1], newarray[2], newarray[3], newarray[4]);
		  printf("GOT SET = [%d %d %d %d %d]\n",
			 gotarray[0], gotarray[1], gotarray[2], gotarray[3], gotarray[4]);
		  printf("oldlb=%d oldub=%d newlb=%d newub=%d flags=0x%x\n",
			 lb, ub, (int)dvar_min_l(&dv), (int)dvar_max_l(&dv), dv.flags);
		  return;
		}
	      }
      }
    }
}
#endif

#if 0

/* Prolog code to compute case analysis. */

:- use_module(library(ordsets)).

equal(p,r) --> opt(pr), opt(pqr), opt(q).
subset(p,r) --> opt(pr), opt(pqr), opt(q), must(r,qr).
superset(p,r) --> opt(pr), opt(pqr), opt(q), must(p,pq).
disjoint(p,r) --> must(p,pq), must(r,qr), opt(q).
intersect(p,r) --> must(p,pq), must(r,qr), must(pr,pqr), opt(q).

equal(q,r) --> opt(qr), opt(pqr), opt(p).
subset(q,r) --> opt(qr), opt(pqr), opt(p), must(r,pr).
superset(q,r) --> opt(qr), opt(pqr), opt(p), must(q,pq).
disjoint(q,r) --> must(q,pq), must(r,pr), opt(p).
intersect(q,r) --> must(q,pq), must(r,pr), must(qr,pqr), opt(p).

opt(X) --> []; [X].

must(X,Y) --> [X]; [Y]; [X,Y].

main(C1, C2, Relations) :-
	case(p, C1),
	case(q, C2),
	combine(C1, C2, Set),
	patterns_to_relations(Set, Relations).

combine(Rel1, Rel2, Set) :-
	findall(P, pattern(Rel1,P), Bag1),
	findall(Q, pattern(Rel2,Q), Bag2),
	sort(Bag1, Set1),
	sort(Bag2, Set2),
	ord_intersection(Set1, Set2, Set).

pattern(Rule, Set) :-
	phrase(Rule, Bag),
	sort(Bag, Set),
	ord_intersect(Set, [pq,pqr]).
	
case(X, equal(X,r)).
case(X, subset(X,r)).
case(X, superset(X,r)).
case(X, disjoint(X,r)).
case(X, intersect(X,r)).

patterns_to_relations([], []).
patterns_to_relations([S|L1], [R|L2]) :-
	pattern_to_relation(S, R),
	patterns_to_relations(L1, L2).

pattern_to_relation(Pat, equal) :-
	ord_disjoint([pq,pr,qr,r], Pat).
pattern_to_relation(Pat, subset) :-
	ord_disjoint([pq], Pat),
	ord_intersect([pr,qr,r], Pat).
pattern_to_relation(Pat, superset) :-
	ord_intersect([pq], Pat),
	ord_disjoint([pr,qr,r], Pat).
pattern_to_relation(Pat, disjoint) :-
	ord_intersect([pq], Pat),
	ord_intersect([pr,qr,r], Pat),
	ord_disjoint([pqr], Pat).
pattern_to_relation(Pat, intersect) :-
	ord_intersect([pq], Pat),
	ord_intersect([pr,qr,r], Pat),
	ord_intersect([pqr], Pat).

#endif

