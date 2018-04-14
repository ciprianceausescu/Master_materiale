/* Copyright(C) 2002, Swedish Institute of Computer Science */

/*  Bound-consistent all_different/1 constraint.  See:
    K. Mehlhorn and Sven Thiel.
    Faster algorithms for bound-consistency of the sortedness 
    and the alldifferent constraint.
    In Sixth Int. Conf. on Principles and Practice of Constraint Programming 
    (CP2000), Lecture Notes in Computer Science 1894, Springer, 2000. 
*/

#include "fd.h"
#include "dvars.h"

#if MULTI_SP_AWARE
#define narrow(A1,A2) narrow(HIDDEN_ARG, A1,A2)
#define cons_batch(A1,A2) cons_batch(HIDDEN_ARG, A1,A2)
#define pairing_filter(A1,A2,A3) pairing_filter(HIDDEN_ARG, A1,A2,A3)
#endif /* MULTI_SP_AWARE */

struct alldiff_data {
  void (SPCDECL *destructor)(void *);
  void (SPCDECL *daemon)(HIDDEN_PROTO
			 Argtype,void *,SP_globref,TAGGED); /* (w,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  long stamp;			/* increases up to backtracking */
  int enqueue_code;
  int nvars;			/* _original_ numberx */
  int nvals;			/* _original_ numbery */
  int numberx;
  int numbery;
  int numberz;			/* nonkernel xs */
  int nscc;
  long yminl;			/* currently smallest domain value */
  int *target;			/* [nvars], "active" var */
  int *xmate;			/* [nvars], x's matched val, volatile */
  int *sortmin;			/* [nvars], vars by ascending min, volatile */
  int *sortmax;			/* [nvars], vars by ascending max, volatile */
  int *sorty;			/* [nvars], ys ascending, volatile */
  int *ymate;			/* [nvals], y's matched var, volatile */
  int *chunk;			/* [nvals], chunk in which var was inserted, volatile */
  int *class;			/* [nvals], class map in off-line-minimum, volatile */
  int *scc;			/* [nvals], scc ID of val, volatile */
  int *root;			/* [nvals], smallest elt of scc of val, volatile */
  int *rightmost;		/* [nvals], greatest elt of scc of val, volatile */
  int *maxx;			/* [nvals], greatest neighbor of scc of val, volatile */
  int *type;			/* [nvals], 1=matched, 2=toplevel, 4=marked, volatile */
  int *max_free_or_marked;	/* [nvals], max. y<=arg that is free or marked, volatile */
  struct {
    int *node;			/* [nvals], volatile */
    int *root;			/* [nvals], volatile */
    int *rightmost;		/* [nvals], volatile */
    int *maxx;			/* [nvals], volatile */
  } stk;
  Dvar dvar;
  int *live;
};

#define RefAttr(T) (pdata->refbase + 2*(T))
#define RefVar(T) (pdata->refbase + 2*(T) + 1)
#define SV(T) (pdata->target[T])
#define DVAR(T) (pdata->dvar+(T))
#define XMIN(I) (dvar_min_l(DVAR(I))-pdata->yminl)
#define XMAX(I) (dvar_max_l(DVAR(I))-pdata->yminl)
#define XMATE(I) (pdata->xmate[I]) /* as offset from yminl */
#define YMATE(I) (pdata->ymate[I]) /* as regular index */
#define YSCC(I) scc[I]
#define XSCC(I) scc[XMATE(I)]

static void SPCDECL bc_alldiff_destructor(void *pdata_v)
{
  struct alldiff_data *pdata = (struct alldiff_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nvars<<1);
  SP_free(pdata);
}

static void SPCDECL 
bc_alldiff_daemon MAGIC (HIDDEN_PROTO
			 Argdecl,
			 void *vdata,
			 SP_globref attr_ref,
			 TAGGED global)
{
  struct alldiff_data *pdata = (struct alldiff_data *)vdata;
  TAGGED tstate, tmin, tmax, tdone;

  tstate = RefMutable(CTagToArg(global,1));
  DerefArg(tdone,tstate,2);
  tmax = RefGlob(attr_ref);
  DerefAttribute(tmax,tmax); /* get dom/4 term */
  tmin = DomainMin(tmax);
  tmax = DomainMax(tmax);
  if (AreSmall(tmin,tmax) &&
      GetSmall(tmax)-GetSmall(tmin)+1 < pdata->nvars-GetSmall(tdone)) /* CLAIM: otherwise, can't prune */
    fd_enqueue_global(w, global, pdata->enqueue_code);
}

static SP_BOOL
match(struct alldiff_data *pdata)
{
  int i, j, e, x;
  int numberx = pdata->numberx;
  int numbery = pdata->numbery;
  int *chunk = pdata->chunk;
  int *class = pdata->class;

  KEYSORT(pdata->target, numberx, int,
	  chunk, numbery, XMIN,
	  pdata->sortmin);
  KEYSORT(pdata->target, numberx, int,
	  chunk, numbery, XMAX,
	  pdata->sortmax);

  /* simulate a priority queue and an iteration from 0 to numbery s.t.
     in iteration j:
     1. add all i to P with j-1<XMIN(i)<=j
     2. if P empty, then j will not be matched
     3. otherwise, extract i with smallest XMAX(i),
        YMATE(j)=i,
	if XMAX(i)<j, then there is no matching

     No actual priority queue is used.
     We solve the off-line-minimum problem instead.
     TODO: add a disjoint-set data structure instead.
  */

  /* compute chunks and unmatched Y nodes */
  e = 0;			/* extract count */
  i = 0;			/* sortmin index */
  x = pdata->sortmin[i];
  for (j=0; j<numbery; j++) {
    while (i<numberx && XMIN(x)<=j) {
      chunk[x] = j;
      i++;
      if (i<numberx)
	x = pdata->sortmin[i];
    }
    if (i==e) {
      class[j] = i<numberx ? XMIN(x) : j+1;
      pdata->type[j] = 0;	/* not matched */
      YMATE(j) = -1;		/* for convenience */
    } else {
      class[j] = j;
      pdata->type[j] = 1;	/* matched */
      e++;
    }
  }
  if (i!=e)
    return FALSE;

  /* poor man's off-line-minimum algorithm */
  for (e=0; e<numberx; e++) {
    x = pdata->sortmax[e];
    j = chunk[x];
    while (j<class[j])		/* dereference class */
      j = class[j];		/* TODO: path compression */
    if (XMAX(x)<j)
      return FALSE;
    YMATE(j) = x;
    XMATE(x) = j;
    class[j] = j+1;		/* merge with next class */
  }

  return TRUE;
}

static void
findscc(struct alldiff_data *pdata)
{
  int j, jmax, sccid, top1, top2;
  int numbery = pdata->numbery;

  /* init */
  j=0; sccid=0; top1=0; top2=0;
  while (j-top1<numbery) {
    int x = j<numbery ? YMATE(j) : pdata->nvars;
    if (x<0)			/* unmatched node */
      j++;
    else if (top2==0) {	/* start 1st component */
      pdata->stk.node[0] = j;
      pdata->stk.root[0] = j;
      pdata->stk.rightmost[0] = j;
      pdata->stk.maxx[0] = XMAX(x);
      top1 = top2 = 1;
      j++;
    } else if (pdata->stk.maxx[top2-1]>=j) { /* start/extend component */
      int min1 = XMIN(x);
      int max1 = XMAX(x);
      
      pdata->stk.node[top1++] = j;
      pdata->stk.root[top2] = j;
      while (top2>0 && min1<=pdata->stk.rightmost[top2-1]) { /* merge components */
	top2--;
	if (max1<pdata->stk.maxx[top2])
	  max1 = pdata->stk.maxx[top2];
      }
      pdata->stk.rightmost[top2] = j;
      pdata->stk.maxx[top2++] = max1;
      j++;
    } else {			/* component done */
      int root1 = pdata->stk.root[--top2];
      int rightmost1 = pdata->stk.rightmost[top2];
      int maxx1 = pdata->stk.maxx[top2];
      int y;

      do {
	y = pdata->stk.node[--top1];
	pdata->scc[y] = sccid;
	pdata->root[y] = root1;
	pdata->rightmost[y] = rightmost1;
	pdata->maxx[y] = maxx1;
      } while (y>root1);
      sccid++;
    }
  }
  pdata->nscc = sccid;

  jmax = numbery;
  for (j=numbery-1; j>=0;) {
    if (!pdata->type[j])	/* unmatched */
      jmax = j--;
    else {			/* a toplevel scc */
      int root1 = pdata->root[j];

      sccid = pdata->scc[j];
      while (j>=root1) {
	if (pdata->scc[j]==sccid && pdata->maxx[j]>=jmax) {
	  pdata->type[j] |= 0x4; /* can reach free y */
	  jmax = j;
	}
	if (pdata->scc[j]==sccid)
	  pdata->type[j] |= 0x2; /* toplevel component */
	j--;
      }
    }
  }
}

/* 0=ok, 1=rerun due to holes, -1=fail */
static int
narrow MAGIC (HIDDEN_PROTO struct alldiff_data *pdata,Argdecl)
{
  int i, j, n, nx;
  int numberx = pdata->numberx;
  int numbery = pdata->numbery;
  long yminl = pdata->yminl;
  int *chunk = pdata->chunk;
  int *class = pdata->class;
  int *scc = pdata->scc;
  int rerun = 0;

  if (pdata->nscc==1)		/* no pruning possible */
    goto nonkernels;

  /* compute max_free_or_marked */
  if (numberx<numbery) {
    n = -1;
    for (i=0; i<numbery; i++) {
      if ((pdata->type[i] & 0x5) != 0x1) /* if unmatched or marked */
	n = i;
      pdata->max_free_or_marked[i] = n;
    }
  }
  
  /* Compute three arrays */
  /* sorty   : matched y's by (a) SCC (b) increasing y */
  /* sortmin : x's by (a) SCC (b) increasing XMIN */
  /* sortmax : x's by (a) SCC (b) increasing XMAX */
  for (i=0, j=0; j<numbery; j++) {
    if (pdata->type[j]) {
      int x = YMATE(j);
      Dvar dv = DVAR(x);
      if (!dvar_is_integer(dv))
	class[i++] = j;
    }
  }
  nx = i;
  if (nx==0)
    goto nonkernels;
  KEYSORT(class, nx, int,
	  chunk, pdata->nscc, YSCC,
	  pdata->sorty);
  for (i=0, j=0; i<numberx; i++) {
    int x = pdata->sortmin[i];
    Dvar dv = DVAR(x);
    if (!dvar_is_integer(dv))
      class[j++] = x;
  }
  KEYSORT(class, nx, int,
	  chunk, pdata->nscc, XSCC,
	  pdata->sortmin);
  for (i=0, j=0; i<numberx; i++) {
    int x = pdata->sortmax[i];
    Dvar dv = DVAR(x);
    if (!dvar_is_integer(dv))
      class[j++] = x;
  }
  KEYSORT(class, nx, int,
	  chunk, pdata->nscc, XSCC,
	  pdata->sortmax);

  /* Adjust all lower bounds. */
  j = 0;
  for (i=0; i<nx; i=n) {
    int y = pdata->sorty[i];
    int sccid = scc[y];
    n = i+1;
    while (n<nx && scc[pdata->sorty[n]]==sccid)
      n++;
    while (j<n) {
      int x = pdata->sortmin[j];
      int xmin = XMIN(x);
      if (xmin<y) {
	int rc = dvar_fix_min_l(DVAR(x), y+yminl);
	if (rc<0)
	  return -1;
	else if ((rc&DV_PRUNED_MIN) && XMIN(x)>y)
	  rerun = 1;		/* new bound hit a hole */
	j++;
      } else if (xmin==y)
	j++;
      else
	if (++i<nx)
	  y = pdata->sorty[i];
    }
  }

  /* Adjust all upper bounds. */
  j = nx-1;
  for (i=nx-1; i>=0; i=n) {
    int y = pdata->sorty[i];
    int sccid = scc[y];
    n = i-1;
    while (n>=0 && scc[pdata->sorty[n]]==sccid)
      n--;
    while (j>n) {
      int x = pdata->sortmax[j];
      int xmax = XMAX(x);
      if (xmax>y) {
	int rc, ub = y;
	if (pdata->type[y] & 0x4) { /* can reach free value */
	  int ub1 = pdata->max_free_or_marked[xmax];
	  if (ub<ub1)
	    ub = ub1;
	}
	rc = dvar_fix_max_l(DVAR(x), ub+yminl);
	if (rc<0)
	  return -1;
	else if ((rc&DV_PRUNED_MAX) && XMAX(x)<ub)
	  rerun = 1;		/* new bound hit a hole */
	j--;
      } else if (xmax==y)
	j--;
      else
	if (--i>=0)
	  y = pdata->sorty[i];
    }
  }

  /* Adjust nonkernels */
 nonkernels:
  if (pdata->numberz>0) {
    FDCONS cons;
    int top = pdata->numberx + pdata->numberz;

    fdcons_init(&cons);
    for (j=0; j<numbery; j++) {
      if ((pdata->type[j] & 0x5) == 0x1) /* matched and not marked */
	fdcons_add(&cons,w,MakeSmall(j+yminl));
    }
    for (i=pdata->numberx; i<top; i++) {
      int elt = SV(i);
      int rc;
      Dvar dv = DVAR(elt);
      TAGGED lb = Inf;
      TAGGED ub = Sup;
      TAGGED d = fdcons_set(&cons);
      while (d!=EmptySet && ub==Sup) {
	TAGGED r = CTagToCar(d);
	TAGGED car = RangeMin(r);
	TAGGED cdr = RangeMax(r);
	d = CTagToCdr(d);
	if (lb==Inf && InInterval(dvar_min_t(dv),car,cdr))
	  lb = FDincr(cdr);
	if (InInterval(dvar_max_t(dv),car,cdr))
	  ub = FDdecr(car);
      }
      rc = dvar_fix_interval_t(dv, lb, ub);
      if (rc<0)
	return -1;
      else if (((rc&DV_PRUNED_MIN) && FDgt(dvar_min_t(dv),lb)) ||
	       ((rc&DV_PRUNED_MAX) && FDlt(dvar_max_t(dv),ub)))
	rerun = 1;		/* new bound hit a hole */
    }
  }
	
  return rerun;
}


/*
  '$fd_bc_alldiff'(+State0, +State, -Actions).
  State = state(Vec,NDone,Handle,Stamp)
*/
void SPCDECL
prolog_fd_bc_alldiff MAGIC (HIDDEN_PROTO
			    SP_term_ref State0,
			    SP_term_ref State,
			    SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec, ymin, ymax, f;
  long state_stamp;
  int i, nvars=0, nvars2, nvals, elt, total_size, rerun;
  struct alldiff_data *pdata;
  SP_BOOL committed;
  Dvar dv;
  char *ptr;
  WAMENV;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


  w->numstack_end = NULL;
/*    X(0) = RefTerm(State0); */
  dvar_export_start(w);
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct alldiff_data,handle);
    nvars = pdata->nvars;
    nvals = pdata->nvals;
  } else {			/* build persistent state */
    ymin = TaggedHigh;
    ymax = TaggedLow;
    DerefArg(tvec,X(0),1);	/* get Vec */
    while (TagIsLST(tvec)) {	/* count terms, moving ground terms to RHS */
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(telt,telt,2);	/* get attribute */
      DerefAttribute(telt,telt); /* dom/4 term */
      if (FDgt(ymin,DomainMin(telt)))
	ymin = DomainMin(telt);
      if (FDlt(ymax,DomainMax(telt)))
	ymax = DomainMax(telt);
      nvars++;
    }
    if (nvars==0) {
      ent = 1;
      goto ret1;
    }
    nvars2 = nvars*nvars;
    if (!AreSmall(ymin,ymax))
      nvals = nvars2;
    else
      nvals = GetSmall(ymax)-GetSmall(ymin)+1;
    if (nvals>nvars2)
      nvals = nvars2;
    total_size = nvars*(sizeof(int) + sizeof(struct dvar));
    pdata = Palloc(struct alldiff_data, total_size, handle);
    pdata->destructor = bc_alldiff_destructor;
    pdata->daemon = bc_alldiff_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->nvars = nvars;
    pdata->nvals = nvals;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr = (char *)(pdata->dvar+nvars);
    pdata->target = (int *)ptr;
    ptr = (char *)(pdata->target+nvars);
#if DBG
    if (ptr != (char *)(pdata+1)+total_size)
      printf("SHOULDN'T HAPPEN: expected ptr=%p, got %p\n",
	     (char *)(pdata+1)+total_size, ptr);
#endif
    DerefArg(f,X(0),3);
    f = SetArity(f,1);
    pdata->enqueue_code =
      f==fd.functor_val ? 0x3 :
      f==fd.functor_minmax ? 0x5 :
      f==fd.functor_dom ? 0x7 : 0;
    DerefArg(tvec,X(0),1);		/* get Vec */
    for (elt=0; elt<nvars; elt++) {
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      get_var_and_attr(telt,RefAttr(elt));
      SV(elt) = elt;
      if (pdata->enqueue_code) {
	dvar_init(DVAR(elt), RefAttr(elt), RefVar(elt));
	dvar_attach_daemon(w, DVAR(elt), pdata, X(1), f);
      }
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  ptr = fd_malloc(4*nvars*sizeof(int) + 13*nvals*sizeof(int));
  pdata->xmate = (int *)ptr;
  ptr = (char *)(pdata->xmate+nvars);
  pdata->sortmin = (int *)ptr;
  ptr = (char *)(pdata->sortmin+nvars);
  pdata->sortmax = (int *)ptr;
  ptr = (char *)(pdata->sortmax+nvars);
  pdata->sorty = (int *)ptr;
  ptr = (char *)(pdata->sorty+nvars);
  pdata->ymate = (int *)ptr;
  ptr = (char *)(pdata->ymate+nvals);
  pdata->chunk = (int *)ptr;
  ptr = (char *)(pdata->chunk+nvals);
  pdata->class = (int *)ptr;
  ptr = (char *)(pdata->class+nvals);
  pdata->scc = (int *)ptr;
  ptr = (char *)(pdata->scc+nvals);
  pdata->root = (int *)ptr;
  ptr = (char *)(pdata->root+nvals);
  pdata->rightmost = (int *)ptr;
  ptr = (char *)(pdata->rightmost+nvals);
  pdata->maxx = (int *)ptr;
  ptr = (char *)(pdata->maxx+nvals);
  pdata->type = (int *)ptr;
  ptr = (char *)(pdata->type+nvals);
  pdata->max_free_or_marked = (int *)ptr;
  ptr = (char *)(pdata->max_free_or_marked+nvals);
  pdata->stk.node = (int *)ptr;
  ptr = (char *)(pdata->stk.node+nvals);
  pdata->stk.root = (int *)ptr;
  ptr = (char *)(pdata->stk.root+nvals);
  pdata->stk.rightmost = (int *)ptr;
  ptr = (char *)(pdata->stk.rightmost+nvals);
  pdata->stk.maxx = (int *)ptr;
  ptr = (char *)(pdata->stk.maxx+nvals);
  DerefArg(telt,X(0),2);
  pdata->numberx = nvars-GetSmall(telt);
  pdata->stamp = state_stamp+1;
  for (i=pdata->numberx-1; i>=0; i--) {
    elt = SV(i);
    dv = DVAR(elt);
    dvar_init(dv, RefAttr(elt), RefVar(elt));
  }
  do {
    int delta = 1;
    ymin = TaggedHigh;
    ymax = TaggedLow;
    DerefArg(telt,X(0),2);
    pdata->numberx = nvars-GetSmall(telt);
    pdata->numberz = 0;
    while (delta>0) {
      /* partition into kernel/nonkernel */
      int inf = 0;
      int sup = pdata->numberx-1;
      int held = SV(sup); /* sup is the hole */
      int current = SV(0);
    
      while (inf<=sup) {
	Dvar dv = DVAR(current);
	if (AreSmall(dvar_min_t(dv),dvar_max_t(dv)) &&
	    dvar_max_l(dv)-dvar_min_l(dv) < pdata->numberx) {
	  SV(inf) = current;
	  inf++;
	  current = (inf>=sup ? held : SV(inf));
	} else {
	  SV(sup) = current;
	  sup--;
	  current = (inf>=sup ? held : SV(sup));
	}
      }
      delta = pdata->numberx - inf;
      pdata->numberz += delta;
      pdata->numberx -= delta;
    } 

    if (pdata->numberx==0) {
      pdata->yminl = 0;
      pdata->numbery = 0;
    } else {
      for (i=0; i<pdata->numberx; i++) {
	elt = SV(i);
	dv = DVAR(elt);
	pdata->sorty[i] = dvar_max_l(dv)-dvar_min_l(dv);
	if (Tgt(ymin,dvar_min_t(dv)))
	  ymin = dvar_min_t(dv);
	if (Tlt(ymax,dvar_max_t(dv)))
	  ymax = dvar_max_t(dv);
      }
      pdata->yminl = GetSmall(ymin);
      pdata->numbery = GetSmall(ymax)-GetSmall(ymin)+1;
      if (pdata->numberx > pdata->numbery)
	goto ret;
    }
#if 0
    /* check sufficient condition for a single SCC */
    {
      int *chunk = pdata->chunk;
      int n;
      
      for (i=pdata->numbery-1; i>=0; i--)
	chunk[i] = 0;
      for (i=0; i<pdata->numberx; i++) {
	n = pdata->sorty[i];
	if (++chunk[n] > n)
	  goto cont;
      }
      n = chunk[0];
      for (i=1; i<pdata->numbery; i++) {
	if (n>=i)
	  goto cont;
	n += chunk[i];
      }
      ent = 0;			/* ??? does a matching exist for sure? */
      goto ret;
    }
  cont:
#endif
    if (!match(pdata))
      goto ret;
    findscc(pdata);
    rerun = narrow(pdata,w);
  } while (rerun==1);
  if (rerun<0)
    goto ret;
  /* determine min/max domain value of any nonground var */
  pdata->numberx += pdata->numberz;
  ymin = TaggedHigh;
  ymax = TaggedLow;
  for (i=0; i<pdata->numberx; i++) {
    Dvar dv = DVAR(SV(i));
    dvar_export(w,dv);
    if (!dvar_is_integer(dv)) {
      if (Tgt(ymin,dvar_min_t(dv)))
	ymin = dvar_min_t(dv);
      if (Tlt(ymax,dvar_max_t(dv)))
	ymax = dvar_max_t(dv);
    }
  }
  /* partition into target/source */
  {
    int inf = 0;
    int sup = pdata->numberx-1;
    int held = SV(sup); /* sup is the hole */
    int current = SV(0);
    
    while (inf<=sup) {
      Dvar dv = DVAR(current);
      TAGGED dvmin = dvar_min_t(dv);
      if (!dvar_is_integer(dv) || InInterval(dvmin,ymin,ymax)) {
	SV(inf) = current;
	inf++;
	current = (inf>=sup ? held : SV(inf));
      } else {
	SV(sup) = current;
	sup--;
	current = (inf>=sup ? held : SV(sup));
      }
    }
    CTagToArg(X(0),2) = MakeSmall(nvars-inf);
    ent = (inf<=1);
  }
 ret:
  SP_free(pdata->xmate);
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(w,Actions, ent);
}

