/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

#if MULTI_SP_AWARE
#define relation_scan_dom_interval(A1,A2,A3,A4,A5,A6,A7,A8,A9) relation_scan_dom_interval(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7,A8,A9)
#define relation_scan_dom(A1,A2,A3,A4,A5,A6,A7) relation_scan_dom(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7)
#define relation_scan_dom_symm(A1,A2,A3,A4,A5,A6,A7) relation_scan_dom_symm(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7)
#define relation_scan_delta(A1,A2,A3,A4,A5) relation_scan_delta(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define relation_scan_delta_symm(A1,A2,A3,A4,A5) relation_scan_delta_symm(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define relation_fdset(A1,A2) relation_fdset(HIDDEN_ARG, A1,A2)
#define element_feasible(A1,A2,A3,A4,A5) element_feasible(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define qsort_mapswap(A1,A2,A3) qsort_mapswap(HIDDEN_ARG, A1,A2,A3)
#define qsort_mapmed3(A1,A2,A3) qsort_mapmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_map(A1,A2) qsort_map(HIDDEN_ARG, A1,A2)
#endif /* MULTI_SP_AWARE */

/*
   '$fd_relation'(+State0, -State, -Actions) :-
   State0 is f(X,XMut,Y,YMut,Xu,Yu,XY,Opt) where
   Xu is the previous X domain,
   Yu is the previous Y domain,
   XY is the extension repr. as Xset-Yset pairs (unique X)
   (Opt & 0x1) iff bound-consistent, otherwise arc-consistent
   (Opt & 0x2) iff Y sets are all disjoint,
   (Opt & 0x4) iff Y sets form increasing, disjoint intervals
   Actions is a list of prunings etc.
*/
struct relation_data {
  void (SPCDECL *destructor)(void *);
  void (SPCDECL *daemon)(HIDDEN_PROTO
			 Argtype,void *,SP_globref,TAGGED); /* (w,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  long stamp;
  long first;			/* head of (-1)-terminated list of queued tuples */
  long *next;			/* next[i]==(-2) iff not in the queue */
  struct dvar *dvbase;
  SP_globref refbase;		/* {XMut, X, UXMut, YMut, Y, UYMut}ntuples, Y{xy_n} */
  int ntuples;			/* #tuples */
  int xy_n;			/* #values in X->Y extension */
  int xy_r;			/* #X intervals in X->Y extension */
  int opt;
  struct mapentry *map;
};

struct mapentry {
  TAGGED min;
  TAGGED max;
  TAGGED ymin;
  TAGGED ymax;
  SP_globref value;
};


static int cmp_map(struct mapentry *t1, struct mapentry *t2)
{
  return TCMP(t1->min,t2->min);
}

#define QType struct mapentry
#define QCmp  cmp_map
#define QSort qsort_map
#include "qsort.ic"

static void SPCDECL relation_destructor(void *pdata_v)
{
  struct relation_data *pdata = (struct relation_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->xy_n+6*pdata->ntuples);
  SP_free(pdata);
}


static TAGGED
relation_fdset MAGIC (HIDDEN_PROTO
		      Argdecl,
		      SP_globref attr_ref)
{
  TAGGED tmp = RefGlob(attr_ref);
  TAGGED *h, min, max, t;

  DerefAttribute(tmp,tmp);	/* get dom/4 term */
  tmp = DomainSet(tmp);
				/* protect from smashing */
  t = CTagToCar(tmp);
  min = RangeMin(t);
  max = RangeMax(t);
  t = CTagToCdr(tmp);
  RequireHeap1(4,t,EVAL_ARITY);
  h = w->global_top;
  w->global_top = h+4;
  h[0] = min;
  h[1] = max;
  h[2] = MakeList(h);
  h[3] = t;
  return MakeList(h+2);
}

static void
relation_scan_dom_interval MAGIC (HIDDEN_PROTO
				  struct relation_data *pdata,
				  Argdecl,
				  TAGGED min, TAGGED max, /* Invariant: this \subseteq the set of X values */
				  TAGGED miny, TAGGED maxy,
				  SP_BOOL nodeltay,
				  TAGGED *punsafex,
				  TAGGED *punsafey)
{
  TAGGED unsafex, mins, maxs;
  int ix = 0;
  int ixend = pdata->xy_r;
  
  unsafex = fd_interval(w,min,max);
  mins = Sup;
  maxs = Inf;

  /* scan */
  while (ix<ixend && FDle(pdata->map[ix].min,max)) {
    struct mapentry *mapix = &pdata->map[ix];

    if (FDgt(min,mapix->max)) {
      /* dichotomic search for next hit */
      int mid;
      int sup = ixend;
      while (ix<sup) {
	mid = (ix+sup)>>1;
	if (FDgt(min,pdata->map[mid].max))
	  ix = mid+1;
	else
	  sup = mid;
      }
    } else {
      if (nodeltay)
	goto hity;
      else if (mapix->ymin==mapix->ymax) {
	if (InInterval(mapix->ymin,miny,maxy)) {
	  goto hit;
	}
      } else {
	switch (fd_compare_interval(RefGlob(mapix->value),miny,maxy)) {
	case FDI_DISJOINT:
	  break;
	default:
	hit:
	  unsafex = fd_subtract_interval(w,unsafex,mapix->min,mapix->max);
	hity:
	  if (FDgt(mins,mapix->ymin))
	    mins = mapix->ymin;
	  if (FDlt(maxs,mapix->ymax))
	    maxs = mapix->ymax;
	  if (nodeltay && FDle(mins,miny) && FDge(maxs,maxy))
	    goto rety;
	}
      }
      ix++;
      if (ix==ixend)
	break;
      min = pdata->map[ix].min;
    }
  }
  if (!nodeltay)
    *punsafex = unsafex;
 rety:
  if (mins==Sup)
    *punsafey = fd_interval(w,Inf,Sup);
  else
    *punsafey = fd_complement(w,fd_interval(w,mins,maxs));
}

static void
relation_scan_dom MAGIC (HIDDEN_PROTO
			 struct relation_data *pdata,
			 Argdecl,
			 TAGGED domainx, /* Invariant: this \subseteq the set of X values */
			 TAGGED domainy,
			 SP_BOOL nodeltay,
			 TAGGED *punsafex,
			 TAGGED *punsafey)
{
  TAGGED unsafex, unsafey, min, max, domainx_rest;
  int ix = 0;
  int ixend = pdata->xy_r;
  
  unsafex = domainx;
  unsafey = domainy;
  min = fd_min(domainx);
  max = fd_max(domainx);
  domainx_rest = fd_interval(w,Inf,Sup);

  /* scan */
  while (ix<ixend && FDle(pdata->map[ix].min,max)) {
    struct mapentry *mapix = &pdata->map[ix];

    if (FDgt(min,mapix->max)) {
      /* dichotomic search for next hit */
      int mid;
      int sup = ixend;
      while (ix<sup) {
	mid = (ix+sup)>>1;
	if (FDgt(min,pdata->map[mid].max))
	  ix = mid+1;
	else
	  sup = mid;
      }
    } else {
      if (nodeltay)
	goto hity;
      else if (mapix->ymin==mapix->ymax) {
	if (fd_member(mapix->ymin,domainy)) {
	  unsafex = fd_subtract_interval(w,unsafex,mapix->min,mapix->max);
	  unsafey = fd_subtract_interval(w,unsafey,mapix->ymin,mapix->ymax);
	}
      } else {
	switch (fd_compare(domainy,RefGlob(mapix->value))) {
	case FDI_DISJOINT:
	  break;
	default:
	  unsafex = fd_subtract_interval(w,unsafex,mapix->min,mapix->max);
	hity:
	  unsafey = fd_subtract(w,unsafey,RefGlob(mapix->value));
	  if (nodeltay && unsafey==EmptySet)
	    goto rety;
	}
      }
      ix++;
      if (ix==ixend)
	break;
      RangeMin(CTagToCar(domainx_rest)) = pdata->map[ix].min;
      min = fd_intersection_min(domainx,domainx_rest);
    }
  }
  if (!nodeltay)
    *punsafex = unsafex;
 rety:
  *punsafey = unsafey;
}

static void
relation_scan_dom_symm MAGIC (HIDDEN_PROTO
			      struct relation_data *pdata,
			      Argdecl,
			      TAGGED domainx, /* Invariant: this \subseteq the set of X values */
			      TAGGED domainy,
			      SP_BOOL nodeltay,
			      TAGGED *punsafex,
			      TAGGED *punsafey)
{
  TAGGED unsafex, unsafey, min, max, ymin, ymax, domainx_rest, domainy_rest;
  int ix = 0;
  int ixend = pdata->xy_r;
  
  unsafex = domainx;
  unsafey = domainy;
  min = fd_min(domainx);
  max = fd_max(domainx);
  domainx_rest = fd_interval(w,Inf,Sup);
  ymin = fd_min(domainy);
  ymax = fd_max(domainy);
  domainy_rest = fd_interval(w,Inf,Sup);

  /* scan */
  while (ix<ixend && FDle(pdata->map[ix].min,max) && FDle(pdata->map[ix].ymin,ymax)) {
    struct mapentry *mapix = &pdata->map[ix];

    if (FDgt(min,mapix->max) || FDgt(ymin,mapix->ymax)) {
      /* dichotomic search for next hit */
      int mid;
      int sup = ixend;
      while (ix<sup) {
	mid = (ix+sup)>>1;
	if (FDgt(min,pdata->map[mid].max) || FDgt(ymin,pdata->map[mid].ymax))
	  ix = mid+1;
	else
	  sup = mid;
      }
    } else {
      if (fd_compare_interval(domainx,mapix->min,mapix->max)==FDI_DISJOINT)
	;
      else if (nodeltay)
	goto hity;
      else if (mapix->ymin==mapix->ymax) {
	if (fd_member(mapix->ymin,domainy)) {
	  unsafex = fd_subtract_interval(w,unsafex,mapix->min,mapix->max);
	  unsafey = fd_subtract_interval(w,unsafey,mapix->ymin,mapix->ymax);
	}
      } else {
	switch (fd_compare_interval(domainy,mapix->ymin,mapix->ymax)) {
	case FDI_DISJOINT:
	  break;
	default:
	  unsafex = fd_subtract_interval(w,unsafex,mapix->min,mapix->max);
	hity:
	  unsafey = fd_subtract_interval(w,unsafey,mapix->ymin,mapix->ymax);
	  if (nodeltay && unsafey==EmptySet)
	    goto rety;
	}
      }
      ix++;
      if (ix==ixend)
	break;
      RangeMin(CTagToCar(domainx_rest)) = pdata->map[ix].min;
      min = fd_intersection_min(domainx,domainx_rest);
      RangeMin(CTagToCar(domainy_rest)) = pdata->map[ix].ymin;
      ymin = fd_intersection_min(domainy,domainy_rest);
    }
  }
  if (!nodeltay)
    *punsafex = unsafex;
 rety:
  *punsafey = unsafey;
}


static void
relation_scan_delta MAGIC (HIDDEN_PROTO
			   struct relation_data *pdata,
			   Argdecl,
			   TAGGED deltax, /* Invariant: this \subseteq the set of X values */
			   TAGGED domainx,
			   TAGGED *punsafey)
{
  TAGGED unsafey, min, max, domainx_rest;
  int ix = 0;
  int ixend = pdata->xy_r;
  
  unsafey = EmptySet;
  if (deltax==EmptySet)
    goto ret;
  min = fd_min(deltax);
  max = fd_max(deltax);
  domainx_rest = fd_interval(w,Inf,Sup);

  /* scan */
  while (ix<ixend && FDle(pdata->map[ix].min,max)) {
    struct mapentry *mapix = &pdata->map[ix];

    if (FDgt(min,mapix->max)) {
      /* dichotomic search for next hit */
      int mid;
      int sup = ixend;
      while (ix<sup) {
	mid = (ix+sup)>>1;
	if (FDgt(min,pdata->map[mid].max))
	  ix = mid+1;
	else
	  sup = mid;
      }
    } else {
      switch (fd_compare_interval(domainx,mapix->min,mapix->max)) {
      case FDI_DISJOINT:
	unsafey = fd_union(w,unsafey,RefGlob(mapix->value));
      }
      ix++;
      if (ix==ixend)
	break;
      RangeMin(CTagToCar(domainx_rest)) = pdata->map[ix].min;
      min = fd_intersection_min(deltax,domainx_rest);
    }
  }
 ret:
  *punsafey = unsafey;
}

static void
relation_scan_delta_symm MAGIC (HIDDEN_PROTO
				struct relation_data *pdata,
				Argdecl,
				TAGGED deltay, /* Invariant: this \subseteq the set of Y values */
				TAGGED domainy,
				TAGGED *punsafex)
{
  TAGGED unsafex, ymin, ymax, domainy_rest;
  int ix = 0;
  int ixend = pdata->xy_r;
  
  unsafex = EmptySet;
  if (deltay==EmptySet)
    goto ret;
  ymin = fd_min(deltay);
  ymax = fd_max(deltay);
  domainy_rest = fd_interval(w,Inf,Sup);

  /* scan */
  while (ix<ixend && FDle(pdata->map[ix].ymin,ymax)) {
    struct mapentry *mapix = &pdata->map[ix];

    if (FDgt(ymin,mapix->ymax)) {
      /* dichotomic search for next hit */
      int mid;
      int sup = ixend;
      while (ix<sup) {
	mid = (ix+sup)>>1;
	if (FDgt(ymin,pdata->map[mid].ymax))
	  ix = mid+1;
	else
	  sup = mid;
      }
    } else {
      switch (fd_compare_interval(domainy,mapix->ymin,mapix->ymax)) {
      case FDI_DISJOINT:
	unsafex = fd_union_interval(w,unsafex,mapix->min,mapix->max);
      }
      ix++;
      if (ix==ixend)
	break;
      RangeMin(CTagToCar(domainy_rest)) = pdata->map[ix].ymin;
      ymin = fd_intersection_min(deltay,domainy_rest);
    }
  }
 ret:
  *punsafex = unsafex;
}


static void SPCDECL 
relation_daemon MAGIC (HIDDEN_PROTO
		       Argdecl,
		       void *vdata,
		       SP_globref attr_ref,
		       TAGGED global)
{
  struct relation_data *pdata = (struct relation_data *)vdata;
  SP_globref refoffset;
  TAGGED tstate;
  int ar, argno, state_stamp;
  long f, n;

  argno = (attr_ref - pdata->refbase)/6;
  tstate = RefMutable(CTagToArg(global,1));
  ar = Arity(TagToHeadfunctor(tstate));
  state_stamp = GetSmall(CTagToArg(tstate,ar));
  refoffset = pdata->refbase + 6*argno;
  if (fd_singleton(RefMutable(RefGlob(refoffset+2))) ||
      fd_singleton(RefMutable(RefGlob(refoffset+5))))
    return;			/* tuple already entailed */
  if (pdata->stamp!=state_stamp) { /* non-incremental */
    for (f = pdata->first; f != -1; f = n) {
      n = pdata->next[f];
      pdata->next[f] = -2;
    }
    pdata->first = -1;
    pdata->stamp = state_stamp;
  }
  if (pdata->first == -1) {
    (void)daemon_copy_state(w,&global);
    pdata->stamp++;
    fd_enqueue_global(w, global, 0x7/* DOM, append*/);
  }
  if (pdata->next[argno] == -2) {
    pdata->next[argno] = pdata->first;
    pdata->first = argno;
  }
}

void SPCDECL
prolog_fd_relation MAGIC (HIDDEN_PROTO
			  SP_term_ref State0,
			  SP_term_ref State,
			  SP_term_ref Actions)
{
  WAMENV;
  int ent = -1;			/* disentailed */
  int current;
  int opt, xy_n, xy_r, ntuples;
  SP_BOOL committed, post;
  SP_BOOL idempotent;
  TAGGED handle;
  char *ptr;
  struct relation_data *pdata;
  
  w->numstack_end = NULL;
/* X(0) = RefTerm(State0); */
  (void)State0;
  dvar_export_start(w);
  RefTerm(State) = static_output_state(w,&handle,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct relation_data,handle);
    ntuples = pdata->ntuples;
    xy_n = pdata->xy_n;
    xy_r = pdata->xy_r;
    post = FALSE;
  } else {
    TAGGED xy, t1;
    int j;
    SP_globref refoffset;
    
    DerefArg(xy,X(0),1);	/* get XY */
    xy_n = list_length(xy);
    xy_r = 0;
    t1 = xy;
    while (TagIsLST(t1)) {
      TAGGED t2;
      DerefCar(t2,t1);
      DerefArg(t2,t2,1);
      DerefCdr(t1,t1);
      xy_r += list_length(t2);
    }
    DerefArg(t1,X(0),3);	/* get Tuples */
    ntuples = list_length(t1);
    pdata = Palloc(struct relation_data,
		   xy_r*sizeof(struct mapentry) + ntuples*sizeof(long) + ntuples*2*sizeof(struct dvar),
		   handle);
    /* store header data */
    pdata->destructor = relation_destructor;
    pdata->daemon = relation_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(xy_n+6*ntuples);
    pdata->ntuples = ntuples;
    pdata->xy_n = xy_n;
    pdata->xy_r = xy_r;
    ptr = (char *)(pdata+1);
    pdata->map = (struct mapentry *)ptr; ptr += xy_r*sizeof(struct mapentry);
    pdata->next = (long *)ptr; ptr += ntuples*sizeof(long);
    pdata->dvbase = (struct dvar *)ptr; ptr += ntuples*2*sizeof(struct dvar);
    pdata->stamp = 0;
    refoffset = pdata->refbase;
    while (TagIsLST(t1)) {
      TAGGED t2, t3;
      DerefCar(t2,t1);
      DerefCdr(t1,t1);
      get_var_and_attr(t2,refoffset); /* init X, XM */
      DerefArg(t3,t2,3);	/* init UXMut */
      RefGlob(refoffset+2) = t3;
      get_var_and_attr(t2+WD(3),refoffset+3); /* init Y, YM */
      DerefArg(t3,t2,6);	/* init UYMut */
      RefGlob(refoffset+5) = t3;
      refoffset += 6;
    }
    pdata->first = ntuples-1;
    for (j=0; j<ntuples; j++)
      pdata->next[j] = j-1;
    DerefArg(t1,X(0),2);
    pdata->opt = (int)GetSmall(t1); /* get Opt */
    {
      int k=0;
      TAGGED ymin, ymax, list=xy;
      for (j=0; j<xy_n; j++) {
	TAGGED t2;
	DerefCar(t1,list);
	DerefCdr(list,list);
	DerefArg(t2,t1,2);	/* the "value" fdset */
	ymin = fd_min(t2);
	ymax = fd_max(t2);
	RefGlob(refoffset) = t2;
	DerefArg(t1,t1,1);	/* the "key" fdset */
	while (TagIsLST(t1)) {
	  t2 = CTagToCar(t1);
	  t1 = CTagToCdr(t1);
	  pdata->map[k].min = RangeMin(t2);
	  pdata->map[k].max = RangeMax(t2);
	  pdata->map[k].ymin = ymin;
	  pdata->map[k].ymax = ymax;
	  pdata->map[k].value = refoffset;
	  k++;
	}
	refoffset++;
      }
      qsort_map(pdata->map,xy_r);
      k = 0x4;
      for (j=0; j<xy_r && k; j++) {
	struct mapentry *mapj0 = &pdata->map[j-1];
	struct mapentry *mapj = &pdata->map[j];
	if ((j>0 && FDge(mapj0->ymax,mapj->ymin)) || CTagToCdr(RefGlob(mapj->value))!=EmptySet)
	  k = 0;
      }
      pdata->opt |= k;
    }
    DerefArg(t1,X(0),4);	/* ensure entailment ctr is dereffed */
    CTagToArg(X(0),4) = t1;
    for (j=1; j<4; j++)
      CTagToArg(X(0),j) = atom_nil; /* [MC] 3.12: free for GC */
    post = TRUE;
  }
  idempotent = ((RefMutable(CTagToArg(X(1),3))&IStep(4))==0); /* STATUS: idempotent */
  opt  = pdata->opt;
  
  current = pdata->first;
  while (current != -1) {
    TAGGED ux, uy, deltax, deltay, unsafex=EmptySet, unsafey=EmptySet;
    SP_globref refoffset = pdata->refbase + 6*current;
    struct dvar *dvoffset = pdata->dvbase + 2*current;

    dvar_init(dvoffset,   refoffset,   refoffset+1);
    dvar_init(dvoffset+1, refoffset+3, refoffset+4);
    ux = RefMutable(RefGlob(refoffset+2));	/* get UXMut */
    uy = RefMutable(RefGlob(refoffset+5));	/* get UYMut */
    deltax = fd_subtract(w,ux,dvar_set(dvoffset,w));
    deltay = fd_subtract(w,uy,dvar_set(dvoffset+1,w));

    if (opt & 0x4) {
      if (fd_size(deltax)+fd_size(deltay) < dvar_value_count(dvoffset)) {
	relation_scan_delta(pdata,w,deltax,dvar_set(dvoffset,w),&unsafey);
	relation_scan_delta_symm(pdata,w,deltay,dvar_set(dvoffset+1,w),&unsafex);
      } else {
	relation_scan_dom_symm(pdata,w,dvar_set(dvoffset,w),dvar_set(dvoffset+1,w),(deltay==EmptySet),&unsafex,&unsafey);
      }
    } else if (opt & 0x2) {
      TAGGED deltax = fd_subtract(w,ux,dvar_set(dvoffset,w));
      if (deltay==EmptySet && fd_size(deltax) < dvar_value_count(dvoffset)) {
	relation_scan_delta(pdata,w,deltax,dvar_set(dvoffset,w),&unsafey);
      } else if (opt & 0x1) {
	goto scan_dom_int;
      } else {
	goto scan_dom;
      }
    } else if (opt & 0x1) {
    scan_dom_int:
      relation_scan_dom_interval(pdata,w,dvar_min_t(dvoffset),dvar_max_t(dvoffset),dvar_min_t(dvoffset+1),dvar_max_t(dvoffset+1),(deltay==EmptySet),&unsafex,&unsafey);
    } else {
    scan_dom:
      relation_scan_dom(pdata,w,dvar_set(dvoffset,w),dvar_set(dvoffset+1,w),(deltay==EmptySet),&unsafex,&unsafey);
    }
    if (unsafex!=EmptySet) {
      if (opt & 0x1) {
	ux = fd_subtract(w,dvar_set(dvoffset,w),unsafex);
	if (dvar_fix_interval_t(dvoffset,fd_min(ux),fd_max(ux))<0)
	  goto ret;
      } else {
	if (dvar_prune_set(dvoffset,unsafex,w)<0)
	  goto ret;
      }
    }
    if (unsafey!=EmptySet) {
      if (opt & 0x1) {
	uy = fd_subtract(w,dvar_set(dvoffset+1,w),unsafey);
	if (dvar_fix_interval_t(dvoffset+1,fd_min(uy),fd_max(uy))<0)
	  goto ret;
      } else {
	if (dvar_prune_set(dvoffset+1,unsafey,w)<0)
	  goto ret;
      }
    }
    dvar_pruning_done(w,dvoffset);
    dvar_pruning_done(w,dvoffset+1);
    if (!idempotent) {
      (dvoffset)->set = fd_localize(w,dvar_set(dvoffset,w));
      (dvoffset+1)->set = fd_localize(w,dvar_set(dvoffset+1,w));
    }
    if (!dvar_is_integer(dvoffset) && !dvar_is_integer(dvoffset+1)) {
      if (post) {
	dvar_attach_daemon(w, dvoffset, pdata, X(1), fd.functor_dom);
	dvar_attach_daemon(w, dvoffset+1, pdata, X(1), fd.functor_dom);
      }
    } else {
      CTagToArg(X(0),4) -= IStep(1); /* decrement entailent counter */
    }
    current = pdata->next[current];
  }

  if (idempotent) {
    /* OK to GC */
    current = pdata->first;
    while (current != -1) {
      SP_globref refoffset = pdata->refbase + 6*current;
      struct dvar *dvoffset = pdata->dvbase + 2*current;
      int next;
      TAGGED ux, uy;
    
      dvar_export(w,dvoffset);
      ux = relation_fdset(w,refoffset);
      update_mutable(w,ux,RefGlob(refoffset+2)); /* put UXMut */
      dvar_export(w,dvoffset+1);
      uy = relation_fdset(w,refoffset+3);
      update_mutable(w,uy,RefGlob(refoffset+5)); /* put UYMut */
      next = pdata->next[current];
      pdata->next[current] = -2;
      current = next;
    }
  } else {
    /* OK to GC */
    current = pdata->first;
    while (current != -1) {
      SP_globref refoffset = pdata->refbase + 6*current;
      struct dvar *dvoffset = pdata->dvbase + 2*current;
      TAGGED ux, uy;
    
      ux = dvar_set(dvoffset,w);
      ux = fd_globalize(w,ux,0,3);
      update_mutable(w,ux,RefGlob(refoffset+2)); /* put UXMut */
      uy = dvar_set(dvoffset+1,w);
      uy = fd_globalize(w,uy,0,3);
      update_mutable(w,uy,RefGlob(refoffset+5)); /* put UYMut */
      current = pdata->next[current];
    }
    current = pdata->first;
    while (current != -1) {
      struct dvar *dvoffset = pdata->dvbase + 2*current;
      int next;
    
      dvar_export(w,dvoffset);
      dvar_export(w,dvoffset+1);
      next = pdata->next[current];
      pdata->next[current] = -2;
      current = next;
    }
  }
  pdata->first = -1;
  ent = Teqz(CTagToArg(X(0),4));
ret:
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}

struct element_data {
  void (SPCDECL *destructor)(void *);
  void (SPCDECL *daemon)(HIDDEN_PROTO
			 Argtype,void *,SP_globref,TAGGED); /* (w,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  int nrefs;
  Dvar dvar;
};

#define RefAttr(T) (pdata->refbase + 2*(T))
#define RefVar(T) (pdata->refbase + 2*(T) + 1)
#define DVAR(T) (pdata->dvar+(T))

static void SPCDECL element_destructor(void *pdata_v)
{
  struct element_data *pdata = (struct element_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

/* Daemon for element/3: effectively disable Xi if i is not in the index domain */
static void SPCDECL 
element_daemon MAGIC (HIDDEN_PROTO
		      Argdecl,
		      void *vdata,
		      SP_globref attr_ref,
		      TAGGED global)
{
  struct element_data *pdata = (struct element_data *)vdata;
  int eltno = ((attr_ref - pdata->refbase)>>1)-1;
  
  dvar_init(DVAR(0), RefAttr(0), RefVar(0)); /* get Index */
  if (dvar_contains_value_l(DVAR(0),eltno))
    fd_enqueue_global(w, global, 0x5/* MINMAX, append*/);
}

/*
  '$fd_element'(+State0, +State, -Actions).
  State = state(Index,Value,Xs,NTargets,IsGround,Handle,Stamp)
  IsGround is 0 -> Xs is a list of Var-Mutable
  IsGround is 1 -> Xs is a list of integers
*/
void SPCDECL
prolog_fd_element MAGIC (HIDDEN_PROTO
			 SP_term_ref State0,
			 SP_term_ref State,
			 SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, tvec, tisground;
  int i, ntargets;
  struct element_data *pdata;
  SP_BOOL committed;
  char *ptr;
  WAMENV;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


  w->numstack_end = NULL;
/*    X(0) = RefTerm(State0); */
  dvar_export_start(w);
  RefTerm(State) = static_output_state(w,&handle,&committed);
  DerefArg(tvec,X(0),4);	/* get NTargets */
  DerefArg(tisground,X(0),5);	/* get Isground */
  ntargets = GetSmall(tvec);
  if (ntargets==0) {
    goto ret;
  } else if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct element_data,handle);
  } else {			/* build persistent state */
    pdata = Palloc(struct element_data,
		   (ntargets+2)*sizeof(struct dvar),
		   handle);
    pdata->destructor = element_destructor;
    pdata->daemon = element_daemon;
    pdata->nrefs = tisground==TaggedZero ? (ntargets+2)<<1 : 4;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(pdata->nrefs);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += (ntargets+2)*sizeof(struct dvar);
    DerefArg(tvec,X(0),1);		/* get Index */
    get_var_and_attr(tvec,RefAttr(0));
    DerefArg(tvec,X(0),2);		/* get Value */
    get_var_and_attr(tvec,RefAttr(1));
    DerefArg(tvec,X(0),3);		/* get Xs */
    for (i=0; i<ntargets; i++) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      if (tisground==TaggedZero) {
	get_var_and_attr(telt,RefAttr(i+2));
	dvar_init(DVAR(i+2), RefAttr(i+2), RefVar(i+2));
	dvar_attach_daemon(w, DVAR(i+2), pdata, X(1), fd.functor_minmax);
      } else {
	DVAR(i+2)->min = DVAR(i+2)->max = telt;
	DVAR(i+2)->flags = DV_INTERVAL;
      }
    }
  }

  /* RESUME HERE */
  dvar_init(DVAR(0), RefAttr(0), RefVar(0));
  dvar_init(DVAR(1), RefAttr(1), RefVar(1));
  {
    TAGGED ymin = dvar_min_t(DVAR(1));
    TAGGED ymax = dvar_max_t(DVAR(1));
    TAGGED minofmin = Sup;
    TAGGED maxofmax = Inf;
    DVITER it;
    FDCONS cons;
    dviter_init(&it, DVAR(0));
    fdcons_init(&cons);
    while (!dviter_empty(&it)) {
      int ix = dviter_next_value_l(&it)+1;
      Dvar dv = DVAR(ix);
      if (tisground==TaggedZero)
	dvar_init(dv, RefAttr(ix), RefVar(ix));    
      if (dvar_compare_interval_t(dv,ymin,ymax,w)==FDI_DISJOINT)
	fdcons_add(&cons,w,MakeSmall(ix-1));
      else {
	if (FDgt(minofmin,dvar_min_t(dv)))
	  minofmin = dvar_min_t(dv);
	if (FDlt(maxofmax,dvar_max_t(dv)))
	  maxofmax = dvar_max_t(dv);
      }
    }
    if (dvar_prune_set(DVAR(0), fdcons_set(&cons), w)<0)
      goto ret;
    if (dvar_fix_interval_t(DVAR(1), minofmin, maxofmax)<0)
      goto ret;
    dvar_pruning_done(w, DVAR(0));
    dvar_pruning_done(w, DVAR(1));
    dvar_export(w, DVAR(0));
    dvar_export(w, DVAR(1));
    if (minofmin==maxofmax) {
      ent = 1;
    } else if (dvar_is_integer(DVAR(0))) {
      int ix = dvar_min_l(DVAR(0))+1;      
      ent = 1;
      dvar_export_equal(w, DVAR(1), DVAR(ix));
    } else {
      ent = 0;
    }
  }
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}

struct minmax_data {
  void (SPCDECL *destructor)(void *);
  void (SPCDECL *daemon)(HIDDEN_PROTO
			 Argtype,void *,SP_globref,TAGGED); /* (w,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  int nrefs;
  int *target;
  Dvar dvar;
};

#define RefAttr(T) (pdata->refbase + 2*(T))
#define RefVar(T) (pdata->refbase + 2*(T) + 1)
#define TARGET(T) (pdata->target[T])
#define DVAR(T) (pdata->dvar+(T))

static void SPCDECL minmax_destructor(void *pdata_v)
{
  struct minmax_data *pdata = (struct minmax_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

/* Daemon for minimum/2: effectively disable Xi that is for sure > Y */
/* Daemon for maximum/2: effectively disable Xi that is for sure < Y */
static void SPCDECL 
minmax_daemon MAGIC (HIDDEN_PROTO
		     Argdecl,
		     void *vdata,
		     SP_globref attr_ref,
		     TAGGED global)
{
  struct minmax_data *pdata = (struct minmax_data *)vdata;
  TAGGED tstate;
  int varno, i, ntargets;

  varno = (attr_ref - pdata->refbase)>>1;
  tstate = RefMutable(CTagToArg(global,1));
  DerefArg(tstate,tstate,3);	/* get NTargets */
  ntargets = GetSmall(tstate);
  for (i=0; i<ntargets; i++)
    if (TARGET(i)==varno)
      goto push;
  return;
 push:
  fd_enqueue_global(w, global, 0x5/* MINMAX, append*/);
}

/*
  '$fd_minmax'(+State0, +State, -Actions).
  State = state(Y,Xs,NTargets,IsMax,Handle,Stamp)
  IsMax is 0 -> minimum/2
  IsMax is 1 -> maximum/2
*/
void SPCDECL
prolog_fd_minmax MAGIC (HIDDEN_PROTO
			       SP_term_ref State0,
			       SP_term_ref State,
			       SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, tvec, tismax;
  TAGGED ymin, ymax;
  long state_stamp;
  int i, ntargets;
  struct minmax_data *pdata;
  SP_BOOL committed;
  char *ptr;
  WAMENV;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


  w->numstack_end = NULL;
/*    X(0) = RefTerm(State0); */
  dvar_export_start(w);
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);
  DerefArg(tvec,X(0),3);	/* get NTargets */
  DerefArg(tismax,X(0),4);	/* get IsMax */
  ntargets = GetSmall(tvec);
  if (ntargets==0) {
    goto ret;
  } else if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct minmax_data,handle);
  } else {			/* build persistent state */
    pdata = Palloc(struct minmax_data,
		   (ntargets+1)*(sizeof(int)+sizeof(struct dvar)),
		   handle);
    pdata->destructor = minmax_destructor;
    pdata->daemon = minmax_daemon;
    pdata->nrefs = (ntargets+1)<<1;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs((ntargets+1)<<1);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += (ntargets+1)*sizeof(struct dvar);
    pdata->target = (int *)ptr;
    ptr += (ntargets+1)*sizeof(int);
    DerefArg(tvec,X(0),1);		/* get Y */
    get_var_and_attr(tvec,RefAttr(0));
    DerefArg(tvec,X(0),2);		/* get Xs */
    for (i=0; i<ntargets; i++) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      TARGET(i) = i+1;
      get_var_and_attr(telt,RefAttr(i+1));
      dvar_init(DVAR(i+1), RefAttr(i+1), RefVar(i+1));
      dvar_attach_daemon(w, DVAR(i+1), pdata, X(1), fd.functor_minmax);
    }
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  dvar_init(DVAR(0), RefAttr(0), RefVar(0));
  for (i=0; i<ntargets; i++) {
    int elt = TARGET(i);
    dvar_init(DVAR(elt), RefAttr(elt), RefVar(elt));
  }
  ymin = dvar_min_t(DVAR(0));
  ymax = dvar_max_t(DVAR(0));
  if (tismax==TaggedZero) {
    TAGGED minofmax = Sup;
    TAGGED minofmin = Sup;
    for (i=0; i<ntargets; i++) {
      int elt = TARGET(i);
      Dvar dv = DVAR(elt);
      if (dvar_fix_min_t(dv, ymin)<0)
	goto ret;
      if (FDgt(minofmax,dvar_max_t(dv)))
	minofmax = dvar_max_t(dv);
      if (FDgt(minofmin,dvar_min_t(dv)))
	minofmin = dvar_min_t(dv);
    }
    if (dvar_fix_interval_t(DVAR(0), minofmin, minofmax)<0)
      goto ret;
  } else {
    TAGGED maxofmax = Inf;
    TAGGED maxofmin = Inf;
    for (i=0; i<ntargets; i++) {
      int elt = TARGET(i);
      Dvar dv = DVAR(elt);
      if (dvar_fix_max_t(dv, ymax)<0)
	goto ret;
      if (FDlt(maxofmax,dvar_max_t(dv)))
	maxofmax = dvar_max_t(dv);
      if (FDlt(maxofmin,dvar_min_t(dv)))
	maxofmin = dvar_min_t(dv);
    }
    if (dvar_fix_interval_t(DVAR(0), maxofmin, maxofmax)<0)
      goto ret;
  }
  dvar_pruning_done(w, DVAR(0));
  for (i=0; i<ntargets; i++) {
    int elt = TARGET(i);
    dvar_pruning_done(w, DVAR(elt));
  }
  dvar_export(w, DVAR(0));
  for (i=0; i<ntargets; i++) {
    int elt = TARGET(i);
    dvar_export(w, DVAR(elt));
  }
  {
    int inf = 0;
    int sup = ntargets-1;
    int held = TARGET(sup);		/* sup is the hole */
    int current = TARGET(inf);
    
    while (inf<=sup) {
      if (tismax==TaggedZero ? FDge(ymax,dvar_min_t(DVAR(current))) : FDle(ymin,dvar_max_t(DVAR(current)))) {
	TARGET(inf) = current;
	inf++;
	current = (inf>=sup ? held : TARGET(inf));
      } else {
	TARGET(sup) = current;
	sup--;
	current = (inf>=sup ? held : TARGET(sup));
      }
    }
    ntargets = inf;
    CTagToArg(X(0),3) = MakeSmall(inf);
  }
  if (ntargets==1) {
    ent = 1;
    dvar_export_equal(w, DVAR(0), DVAR(TARGET(0)));
  } else {
    ent = 0;
  }
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}
