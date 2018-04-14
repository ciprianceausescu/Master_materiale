/* Copyright(C) 2000, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

#if MULTI_SP_AWARE
#define eval_leaf(A1,A2,A3,A4) eval_leaf(HIDDEN_ARG, A1,A2,A3,A4)
#define eval_dag(A1,A2,A3,A4) eval_dag(HIDDEN_ARG, A1,A2,A3,A4)
#endif /* MULTI_SP_AWARE */


#define CASE_DOM 0
#define CASE_MINMAX 1
#define CASE_MIN 2
#define CASE_MAX 3
#define CASE_VAL 4
#define CASE_NONE 5

struct case_data {
  void (SPCDECL *destructor)(void*);
  void (SPCDECL *daemon)(HIDDEN_PROTO
			 Argtype,void *,SP_globref,TAGGED); /* (w,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  SP_globref refbase;
  long stamp;			/* increases up to backtracking */
  long first;			/* head of (-1)-terminated list of queued tuples */
  long *next;			/* next[i]==(-2) iff not in the queue */
  int use_leaf;
  int ntuples;
  int nvars;
  int nnodes;
  unsigned long false_date;
  unsigned long true_date;
  TAGGED relevant_vars;		/* volatile, numstack data */
  Dvar dvar;
  TAGGED *prune_set;		/* for DOM, volatile, GC-unsafe */
  TAGGED *prune_min;		/* for MINMAX|MIN|VAL, volatile, atomic */
  TAGGED *prune_max;		/* for MINMAX|MAX|VAL, volatile, atomic */
  long   *prune_mask;		/* any=1, min=2, max=4, at least two=8, volatile */
  unsigned long *control;
  unsigned long *on;
  unsigned long *latest_date;
  long *latest_size;
  struct {
    unsigned long *date;	/* TRUE(FALSE) if = true_date(false_date), undefined otherwise */
    long *id;			/* unique by definition */
    long *var_index;
    long *child_index;
    long *child_end;
  } dag_node;
  struct {
    TAGGED *min, *max;
    long *node_index;
  } child;
};

#define DATE(R,N) (pdata->dag_node.date[nnodes*(R)+(N)])
#define ID(N) (pdata->dag_node.id[N])
#define VAR(N) (pdata->dag_node.var_index[N])
#define CHILD(N) (pdata->dag_node.child_index[N])
#define CHILD_END(N) (pdata->dag_node.child_end[N])
#define VARSET(N) RefGlob(pdata->refbase + (N))
#define ATTRIBUTE(R,C) RefGlob(pdata->refbase+((nvars*(R)+(C))<<1) + nnodes)
#define ATTRIBUTE_LOC(R,C) (pdata->refbase+((nvars*(R)+(C))<<1) + nnodes)
#define ENT_FLAG(R) RefGlob(pdata->refbase+2*ntuples*nvars+pdata->nnodes+(R))
#define CMIN(Ch) (pdata->child.min[Ch])
#define CMAX(Ch) (pdata->child.max[Ch])
#define NODE(Ch) (pdata->child.node_index[Ch])
#define LATEST_DATE(R,C) (pdata->latest_date[nvars*(R)+(C)])
#define LATEST_SIZE(R,C) (pdata->latest_size[nvars*(R)+(C)])

static void SPCDECL case_destructor(void *pdata_v)
{
  struct case_data *pdata = (struct case_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  SP_free_globrefs(pdata->refbase,2*pdata->ntuples*pdata->nvars+pdata->nnodes+pdata->ntuples);
  SP_free(pdata);
}


static int eval_leaf MAGIC (HIDDEN_PROTO
			    Argdecl,
			    int row,
			    long id,
			    struct case_data *pdata)
{
  int nvars = pdata->nvars;
  int col = nvars-1;
  int nnodes = pdata->nnodes;
  int ctrl;
  TAGGED tcol = MakeSmall(col);
  TAGGED tid = MakeSmall(id);
  TAGGED dom, set, min, max;
    
  DerefAttribute(dom,ATTRIBUTE(row,col)); /* dom/4 term */
  set = DomainSet(dom);
  min = DomainMin(dom);
  max = DomainMax(dom);
    
  if (InInterval(tid,min,max) && fd_member(tid,set)) { /* feasible */
    if (fd_member(tcol,pdata->relevant_vars)) {
      pdata->prune_mask[col] |= 0x1; /* support for some domain element - see CASE_VAL */
      if (min==tid)
	pdata->prune_mask[col] |= 0x2;	/* support for min element */
      if (max==tid)
	pdata->prune_mask[col] |= 0x4;	/* support for max element */
      switch ((ctrl=pdata->control[col])) {
      case CASE_DOM:
	{
	  TAGGED set1;
	  pdata->prune_set[col] = set1 =
	    fd_delete(w,pdata->prune_set[col],tid);
	  if (set1==EmptySet)
	    pdata->relevant_vars = fd_delete(w,pdata->relevant_vars,tcol);
	}
	break;
      case CASE_MIN:
	if (pdata->prune_mask[col] & 0x2)
	  pdata->relevant_vars = fd_delete(w,pdata->relevant_vars,tcol);
	else {
	  if (FDgt(pdata->prune_min[col],tid))
	    pdata->prune_min[col] = tid;
	}
	break;
      case CASE_MAX:
	if (pdata->prune_mask[col] & 0x4)
	  pdata->relevant_vars = fd_delete(w,pdata->relevant_vars,tcol);
	else {
	  if (FDlt(pdata->prune_max[col],tid))
	    pdata->prune_max[col] = tid;
	}
	break;
      case CASE_MINMAX:
      case CASE_VAL:
	if ((pdata->prune_mask[col] & 0x6) == 0x6)
	  pdata->relevant_vars = fd_delete(w,pdata->relevant_vars,tcol);
	else {		/* detect when we have support for at least two values */
	  if (FDgt(pdata->prune_min[col],tid))
	    pdata->prune_min[col] = tid;
	  if (FDlt(pdata->prune_max[col],tid))
	    pdata->prune_max[col] = tid;
	  if (ctrl==CASE_VAL &&
	      pdata->prune_min[col] != pdata->prune_max[col])
	    pdata->relevant_vars = fd_delete(w,pdata->relevant_vars,tcol);
	}
	break;
      }
    }
    return TRUE;
  } else { /* infeasible */
    return FALSE;
  }
}

static int eval_dag MAGIC (HIDDEN_PROTO Argdecl,
			   int row,
			   int node,
			   struct case_data *pdata)
{
  int state = FALSE;
  int leaf_state = TRUE;
  int nnodes = pdata->nnodes;
  int use_leaf = pdata->use_leaf;
  
  if (DATE(row,node) == pdata->true_date)
    return TRUE;
  if (DATE(row,node) == pdata->false_date)
    return FALSE;
  {
    int col = VAR(node);
    TAGGED tcol = MakeSmall(col);
    int child = CHILD(node);
    int child_end = CHILD_END(node);
    int nvars = pdata->nvars;
    int mid, sup;		/* for dichotomic search */
    TAGGED dom, set, min, max;
    
    DerefAttribute(dom,ATTRIBUTE(row,col)); /* dom/4 term */
    set = DomainSet(dom);
    min = DomainMin(dom);
    max = DomainMax(dom);
    
    /* dichotomic search for first child */
    sup = child_end;
    while (child<sup) {
      mid = (child+sup)>>1;
      if (FDgt(min,CMAX(mid)))
	child = mid+1;
      else
	sup = mid;
    }

    /* scan the compatible children */
    while (child < child_end &&
	   leaf_state &&
	   !FDgt(CMIN(child),max) &&
	   (!state ||
	    fd_compare(VARSET(node),pdata->relevant_vars)!=FDI_DISJOINT)) {
      int lstate = TRUE;

      if (CMIN(child)==CMAX(child)) { /* singleton interval special case */
	if (fd_member(CMIN(child),set))
	  goto intersect;
	else
	  goto disjoint;
      }
      switch (fd_compare_interval(set,CMIN(child),CMAX(child))) {
      case FDI_DISJOINT:
      disjoint:
	lstate = FALSE;
	break;
      case FDI_SUBSET:
      case FDI_EQUAL:
	if (NODE(child) > -1) {
	  /* TODO: assert ATTRIBUTE(row,col) in CMIN(child) .. CMAX(child) */
	  lstate = eval_dag(w,row,NODE(child),pdata);
	  /* TODO: retract ATTRIBUTE(row,col) in CMIN(child) .. CMAX(child) */
	} else if (use_leaf-- > 0)
	  lstate = leaf_state = eval_leaf(w,row,ID(node),pdata);
	if (lstate && fd_member(tcol,pdata->relevant_vars))
	  pdata->relevant_vars = fd_delete(w,pdata->relevant_vars,tcol);
	break;
      default:
      intersect:
	if (NODE(child) > -1) {
	  /* TODO: assert ATTRIBUTE(row,col) in CMIN(child) .. CMAX(child) */
	  lstate = eval_dag(w,row,NODE(child),pdata);
	  /* TODO: retract ATTRIBUTE(row,col) in CMIN(child) .. CMAX(child) */
	} else if (use_leaf-- > 0)
	  lstate = leaf_state = eval_leaf(w,row,ID(node),pdata);
	if (lstate && fd_member(tcol,pdata->relevant_vars)) {
	  pdata->prune_mask[col] |= 0x1; /* support for some domain element - see CASE_VAL */
	  if (!FDgt(CMIN(child),min)) /* min >= CMIN(child) */
	    pdata->prune_mask[col] |= 0x2;	/* support for min element */
	  if (!FDgt(max,CMAX(child))) /* max <= CMAX(child) */
	    pdata->prune_mask[col] |= 0x4;	/* support for max element */
	  switch (pdata->control[col]) {
	  case CASE_DOM:
	    {
	      TAGGED set1;
	      pdata->prune_set[col] = set1 =
		fd_subtract_interval(w,pdata->prune_set[col],CMIN(child),CMAX(child));
	      if (set1==EmptySet)
		pdata->relevant_vars = fd_delete(w,pdata->relevant_vars,tcol);
	    }
	    break;
	  case CASE_MIN:
	    if (pdata->prune_mask[col] & 0x2)
	      pdata->relevant_vars = fd_delete(w,pdata->relevant_vars,tcol);
	    else {
	      if (FDgt(pdata->prune_min[col],CMIN(child)))
		pdata->prune_min[col] = CMIN(child);
	    }
	    break;
	  case CASE_MAX:
	    if (pdata->prune_mask[col] & 0x4)
	      pdata->relevant_vars = fd_delete(w,pdata->relevant_vars,tcol);
	    else {
	      if (FDlt(pdata->prune_max[col],CMAX(child)))
		pdata->prune_max[col] = CMAX(child);
	    }
	    break;
	  case CASE_MINMAX:
	    if ((pdata->prune_mask[col] & 0x6) == 0x6)
	      pdata->relevant_vars = fd_delete(w,pdata->relevant_vars,tcol);
	    else {
	      if (FDgt(pdata->prune_min[col],CMIN(child)))
		pdata->prune_min[col] = CMIN(child);
	      if (FDlt(pdata->prune_max[col],CMAX(child)))
		pdata->prune_max[col] = CMAX(child);
	    }
	    break;
	  case CASE_VAL:
	    if ((pdata->prune_mask[col] & 0x6) == 0x6)
	      pdata->relevant_vars = fd_delete(w,pdata->relevant_vars,tcol);
	    else {		/* detect when we have support for at least two values */
	      /* TODO: fd_compare case analysis */
	      TAGGED set1 = fd_intersection_interval(w,set,CMIN(child),CMAX(child));
	      TAGGED min = fd_min(set1);
	      TAGGED max = fd_max(set1);
	      
	      if (FDgt(pdata->prune_min[col],min))
		pdata->prune_min[col] = min;
	      if (FDlt(pdata->prune_max[col],max))
		pdata->prune_max[col] = max;
	      if (pdata->prune_min[col] != pdata->prune_max[col])
		pdata->relevant_vars = fd_delete(w,pdata->relevant_vars,tcol);
	    }
	    break;
	  }
	}
      }
      /* update state of current node */
      state |= lstate;
      child++;
    }
  }
  /* record state */
  DATE(row,node) = (state ? pdata->true_date : pdata->false_date);
  return state;
}

static void SPCDECL 
case_daemon MAGIC (HIDDEN_PROTO
		   Argdecl,
		   void *vdata,
		   SP_globref attr_ref,
		   TAGGED global)
{
  struct case_data *pdata = (struct case_data *)vdata;
  int ntuples = pdata->ntuples;
  int nvars = pdata->nvars;
  TAGGED t, tstate;
  int ar, row, col, state_stamp;
  long f, n;

  row =  (attr_ref - pdata->refbase - pdata->nnodes)/(pdata->nvars<<1);
  col = ((attr_ref - pdata->refbase - pdata->nnodes)%(pdata->nvars<<1))>>1;
  t = ENT_FLAG(row);
  DerefSwitch(t,goto cont;);
  return;
 cont:
  tstate = RefMutable(CTagToArg(global,1));
  ar = Arity(TagToHeadfunctor(tstate));
  state_stamp = GetSmall(CTagToArg(tstate,ar));
  if (pdata->stamp!=state_stamp) { /* non-incremental */
    for (f = pdata->first; f != -1; f = n) {
      n = pdata->next[f];
      pdata->next[f] = -2;
    }
    pdata->first = -1;
    pdata->stamp = state_stamp;
    pdata->false_date += 2;
  } else {
    TAGGED tmp = RefGlob(attr_ref);
    DerefAttribute(tmp,tmp); /* get dom/4 term */
    if (LATEST_DATE(row,col)==pdata->false_date &&
	LATEST_SIZE(row,col)==GetSmall(DomainSize(tmp)))
      return;			/* self-invocation */
  }
  if (pdata->first == -1) {
    int q;
    (void)daemon_copy_state(w,&global);
    pdata->stamp++;
    switch (pdata->on[col]) {
    case CASE_MIN:
    case CASE_MAX:
    case CASE_MINMAX:
      q = 0x5;
      break;
    case CASE_VAL:
      q = 0x3;
      break;
    case CASE_DOM:
    default:
      q = 0x7;
    }
    fd_enqueue_global(w, global, q);
  }
  if (pdata->next[row] == -2) {
    pdata->next[row] = pdata->first;
    pdata->first = row;
  }
}

/*
   '$fd_case'(+State0, -State, -Actions) :-
   State0 = State = state(f(NVars,NNodes,NChildren,TVars,Dag,On,Prune,UseLeaf),EntailmentCtr,_Handle,Stamp)
 */
void SPCDECL
prolog_fd_case MAGIC (HIDDEN_PROTO
		      SP_term_ref State0,
		      SP_term_ref State,
		      SP_term_ref Actions)
{
  WAMENV;
  struct case_data *pdata;
  int i, j, row, ntuples, nvars, nnodes, nchildren, nnonground;
  int ent = -1;
  TAGGED item, tmp, state, handle;
  SP_BOOL committed;
  char *ptr;
  FDITER it;
  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State0); */
  (void)State0;
  dvar_export_start(w);
  RefTerm(State) = static_output_state(w,&handle,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct case_data,handle);
    ntuples = pdata->ntuples;
    nvars = pdata->nvars;
    nnodes = pdata->nnodes;
  } else {			/* build persistent state */
    int child;
    TAGGED tuples, tvars, dag, *h;
    
    DerefArg(state,X(0),1);
    DerefArg(tmp,state,1); 
    nvars = GetSmall(tmp);
    DerefArg(tmp,state,2); 
    nnodes = GetSmall(tmp);
    DerefArg(tmp,state,3); 
    nchildren = GetSmall(tmp);
    DerefArg(tuples,state,4); 
    ntuples = list_length(tuples);
    DerefArg(dag,state,5); 
    pdata = Palloc(struct case_data,
		   nvars*sizeof(struct dvar)+
		   2*nvars*sizeof(long)+
		   2*ntuples*nvars*sizeof(long)+
		   ntuples*nnodes*sizeof(long)+
		   nnodes*4*sizeof(long)+
		   nchildren*3*sizeof(long)+
		   ntuples*sizeof(long),
		   handle);
    pdata->destructor = case_destructor;
    pdata->daemon = case_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->stamp = 0;
    pdata->refbase = SP_alloc_globrefs(2*ntuples*nvars+nnodes+ntuples);
    pdata->ntuples = ntuples;
    pdata->nvars = nvars;
    pdata->nnodes = nnodes;
    pdata->false_date = 0L;
    pdata->true_date = 1L;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr; ptr += nvars*sizeof(struct dvar);
    pdata->control = (unsigned long *)ptr; ptr += nvars*sizeof(long);
    pdata->on = (unsigned long *)ptr; ptr += nvars*sizeof(long);
    pdata->latest_date = (unsigned long *)ptr; ptr += ntuples*nvars*sizeof(long);
    pdata->latest_size = (long *)ptr; ptr += ntuples*nvars*sizeof(long);
    pdata->dag_node.date = (unsigned long *)ptr; ptr += ntuples*nnodes*sizeof(long);
    pdata->dag_node.id = (long *)ptr; ptr += nnodes*sizeof(long);
    pdata->dag_node.var_index = (long *)ptr; ptr += nnodes*sizeof(long);
    pdata->dag_node.child_index = (long *)ptr; ptr += nnodes*sizeof(long);
    pdata->dag_node.child_end = (long *)ptr; ptr += nnodes*sizeof(long);
    pdata->child.min = (TAGGED *)ptr; ptr += nchildren*sizeof(long);
    pdata->child.max = (TAGGED *)ptr; ptr += nchildren*sizeof(long);
    pdata->child.node_index = (long *)ptr; ptr += nchildren*sizeof(long);
    pdata->next = (long *)ptr; ptr += ntuples*sizeof(long);
    pdata->first = ntuples-1;
    for (i=ntuples-1; i>=0; i--)
      pdata->next[i] = i-1;
    DerefArg(tmp,state,8);
    pdata->use_leaf = GetSmall(tmp);
    
    for (i=0; i<ntuples; i++) {
      DerefCar(tvars,tuples);
      DerefCdr(tuples,tuples);
      for (j=0; j<nvars; j++) {
	SP_globref ref = ATTRIBUTE_LOC(i,j);

	DerefCar(item,tvars);
	DerefCdr(tvars,tvars);
	get_var_and_attr(item,ref);
      }
    }
    /* build the DAG */
    child = 0;
    for (i=0; i<nnodes; i++) {
      int node = i;
      TAGGED children, cmin, cmax;

      DerefCar(item,dag);	/* get dagnode(ID,VAR,VARSET,Children) */
      DerefCdr(dag,dag);
      DerefArg(tmp,item,1);	/* node ID */
      ID(node) = GetSmall(tmp);
      DerefArg(tmp,item,2);	/* var index */
      for (row=ntuples-1; row>=0; row--)
	DATE(row,node) = -1L;
      VAR(node) = GetSmall(tmp);
      DerefArg(tmp,item,3);	/* var set */
      VARSET(node) = tmp;
      CHILD(node) = child;
      DerefArg(children,item,4); /* children */
      while (TagIsLST(children)) {
	DerefCar(item,children);
	DerefCdr(children,children);
	DerefArg(tmp,item,2);
	DerefArg(item,item,1);
	DerefArg(cmin,item,1);
	DerefArg(cmax,item,2);
	CMIN(child) = cmin;
	CMAX(child) = cmax;
	NODE(child) = (TagIsSmall(tmp) ? GetSmall(tmp) : -1);
	child++;
      }
      CHILD_END(node) = child;
    }
    /* build control+on info */
    for (i=0; i<2; i++) {
      unsigned long *map;
      TAGGED prune;
      if (i==0) {
	map = pdata->on;
	DerefArg(prune,state,6); 
      } else {
	map = pdata->control;
	DerefArg(prune,state,7); 
      }
      for (j=0; j<nvars; j++) {
	DerefCar(item,prune);
	DerefCdr(prune,prune);
	item = SetArity(item,1);
	if (item==fd.functor_dom)
	  map[j] = CASE_DOM;
	else if (item==fd.functor_min)
	  map[j] = CASE_MIN;
	else if (item==fd.functor_max)
	  map[j] = CASE_MAX;
	else if (item==fd.functor_minmax)
	  map[j] = CASE_MINMAX;
	else if (item==fd.functor_val)
	  map[j] = CASE_VAL;
	else
	  map[j] = CASE_NONE;
      }
    }
    for (i=0; i<ntuples; i++) {
      for (j=0; j<nvars; j++) {
	SP_globref ref = ATTRIBUTE_LOC(i,j);
	Dvar dv = pdata->dvar+j;
	
	dvar_init(dv, ref, ref+1);
	LATEST_DATE(i,j) = 0L;
	LATEST_SIZE(i,j) = dvar_value_count(dv);
	switch (pdata->on[j]) {
	case CASE_DOM:
	  dvar_attach_daemon(w, dv, pdata, X(1), fd.functor_dom);
	  break;
	case CASE_MIN:
	  dvar_attach_daemon(w, dv, pdata, X(1), fd.functor_min);
	  break;
	case CASE_MAX:
	  dvar_attach_daemon(w, dv, pdata, X(1), fd.functor_max);
	  break;
	case CASE_MINMAX:
	  dvar_attach_daemon(w, dv, pdata, X(1), fd.functor_minmax);
	  break;
	case CASE_VAL:
	  dvar_attach_daemon(w, dv, pdata, X(1), fd.functor_val);
	  break;
	}
      }
    }
    RequireHeap(ntuples,3);
    h = w->global_top;
    for (i=ntuples-1; i>=0; i--)
      ENT_FLAG(i) = h[i] = TagREF(h+i);
    w->global_top = h+ntuples; 
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    DerefArg(tmp,X(0),2);	/* ensure entailment ctr is dereffed */
    CTagToArg(X(0),2) = tmp;
  }
  
  /* RESUME */
  ptr = fd_malloc(4*nvars*sizeof(TAGGED));
  pdata->prune_set = (TAGGED *)ptr; ptr += nvars*sizeof(TAGGED);
  pdata->prune_min = (TAGGED *)ptr; ptr += nvars*sizeof(TAGGED);
  pdata->prune_max = (TAGGED *)ptr; ptr += nvars*sizeof(TAGGED);
  pdata->prune_mask = (long *)ptr;  ptr += nvars*sizeof(long);
  pdata->true_date += 2;

  row = pdata->first;
  while (row != -1) {
    /* init for each tuple */
    pdata->relevant_vars = EmptySet;
    nnonground = 0;
    for (i=0; i < nvars; i++) {
      SP_globref ref = ATTRIBUTE_LOC(row,i);
      Dvar dv = pdata->dvar+i;
    
      dvar_init(dv, ref, ref+1);
      if (pdata->control[i]==CASE_DOM)
	pdata->prune_set[i] = dvar_set(dv,w);
      else {
	pdata->prune_min[i] = Sup; /* smallest so far */
	pdata->prune_max[i] = Inf; /* greatest so far */
      }
      pdata->prune_mask[i] = 0;
      if (!dvar_is_integer(dv) && pdata->control[i]!=CASE_NONE) {
	pdata->relevant_vars =
	  fd_insert_into(w,MakeSmall(i),pdata->relevant_vars);
	nnonground++;
      }
    }
    /* evaluate the DAG */
    if (!eval_dag(w,row,0,pdata))
      goto ret;

    /* compute pruning acc. to generalization */
    fditer_init(&it, pdata->relevant_vars);
    while (!fditer_empty(&it)) {
      Dvar dv;
    
      tmp = fditer_next(&it);
      i = GetSmall(tmp);
      dv = pdata->dvar+i;
      switch (pdata->control[i]) {
      case CASE_DOM:
	dvar_prune_set(dv, pdata->prune_set[i],w);
	break;
      case CASE_MIN:
	dvar_fix_min_t(dv, pdata->prune_min[i]);
	break;
      case CASE_MAX:
	dvar_fix_max_t(dv, pdata->prune_max[i]);
	pdata->prune_set[i] = fd_interval(w,Inf,pdata->prune_max[i]);
	break;
      case CASE_MINMAX:
      case CASE_VAL:
	dvar_fix_interval_t(dv, pdata->prune_min[i], pdata->prune_max[i]);
	break;
      }
      dvar_pruning_done(w,dv);
      if (dvar_is_integer(dv))
	nnonground--;
    }

    /* propagate, update counts -- must do it also for vas that were not pruned */
    for (i=0; i < nvars; i++) {
      Dvar dv = pdata->dvar+i;
      LATEST_DATE(row,i) = pdata->false_date;
      LATEST_SIZE(row,i) = dvar_value_count(dv);
      dvar_export(w,dv);
    }
    if (nnonground==0) {
      TAGGED t = ENT_FLAG(row);
      LetShadowGlobal;
      BindHVA(t,atom_nil);
      CTagToArg(X(0),2) -= IStep(1); /* decrement entailent counter */
    }
    pdata->first = pdata->next[row];
    pdata->next[row] = -2;
    row = pdata->first;
  }
  pdata->first = -1;
  ent = Teqz(CTagToArg(X(0),2));
 ret:
  SP_free(pdata->prune_set);
  if (ent==1)
    Pfree;
  dvar_export_done(w,Actions, ent);
}

