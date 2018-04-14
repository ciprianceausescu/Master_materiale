/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

#if MULTI_SP_AWARE
#define rank_var(A1,A2,A3,A4,A5) rank_var(HIDDEN_ARG, A1,A2,A3,A4,A5)
#endif /* MULTI_SP_AWARE */

#if LogSizeOfWord==2
# define SignBit 0x80000000
#else
# define SignBit 0x8000000000000000
#endif

/* '$fd_set_singleton(+Val, +Mutable) */
void SPCDECL
prolog_fd_set_singleton MAGIC (HIDDEN_PROTO
			       SP_term_ref ValR,
			       SP_term_ref MutR)
{
  WAMENV;
  TAGGED val, mutable, t1, *h;
  LetShadowGlobal;

  val = RefTerm(ValR);
  mutable = RefTerm(MutR);
  DerefNonvar(val);
  DerefNonvar(mutable);
  t1 = RefMutable(mutable);
  if (!fd_member(val, DomainSet(t1)))
    SP_fail();
  else if (TagToSTR(t1) >= GLOBAL_UNCOND) {
				/* can safely smash it */
    TAGGED *arg = TagToArg(t1,0);
      
    h = w->global_top;
    *h++ = MakeList(arg+2);
    *h++ = atom_nil;
    w->global_top = h;
    arg[1] = MakeList(h-2);
    arg[2] = val;
    arg[3] = val;
    arg[4] = TaggedOne;
  } else {
    h = w->global_top;
    *h++ = MakeList(w->global_top+4);
    *h++ = atom_nil;
    *h++ = fd.functor_dom4;
    *h++ = MakeList(w->global_top);
    *h++ = val;
    *h++ = val;
    *h++ = TaggedOne;
    w->global_top = h;
    update_mutable(w,MakeStructure(h-5), mutable);
  }
}

/* [MC] 3.11.1                                         */
/* Support for incremental variable choice in labeling */

struct labeling_data {
  void (SPCDECL *destructor)(void *);
  void (SPCDECL *daemon)(HIDDEN_PROTO
			 Argtype,void *,SP_globref,TAGGED); /* (w,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  long stamp;
  long *key;			/* cached domain size */
  int *heap;			/* kept in whack wrt. key */
  int *vheap;
  SP_globref refbase;
  int heapsize;
  int heapsize_committed;
  int nvars;
  int type;			/* 1-min, 2-max, 3-ff/ffc */
};

static void SPCDECL labeling_destructor(void *pdata_v)
{
  struct labeling_data *pdata = (struct labeling_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nvars<<1);
  SP_free(pdata);
}

#define SWAP(I,J)				\
{						\
  int vi = pdata->heap[I];			\
  int vj = pdata->heap[J];			\
  pdata->heap[I] = vj;				\
  pdata->heap[J] = vi;				\
  pdata->vheap[vi] = (J);			\
  pdata->vheap[vj] = (I);			\
}

/* TODO: generalize
   For now: (i) size, (ii) ref
*/
static int
cmp_items(struct labeling_data *pdata, int i1, int i2)
{
  return (pdata->key[i1] < pdata->key[i2] ? -1 :
          pdata->key[i1] > pdata->key[i2] ?  1 :
	                i1 < i2               ? -1 : 1);
}



/* the key if heap[i] has decreased -
   move it up until it is at the top or GT its parent.
*/
static void 
decrease_key(struct labeling_data *pdata,
	     int i)
{
  int p = (i-1)>>1;
  while (i>0 && cmp_items(pdata,pdata->heap[i],pdata->heap[p])<0) {
    SWAP(i,p);
    i = p;
    p = (i-1)>>1;
  }
}

static void 
spheapify(struct labeling_data *pdata,
	  int i)
{
  int *heap = pdata->heap;
  
  for (;;) {
    int l = (i<<1)+1;
    int topmost = i;
    if (l<pdata->heapsize && cmp_items(pdata,heap[l],heap[topmost])<0)
      topmost = l;
    if (l+1<pdata->heapsize && cmp_items(pdata,heap[l+1],heap[topmost])<0)
      topmost = l+1;
    if (topmost==i)
      break;
    SWAP(i,topmost);
    i = topmost;
  }
}

static void
set_key(struct labeling_data *pdata,
	int ix)
{
  TAGGED tmp = RefGlob((ix<<1)+pdata->refbase);
  
  DerefAttribute(tmp,tmp); /* get dom/4 term */
  switch (pdata->type) {
  case 1:
    pdata->key[ix] =  GetSmall(DomainMin(tmp));
    break;
  case 2:
    pdata->key[ix] = -GetSmall(DomainMax(tmp));
    break;
  case 3:
  default:
    pdata->key[ix] =  GetSmall(DomainSize(tmp));
  }
}

static void SPCDECL 
labeling_daemon MAGIC (HIDDEN_PROTO
		       Argdecl,
		       void *vdata,
		       SP_globref attr_ref,
		       TAGGED global)
{
  struct labeling_data *pdata = (struct labeling_data *)vdata;
  TAGGED tstate;
  int ar, state_stamp;

  tstate = RefMutable(CTagToArg(global,1));
  ar = Arity(TagToHeadfunctor(tstate));
  state_stamp = GetSmall(CTagToArg(tstate,ar));
  if (pdata->stamp==state_stamp) { /* incremental */
    int ix = (attr_ref - pdata->refbase)>>1;
    int vh = pdata->vheap[ix];
    TAGGED tvar;

    (void)daemon_copy_state(w,&global);
    pdata->stamp++;
    tvar = RefGlob(attr_ref+1);
    DerefSwitch(tvar,goto isvar;);
    --pdata->heapsize;
    if (pdata->heapsize>vh) {
      SWAP(vh,pdata->heapsize);
      spheapify(pdata,vh);
    }
    return;
  isvar:    
    set_key(pdata,ix);
    switch (pdata->type) {
    case 1:
    case 2:
      spheapify(pdata,vh);
      break;
    case 3:
    default:
      decrease_key(pdata,vh);
    }
  }
}

void SPCDECL
prolog_fd_labeling MAGIC (HIDDEN_PROTO
			  SP_term_ref State0,
			  SP_term_ref State,
			  SP_term_ref SelectedOrNil,
			  SP_term_ref Global)
{
  WAMENV;
  TAGGED tvec, telt, handle, functor;
  SP_BOOL committed;		/* TRUE if state can't be backtracked over */
  int nvars, i;
  long state_stamp;
  char *ptr;
  struct labeling_data *pdata;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct labeling_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    nvars = 0;
    DerefArg(tvec,X(0),1);
    DerefArg(functor,X(0),2);
    while (TagIsLST(tvec)) {	/* count terms */
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      if (IsVar(telt))
	nvars++;
    }
    pdata = Palloc(struct labeling_data,
		   nvars*(sizeof(int)+sizeof(int)+sizeof(long)),
		   handle);
    ptr = (char *)(pdata+1);
  
    pdata->key = (long *)ptr;
    ptr += nvars*sizeof(long);
    pdata->heap = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->vheap = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->destructor = labeling_destructor;
    pdata->daemon = labeling_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    pdata->stamp = state_stamp-1;
    pdata->nvars = nvars;
    pdata->heapsize_committed = nvars;
    pdata->heapsize = nvars;
    functor = SetArity(functor,1);
    pdata->type = (functor==fd.functor_min ? 1 :
		   functor==fd.functor_max ? 2 : 3);    
    DerefArg(X(0),X(0),1);
    i = 0;
    while (TagIsLST(X(0))) {
      DerefCar(telt,X(0));
      DerefCdr(X(0),X(0));
      if (IsVar(telt)) {
	SP_globref ref = (i<<1)+pdata->refbase;
	struct dvar dv;
	
	RefGlob(ref) = get_attributes(telt,fd.fd_module);
	RefGlob(ref+1) = telt;
	dvar_init(&dv, ref, ref+1);
	dvar_attach_daemon(w, &dv, pdata, RefTerm(Global), functor);
	pdata->heap[i] = pdata->vheap[i] = i;
	i++;
      }
    }
  }

  if (state_stamp != pdata->stamp) { /* non-incremental */
    pdata->heapsize = pdata->heapsize_committed;
    for (i=0; i<pdata->heapsize; i++) { /* refresh key[] */
      int ix = pdata->heap[i];
      SP_globref ref = (ix<<1)+pdata->refbase;
      TAGGED tvar = RefGlob(ref+1);

      DerefSwitch(tvar,goto isvar;);
      pdata->key[ix] = SignBit;
      continue;
    isvar:
      set_key(pdata,ix);
    }
    for (i=(pdata->heapsize-2)>>1; i>=0; i--) {	/* restore heap property */
      spheapify(pdata,i);
    }
    while (pdata->heapsize>0 &&
	   pdata->key[pdata->heap[0]]==SignBit) { /* delete integers */
      --pdata->heapsize;
      if (pdata->heapsize>0) {
	SWAP(0,pdata->heapsize);
	spheapify(pdata,0);
      }
    }
  }
  pdata->stamp = state_stamp+1;

  /* INVARIANT: heap property holds + no integers in heap */

  if (committed)
    pdata->heapsize_committed = pdata->heapsize;
  if (pdata->heapsize>0) {
    int elt = pdata->heap[0];
    telt = RefGlob((elt<<1)+1+pdata->refbase);
  } else {
    telt = atom_nil;
    Pfree;
  }
  RefTerm(SelectedOrNil) = telt;
}

/******* Variable choice support. *******/

enum var_choice {
  min=1, max, ff, ffc
};



static void
rank_var MAGIC (HIDDEN_PROTO
		Argdecl,
		TAGGED var,
		enum var_choice option,
		long *key1, long *key2)
{
  TAGGED attr = check_argument(w,var,Inf,Sup,Sup);
  TAGGED dom4, susp_mut;

  DerefAttribute(dom4,attr); /* get dom/4 term */

  switch (option) {
  case min:
    *key1 = GetSmall(DomainMin(dom4));
    break;
  case max:
    *key1 = -GetSmall(DomainMax(dom4));
    break;
  case ffc:
    AttrToSuspM(attr,susp_mut);
    *key2 = -GetSmall(CTagToArg(RefMutable(susp_mut),1));
  case ff:
    *key1 = GetSmall(DomainSize(dom4));
    break;
  }
}

/* '$fd_delete'(+List, -Variable, +Option) */
/* 1 - min */
/* 2 - max */
/* 3 - ff  */
/* 4 - ffc */
void SPCDECL
prolog_fd_delete MAGIC (HIDDEN_PROTO
			SP_term_ref ListR,
			SP_term_ref VarR,
			long loption)
{
  WAMENV;
  enum var_choice option = loption;
  long bestk1=0, bestk2=0;
  long currk1=0, currk2=0;
  TAGGED bestvar, list, var;

  list = RefTerm(ListR);	/* known to be list */
  DerefNonvar(list);
  DerefCar(var,list);		/* known to be var */
  DerefCdr(list,list);
  rank_var(w, var, option, &bestk1, &bestk2);
  bestvar = var;
  while (TagIsLST(list) && !(option==ff && bestk1==2)) {
    DerefCar(var,list);
    DerefCdr(list,list);
    if (!IsVar(var))
      continue;
    rank_var(w, var, option, &currk1, &currk2);
    if (currk1<bestk1 || (currk1==bestk1 && currk2<bestk2)) {
      bestk1 = currk1;
      bestk2 = currk2;
      bestvar = var;
    }
  }
  RefTerm(VarR) = bestvar;  
}

