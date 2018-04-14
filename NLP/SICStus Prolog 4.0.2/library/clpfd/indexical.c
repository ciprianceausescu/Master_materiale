/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "fd_insn.h"
#include "dvars.h"

#if DBG
#if defined(_MSC_VER)
/* [PM] 3.9.2b1 if using MS C compile we use inline assembly to break into the debugger (see support2.h) */
#else  /* !_MSC_VER */
#undef DEBUG_BREAK              /* from support2.h */
#define DEBUG_BREAK() clpfd_debug_break()

#if !SP_FD_GLOBAL_DESTRUCTOR       /* avoid cc warning "declared but unused static function" */

/* put gdb breakpoint in this function */
static void clpfd_debug_break(void)
{
  int do_abort = 1;           /* can be cleared from debugger */

  fprintf(stderr, "\nCLPFD Debugger breakpoint -- aborting\n");
  fflush(stderr);
  if (do_abort) {
      abort();
  }
}
#endif /* !SP_FD_GLOBAL_DESTRUCTOR */

#endif /* defined(_MSC_VER) */
#endif /* DBG */


#if MULTI_SP_AWARE
#define fd_neg_offset(A1,A2,A3) fd_neg_offset(HIDDEN_ARG, A1,A2,A3)
#define fd_merge(A1,A2,A3,A4) fd_merge(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_plus(A1,A2,A3,A4,A5) fd_plus(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define fd_minus(A1,A2,A3,A4,A5) fd_minus(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define fd_setmod(A1,A2,A3) fd_setmod(HIDDEN_ARG, A1,A2,A3)
#define fd_prune_and_enqueue(A1,A2,A3,A4) fd_prune_and_enqueue(HIDDEN_ARG, A1,A2,A3,A4)
#define handle_global(A1,A2,A3,A4) handle_global(HIDDEN_ARG, A1,A2,A3,A4)
#define finish_pruning(A1,A2) finish_pruning(HIDDEN_ARG, A1,A2)
#define prune_interval(A1,A2,A3,A4,A5,A6) prune_interval(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define prune_interval_compl(A1,A2,A3,A4,A5,A6) prune_interval_compl(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define fd_store_token(A1,A2,A3) fd_store_token(HIDDEN_ARG, A1,A2,A3)
#define fd_store_literal(A1,A2) fd_store_literal(HIDDEN_ARG, A1,A2)
#define free_fd_info(A1) free_fd_info(HIDDEN_ARG, A1)
#define fd_install_indexicals(A1,A2,A3,A4) fd_install_indexicals(HIDDEN_ARG, A1,A2,A3,A4)
#define init_fd_constraint(A1) init_fd_constraint(HIDDEN_ARG, A1)
#define fd_install(A1,A2,A3,A4,A5) fd_install(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define link_indexical(A1,A2) link_indexical(HIDDEN_ARG, A1,A2)
#define dispatch_global_fast(A1,A2,A3,A4,A5,A6) dispatch_global_fast(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#endif /* MULTI_SP_AWARE */

#define PositiveBound(B) (Tgez(B) && TagIsSmall(B))

#define Top (top[-1])

#define Pop (*(--top))

#define Push(X) (*top++ = (X))

static SP_BOOL fd_infinite(TAGGED d1)
{
  TAGGED r1;

  r1 = CTagToCar(d1);
  d1 = CTagToCdr(d1);
  if (RangeMin(r1)==Inf)
    return TRUE;
  while (d1!=EmptySet) {
    r1 = CTagToCar(d1);
    d1 = CTagToCdr(d1);
  }
  if (RangeMax(r1)==Sup)
    return TRUE;
  return FALSE;
}

#define EMIT_INTERVAL(H,B,E)			\
      *valuep = MakeList(H);			\
      H[0] = MakeList(H+2);			\
      valuep = H+1;				\
      H[2] = (B);				\
      H[3] = (E);				\
      H += 4;    				\

/* d1 x d2 -> {s+t | s in d1 & t in d2} */
static TAGGED fd_plus MAGIC (HIDDEN_PROTO
			     Argdecl,
			     TAGGED d1, TAGGED d2,
			     TAGGED lb, TAGGED ub)
{
  int i, j, count;
  TAGGED t1, t2, *h, *h0, *h1;
  TAGGED value;
  TAGGED *valuep = &value;

  if (d1==EmptySet || d2==EmptySet)
    return EmptySet;
  for (i=0, t1=d1; t1!=EmptySet; i+=1)
    t1 = CTagToCdr(t1);
  for (j=0, t2=d2; t2!=EmptySet; j+=i)
    t2 = CTagToCdr(t2);
				/* collect start and end events */
  NumstackAlloc(6*j,h);
  h0 = h;
  count = 0;
  for (t1=d1; t1!=EmptySet;) {
    TAGGED min1, max1, r1;
    r1 = CTagToCar(t1);
    t1 = CTagToCdr(t1);
    min1 = RangeMin(r1);
    max1 = RangeMax(r1);
    for (t2=d2; t2!=EmptySet;) {
      TAGGED min2, max2, r2;
      r2 = CTagToCar(t2);
      t2 = CTagToCdr(t2);
      min2 = safe_plus(min1,RangeMin(r2));
      max2 = safe_plus(max1,RangeMax(r2));
      if (FDgt(min2,ub))
	break;
      if (FDle(lb,max2)) {
	if (TagIsSmall(min2))
	  *h++ = min2;
	else
	  count++;
	if (TagIsSmall(max2))
	  *h++ = max2+IStep(1)+1;
      }
    }
  }

				/* perform sweep */
  h1 = h;
  t1 = Inf;
  qsort_asc_long((long *)h0,h1-h0);
  while (h0<h1) {
    TAGGED event = *h0++;
    if (TagIsSmall(event)) {	/* start event */
      if (count++ == 0) {
	t1 = event;
      }
    } else {			/* end event */
      if (--count == 0) {
	EMIT_INTERVAL(h,t1,event-IStep(1)-1);
      }
    }
  }
  if (count>0) {
    EMIT_INTERVAL(h,t1,Sup);
  }
  *valuep = EmptySet;
  return value;
}


/* d1 x d2 -> {s-t | s in d1 & t in d2} */
static TAGGED fd_minus MAGIC (HIDDEN_PROTO
			      Argdecl,
			      TAGGED d1, TAGGED d2,
			      TAGGED lb, TAGGED ub)
{
  int i, j, count;
  TAGGED t1, t2, *h, *h0, *h1;
  TAGGED value;
  TAGGED *valuep = &value;

  if (d1==EmptySet || d2==EmptySet)
    return EmptySet;
  for (i=0, t1=d1; t1!=EmptySet; i+=1)
    t1 = CTagToCdr(t1);
  for (j=0, t2=d2; t2!=EmptySet; j+=i)
    t2 = CTagToCdr(t2);
				/* collect start and end events */
  NumstackAlloc(6*j,h);
  h0 = h;
  count = 0;
  for (t1=d1; t1!=EmptySet;) {
    TAGGED min1, max1, r1;
    r1 = CTagToCar(t1);
    t1 = CTagToCdr(t1);
    min1 = RangeMin(r1);
    max1 = RangeMax(r1);
    for (t2=d2; t2!=EmptySet;) {
      TAGGED min2, max2, r2;
      r2 = CTagToCar(t2);
      t2 = CTagToCdr(t2);
      min2 = safe_minus(min1,RangeMax(r2));
      max2 = safe_minus(max1,RangeMin(r2));
      if (FDle(lb,max2) && FDle(min2,ub)) {
	if (TagIsSmall(min2))
	  *h++ = min2;
	else
	  count++;
	if (TagIsSmall(max2))
	  *h++ = max2+IStep(1)+1;
      }
    }
  }
				/* perform sweep */
  h1 = h;
  t1 = Inf;
  qsort_asc_long((long *)h0,h1-h0);
  while (h0<h1) {
    TAGGED event = *h0++;
    if (TagIsSmall(event)) {	/* start event */
      if (count++ == 0) {
	t1 = event;
      }
    } else {			/* end event */
      if (--count == 0) {
	EMIT_INTERVAL(h,t1,event-IStep(1)-1);
      }
    }
  }
  if (count>0) {
    EMIT_INTERVAL(h,t1,Sup);
  }
  *valuep = EmptySet;
  return value;
}

/* support for FD_SETMOD */
static TAGGED fd_setmod MAGIC (HIDDEN_PROTO
			       Argdecl,
			       TAGGED t1, TAGGED t2)
{
  TAGGED value = EmptySet;
  SP_WORD n1, n2;
  SP_WORD modulus = GetSmall(t2);
  
  if (Tlez(t2))
    return ERRORTAG;
  while (t1!=EmptySet) {
    TAGGED min1, max1, r1, subset, diff;

    r1 = CTagToCar(t1); t1 = CTagToCdr(t1);
    min1 = RangeMin(r1);
    max1 = RangeMax(r1);
    diff = max1-min1;

    if (min1==Inf || max1==Sup || Tge(FDincr(diff),t2-TaggedZero)) {
      subset = fd_interval(w,TaggedZero, FDdecr(t2));
      t1 = EmptySet;
    } else {
      n1 = GetSmall(min1) % modulus;
      n2 = GetSmall(max1) % modulus;
      if (n1 <= n2)
	subset = fd_interval(w,MakeSmall(n1), MakeSmall(n2));
      else
	subset = fd_union_dest(fd_interval(w,TaggedZero,MakeSmall(n2)),
			       fd_interval(w,MakeSmall(n1),FDdecr(t2)));
    }
    value = fd_union_dest(value,subset);
  }
  return value;
}


/*** telling ***/

/* Precondition:
   X(EVAL_ARITY) = dest_attribute;
*/
int fd_tell_value MAGIC (HIDDEN_PROTO
			 Argdecl,
			 TAGGED value)
{
  TAGGED *h, t1;
  TAGGED domain, mutable;
  int why = MASK_DOM+MASK_MINMAX+MASK_VAL;
  LetShadowGlobal;

  AttrToDomM(X(EVAL_ARITY),mutable);
  domain = RefMutable(mutable);
  if (!TagIsSmall(value))
    fd.fd_overflow = TRUE;
  if (DomainMin(domain)!=value)
    why += MASK_MIN;
  if (DomainMax(domain)!=value)
    why += MASK_MAX;
  
  if (TagToSTR(domain) >= GLOBAL_UNCOND && TagToLST(DomainSet(domain)) >= GLOBAL_UNCOND) { /* can safely smash domain _and set_ */
    TAGGED old = DomainSet(domain);
    DomainMin(domain) = value;
    DomainMax(domain) = value;
    DomainSize(domain) = TaggedOne;
    CTagToCar(old) = MakeList(&DomainMin(domain));
    CTagToCdr(old) = EmptySet;
  } else {
    RequireHeap(7,EVAL_ARITY+2);	/* GC */
    AttrToDomM(X(EVAL_ARITY),mutable); /* refresh dest_attribute */
    h = w->global_top;
    t1 = MakeList(h);
    h[0] = t1+WD(4);
    h[1] = EmptySet;
    h[2] = fd.functor_dom4;
    h[3] = t1;
    h[4] = value;
    h[5] = value;
    h[6] = TaggedOne;
    w->global_top = h+7;
    update_mutable(w,MakeStructure(h+2), mutable);
  }
  return why;
}


/* Precondition:
   X(EVAL_ARITY) = dest_attribute;
*/
int fd_tell_interval MAGIC (HIDDEN_PROTO Argdecl,
			    TAGGED min, TAGGED max,
			    int why)
{
  TAGGED *h, t1;
  TAGGED card, domain, mutable;
  LetShadowGlobal;
  
  if (min==max)
    return fd_tell_value(w,min); /* GC */
  if (AreSmall(min,max)) {
    long size = GetSmall(max)-GetSmall(min)+1;
    card = (IntIsSmall(size) ? MakeSmall(size) : Sup);
  } else {
    card = Sup;
  }
  AttrToDomM(X(EVAL_ARITY),mutable);
  domain = RefMutable(mutable);
  if (min==Sup || max==Inf)
    fd.fd_overflow = TRUE;
  if (TagToSTR(domain) >= GLOBAL_UNCOND && TagToLST(DomainSet(domain)) >= GLOBAL_UNCOND) { /* can safely smash domain _and set_ */
    TAGGED old = DomainSet(domain);
    DomainMin(domain) = min;
    DomainMax(domain) = max;
    DomainSize(domain) = card;
    CTagToCar(old) = MakeList(&DomainMin(domain));
    CTagToCdr(old) = EmptySet;
  } else {
    RequireHeap(7,EVAL_ARITY+2);	/* GC */
    AttrToDomM(X(EVAL_ARITY),mutable); /* refresh dest_attribute */
    h = w->global_top;
    t1 = MakeList(h);
    h[0] = t1+WD(4);
    h[1] = EmptySet;
    h[2] = fd.functor_dom4;
    h[3] = t1;
    h[4] = min;
    h[5] = max;
    h[6] = card;
    w->global_top = h+7;
    update_mutable(w,MakeStructure(h+2), mutable);
  }
  return why;
}


/* Precondition:
   X(EVAL_ARITY) = dest_attribute;
   X(EVAL_ARITY+1) = dest_var;

   Precondition: new is an interval, or does not point to the heap
*/
int fd_tell MAGIC (HIDDEN_PROTO
		   Argdecl,
		   TAGGED new)
{
  int why = MASK_DOM;
  TAGGED *h;
  TAGGED min=0, max, card, domain, dmin, dmax, mutable;
  
  DerefAttribute(domain,X(EVAL_ARITY));
  dmin = DomainMin(domain);
  dmax = DomainMax(domain);  
  if (CTagToCdr(new)==EmptySet) { /* new domain is an interval */
    new = CTagToCar(new);
    min = RangeMin(new);
    max = RangeMax(new);
    if (min!=dmin)
      why |= MASK_MIN+MASK_MINMAX;
    if (max!=dmax)
      why |= MASK_MAX+MASK_MINMAX;
    return fd_tell_interval(w,min,max,why);
  } else {
    TAGGED d2=new, r2, b2, e2;
    long size=0;

    do {
      r2 = CTagToCar(d2); d2 = CTagToCdr(d2);
      b2 = RangeMin(r2);  e2 = RangeMax(r2);
      if (size==0)
	min = b2;
      if (size>=0 && AreSmall(b2,e2))
	size += GetSmall(e2)-GetSmall(b2)+1;
      else
	size = -1;
    } while (d2!=EmptySet);
    max = e2;
    card = (size>=0 ? MakeSmall(size) : Sup);

    if (min!=dmin)
      why |= MASK_MIN+MASK_MINMAX;
    if (max!=dmax)
      why |= MASK_MAX+MASK_MINMAX;
    new = fd_globalize(w,new,5,EVAL_ARITY+2); /* can GC */
    AttrToDomM(X(EVAL_ARITY),mutable);
    domain = RefMutable(mutable);
    {
      LetShadowGlobal;
      if (TagToSTR(domain) >= GLOBAL_UNCOND) {
	h = TagToArg(domain,0);	  
	h[1] = new;
	h[2] = min;
	h[3] = max;
	h[4] = card;
      } else {
	h = w->global_top;
	h[0] = fd.functor_dom4;
	h[1] = new;
	h[2] = min;
	h[3] = max;
	h[4] = card;
	w->global_top = h+5;
	update_mutable(w,MakeStructure(h), mutable);
      }
    }
  }
  return why;
}

/* Special case of the above for indexicals.
   Preconditions:
   X(EVAL_ARITY) = dest_attribute;
   X(EVAL_ARITY+1) = dest_var;
   new has NOT been fd_localized.
*/
int fd_tell_unsafe MAGIC (HIDDEN_PROTO
			  Argdecl,
			  TAGGED new)
{
  int why = MASK_DOM;
  TAGGED *h;
  TAGGED min=0, max, card, domain, dmin, dmax, mutable;
  
  DerefAttribute(domain,X(EVAL_ARITY));
  dmin = DomainMin(domain);
  dmax = DomainMax(domain);  
  if (CTagToCdr(new)==EmptySet) { /* new domain is an interval */
    new = CTagToCar(new);
    min = RangeMin(new);
    max = RangeMax(new);
    if (min!=dmin)
      why |= MASK_MIN+MASK_MINMAX;
    if (max!=dmax)
      why |= MASK_MAX+MASK_MINMAX;
    return fd_tell_interval(w,min,max,why);
  } else {
    TAGGED d2=new, r2, b2, e2;
    long size=0;

    do {
      r2 = CTagToCar(d2); d2 = CTagToCdr(d2);
      b2 = RangeMin(r2);  e2 = RangeMax(r2);
      if (size==0)
	min = b2;
      if (size>=0 && AreSmall(b2,e2))
	size += GetSmall(e2)-GetSmall(b2)+1;
      else
	size = -1;
    } while (d2!=EmptySet);
    max = e2;
    card = (size>=0 ? MakeSmall(size) : Sup);

    if (min!=dmin)
      why |= MASK_MIN+MASK_MINMAX;
    if (max!=dmax)
      why |= MASK_MAX+MASK_MINMAX;
    new = fd_globalize_unsafe(w,new,5,EVAL_ARITY+2); /* can GC */
    AttrToDomM(X(EVAL_ARITY),mutable);
    domain = RefMutable(mutable);
    {
      LetShadowGlobal;
      if (TagToSTR(domain) >= GLOBAL_UNCOND) {
	h = TagToArg(domain,0);	  
	h[1] = new;
	h[2] = min;
	h[3] = max;
	h[4] = card;
      } else {
	h = w->global_top;
	h[0] = fd.functor_dom4;
	h[1] = new;
	h[2] = min;
	h[3] = max;
	h[4] = card;
	w->global_top = h+5;
	update_mutable(w,MakeStructure(h), mutable);
      }
    }
  }
  return why;
}


/* This function performs two actions:
   1. On entry, why is a bitmask of the effect of the pruning just done.
      This is and:ed with the suspension list bitmask, and or:ed with
      MASK_SINGLETON if appropriate.
   2. If the domain is now a singleton, the variable in question is bound. 

   Precondition:
   X(EVAL_ARITY) = dest_attribute;
   X(EVAL_ARITY+1) = dest_var;
*/
static int finish_pruning MAGIC (HIDDEN_PROTO
				 Argdecl,
				 int why)
{
  TAGGED mutable;
  int mask;
  TAGGED dest_var;
  LetShadowGlobal;
  
  fd.prunings++;
  AttrToSuspM(X(EVAL_ARITY),mutable);
  dest_var = X(EVAL_ARITY+1);
  mask = GetSmall(CTagToArg(RefMutable(mutable),2)); /* bitmask of susp. lists */
  if (why & MASK_VAL) {
    TAGGED value;

    DerefAttribute(value,X(EVAL_ARITY));
    value = DomainMin(value);
				/* purify and bind argument */
    DerefSwitch(dest_var,
    {
      delete_attributes(dest_var,fd.fd_module);
      dest_var = CTagToPointer(CTagToPointer(dest_var));
      if (GVarIsHVA(dest_var))
	{BindHVA(dest_var,value);}
      else
	{BindCVA(dest_var,value); Wake;}
    });
    return (why & mask) + MASK_SINGLETON;
  } else
    return (why & mask);
}

/*** predicates ***/

/* $fd_tell(Var, +FDSet, -Effect)
   Effect is (value returned by fd_tell())/\7.
   Similar to prune_set().
*/
long SPCDECL
prolog_fd_tell MAGIC (HIDDEN_PROTO
		      SP_term_ref VarR,
		      SP_term_ref SetR)
{
  WAMENV;
  TAGGED domain, old, dest_attribute, src, var;
  int why = 0;

  var = RefTerm(VarR);
  src = RefTerm(SetR);
  DerefNonvar(src);
  DerefSwitch(var,goto prune;);
  if (!TagIsSmall(var) || !fd_member(var,src))
    goto fail;
  else
    goto ret;
 prune:
  dest_attribute = check_argument(w,var,Inf,Sup,Sup);
  w->numstack_end = NULL;
  DerefAttribute(domain,dest_attribute);
  old = DomainSet(domain);
  switch (fd_compare(old,src)) {
  case FDI_SUBSET:
  case FDI_EQUAL:
    break;
  case FDI_DISJOINT:
    goto fail;
  case FDI_SUPERSET:
    X(3) = dest_attribute;		/* preserve over GC */
    X(4) = var;
    why = fd_tell_unsafe(w,src); /* GC */
    break;
  case FDI_INTERSECT:
    X(3) = dest_attribute;		/* preserve over GC */
    X(4) = var;
    why = fd_tell_unsafe(w,fd_intersection(w,old,src)); /* GC */
  }
 ret:
  return why&7;
 fail:
  SP_fail();
  return 0;
}


/* Guts of below, used in fd_prune_and_enqueue().
   Precondition:
   X(EVAL_ARITY) = dest_attribute;
   X(EVAL_ARITY+1) = dest_var;
*/
void
fd_told MAGIC (HIDDEN_PROTO Argdecl,
	       int why,
	       TAGGED filter)
{
  TAGGED domain;
  
  DerefAttribute(domain,X(EVAL_ARITY));
				/* merge in implied bits */
  if (why & (MASK_MIN | MASK_MAX))
    why |= MASK_MINMAX;
  if (DomainSize(domain)==TaggedOne)
    why |= MASK_VAL;
				/* maybe bind; filter why */
  why = finish_pruning(w,why);
  if (why > 0) {
    TAGGED mutable;

    AttrToSuspM(X(EVAL_ARITY),mutable);
    fd_enqueue_all(w,why,filter,mutable);
  }
}


static  int fd_prune_and_enqueue MAGIC (HIDDEN_PROTO Argdecl,
					SP_term_ref ActionsR,
					SP_term_ref GlobalR,
					SP_term_ref ItemR)
{
  TAGGED dest_var, glob;
  TAGGED statmut, status;
  int why;

  fd_sync(Arg);
  while (TagIsLST(X(0))) {
    TAGGED item;
    
    DerefCar(item,X(0));
    DerefCdr(X(0),X(0));
    if (TagIsATM(item)) {
      LetShadowGlobal;
      
      if (item==atom_fail) {
	return FALSE;
      }
      fd.entailments++;
      DEREF(glob,RefTerm(GlobalR)); /* get global/5 */
      DerefArg(glob,glob,4);	/* get Ent */
      BindHVA(glob,MakeSmall(1)); /* always HVA */
    } else {
      TAGGED functor = TagToHeadfunctor(item);
      
      if (functor == fd.functor_dom)
	why = 1;
      else if (functor == fd.functor_min)
	why = 3;
      else if (functor == fd.functor_max)
	why = 5;
      else if (functor == fd.functor_minmax)
	why = 7;
      else {
	RefTerm(ActionsR) = X(0);
	RefTerm(ItemR) = item;
	switch (SP_query(fd.call_action1,ItemR)) {
	case SP_FAILURE:
	  return SP_FAILURE;
	case SP_ERROR:
	  return SP_ERROR;
	default:
	  X(0) = RefTerm(ActionsR);
	  continue;
	}
      }
      DerefArg(dest_var,item,1);
      if (IsVar(dest_var)) {	/* otherwise, co-reference already dealt with */
	X(EVAL_ARITY) = get_attributes(dest_var,fd.fd_module);
	X(EVAL_ARITY+1) = dest_var;
	fd_told(w, why, atom_nil);
      }
    }
  }
  DEREF(glob,RefTerm(GlobalR));	/* get global/5 */
  DerefArg(statmut,glob,3);	/* get status mutable */
  status = RefMutable(statmut);
  if (status&IStep(4))		/* STATUS: not idempotent */
    status &= ~IStep(8);	/* STATUS: not current */
  else
    status &= ~IStep(9);	/* STATUS: not current, not enqueued */
  update_mutable(w,status,statmut);
  return TRUE;
}

/*
% prune_and_enqueue(+Actions, +Global)

*/
void SPCDECL
prolog_fd_prune_and_enqueue MAGIC (HIDDEN_PROTO
				   SP_term_ref ActionsR,
				   SP_term_ref GlobalR)
{
  WAMENV;

  /*    X(0) = RefTerm(ActionsR); */
  DerefNonvar(X(0));
  switch (fd_prune_and_enqueue(Arg,ActionsR,GlobalR,SP_new_term_ref())) {
  case SP_FAILURE:
    fd.failures++;
    SP_fail();
    break;
  case SP_ERROR:
    SP_exception_term(ActionsR);
    SP_raise_exception(ActionsR);
  default:
    break;
  }
}



/* $fd_check_arguments(+Goal, -Attv) */
void SPCDECL
prolog_fd_check_arguments MAGIC (HIDDEN_PROTO
				 SP_term_ref GoalR,
				 SP_term_ref AttvR)
{
  WAMENV;
  int i, ar;
  TAGGED attr[ARITYLIMIT];
  TAGGED *h, v;

  X(0) = RefTerm(GoalR);
  DerefNonvar(X(0));
  if (!TagIsSTR(X(0))) {
    RefTerm(AttvR) = X(0);
  } else {
    attr[0] = TagToHeadfunctor(X(0));
    ar = Arity(attr[0]);
    RequireHeap(ar*(FD_ATTRIBUTE_SIZE+ARITYLIMIT+6), 2); /* GC */
    for (i=1; i<=ar; i++)
      if (!(attr[i]=check_argument(w,CTagToArg(X(0),i),Inf,Sup,Sup))) {
	SP_fail();
	return;
      }
    h = w->global_top;
    v = MakeStructure(h);
    *h++ = attr[0];
    for (i=1; i<=ar; i++)
      *h++ = attr[i];
    w->global_top = h;
    RefTerm(AttvR) = v;
  }
}


static ix_byte *fd_store_token MAGIC (HIDDEN_PROTO
				      ix_byte *ptr,
				      TAGGED token,
				      TAGGED *literals)
{
  if (TagIsSIN(token)) {	/* opcode */
    token = DispatchLabel((TAGGED)GetSmall(token));
  } else {
    TAGGED f = TagToHeadfunctor(token);
    TAGGED arg;
    int i;

    DerefArg(arg,token,1);
    i = GetSmall(arg);
    if (f==fd.token_a)		/* argreg offset */
      token = (ix_byte)i;
    else if (f==fd.token_t)	/* tagged literal offset */
      token = (ix_byte)literals[i];
    else if (f==fd.token_d) {	/* ground domain */
      token = (ix_byte)literals[i];
      if (TagIsSTR(token))
	token = MakeList(TagToArg(token,1));
    } else if (f==fd.token_h)		/* hash table literal offset */
      token = (ix_byte)TermToPointer(LIST_INT(literals[i]));
    else if (f==fd.token_l)		/* code offset, RELATIVE */
      token = (ix_byte)i;
  }
  *ptr++ = token;
  return ptr;
}



static TAGGED *fd_store_literal MAGIC (HIDDEN_PROTO
				       TAGGED *literals,
				       TAGGED token)
{
  switch (TagOf(token)) {
  case CONST_TAG:			/* constant term or inf/sup */
    break;
  case STRUCT_TAG:
    {
      TAGGED f = TagToHeadfunctor(token);
      DerefArg(token, token, 1);
      if (f==fd.token_d) {	/* domain */
	int k = 1;
	TAGGED p = token;
	TAGGED *q, car, cdr;

	if (p==atom_nil)
	  break;
	k += list_length(p)<<2;
	q = sp_checkalloc(k*sizeof(TAGGED), TRUE);
	q[0] = BIGNUM_HEADER - LStep(k-2);
	k = 1;
	p = token;
	while (TagIsLST(p)) {
	  DerefCar(car,p);
	  DerefCdr(cdr,car);
	  DerefCar(car,car);
	  DerefCdr(p,p);
	  q[k+0] = MakeList(q+k+2);
	  q[k+1] = MakeList(q+k+4);
	  q[k+2] = car;
	  q[k+3] = cdr;
	  k += 4;
	}
	q[k-3] = atom_nil;
	token = MakeStructure(q);
      } else {		/* hash table */
	struct sw_on_key *sw = new_switch_on_key(2,NULL);
	TAGGED item, key, value;
	    
	while (TagIsLST(token)) {
	  DerefCar(item, token);
	  DerefCdr(token, token);
	  DerefArg(key, item, 1);
	  DerefArg(value, item, 2);
	  dyn_puthash(&sw,key)->value.arities = value;
	}
	token = INT_LIST(PointerToTerm(sw)); /* mark for dealloc */
      }
    }
    break;
  }
  *literals++ = token;
  return literals;
}

static void free_fd_info MAGIC (HIDDEN_PROTO
				struct indexical_info **infop)
{
  struct indexical_info *ixinfo;

  /* [PM] 3.9b4 note that *infop may be NULL (except when called from free_fd_info_hook)*/

  while ((ixinfo = (*infop))) { 

#if DBG
    if (ixinfo->identifier != fd.generation) {
#if SP_FD_GLOBAL_DESTRUCTOR
#if DBG>1
      fprintf(stderr, "DBG CLPFD: free_fd_info clpfd has been unloaded/reloaded since info was created (id==%d, clpfd_generation==%d) (this is expected)\n",
	      (int)ixinfo->identifier, (int)fd.generation); fflush(stderr);
#endif

#else /* !SP_FD_GLOBAL_DESTRUCTOR */
      DEBUG_BREAK_MSG((stderr, "free_fd_info clpfd has been unloaded/reloaded since info was created (id==%d, clpfd_generation==%d)",
		       (int)ixinfo->identifier, (int)fd.generation));
#endif /* !SP_FD_GLOBAL_DESTRUCTOR */
    }
#endif/* DBG */

    if (ixinfo->linkage)
      sp_checkdealloc((TAGGED *)(ixinfo->linkage),
		      ixinfo->length_of_linkage*sizeof(ix_byte),
		      FALSE);
    if (ixinfo->code)
      sp_checkdealloc((TAGGED *)(ixinfo->code),
		      ixinfo->length_of_bytecode*sizeof(ix_byte),
		      FALSE);
    if (ixinfo->literals) {
      int i, len = ixinfo->length_of_literals;
      TAGGED *p = ixinfo->literals;
      struct sw_on_key *htab;
      TAGGED *q;

      for (i=0; i<len; i++) {
	switch (TagOf(p[i])) {
	case LIST_TAG:	/* hash table */
	  htab = (struct sw_on_key *)TermToPointer(LIST_INT(p[i]));
	  dispose_switch_on_key(htab);
	  break;
	case STRUCT_TAG:	/* FD set */
	  q = TagToSTR(p[i]);
	  sp_checkdealloc(q, LargeArity(q[0])*sizeof(TAGGED), TRUE);
	}
      }
      sp_checkdealloc(p, len*sizeof(TAGGED), FALSE);
    }
    (*infop) = ixinfo->next;
    sp_checkdealloc((TAGGED *)ixinfo, sizeof(struct indexical_info), FALSE);
  }
}


#if !SP_FD_GLOBAL_DESTRUCTOR
void SPCDECL free_fd_info_hook(struct indexical_info **infop)
{
  /* [PM] note that this is a little special compared to other
     destructors, needing an extra indirection */
  FD_SETUP_SPENV((*infop)->spenv)
  free_fd_info(infop);          /* passes the HIDDEN_ARG SPEnv extracted from (the non-NULL!) *infop */
}
#endif /* !SP_FD_GLOBAL_DESTRUCTOR */


#if SP_FD_GLOBAL_DESTRUCTOR

void SPCDECL fd_destructor_hook(
     SP_HOOKS_COOKIE_PROTO_COMMA
     struct indexical_info **infop
     )
{
  SP_HOOKS_COOKIE_USE;
  free_fd_info(infop);
}

#endif /* SP_FD_GLOBAL_DESTRUCTOR */


static void fd_install_indexicals MAGIC (HIDDEN_PROTO
					 TAGGED Info,
					 int type, int no_indexicals,
					 struct definition *f)
{
  TAGGED Indexical, tmp, tmp2, tmp3;
  int i,j,length;
  struct indexical_info *tmp_ix=NULL;
  struct indexical_info *Ix;
  ix_byte *code;
  TAGGED *literals;

  Ix = (struct indexical_info *)
    sp_checkalloc(sizeof(struct indexical_info), FALSE);

#if DBG                         /* [PM] 3.9.2b1 */
  Ix->identifier = fd.generation;
#endif


  f->proc.code.fdinfo->info[type] = Ix;

#if !SP_FD_GLOBAL_DESTRUCTOR   /* [PM] 3.9.2b1 */
  {
#if 1                       /* [PM] 3.9b4 */
    Ix->destructor_fun = fd.fd_destructor_fun;
#else
    Ix->destructor = fd.fd_destructor;
#endif
    FD_STORE_SPENV(Ix->spenv);
  }
#endif/* !SP_FD_GLOBAL_DESTRUCTOR */


  for (i=0; i<no_indexicals; i++) { /* for each of the indexicals */
    if (i == 0) {
      tmp_ix = Ix;
    } else {
      tmp_ix = tmp_ix->next =
	(struct indexical_info *)
	sp_checkalloc(sizeof(struct indexical_info), FALSE);
#if DBG               /* [PM] 3.9.2b1 */
      tmp_ix->identifier = fd.generation;
#endif
    }

#if !SP_FD_GLOBAL_DESTRUCTOR   /* [PM] 3.9.2b1 */
    {
#if 1                   /* [PM] 3.9b4 */
      tmp_ix->destructor_fun = fd.fd_destructor_fun;
#else
      tmp_ix->destructor = fd.fd_destructor;
#endif
      FD_STORE_SPENV(tmp_ix->spenv);
    }
#endif/* !SP_FD_GLOBAL_DESTRUCTOR */

    tmp_ix->pred = f;
    tmp_ix->next = NULL;
    tmp_ix->checking = type>>1;
    tmp_ix->truth_value = type;
    DerefCar(Indexical, Info);
    DerefCdr(Info, Info);
    DerefArg(tmp, Indexical, 2); /* get length of fd linkage */
    tmp_ix->length_of_linkage = length = GetSmall(tmp);
    if (!length)		/* If the linkage list is empty */
      tmp_ix->linkage = NULL;
    else {
      tmp_ix->linkage =
	(ix_byte *)sp_checkalloc(length*sizeof(ix_byte), FALSE);
      DerefArg(tmp, Indexical, 1); /* tmp is now the linkage list */
      for (j = 0; j<length; j++) {
	DerefCar(tmp2, tmp);
	DerefArg(tmp3, tmp2, 1);
	DerefArg(tmp2, tmp2, 2);
	tmp_ix->linkage[j] = (GetSmall(tmp3)<<8) + GetSmall(tmp2);
	DerefCdr(tmp, tmp);
      }
    }
    DerefArg(tmp, Indexical, 3); /* get pruned */
    tmp_ix->pruned = GetSmall(tmp);

    DerefArg(tmp, Indexical, 7); /* get length of literals */
    tmp_ix->length_of_literals = length = GetSmall(tmp);
    if (!length)
      tmp_ix->literals = NULL;
    else {
      tmp_ix->literals =
	literals =
	sp_checkalloc(length*sizeof(TAGGED), FALSE);
      DerefArg(tmp, Indexical, 6);
      while (TagIsLST(tmp)) {
	DerefCar(tmp2, tmp);
	DerefCdr(tmp, tmp);
	literals = fd_store_literal(literals, tmp2);
      }
    }

    DerefArg(tmp, Indexical, 5); /* get length of bytecode */
    tmp_ix->length_of_bytecode = length = GetSmall(tmp);
    if (!length)
      tmp_ix->code = NULL;
    else {
      tmp_ix->code =
	code =
	(ix_byte *)sp_checkalloc(length*sizeof(ix_byte), FALSE);
      DerefArg(tmp, Indexical, 4);
      while (TagIsLST(tmp)) {
	DerefCar(tmp2, tmp);
	DerefCdr(tmp, tmp);
	code = fd_store_token(code, tmp2, tmp_ix->literals);
      }
    }
    tmp_ix->next = NULL;
  }
}

/* Precondition: the predicate is currently undefined. */
static void init_fd_constraint MAGIC (HIDDEN_PROTO
				      struct definition *f)
{
  int i;
  
  /* [MC 4.0] TODO: memory leak here is currently not FD_CONSTRAINT */
  /* was: SP_install_c_predicate(s,f->proc.arity,f->module,0(*uninstall*),NULL,NULL); */
  f->proc.code.fdinfo =
    (struct fd_info *)sp_checkalloc(sizeof(struct fd_info), FALSE);
  for (i=0; i<4; i++)
    f->proc.code.fdinfo->info[i] = NULL;
  update_exports(f,FD_CONSTRAINT);
}


static void fd_install MAGIC (HIDDEN_PROTO
			      TAGGED head, TAGGED Module,
			      long Type, long InfoLength,
			      TAGGED Info)
{
  TAGGED *junk;
  struct mod_def *m = find_module(Module,TRUE);
  struct definition *f = find_definition(m,head,&junk,TRUE);
    
  if (f->proc.properties.prop.predtyp!=FD_CONSTRAINT)
    init_fd_constraint(f);
  else
    free_fd_info((struct indexical_info **)&f->proc.code.fdinfo->info[Type]);
  fd_install_indexicals(Info, Type, InfoLength, f);
}



/* $fd_install(+Name/+Arity, +Module, +Type, +InfoLength, +Info) */
void SPCDECL
prolog_fd_install MAGIC (HIDDEN_PROTO
			 SP_term_ref PredSpecR,
			 SP_atom module,
			 long type,
			 long length,
			 SP_term_ref InfoR)
{
  WAMENV;
  TAGGED name, arity, spec, info;

  spec = RefTerm(PredSpecR);
  info = RefTerm(InfoR);
  DerefNonvar(spec);
  DerefNonvar(info);
  DerefArg(name, spec, 1);
  DerefArg(arity, spec, 2);
  if (Tnez(arity)) {
    int i = GetSmall(arity);
    TAGGED *h = w->global_top;
	
    *h++ = SetArity(name,i);
    for (; i>0; --i)
      Load0HVA(h);
    name = MakeStructure(w->global_top);
  }
  fd_install(name, module, type, length, info);
}

static TAGGED fd_mon_vars(Argdecl,
			  struct indexical_info *current,
			  TAGGED goal)
{
  unsigned int mask = (current->checking ? 16 : 8);
  int length = current->length_of_linkage;
  TAGGED list = atom_nil;
  TAGGED *h = w->global_top;
  int i;
  ix_byte tmp_link;
  TAGGED t1;
  
  for (i=0; i<length; i++) {
    tmp_link = current->linkage[i];
    if (tmp_link & mask) {
      DerefArg(t1, goal, (tmp_link>>8)+1);
      if (IsVar(t1)) {
	HeapPush(h,t1);
	HeapPush(h,list);
	list = MakeList(h-2);
      }
    }
  }
  w->global_top = h;
  return list;
}



/* link in the indexical X(0) upon initial suspension */
/* called by $fd_evaluate_indexical only */
static void link_indexical MAGIC (HIDDEN_PROTO
				  Argdecl,
				  struct indexical_info *current)
{				/* link indexical into suspension lists */
  TAGGED t1;
  unsigned int tmp_link;
  int length, i;
  ix_byte mask;
  TAGGED ix, *args;

  mask = (current->checking ? 16 : 8);
  length = current->length_of_linkage;
  RequireHeap(length*(256+38),EVAL_ARITY); /* GC, max. 256 modules? */
  ix = X(0);
  args = TagToArg(CTagToArg(ix,2),1);	/* refreshed after GC */
  for (i=0; i<length; i++) {
    tmp_link = current->linkage[i];
    if (!(tmp_link & mask)) {
      t1 = args[tmp_link>>8];
      DerefSwitch(t1,fd_link(w,t1,fd.linkage_keys[tmp_link & 7],ix););
    }
  }
}


/* $fd_post_reified(+DefPtr, +Goal, +Attv, -ZeroOne, -ZeroOneAttr) */
/* Ensure: ZeroOne in 0..1 has been done. */
/* [MC] 4.0: DefPtr is passed as a PtrToTerm. */
void SPCDECL
prolog_fd_post_reified MAGIC (HIDDEN_PROTO
			      SP_term_ref DefPtr,
			      SP_term_ref Goal,
			      SP_term_ref Attv,
			      SP_term_ref ZeroOne,
			      SP_term_ref ZeroOneAttr)
{
  WAMENV;
  struct definition *def;
  struct indexical_info *ix, **p, **q;
  struct indexical_info *array[512];
  TAGGED *h;
  int type, count;
  int mintype = 0;
  int maxtype = 3;

  DEREF(X(0),RefTerm(DefPtr));
  def = (struct definition *)TermToPointer(X(0));
  DEREF(X(0),RefTerm(Goal));
  DEREF(X(1),RefTerm(Attv));
  DEREF(X(2),RefTerm(ZeroOne));
  h = w->global_top;
  LoadHVA(X(3),h);
  LoadHVA(X(4),h);
  LoadHVA(X(5),h);
  DEREF(X(6),RefTerm(ZeroOneAttr));
  w->global_top = h;
  fd.constraints++;
  fd_sync(Arg);
  if (X(2)==TaggedOne) {	/* post positive */
    mintype = maxtype = 1;
  } else if (X(2)==TaggedZero) { /* post negative */
    mintype = maxtype = 0;
  }
  
  count = 0;
  for (type=mintype; type<=maxtype; type++)
    for (ix=(struct indexical_info *)def->proc.code.fdinfo->info[type]; ix; ix=ix->next)
      count++;

  count *= 2*def->proc.arity + 29; /* ix(8) + $mutable(3) + fd_mon_vars(2*arity) + -(3) + iff(5) + link(10) */
  RequireHeap(count,7);	

  for (type=mintype; type<=maxtype; type++) {
    p = q = array+256;
    for (ix=(struct indexical_info *)def->proc.code.fdinfo->info[type]; ix; ix=ix->next) {
      TAGGED t1;
      DerefArg(t1, X(0), ix->pruned+1);
      if (IsVar(t1))
	*(--p) = ix;
      else
	*q++ = ix;
    }
    for (ix = *(--q); p<=q; ix = *(--q)) {
      TAGGED vars = fd_mon_vars(w,ix,X(0)); /* at most 512 words */
      TAGGED item;

      h = w->global_top;
      h[0] = functor_Dmutable;
      h[1] = TaggedOne;
      h[2] = TaggedZero;
      h[3] = functor_ix7;
      h[4] = PointerToTerm(ix);	/* pointer */
      h[5] = X(0);		/* goal */
      h[6] = MakeStructure(h);	/* status mutable */
      h[7] = (type<2 ? X(5) : X(3)); /* entailment flag */
      h[8] = (type<2 ? X(4) : X(2)); /* zero-one var */
      h[9] = X(1);		/* Attv */
#if 0
      h[10] = CTagToArg(X(1),ix->pruned+1); /* LHs attribute */
#else
      h[10] = X(6);		/* zero-one attribute */
#endif
      item = MakeStructure(h+3);
      w->global_top = h+11;

      if (type<2 && mintype<maxtype) { /* suspend the indexical on an iff/4 */
	h = w->global_top;
	h[0] = functor_minus;
	h[1] = vars;
	h[2] = MakeStructure(h+3);
	h[3] = functor_iff4;
	h[4] = item;
	h[5] = X(2);
	h[6] = MakeSmall(type);
	h[7] = X(3);
	w->global_top = h+8;
	fd_link(w,X(2),fd.functor_val,MakeStructure(h)); /* at most 10 words */
      } else if (TagIsLST(vars)) { /* suspend the indexical on a val list */
	h = w->global_top;
	h[0] = functor_minus;
	h[1] = CTagToCdr(vars);
	h[2] = item;
	w->global_top = h+3;
	fd_link(w,CTagToCar(vars),fd.functor_val,MakeStructure(h)); /* at most 10 words */
      } else {			/* post the indexical */
	fd_enqueue_append(w, item, 0x0);
      }
    }
  }
}

/* $fd_post_global(+Constraint, +State, +Status, +Source, +Susp, -Global) */
void SPCDECL
prolog_fd_post_global MAGIC (HIDDEN_PROTO
			     SP_term_ref Constraint,
			     SP_term_ref State,
			     long status,
			     SP_term_ref Source,
			     SP_term_ref Susp,
			     SP_term_ref Global)
{
  WAMENV;
  TAGGED *h;

  fd.constraints++;
  DEREF(RefTerm(Constraint),RefTerm(Constraint));
  DEREF(RefTerm(Susp),RefTerm(Susp));
  h = w->global_top;
  h[0] = functor_Dmutable;
  h[1] = RefTerm(State);
  h[2] = TaggedZero;
  h[3] = functor_Dmutable;
  h[4] = MakeSmall(status|2);
  h[5] = TaggedZero;
  h[6] = functor_global5;
  h[7] = MakeStructure(h+0);
  h[8] = RefTerm(Constraint);
  h[9] = MakeStructure(h+3);
  h[10] = TagREF(h+10);
  h[11] = RefTerm(Source);
  RefTerm(Global) = MakeStructure(h+6);
  w->global_top = h+12;

  while (TagIsLST(RefTerm(Susp))) {
    TAGGED var, request, fun;
    RequireHeap(256+38,0);
    DerefCar(request,RefTerm(Susp));
    DerefCdr(RefTerm(Susp),RefTerm(Susp));
    fun = TagToHeadfunctor(request);
    DerefArg(var,request,1);
    if (IsVar(var) && fun!=fd.functor_none)
      fd_link(w,var,fun,RefTerm(Global));
  }
  fd_enqueue_append(w, RefTerm(Global), 0x1);	/* for immediate dequeue */
}

/* $fd_find_definition(+Constraint, +Module, -DefPtr) */
/* [MC] 4.0: DefPtr is passed as a PtrToTerm. */
void SPCDECL
prolog_fd_find_definition MAGIC (HIDDEN_PROTO
				 SP_term_ref Constraint,
				 SP_atom module,
				 SP_term_ref DefPtrR)
{
  WAMENV;
  struct definition *f;
  TAGGED *junk;
  
/*    X(0) = RefTerm(Constraint); */
  (void)Constraint;
  DerefNonvar(X(0));
  f = find_definition(find_module(module,FALSE),X(0),&junk,FALSE);
  if (f && f->proc.properties.prop.predtyp!=FD_CONSTRAINT)
    f = NULL;

  RefTerm(DefPtrR) = PointerToTerm(f);
}


/* $fd_indexical_data(+Ptr, -Type, -Module)
   [MC] 4.0: Ptr is passed as a PtrToTerm.
*/
SP_atom SPCDECL
prolog_fd_indexical_data MAGIC (HIDDEN_PROTO
				SP_term_ref PtrR,
				long *type)
{
  WAMENV;
  struct indexical_info *current = (struct indexical_info *)TermToPointer(RefTerm(PtrR));
  
  (void)HIDDEN_ARG_OR_ZERO;
  *type = (current->checking<<1) + current->truth_value;
  return current->pred->module->name;
}


typedef void (SPCDECL *FdFilterFun)(HIDDEN_PROTO SP_term_ref, SP_term_ref, SP_term_ref);

static int
dispatch_global_fast MAGIC (HIDDEN_PROTO
			    Argdecl,
			    TAGGED constraint,	/* dereffed */
			    SP_term_ref State,	/* dereffed */
			    SP_term_ref NewState,
			    SP_term_ref Actions,
			    SP_term_ref Global) /* dereffed */
{
  TAGGED f = IsAtomic(constraint) ? constraint : TagToHeadfunctor(constraint);
  struct sw_on_key_node *hnode = incore_gethash(fd.dispatch,f);

  if (!hnode->value.arities)
    return 0;
  X(0) = RefTerm(State);
  X(1) = RefTerm(Global);
  (*(FdFilterFun)hnode->value.arities)(HIDDEN_ARG_COMMA State, NewState, Actions);
  return w->fli_data->excp_flag;
}

/* $fd_dispatch_global_fast(+Constraint, +State, -NewState, -Actions, +Global)
   Effect is (value returned by fd_tell())/\7.
   Similar to prune_set().
*/
long SPCDECL
prolog_fd_dispatch_global_fast MAGIC (HIDDEN_PROTO
				      SP_term_ref Constraint,
				      SP_term_ref State,
				      SP_term_ref NewState,
				      SP_term_ref Actions,
				      SP_term_ref Global)
{
  WAMENV;
  TAGGED constraint = RefTerm(Constraint);
  DEREF(constraint,constraint);
  DEREF(RefTerm(State),RefTerm(State));
  DEREF(RefTerm(Global),RefTerm(Global));

  return dispatch_global_fast(w, constraint, State, NewState, Actions, Global);
}

/* '$fd_evaluate_indexical'(-RC, -Global)

Indexical = ix(+Ptr,+Goal,+StatusM,?Ent,-ZeroOne,+Attv,+LAttr)

X(1) holds status mutable
X(2) holds entailment variable.

Xref EVAL_ARITY
*/
long SPCDECL
prolog_fd_evaluate_indexical MAGIC (HIDDEN_PROTO
				    SP_term_ref Global)
{
  WAMENV;
  struct indexical_info *current;
  int ground;
  TAGGED gtemp, *atts, *args;
  TAGGED truth_value=TaggedOne, min2=0, max2=0;
  ix_byte *code;
  TAGGED *top=&X(EVAL_ARITY);
  TAGGED qval[128];
  TAGGED statem, statmut;
  SP_term_ref State = SP_new_term_ref();
  SP_term_ref Actions = SP_new_term_ref();
  struct dvar dvar;
  DispatchDef;
  
  fd_sync(Arg);
  fd.fd_overflow = FALSE;
 restart:
  switch (fd_dequeue(w,&X(0))) {
  case 0:
    fd_end(Arg);
    return 0;
  case 3:
    return 1;
  case 2:
    RefTerm(Global) = X(0);
    statem = CTagToArg(X(0),4); /* skip if entailed, does happen */
    DerefSwitch(statem,goto run_global;);
    goto restart;
  run_global:
    statmut = CTagToArg(X(0),3);
    gtemp = RefMutable(statmut);
    if (gtemp&IStep(4))		/* STATUS: not idempotent */
      gtemp &= ~IStep(1);	/* STATUS: not enqueued */
    update_mutable(w, gtemp|IStep(8), statmut);
    fd.resumptions++;
    if (fd.debugging)	       /* status must be updated even if we return 2 */
      return 2;
    statem = CTagToArg(X(0),1);
    RefTerm(State) = RefMutable(statem);
    if (!dispatch_global_fast(w,CTagToArg(X(0),2),State,State,Actions,Global))
      return 2;
    /* X(1) = RefTerm(Global); */
    DerefArg(statem,X(1),1);
    update_mutable(w,RefTerm(State),statem);
    X(0) = RefTerm(Actions);
    /* X(1) = RefTerm(Global); */
    switch (fd_prune_and_enqueue(Arg,Actions,Global,State/*recycled ref*/)) {
    case SP_FAILURE:
      goto fail;
    case SP_ERROR:
      SP_exception_term(Actions);
      SP_raise_exception(Actions);
      return 0;
    default:
      goto restart;
    }
  }      
  ground = 1;
  {
    TAGGED *ix_args = TagToArg(X(0),1);
    
    X(2) = ix_args[3];	/* Ent, always a HVA */
    DerefSwitch(X(2),goto run_ix;); /* skip if (dis)entailed, does happen */
    goto restart;
  run_ix:
    current = (struct indexical_info *)TermToPointer(ix_args[0]);
    args = TagToArg(ix_args[1],1); /* Goal */
    X(1) = ix_args[2];		/* StatusM */
    atts = TagToArg(ix_args[5],1); /* Attv */
    dvar_init_ix(&dvar,atts[current->pruned],args[current->pruned]);
  }
  fd.resumptions++;
  w->numstack_end = NULL;
  code = current->code;
  DispatchFirst {
    DispatchHead {
    CaseX(FD_DUP_RANGE):	/* int x int -> set */
      {
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_interval(w,t1,t1);
	Dispatch;
      }

    CaseX(FD_RANGE_OO):		/* -> set */
      {
	Prefetch;
	
	Push(fd_interval(w,Inf, Sup));
	Dispatch;
      }

    CaseX(FD_RANGE_OC):		/* int -> set */
      {
	TAGGED t2 = Top;
	Prefetch;
	
	Top = (t2==Inf ? EmptySet : fd_interval(w,Inf, t2));
	Dispatch;
      }

    CaseX(FD_RANGE_CO):		/* int -> set */
      {
	TAGGED t1 = Top;
	Prefetch;
	
	Top = (t1==Sup ? EmptySet : fd_interval(w,t1, Sup));
	Dispatch;
      }

    CaseX(FD_RANGE_CC):		/* int x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (t1==t2 && !TagIsSmall(t1)) {
	  fd.fd_overflow = TRUE;
	  goto ov_loop;
	}
	Top = (EmptyInterval(t1,t2) ? EmptySet : fd_interval(w,t1, t2));
	Dispatch;
      }

    CaseX(FD_SETADD):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_lsh(w,t1,t2);
	Dispatch;
      }

    CaseX(FD_SETSUB):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_lsh(w,t1,Tminus(t2));
	Dispatch;
      }

    CaseX(FD_SETNEG):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_neg_offset(w,t1,t2);
	Dispatch;
      }

    CaseX(FD_SETMOD):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if ((Top = fd_setmod(w,t1,t2))==ERRORTAG)
	  goto abort;
	Dispatch;
      }

    CaseX(FD_SETPLUS):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_plus(w,t1,t2,Inf,Sup);
	Dispatch;
      }

    CaseX(FD_SETMINUS):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_minus(w,t1,t2,Inf,Sup);
	Dispatch;
      }

    CaseX(FD_COMPL_T):		/* int -> set */
      {
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_compl_interval(w,t1,t1);
	Dispatch;
      }

    CaseX(FD_COMPL_D):		/* set -> set */
      {
	Prefetch;
	
	Top = fd_complement(w,Top);
	Dispatch;
      }

    CaseX(FD_UNION_TT):		/* int x int -> set */
      {
	TAGGED t1 = Pop;
	TAGGED t2 = Top;
	Prefetch;
	
	Top = fd_pair(w,t1,t2);
	Dispatch;
      }

    CaseX(FD_UNION_TD):		/* int x set -> set */
      {
	TAGGED t1 = Pop;
	TAGGED t2 = Top;
	Prefetch;
	
	Top = fd_insert_into(w,t2,t1);
	Dispatch;
      }
      
    CaseX(FD_UNION_DT):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_insert_into(w,t2,t1);
	Dispatch;
      }

    CaseX(FD_UNION_DD):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_union(w,t1,t2);
	Dispatch;
      }

    CaseX(FD_INTER_TT):		/* int x int -> set */
      {
	TAGGED t1 = Pop;
	TAGGED t2 = Top;
	Prefetch;
	
	if (t1!=t2)
	  Top = EmptySet;
	else
	  Top = fd_interval(w,t1,t1);
	Dispatch;
      }

    CaseX(FD_INTER_TD):		/* int x set -> set */
      {
	TAGGED t1 = Pop;
	TAGGED t2 = Top;
	Prefetch;
	
	if (fd_member(t2,t1))
	  Top = fd_interval(w,t2,t2);
	else
	  Top = EmptySet;
	Dispatch;
      }
      
    CaseX(FD_INTER_DT):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (fd_member(t2,t1))
	  Top = fd_interval(w,t2,t2);
	else
	  Top = EmptySet;
	Dispatch;
      }

    CaseX(FD_INTER_DD):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_intersection(w,t1,t2);
	Dispatch;
      }

    CaseX(FD_SUBTRACT_TT):		/* int x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (t1==t2)
	  Top = EmptySet;
	else
	  Top = fd_interval(w,t1,t1);
	Dispatch;
      }

    CaseX(FD_SUBTRACT_TD):		/* int x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (fd_member(t1,t2))
	  Top = EmptySet;
	else
	  Top = fd_interval(w,t1,t1);
	Dispatch;
      }
      
    CaseX(FD_SUBTRACT_DT):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_delete(w,t1,t2);
	Dispatch;
      }

    CaseX(FD_SUBTRACT_DD):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_subtract(w,t1,t2);
	Dispatch;
      }

    CaseX(FD_CHECK_UNION):	/* set -> set (sort of) */
      {				/* (D1 ? (inf..sup) \/ D2) */
	TAGGED t1 = Pop;
	ix_byte *altcode = current->code + (*code++);
	
	if (t1!=EmptySet) {
	  Push(fd_interval(w,Inf, Sup));
	  code = altcode;
	}
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_CHECK):		/* set -> set (sort of) */
      {
	TAGGED t1 = Pop;
	ix_byte *altcode = current->code + (*code++);
	
	if (t1==EmptySet) {
	  Push(t1);
	  code = altcode;
	}
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_ADD):		/* int x int -> int */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (TagIsSmall(t1)) {
	  if (TagIsSmall(t2)) {
	    TADDCHK(t1,t2);
	  } else
	    t1 = t2;
	} else if (t1!=t2 && !TagIsSmall(t2))
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_SUB):		/* int x int -> int */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (TagIsSmall(t1)) {
	  if (TagIsSmall(t2)) {
	    TSUBCHK(t1,t2);
	  } else
	    t1 = (t2==Inf ? Sup : Inf);
	} else if (t1==t2)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_MULT_IMM):		/* int -> int */
      {
	TAGGED t2 = *code++;
	TAGGED t1 = Top;
	Prefetch;
	
	t1 = safe_mul(t1,t2);
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVD_IMM):		/* int -> int */
      {				/* floor of quotient */
	TAGGED t2 = *code++;
	TAGGED t1 = Top;
	Prefetch;

	t1 = safe_divd(t1,t2);
	if (t1==ERRORTAG)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVU_IMM):		/* int -> int */
      {				/* ceiling of quotient */
	TAGGED t2 = *code++;
	TAGGED t1 = Top;
	Prefetch;
	
	t1 = safe_divu(t1,t2);
	if (t1==ERRORTAG)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_MULT_VAL):		/* int -> int */
      {
	int i = *code++;
	TAGGED t2;
	TAGGED t1 = Top;
	Prefetch;
	
	t2 = args[i];
	DerefNonvar(t2);
	/* ensured by term expansion
	if (!PositiveBound(t2))
	  fprintf(stderr, "* FD: non-monotone multiply\n");
	*/
	t1 = safe_mul(t1,t2);
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVD_VAL):		/* int -> int */
      {				/* floor of quotient */
	int i = *code++;
	TAGGED t2;
	TAGGED t1 = Top;
	Prefetch;

	t2 = args[i];
	DerefNonvar(t2);
	/* ensured by term expansion
	if (!PositiveBound(t2))
	  fprintf(stderr, "* FD: non-monotone divide\n");
	*/
	t1 = safe_divd(t1,t2);
	if (t1==ERRORTAG)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVU_VAL):		/* int -> int */
      {				/* ceiling of quotient */
	int i = *code++;
	TAGGED t2;
	TAGGED t1 = Top;
	Prefetch;
	
	t2 = args[i];
	DerefNonvar(t2);
	/* ensured by term expansion
	if (!PositiveBound(t2))
	  fprintf(stderr, "* FD: non-monotone divide\n");
	*/
	t1 = safe_divu(t1,t2);
	if (t1==ERRORTAG)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_MULT_QVAL):		/* int -> int */
      {
	int i = *code++;
	TAGGED t1 = Top;
	Prefetch;
	
	t1 = safe_mul(t1,qval[i]);
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVD_QVAL):		/* int -> int */
      {				/* floor of quotient */
	int i = *code++;
	TAGGED t1 = Top;
	Prefetch;

	t1 = safe_divd(t1,qval[i]);
	if (t1==ERRORTAG)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVU_QVAL):		/* int -> int */
      {				/* ceiling of quotient */
	int i = *code++;
	TAGGED t1 = Top;
	Prefetch;
	
	t1 = safe_divu(t1,qval[i]);
	if (t1==ERRORTAG)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_MOD):		/* int x int -> int */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (Teqz(t2) || !TagIsSmall(t1))
	  goto abort;	/* inf mod any OR any mod 0 -> give up */
	else if (TagIsSmall(t2))
	  t1 = ((SP_WORD)(t1-TaggedZero)%(SP_WORD)(t2-TaggedZero)+TaggedZero);
	/*else
	  t1 = t1;*/
	Top = t1;
	Dispatch;
      }

    CaseX(FD_QVAL):		/* -> int */
      {
	TAGGED t1 = *code++;
	{
	  Prefetch;
	  Push(qval[t1]);
	  Dispatch;
	}
      }

    CaseX(FD_VAL):		/* -> int */
       				/* the domain is a singleton, and the arg. is
				   almost always ground, but after 
				   var-var-unifications, the variable binding
				   may not have propagated yet */
      {
	int i = *code++;
	Prefetch;
	
	gtemp = args[i];
	DerefNonvar(gtemp);
	Push(gtemp);
	Dispatch;
      }
    
    CaseX(FD_VAL_0):		/* -> int */
      {
	Prefetch;
	
	gtemp = args[0];
	DerefNonvar(gtemp);
	Push(gtemp);
	Dispatch;
      }

    CaseX(FD_VAL_1):		/* -> int */
      {
	Prefetch;
	
	gtemp = args[1];
	DerefNonvar(gtemp);
	Push(gtemp);
	Dispatch;
      }

    CaseX(FD_VAL_2):		/* -> int */
      {
	Prefetch;
	
	gtemp = args[2];
	DerefNonvar(gtemp);
	Push(gtemp);
	Dispatch;
      }

    CaseX(FD_DOM):		/* -> set */
      {
	gtemp = atts[*code++];
      fd_dom:
	{
	  TAGGED *pt1;
	  Prefetch;

	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3])
	    ground = 0;
	  Push(pt1[1]);
	  Dispatch;
	}
      }

    CaseX(FD_DOM_0):		/* -> set */
      gtemp = atts[0];
      goto fd_dom;

    CaseX(FD_DOM_1):		/* -> set */
      gtemp = atts[1];
      goto fd_dom;

    CaseX(FD_DOM_2):		/* -> set */
      gtemp = atts[2];
      goto fd_dom;

    CaseX(FD_SET_1):		/* -> set */
	{
	  Prefetch;

	  Push(atts[1]);
	  Dispatch;
	}


    CaseX(FD_MIN):		/* -> int */
      {
	gtemp = atts[*code++];
      fd_min:
	{
	  TAGGED *pt1;
	  Prefetch;
	  
	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3])
	    ground = 0;
	  Push(pt1[2]);
	  Dispatch;
	}
      }

    CaseX(FD_MIN_0):		/* -> int */
      gtemp = atts[0];
      goto fd_min;

    CaseX(FD_MIN_1):		/* -> int */
      gtemp = atts[1];
      goto fd_min;

    CaseX(FD_MIN_2):		/* -> int */
      gtemp = atts[2];
      goto fd_min;

    CaseX(FD_MAX):		/* -> int */
      {
	gtemp = atts[*code++];
      fd_max:
	{
	  TAGGED *pt1;
	  Prefetch;

	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3])
	    ground = 0;
	  Push(pt1[3]);
	  Dispatch;
	}
      }

    CaseX(FD_MAX_0):		/* -> int */
      gtemp = atts[0];
      goto fd_max;

    CaseX(FD_MAX_1):		/* -> int */
      gtemp = atts[1];
      goto fd_max;

    CaseX(FD_MAX_2):		/* -> int */
      gtemp = atts[2];
      goto fd_max;

    CaseX(FD_CARD):		/* -> int */
      {
	gtemp = atts[*code++];
      fd_card:
	{
	  TAGGED *pt1;
	  Prefetch;
	  
	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3])
	    ground = 0;
	  Push(pt1[4]);
	  Dispatch;
	}
      }

    CaseX(FD_CARD_0):		/* -> int */
      gtemp = atts[0];
      goto fd_card;

    CaseX(FD_CARD_1):		/* -> int */
      gtemp = atts[1];
      goto fd_card;

    CaseX(FD_CARD_2):		/* -> int */
      gtemp = atts[2];
      goto fd_card;

    CaseX(FD_CONST):		/* -> int or set */
      {
	Push(*code++);
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_UNIONOF):		/* set -> set x set */
      {				/* FD_UNIONOF LabelB */
	TAGGED t1 = Top;
	TAGGED t2;
	
	Top = EmptySet;
	code = current->code + (*code);
	if (t1!=EmptySet) {
	  if (fd_infinite(t1))
	    goto abort;
	  t2 = code[-2];	  
	  code = current->code + code[-1];
	  Push(t1);
	  fd_first_and_rest(w,t1,qval+t2,&top[-1]);
	}
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_UNIONOF_NEXT):	/* set x set x set -> set x set */
      {				/* FD_UNIONOF_NEXT Qvar LabelB */
	TAGGED t2 = Pop;
	TAGGED t1 = Pop;	/* remaining bits */
	ix_byte *altcode;
      
	Top = fd_union(w,Top,t2);	/* update union so far */
	t2 = *code++;
	altcode = current->code + (*code++);
	if (t1!=EmptySet) {
	  Push(t1);
	  fd_first_and_rest(w,t1,qval+t2,&top[-1]);
	  code = altcode;
	}
	{
	  Prefetch;
	  Dispatch;
	}
      }
	  
	  
    CaseX(FD_SWITCH):		/* -> set or label */
      {
	struct sw_on_key *sw =	/* get hash table */
	  (struct sw_on_key *)(*code++);
	ix_byte *altcode = current->code + (*code++); /* get join label */
	TAGGED t2 = incore_gethash(sw,Top)->value.arities;
	if (!t2) {
	  Top = EmptySet;	/* default value is empty set */
	  code = altcode;	/* branch to join */
	} else {
	    Top = (TAGGED)altcode; /* push join label */
	    code = current->code+GetSmall(t2); /* and branch to case */
	}
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_POPJ):		/* label x set -> set */
      {
	TAGGED t2 = Pop;
	code = (ix_byte *)Top;
	Top = t2;
	{
	  Prefetch;
	  Dispatch;
	}
      }
    
    CaseX(FD_ERROR):
      goto error;
    
    CaseX(FD_PRUNE_RANGE_OO):	/* inf..sup (weird) */
      goto ent_link_ov_loop;
      
    CaseX(FD_PRUNE_RANGE_OC):	/* int -> */
				/* inf..Max */
      max2 = Pop;
      min2 = Inf;
      goto prune_range;

    CaseX(FD_PRUNE_RANGE_CO):	/* int -> */
				/* Min..sup */
      max2 = Sup;
      min2 = Pop;
      goto prune_range;

    CaseX(FD_PRUNE_RANGE_CC):	/* int x int -> */
				/* Min..Max */
      max2 = Pop;		
      min2 = Pop; 
    prune_range:
      if (min2==max2 && !TagIsSmall(min2)) {
	fd.fd_overflow = TRUE;
	goto ov_loop;
      }
      {
	TAGGED filter =
#if 0	
	  (code[0] && (dvar.flags & DV_INTERVAL)) ? X(2) :
#endif
	  atom_nil;

	switch (dvar_fix_interval_t(&dvar,min2,max2)) {
	case -1:
	  goto fail;
	case 0:
	  goto ent_link_ov_loop;
	}
	dvar_export_ix(w,&dvar,filter);	/* GC */
	goto ent_link_ov_loop;
      }
	  
    CaseX(FD_PRUNE_TERM_COMPL):	/* int -> */
				/* unify LHS with complement */
      gtemp = Pop;
      switch (dvar_prune_interval_t(&dvar,gtemp,gtemp,w)) {
      case -1:
	goto fail;
      case 0:
	goto ent_link_ov_loop;
      }
      dvar_export_ix(w,&dvar,atom_nil);	/* GC */
      goto ent_link_ov_loop;

	  
    CaseX(FD_PRUNE_TERM):	/* int -> */
				/* unify LHS with value */
      {
	TAGGED filter =
#if 0
	  code[0] ? X(2) :
#endif
	  atom_nil;
	gtemp = Pop;
	switch (dvar_fix_interval_t(&dvar,gtemp,gtemp)) {
	case -1:
	  goto fail;
	case 0:
	  goto ent_link_ov_loop;
	}
	dvar_export_ix(w,&dvar,filter);	/* GC */
	goto ent_link_ov_loop;
      }


    CaseX(FD_PRUNE_COMPL):	/* set -> */
      gtemp = Pop;
      switch (dvar_prune_set(&dvar,gtemp,w)) {
      case -1:
	goto fail;
      case 0:
	goto ent_link_ov_loop;
      }
      dvar_export_ix(w,&dvar,atom_nil);	/* GC */
      goto ent_link_ov_loop;


    CaseX(FD_PRUNE):		/* set -> */
      {
	gtemp = Pop;
      prune:
	if (gtemp==EmptySet) {
	  goto fail;
	} else if (CTagToCdr(gtemp)==EmptySet) {
	  gtemp = CTagToCar(gtemp);
	  min2 = RangeMin(gtemp);
	  max2 = RangeMax(gtemp);
	  goto prune_range;
	}
	switch (dvar_fix_set(&dvar,gtemp,w)) {
	case -1:
	  goto fail;
	case 0:
	  goto ent_link_ov_loop;
	}
	dvar_export_ix(w,&dvar,(code[0] ? X(2) : atom_nil)); /* GC */
	goto ent_link_ov_loop;
      }

    CaseX(FD_PRUNE_PLUS):	/* set x set -> */
      {
	gtemp = fd_plus(w,Pop,Pop,dvar.min,dvar.max);
	goto prune;
      }

    CaseX(FD_PRUNE_MINUS):	/* set x set -> */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Pop;
	gtemp = fd_minus(w,t1,t2,dvar.min,dvar.max);
	goto prune;
      }

    CaseX(FD_TEST_RANGE_OO):	/* inf..sup (weird) */
      truth_value = MakeSmall(current->truth_value);
      goto ret_entailed;
      
    CaseX(FD_TEST_RANGE_OC):	/* int -> */
				/* inf..Max */
      max2 = Pop;
      min2 = Inf;
      goto test_range;

    CaseX(FD_TEST_RANGE_CO):	/* int -> */
				/* Min..sup */
      max2 = Sup;
      min2 = Pop;
      goto test_range;

    CaseX(FD_TEST_RANGE_CC):	/* int x int -> */
				/* Min..Max */
      max2 = Pop;		
      min2 = Pop; 
    test_range:
      truth_value = MakeSmall(current->truth_value);
      if (min2==max2) {
	if (!TagIsSmall(min2)) {
	  /* bug in 3.8.3
	     fd.fd_overflow = TRUE;
	     goto ov_loop;
	  */
	  goto ret_disentailed;
	}
	gtemp = min2;
	goto test_term;
      } else if (EmptyInterval(min2,max2)) {
	goto ret_disentailed;
      } else if (dvar.min==dvar.max) {
	if (InInterval(dvar.min,min2,max2))
	  goto ret_entailed;
	else
	  goto ret_disentailed;
      } else {		/* need precise test here! */
	switch (fd_compare_interval(dvar.set,min2,max2)) {
	case FDI_SUBSET:
	case FDI_EQUAL:
	  goto ret_entailed;
	case FDI_INTERSECT:
	case FDI_SUPERSET:
	  goto link_ov_loop;
	default: /* disjoint */
	  goto ret_disentailed;
	}
      }
	  
    CaseX(FD_TEST_TERM_COMPL):	/* int -> */
      truth_value = MakeSmall(current->truth_value)^IStep(1);
      gtemp = Pop;
      goto test_term;

    CaseX(FD_TEST_TERM):	/* int -> */
      truth_value = MakeSmall(current->truth_value);
      gtemp = Pop; 
    test_term:
      if (dvar.min==dvar.max) {
	if (dvar.min!=gtemp)
	  goto ret_disentailed;
	else
	  goto ret_entailed;
      } else {
	if (!fd_member(gtemp,dvar.set))
	  goto ret_disentailed;
	else
	  goto link_ov_loop;
      }

    CaseX(FD_TEST_COMPL):	/* set -> */
      truth_value = MakeSmall(current->truth_value);
      gtemp = Pop;
      if (gtemp==EmptySet) {
	goto ret_entailed;
      } else if (dvar.min==dvar.max) {
	if (!fd_member(dvar.min,gtemp))
	  goto ret_entailed;
	else
	  goto ret_disentailed;
      } else {
	switch (fd_compare(dvar.set,gtemp)) {
	case FDI_DISJOINT:
	  goto ret_entailed;
	case FDI_INTERSECT:
	case FDI_SUPERSET:
	  goto link_ov_loop;
	default: /* subset */
	  goto ret_disentailed;
	}
      }

    CaseX(FD_TEST):		/* set -> */
      truth_value = MakeSmall(current->truth_value);
      gtemp = Pop;
      if (gtemp==EmptySet) {
	goto ret_disentailed;
      } else if (dvar.min==dvar.max) {
	if (fd_member(dvar.min,gtemp))
	  goto ret_entailed;
	else
	  goto ret_disentailed;
      } else {
	switch (fd_compare(dvar.set,gtemp)) {
	case FDI_SUBSET:
	case FDI_EQUAL:
	  goto ret_entailed;
	case FDI_INTERSECT:
	case FDI_SUPERSET:
	  goto link_ov_loop;
	default: /* disjoint */
	  goto ret_disentailed;
	}
      }
    }
  }

 ret_disentailed:
  truth_value ^= IStep(1);
  if (!ground)
    goto link_ov_loop;
 ret_entailed:
  {
    TAGGED zero_one;
    LetShadowGlobal;
      
    fd.entailments++;
    BindHVA(X(2),truth_value);
				/* propagate to the zero_one variable */
    zero_one = CTagToArg(X(0),5);
    DerefSwitch(zero_one,goto ret_entailed_2;);
    if (0) {
    ret_entailed_2:
#if 0
      dvar_init_ix(&dvar,get_attributes(zero_one,fd.fd_module),zero_one);
#else  /* 20061009 */
      dvar_init_ix(&dvar,CTagToArg(X(0),7),zero_one);
#endif
      switch (dvar_fix_value_t(&dvar,truth_value)) {
      case -1:
	goto fail;
      case 0:
	goto link_ov_loop;
      }
      dvar_export_ix(w,&dvar,atom_nil);	/* GC */
    }
  }
  goto link_ov_loop;

 abort:
  top=&X(EVAL_ARITY);		/* reset stack pointer */
 ent_link_ov_loop:
  if (ground) {
    LetShadowGlobal;
    fd.entailments++;
    BindHVA(X(2),truth_value);
  }
 link_ov_loop:
  gtemp = RefMutable(X(1));
  if (gtemp&IStep(4))		/* STATUS: not idempotent */
    gtemp &= ~IStep(8);		/* STATUS: not current */
  else
    gtemp &= ~IStep(9);	       /* STATUS: not current, not enqueued */
  if (!(gtemp&IStep(2))) {	/* STATUS: linked */
    gtemp |= IStep(2);		/* STATUS: linked */
    link_indexical(w,current);
  }
  update_mutable(w,gtemp,X(1));
 ov_loop:
  if (!fd_check_overflow(CTagToArg(X(0),2)))
    goto error;
  goto restart;
 fail:
  fd.failures++;
 error:
  SP_fail();
  return -1;
}


/* '$fd_in_interval'(+Var, +Min, +Max, +Init) */
/* X(1) temporarily holds Var's FD att */
/* implies $fd_begin if Init=1 */
void SPCDECL
prolog_fd_in_interval MAGIC (HIDDEN_PROTO
			     SP_term_ref Var,
			     SP_term_ref Min,
			     SP_term_ref Max,
			     long init)
{
  WAMENV;
  TAGGED var, min, max;
  struct dvar dvar;

  if (init)
    SP_MANGLE(prolog_fd_begin)(HIDDEN_ARG);
  var = RefTerm(Var);
  min = RefTerm(Min);
  max = RefTerm(Max);
  DerefNonvar(min);
  DerefNonvar(max);
  DerefSwitch(var,goto prune;);
  if (TagIsSmall(var)) {
    if (!InInterval(var,min,max))
      goto fail;
    else
      return;
  }
prune:
  fd_sync(Arg);
  X(1) = check_argument(w,var,Inf,Sup,Sup);
  if (!X(1))
    goto fail;
  w->numstack_end = NULL;
  dvar_init_ix(&dvar,X(1),var);
  switch (dvar_fix_interval_t(&dvar,min,max)) {
  case -1:
    goto fail;
  case 0:
    goto ret;
  }
  dvar_export_ix(w,&dvar,atom_nil); /* GC */
 ret:
  return;
fail:
  fd.failures++;
  SP_fail();
}

/* '$fd_in_set'(+Var, +Domain, +Init) */
/* X(1) temporarily holds Var's FD att */
/* implies $fd_begin if Init=1 */
void SPCDECL
prolog_fd_in_set MAGIC (HIDDEN_PROTO
			SP_term_ref Var,
			SP_term_ref Domain,
			long init)
{
  WAMENV;
  TAGGED var, domain;
  struct dvar dvar;
  
  var = RefTerm(Var);
  domain = RefTerm(Domain);
  DerefNonvar(domain);
  if (domain==EmptySet)
    goto fail;
  if (init)
    SP_MANGLE(prolog_fd_begin)(HIDDEN_ARG);
  DerefSwitch(var,goto prune;);
  if (TagIsSmall(var)) {
    if (!fd_member(var,domain))
      goto fail;
    else
      return;
  }
 prune:
  fd_sync(Arg);
  X(1) = check_argument(w,var,Inf,Sup,Sup);
  if (!X(1))
    goto fail;
  w->numstack_end = NULL;
  dvar_init_ix(&dvar,X(1),var);
  switch (dvar_fix_set(&dvar,domain,w)) {
  case -1:
    goto fail;
  case 0:
    goto ret;
  }
  dvar_export_ix(w,&dvar,atom_nil); /* GC */
 ret:
  return;
fail:
  fd.failures++;
  SP_fail();
}



/*** support for save/restore ***/

static int find_fdset(TAGGED fdset, TAGGED *lits)
{
  int i=0;
  TAGGED key = (TAGGED)(TagToLST(fdset)-1)+STRUCT_TAG;

  while (lits[i]!=key)
    i++;

  return i;
}


static int find_htab(struct sw_on_key *htab,
		     TAGGED *lits)
{
  int i=0;
  TAGGED key = AddrToTag(htab)+LIST_TAG;

  while (lits[i]!=key)
    i++;

  return i;
}


/* Relocate FDset refs and hashcode refs in the bytecode */
static void relocate_bytecode(ix_byte *code,
			      int n,
			      TAGGED *lits,
			      SP_BOOL mkrel) /* TRUE if make relative */
{
  ix_byte *code_end = code+n;

  code++;			/* flag for filter etc. */
  while (code<code_end)
    switch (*code++) {
    case FD_QVAL:
    case FD_MULT_IMM:
    case FD_DIVD_IMM:
    case FD_DIVU_IMM:
    case FD_MOD:
    case FD_VAL:
    case FD_DOM:
    case FD_MIN:
    case FD_MAX:
    case FD_MULT_VAL:
    case FD_DIVD_VAL:
    case FD_DIVU_VAL:
    case FD_CARD:
    case FD_CHECK_UNION:
    case FD_CHECK:
    case FD_UNIONOF:
    case FD_MULT_QVAL:
    case FD_DIVD_QVAL:
    case FD_DIVU_QVAL:
      code++;
      break;

    case FD_CONST:
      if (!IsAtomic(code[0])) {
	if (mkrel)
	  code[0] = find_fdset(code[0],lits);
	else
	  code[0] = MakeList(TagToAddr(lits[code[0]]-STRUCT_TAG)+1);
      }	  
      code++;
      break;
	
    case FD_UNIONOF_NEXT:
      code+=2;
      break;

    case FD_SWITCH:
      if (mkrel)
	code[0] = find_htab((struct sw_on_key *)code[0],lits);
      else
	code[0] = lits[code[0]]-LIST_TAG;
      code+=2;
      break;
    }
}



static void
fd_save_literals(TAGGED *lits,
		 int n,
		 fwrite_fun *fwr, /* fwrite passed from the emulator */
		 struct saverest_info *sr_info)
{
  int i;
  int saved = 0;

  /* First, save the skeletal structure. */
  for (i=0; i<n; i++) {
    TAGGED lit = lits[i];

    switch (TagOf(lit)) {
    case LIST_TAG:	/* hash table */
      if (saved<i)
	(*fwr)(lits+saved, (i-saved)*sizeof(TAGGED), sr_info);
      {
	struct sw_on_key *htab = (struct sw_on_key *)TermToPointer(LIST_INT(lit));
	TAGGED aux = sizeof(struct sw_on_key)+
	  (SwitchSize(htab)-ANY)*sizeof(struct sw_on_key_node) + LIST_TAG;

	(*fwr)(&aux, sizeof(TAGGED), sr_info);
      }
      saved = i+1;
      break;
    case STRUCT_TAG:	/* FD set */
      if (saved<i)
	(*fwr)(lits+saved, (i-saved)*sizeof(TAGGED), sr_info);
      {
	TAGGED aux = LargeArity(CTagToSTR(lit))*sizeof(TAGGED) + STRUCT_TAG;
	  
	(*fwr)(&aux, sizeof(TAGGED), sr_info);
      }
      saved = i+1;
      break;
    }
  }
  if (saved<i)
    (*fwr)(lits+saved, (i-saved)*sizeof(TAGGED), sr_info);
  
  /* Then, save the residues. */
  for (i=0; i<n; i++) {
    TAGGED lit = lits[i];
      
    switch (TagOf(lit)) {
    case LIST_TAG:	/* hash table */
      {
	struct sw_on_key *htab = (struct sw_on_key *)TermToPointer(LIST_INT(lit));
	
	(*fwr)(htab,
	       sizeof(struct sw_on_key)+
	       (SwitchSize(htab)-ANY)*sizeof(struct sw_on_key_node), sr_info);
      }
      break;
    case STRUCT_TAG:	/* FD set */
      (*fwr)(TagToSTR(lit), LargeArity(CTagToSTR(lit))*sizeof(TAGGED), sr_info);
      break;
    }
  }
}


void SPCDECL fd_save_hook(
     SP_HOOKS_COOKIE_PROTO_COMMA
     struct saverest_record *record,
     struct definition *pred,
     fwrite_fun *fwr,		/* fwrite passed from the emulator */
     struct saverest_info *sr_info
  )
{
  int i;
  struct fd_info *fdinfo = pred->proc.code.fdinfo;

  SP_HOOKS_COOKIE_USE;
  for (i=0; i<4; i++) {
    struct indexical_info *info = (struct indexical_info *)fdinfo->info[i];
      
    while (info) {
      record->body.fd_data.pred = pred;
      record->body.fd_data.info =
	(info->pruned<<10) + (info->length_of_linkage<<2) +
	(info->checking<<1) + (info->truth_value);
      record->body.fd_data.length_of_bytecode = info->length_of_bytecode;
      record->body.fd_data.length_of_literals = info->length_of_literals;
      (*fwr)(record, sizeof(*record), sr_info);
      if (info->linkage)
	(*fwr)(info->linkage,
	       info->length_of_linkage*sizeof(ix_byte), sr_info);
      if (info->literals)
	fd_save_literals(info->literals,
			 info->length_of_literals,
			 fwr, sr_info);
      relocate_bytecode(info->code, info->length_of_bytecode, info->literals, TRUE);
      (*fwr)(info->code, info->length_of_bytecode*sizeof(ix_byte), sr_info);
      relocate_bytecode(info->code, info->length_of_bytecode, info->literals, FALSE);
      info = info->next;
    }
  }
}

void SPCDECL fd_restore_hook(
     SP_HOOKS_COOKIE_PROTO_COMMA     /* [PM] special, always(? not true/PM 3.11.2) passed by call_fd_restorer */
     struct saverest_record *record,
     fread_longs_fun *frd,		/* fread_longs passed from the emulator */
     struct saverest_info *sr_info
  )
{
  int i, type;
  struct definition *pred = record->body.fd_data.pred; /* assume relocated */
  struct indexical_info **tail;
  struct indexical_info *info = 
    (struct indexical_info *)
    sp_checkalloc(sizeof(struct indexical_info), FALSE);

  SP_HOOKS_COOKIE_USE;

#if DBG                         /* [PM] 3.9.2b1 */
  info->identifier = fd.generation;
#endif

  if (pred->proc.properties.prop.predtyp!=FD_CONSTRAINT)
    init_fd_constraint(pred);

#if !SP_FD_GLOBAL_DESTRUCTOR   /* [PM] 3.9.2b1 */
  {
#if 1                       /* [PM] 3.9b4 */
    info->destructor_fun = fd.fd_destructor_fun;
#else
    info->destructor = fd.fd_destructor;
#endif
    FD_STORE_SPENV(info->spenv);
  }
#endif/* !SP_FD_GLOBAL_DESTRUCTOR */

  info->pred = pred;
  info->next = NULL;
  info->pruned = record->body.fd_data.info>>10;
  info->checking = record->body.fd_data.info>>1;
  info->truth_value = record->body.fd_data.info;
  type = record->body.fd_data.info&3;
  tail = (struct indexical_info **)&pred->proc.code.fdinfo->info[type]; 
  while (*tail)
    tail = &(*tail)->next;
  *tail = info;
  info->length_of_linkage = (record->body.fd_data.info>>2) & 0xff;
  info->length_of_bytecode = record->body.fd_data.length_of_bytecode;
  info->length_of_literals = record->body.fd_data.length_of_literals;
  if (info->length_of_linkage) {
    int size = info->length_of_linkage*sizeof(ix_byte);
      
    info->linkage = (ix_byte *)sp_checkalloc(size, FALSE);
    (*frd)(info->linkage, size, sr_info);
  } else
    info->linkage = NULL;
  if (info->length_of_literals) {
    int size = info->length_of_literals*sizeof(TAGGED);
      
    info->literals = (TAGGED *)sp_checkalloc(size, FALSE);
    (*frd)(info->literals, size, sr_info);
    /* Restore the residues. */
    for (i=0; i<info->length_of_literals; i++) {
      TAGGED lit = info->literals[i];
	  
      switch (TagOf(lit)) {
      case LIST_TAG:	/* hash table */
	{
	  int size = (lit-LIST_TAG-sizeof(struct sw_on_key))/sizeof(struct sw_on_key_node) + ANY;
	  struct sw_on_key *htab =
	    new_switch_on_key(size,NULL);
	      
	  (*frd)(htab, lit-LIST_TAG, sr_info);
	  info->literals[i] = AddrToTag(htab)+LIST_TAG;
	}
	break;
      case STRUCT_TAG:	/* FD set */
	{
	  TAGGED *fdset = sp_checkalloc(lit-STRUCT_TAG, FALSE);
	  int j;
	  int size = lit>>LogSizeOfWord;
	      
	  (*frd)(fdset, lit-STRUCT_TAG, sr_info);
	  info->literals[i] = AddrToTag(fdset)+STRUCT_TAG;
	  for (j=1; j<size; j+=4) /* relocate the ranges */
	    fdset[j]   = MakeList(fdset+j+2);
	  for (j=1; j<size-4; j+=4)	/* relocate the tails */
	    fdset[j+1] = MakeList(fdset+j+4);
	}
	break;
      }
    }
  } else
    info->literals = NULL;
  {
    int size = info->length_of_bytecode*sizeof(ix_byte);
    
    info->code = (TAGGED *)sp_checkalloc(size, FALSE);
    (*frd)(info->code, size, sr_info);
    relocate_bytecode(info->code, info->length_of_bytecode, info->literals,
		      FALSE);
  }
}
