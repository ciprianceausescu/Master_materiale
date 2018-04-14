/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#if MULTI_SP_AWARE
#define gc_suspensions(A1,A2,A3,A4) gc_suspensions(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_enqueue_list_gc(A1,A2,A3,A4) fd_enqueue_list_gc(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_enqueue_list(A1,A2,A3,A4) fd_enqueue_list(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_enqueue_val(A1,A2,A3,A4,A5) fd_enqueue_val(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define qsort_asc_long1swap(A1,A2,A3) qsort_asc_long1swap(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_long1med3(A1,A2,A3) qsort_asc_long1med3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_long1(A1,A2) qsort_asc_long1(HIDDEN_ARG, A1,A2)
#endif /* MULTI_SP_AWARE */

/* Returns FD attribute for a dvar or integer.
   Heap consumption bounds:
   existing dvar - 0
   integer - INT_ATTRIBUTE_SIZE
   non-domain variable - FD_ATTRIBUTE_SIZE + ARITYLIMIT + 4
*/
TAGGED check_argument MAGIC (HIDDEN_PROTO
			     Argdecl,
			     TAGGED argument,
			     TAGGED min, TAGGED max, TAGGED size)
{
  TAGGED *h, t1, attr;
  int j;

  DerefSwitch(argument,;);
  switch (TagOf(argument)) {
  case REF_TAG:
    if (GVarIsCVA(argument)) {
      if ((t1=get_attributes(argument,fd.fd_module)))
	return t1;
    }
    h = w->global_top;
    attr = MakeStructure(h);
    for (j=0; j<FD_ATTRIBUTE_SIZE; j++) {
      t1 = fd_attribute[j];
      if (!TagIsAtomic(t1))
	t1 += TagREF(w->global_top);
      *h++ = t1;
    }
    w->global_top[FD_ATTR_MIN_OFFSET] = min;
    w->global_top[FD_ATTR_MAX_OFFSET] = max;
    w->global_top[FD_ATTR_SIZE_OFFSET] = size;
    w->global_top = h;
    put_attributes(argument,attr,fd.fd_module);
    return attr;
  case CONST_TAG:
    if (TagIsATM(argument))
      return ERRORTAG;
    h = w->global_top;
    attr = MakeStructure(h);
    *h++ = fd_attribute[0];
    *h++ = fd_attribute[1];
    *h++ = fd_attribute[2];
    *h++ = attr + WD(5);
    *h++ = TaggedZero;
    /* 5*/*h++ = functor_Dmutable;
    *h++ = attr + WD(8);
    *h++ = TaggedZero;
    /* 8*/*h++ = fd.functor_dom4;
    *h++ = MakeList(w->global_top+13);
    /*10*/*h++ = argument;
    *h++ = argument;
    *h++ = TaggedOne;
    /*13*/*h++ = MakeList(w->global_top+10);
    *h++ = atom_nil;
    w->global_top = h;
    return attr;
  default:
    return ERRORTAG;
  }
}


/* '$fd_arg_attribute'(+Var, +Finitep, -Attr)
*/
void SPCDECL
prolog_fd_arg_attribute MAGIC (HIDDEN_PROTO
			       SP_term_ref Var,
			       long finitep,
			       SP_term_ref Attr)
{
  WAMENV;
  TAGGED attr, domain;

  attr = check_argument(w,RefTerm(Var),Inf,Sup,Sup);
  if (attr && finitep) {
    DerefAttribute(domain,attr); /* dom/4 term */
    if (!AreSmall(DomainMin(domain),DomainMax(domain)))
      attr = ERRORTAG;
  }
  if (attr)
    RefTerm(Attr) = attr;
  else
    SP_fail();  
}

/* '$fd_dvar_list'(+List, +Finitep)
*/
void SPCDECL
prolog_fd_dvar_list MAGIC (HIDDEN_PROTO
			   SP_term_ref List,
			   long finitep)
{
  WAMENV;
  TAGGED domain;

  /* [MC] SPRM 8731 - don't over-estimate memory need */

  DEREF(X(0),RefTerm(List));
  while (TagIsLST(X(0))) {
    DerefCar(X(1),X(0));
    DerefCdr(X(0),X(0));
    if (!TagIsSmall(X(1))) {
      RequireHeap(FD_ATTRIBUTE_SIZE + ARITYLIMIT + 4,2);
      X(1) = check_argument(w,X(1),Inf,Sup,Sup);
      if (!X(1))
	goto fail;
      if (finitep) {
	DerefAttribute(domain,X(1)); /* dom/4 term */
	if (!AreSmall(DomainMin(domain),DomainMax(domain)))
	  goto fail;
      }
    }
  }
  if (X(0)!=atom_nil)		/* type error? */
    goto fail;
  return;
 fail:
  SP_fail();
}

/* '$fd_coref'(+List)
   Succeeds if List contains F1(X) and F2(X) for some X.
*/
void SPCDECL
prolog_fd_coref MAGIC (HIDDEN_PROTO SP_term_ref List)
{
  WAMENV;
  TAGGED list, var;

  DEREF(list,RefTerm(List));
  while (TagIsLST(list)) {
    DerefCar(var,list);
    DerefCdr(list,list);
    var = CTagToArg(var,1);
    DerefHeapSwitch(var,goto bind;);
    if (var==atom_nil)		/* F(Var), 2nd occurrence */
      return;
    else
      continue;			/* F(Integer) */
  bind:				/* F(Var), 1st occurrence */
    TrailPushCheck(var);
    CTagToPointer(var) = atom_nil;
  }
  SP_fail();			/* No coreference found. */
}


SP_BOOL fd_member(TAGGED x, TAGGED set)
{
  TAGGED range;

  while (set!=EmptySet) {
    range = CTagToCar(set); 
    set = CTagToCdr(set);   
    switch (val_vs_range(x,range)) {
    case CMP_INSIDE:
      return TRUE;
    case CMP_BEFORE:
      return FALSE;
    }
  }
  return FALSE;
}



SP_BOOL fd_check_overflow MAGIC (HIDDEN_PROTO
			      TAGGED goal)
{
  if (!fd.fd_overflow) {
    return TRUE;
  } else if (!fd.overflowing) {
    return FALSE;
  } else {
    SP_term_ref goalref = SP_new_term_ref();
    WAMENV;

    RefTerm(goalref) = goal;
    SP_query(fd.overflow_action1,goalref);
    if (SP_exception_term(goalref))
      SP_raise_exception(goalref);
    SP_reset_term_refs(goalref);
    return FALSE;
  }
}

/* Build a copy of old on the heap.
   Precondition: old is built entirely on the numstack.
*/
TAGGED fd_globalize MAGIC (HIDDEN_PROTO
			   Argdecl,
			   TAGGED old, long req, int ar)
{
  TAGGED d1, r1, b, e, *h, value;
  TAGGED *valuep = &value;

  d1 = old;
  while (d1!=EmptySet) {
    req += 4;
    d1 = CTagToCdr(d1);
  }
  RequireHeap(req,ar);
  h = w->global_top;
  d1 = old;
  while (d1!=EmptySet) {
    r1 = CTagToCar(d1);
    d1 = CTagToCdr(d1);
    b = RangeMin(r1);
    e = RangeMax(r1);
    *valuep = MakeList(h);
    valuep = h+1;
    h[0] = MakeList(h+2);
    h[2] = b;
    h[3] = e;
    h += 4;
    if (b==Sup || e==Inf)
      fd.fd_overflow = TRUE;
  }
  *valuep = atom_nil;
  w->global_top = h;
  return value;
}

/* Build a copy of old on the heap.
   Precondition: old has NOT been fd_localized.
*/
TAGGED fd_globalize_unsafe MAGIC (HIDDEN_PROTO
				  Argdecl,
				  TAGGED old, long pad, int ar)
{
  TAGGED d1, r1, b, e, *h, value;
  TAGGED *valuep = &value;

  while (TRUE) {
    long req = pad;
    d1 = old;
    while (d1!=EmptySet && !OnHeap(d1)) {
      r1 = CTagToCar(d1);
      d1 = CTagToCdr(d1);
      req += (OnHeap(r1) ? 2 : 4);
    }
    if (HeapDifference(w->global_top,w->stack_start) >= CONTPAD+req)
      break;
    old = fd_localize(w,old);
    call_overflow(w,CONTPAD+req, ar);
  }
  h = w->global_top;
  d1 = old;
  while (d1!=EmptySet && !OnHeap(d1)) {
    r1 = CTagToCar(d1);
    d1 = CTagToCdr(d1);
    *valuep = MakeList(h);
    valuep = h+1;
    h += 2;
    if (OnHeap(r1)) {
      h[-2] = r1;
    } else {
      b = RangeMin(r1);
      e = RangeMax(r1);
      h[-2] = MakeList(h);
      h[0] = b;
      h[1] = e;
      h += 2;
      if (b==Sup || e==Inf)
	fd.fd_overflow = TRUE;
    }
  }
  *valuep = d1;
  w->global_top = h;
  return value;
}



void update_mutable MAGIC (HIDDEN_PROTO
			   struct worker *w,
			   TAGGED new_value, TAGGED mutable)
{
  TAGGED *h, *arg, value;
  LetShadowGlobal;
  
  value=RefMutable(mutable);
  while (TagIsSTR(value) && TagToHeadfunctor(value)==functor_Dmutable) {
    mutable=value; value=RefMutable(value);
  }							
  arg = TagToArg(mutable,0);
  if (arg[2] < TrailToInt(w->node->trail_top) &&
      arg < GLOBAL_UNCOND) {
    h = w->trail_top; /* trail mutable if need be */
    *h++ = mutable;
    *h++ = arg[1];
    *h++ = arg[2];
    arg[1] = new_value;	/* must come BEFORE choice_overflow */
    arg[2] = TrailToInt(w->trail_top);
    w->trail_top = h;
    if (ChoiceYounger(ChoiceOffset(w->node,CHOICEPAD),w->trail_top))
      choice_overflow(w,CHOICEPAD);
  } else
    arg[1] = new_value;
}

void fd_link MAGIC (HIDDEN_PROTO
		    struct worker *w,
		    TAGGED var,
		    long key,
		    TAGGED item)
{
  TAGGED mutable;
  TAGGED *h;
  TAGGED *s=NULL;
  TAGGED queues;
  long mask;
  LetShadowGlobal;

  AttrToSuspM(check_argument(w,var,Inf,Sup,Sup),mutable);
  queues = RefMutable(mutable);
  h = w->global_top;
  
  if (TagToSTR(queues) < GLOBAL_UNCOND)/* can't smash */ {
    int i;
	  
    for (s = TagToSTR(queues), i=0; i<8; i++)
      *h++ = *s++;
    queues = MakeStructure(h-8);
    update_mutable(w,queues,mutable);
  }
  if (key==fd.functor_dom) {
    mask = IStep(MASK_DOM);
    s = TagToArg(queues,3);
  } else if (key==fd.functor_min) {
    mask = IStep(MASK_MIN);
    s = TagToArg(queues,4);
  } else if (key==fd.functor_max) {
    mask = IStep(MASK_MAX);
    s = TagToArg(queues,5);
  } else if (key==fd.functor_minmax) {
     mask = IStep(MASK_MINMAX);
    s = TagToArg(queues,6);
  } else {
    /* patch item for globals suspending on val */
    if (TagToHeadfunctor(item)!=functor_minus) {
      HeapPush(h,functor_minus);
      HeapPush(h,atom_nil);
      HeapPush(h,item);
      item = MakeStructure(h-3);
    }
    mask = IStep(MASK_VAL);
    s = TagToArg(queues,7);
  }
  CTagToArg(queues,1) += IStep(1);
  CTagToArg(queues,2) |= mask;
  HeapPush(h,item);
  HeapPush(h,*s);
  *s = MakeList(h-2);
  w->global_top = h;
}


/*** support for queues of indexicals and globals ***/

#define CLPFD_MUTABLE RefTerm(5) /* xref Emulator/sicstus.c */

void fd_sync MAGIC (HIDDEN_PROTO Argdecl)
{
  TAGGED ptr = RefMutable(CLPFD_MUTABLE)
#if SICSTUS_MAJOR_VERSION < 4
    & -4L /* clear GC bits */
#endif
    ;
  struct propagator *current = (struct propagator *)TermToPointer(ptr);
  struct propagator *cur = fd.current_propagator;

  while (cur && cur!=current) {
    int i;
    for (i=0; i<FD_NB_QUEUES; i++) {
      cur->queue[i].first = 0;
      cur->queue[i].last = 0;
    }
    fd.current_propagator = cur->next;
    cur->next = fd.free_propagators;
#if DBG > 1
    cur->chpt = ChoiceToInt(w->node);
#endif
    fd.free_propagators = cur;
    cur = fd.current_propagator;
  }
  /* [MC 3.11.1] see SPRM 7785.  Shouldn't happen, but does.  Exact cause
     not understood.  Hypothesis: a combination of freeze and
     backtracking causes contents of CLPFD_MUTABLE to be outside the
     fd.current_propagator chain.  Rescue op: pop fd.free_propagators
     stack until found. */

  while (cur!=current) {
    cur = fd.free_propagators;
    fd.free_propagators = cur->next;
    cur->next = fd.current_propagator;
    fd.current_propagator = cur;
  }
}


/* '$fd_begin'
*/
void SPCDECL
prolog_fd_begin MAGIC (HIDDEN_PROTO_VOID)
{
  WAMENV;
  struct propagator *cur;
  int i;
  
  fd_sync(Arg);
  if (fd.free_propagators) {
    cur = fd.free_propagators;
    fd.free_propagators = cur->next;
  } else {
    cur = (struct propagator *)sp_checkalloc(sizeof(struct propagator), TRUE);
    for (i=0; i<FD_NB_QUEUES; i++) {
      cur->queue[i].first = 0;
      cur->queue[i].last = 0;
      cur->queue[i].size = 4;
      cur->queue[i].items = sp_checkalloc(4*sizeof(TAGGED), TRUE);
    }
    cur->hint = i;
  }
  cur->next = fd.current_propagator;
  fd.current_propagator = cur;
  update_mutable(w,PointerToTerm(cur),CLPFD_MUTABLE);
}


/* '$fd_end'
*/
void fd_end MAGIC (HIDDEN_PROTO Argdecl)
{
  struct propagator *cur;

  fd_sync(Arg);
  cur = fd.current_propagator;
  fd.current_propagator = cur->next;
  cur->next = fd.free_propagators;
  fd.free_propagators = cur;
  update_mutable(w,PointerToTerm(fd.current_propagator),CLPFD_MUTABLE);
}


void
fd_dealloc MAGIC (HIDDEN_PROTO_VOID)
{
  WAMENV;
  struct propagator *cur;
  
  RefMutable(CLPFD_MUTABLE) = TaggedZero;
  fd_sync(Arg);
  while ((cur=fd.free_propagators)) {
    int i;
    for (i=0; i<FD_NB_QUEUES; i++)
      sp_checkdealloc(cur->queue[i].items, cur->queue[i].size*sizeof(TAGGED), TRUE);
    fd.free_propagators = cur->next;
    sp_checkdealloc((TAGGED *)cur, sizeof(struct propagator), TRUE);
  }
}  


/* support function for stack shifting and gc */

void SPCDECL
fd_manager_hook(SP_HOOKS_COOKIE_PROTO_COMMA Argdecl,int msg,TAGGED *ptr)
{
  long reloc;
  struct propagator *cur;
  TAGGED t, *p;
  int i, j;
  
  SP_HOOKS_COOKIE_USE;
  switch (msg) {
  case 1:			/* stack shifter */
    reloc = (long)ptr;
    fd_sync(Arg);
    cur = fd.current_propagator;
    while (cur) {
      for (j=cur->hint; j<FD_NB_QUEUES; j++) {
	int last = cur->queue[j].last;
	for (i=cur->queue[j].first; i!=last; ) {
	  cur->queue[j].items[i] += reloc;
	  i++;
	  if (i==cur->queue[j].size)
	    i = 0;
	}
      }
      cur = cur->next;
    }
    break;
  case 2:			/* gc, mark phase */
    fd_sync(Arg);
    cur = fd.current_propagator;
    while (cur) {
      for (j=0; j<FD_NB_QUEUES; j++) {
	int last = cur->queue[j].last;
	for (i=cur->queue[j].first; i!=last; ) {
	  markTerm(cur->queue[j].items[i]);
	  i++;
	  if (i==cur->queue[j].size)
	    i = 0;
	}
      }
      cur = cur->next;
    }
    break;
  case 3:			/* gc, sweep phase */
    cur = fd.current_propagator;
    while (cur) {
      for (j=0; j<FD_NB_QUEUES; j++) {
	int last = cur->queue[j].last;
	for (i=cur->queue[j].first; i!=last; ) {
	  t = cur->queue[j].items[i];
	  p = TagToPointer(t);
	  if (OffHeaptop(p,ptr))
	    intoRelocationChain(w,p,&cur->queue[j].items[i]);
	  i++;
	  if (i==cur->queue[j].size)
	    i = 0;
	}
      }
      cur = cur->next;
    }
    break;
  case 4:			/* gc, proofreading */
    fd_sync(Arg);
    cur = fd.current_propagator;
    while (cur) {
      for (j=0; j<FD_NB_QUEUES; j++) {
	int last = cur->queue[j].last;
	for (i=cur->queue[j].first; i!=last; ) {
	  (*(void(*)(TAGGED *))ptr)(&cur->queue[j].items[i]);
	  i++;
	  if (i==cur->queue[j].size)
	    i = 0;
	}
      }
      cur = cur->next;
    }
    break;
  }
}



struct daemon_frame {
  void (SPCDECL *destructor)(void *);
  void (SPCDECL *daemon)(HIDDEN_PROTO
			 Argtype,void *,SP_globref,TAGGED); /* (w,handle,attr_ref,global) */
};

/* where=0x0/1 -- prepend/append to indexical queue
         0x2/3 -- prepend/append to 'val' global queue
         0x4/5 -- prepend/append to 'minmax' global queue
         0x6/7 -- prepend/append to 'dom' global queue
         0x8/9 -- prepend/append to daemon queue
*/
void fd_enqueue_general MAGIC (HIDDEN_PROTO
			       Argdecl,
			       TAGGED item, int where)
{
  int ix = where>>1;
  struct propagator *cur = fd.current_propagator;
  struct prop_queue *q = &cur->queue[ix];
  int i, pop, size;
  
  if (ix<4) {
    TAGGED mutable = CTagToArg(item,3);
    
    update_mutable(w,RefMutable(mutable)|IStep(1),mutable); /* STATUS: enqueued */
  }
  
#if DBG > 1
  if (cur->chpt != ChoiceToInt(w->node)) {	/* [PRM 8883] */
    printf("!!! propagator's chpt = %p, WAM's chpt = %p, item = %p\n", ChoiceFromInt(cur->chpt), w->node, (long *)item);
  }
#endif
  /* room for item? */
  pop = q->last-q->first;
  size = q->size;
  if (pop<0)
    pop += size;
  if (pop+1 >= size) { /* grow */
    q->items = sp_checkrealloc(q->items,
			       size*sizeof(TAGGED),
			       size*sizeof(TAGGED)<<1, TRUE);
    q->size += size;
    if (q->first>q->last) {
      for (i=size-1; i>=q->first; i--)
	q->items[i+size] = q->items[i];
      q->first += size;
    }
    size += size;
  }
  if (where & 0x1) {
    int pos = q->last;
    
    q->items[pos++] = item;
    if (pos==size)
      pos = 0;
    q->last = pos;
  } else {
    int pos = q->first;
    
    if (pos==0)
      pos = size;
    q->items[--pos] = item;
    q->first = pos;
  }
  if (cur->hint > ix)
    cur->hint = ix;
}

/* enqueue if not already in the queue */
/* where is as above */
void fd_enqueue_global MAGIC (HIDDEN_PROTO
			      Argdecl,
			      TAGGED item, int where)
{
  TAGGED t1 = CTagToArg(item,3);
  TAGGED status = RefMutable(t1);
  
  if (!(status & IStep(1))) {	/* STATUS: not enqueued */
    int ix = where>>1;
    if (where & 0x1) {
      fd_enqueue_append(w,item,ix); /* DOM or MINMAX */
    } else {
      fd_enqueue_prepend(w,item,ix); /* DOM or MINMAX */
    }
  }
}


/* 0 -- empty queue
   1 -- dequeued indexical
   2 -- dequeued global
   3 -- pending wakeup goal
*/
int fd_dequeue MAGIC (HIDDEN_PROTO
		      Argdecl,
		      TAGGED *item)
{
  struct propagator *cur = fd.current_propagator;
  SP_globref attr_ref;
  struct daemon_frame *handle;
  int i;

  while (cur->queue[4].first != cur->queue[4].last) {
    TAGGED daemon = cur->queue[4].items[cur->queue[4].first++];
    if (cur->queue[4].first==cur->queue[4].size)
      cur->queue[4].first = 0;
    attr_ref = (SP_globref)TermToPointer(CTagToArg(daemon,2));
    handle = (struct daemon_frame *)TermToPointer(CTagToArg(daemon,5));
    (*handle->daemon)(HIDDEN_ARG_COMMA
		      w, handle, attr_ref, CTagToArg(daemon,1));
  }
  if (OffHeaptop(w->global_top,w->heap_warn_soft))
    return 3;
  for (i=cur->hint; i<FD_NB_QUEUES-1; i++) {
    struct prop_queue *q = &cur->queue[i];
    if (q->first != q->last) {
      int pos = q->first;
      
      *item = q->items[pos++];
      if (pos==q->size)
	pos = 0;
      q->first = pos;
      if (pos==q->last)
	cur->hint = i+1;
      return (i==0 ? 1 : 2);
    }
  }
  cur->hint = i;
  return 0;
}

static void gc_suspensions MAGIC (HIDDEN_PROTO
				  Argdecl,
				  int index,
				  TAGGED lists_loc,
				  TAGGED tail)
{
  TAGGED *lists = TagToArg(RefMutable(lists_loc),0);
  TAGGED list = lists[index];
  TAGGED *h = w->global_top;
  TAGGED item, entvar, *target;
  int decr=0;
  LetShadowGlobal;

  if (lists < GLOBAL_UNCOND) { /* can't smash */
    int i;
    for (i=0; i<8; i++)
      h[i] = lists[i];
    lists = h;
    h += 8;
    update_mutable(w,MakeStructure(lists),lists_loc);
  }
  target = &lists[index];
  while (list!=tail) {
    item = CTagToCar(list);
    entvar = CTagToArg(item,4);
    DerefSwitch(entvar,goto live;);
    decr--;
    list = CTagToCdr(list);
    continue;
  live:
    if (TagToLST(list) < GLOBAL_UNCOND) {	/* can't smash */
      h[0] = CTagToCar(list);
      h[1] = CTagToCdr(list);
      list = MakeList(h);
      h += 2;
    }
    *target = list;
    target = TagToCdr(list);
    list = *target;
  }
  *target = tail;
  lists[1] -= IStep(decr);
  if (lists[index]==atom_nil) {
    index = GetSmall(lists[2]) & MASK_VAL;
    if (TagIsLST(lists[3]))
      index |= MASK_DOM;
    if (TagIsLST(lists[4]))
      index |= MASK_MIN;
    if (TagIsLST(lists[5]))
      index |= MASK_MAX;
    if (TagIsLST(lists[6]))
      index |= MASK_MINMAX;
    lists[2] = MakeSmall(index);
  }
  w->global_top = h;
}


static void fd_enqueue_list_gc MAGIC (HIDDEN_PROTO
				      Argdecl,
				      int index,
				      TAGGED filter, TAGGED lists_loc)
{
  TAGGED *lists = TagToArg(RefMutable(lists_loc),0);
  TAGGED list, tail, item, status, entvar, functor;
  int ndead=0;

  list = lists[index];
  tail = list;
  while (TagIsLST(list)) {
    item = CTagToCar(list);
    list = CTagToCdr(list);
    entvar = CTagToArg(item,4);
    DerefSwitch(entvar,goto live;);
    ndead++;
    tail = list;
    continue;
  live:
    functor = TagToHeadfunctor(item);
    status = RefMutable(CTagToArg(item,3));
    if (functor==functor_daemon5) {
      if ((status & IStep(12)) != IStep(8)) /* STATUS: not current or not idempotent */
	fd_enqueue_daemon(w, item);
    } else {
      if (!(status & IStep(1))) { /* STATUS: not enqueued */
	if (functor==functor_ix7) {
	  if (entvar!=filter)
	    fd_enqueue_append(w, item, 0x0);
	} else {
	  fd_enqueue_append(w, item, (index==3 ? 0x3 : 0x2)); /* DOM or MINMAX */
	}
      }
    }
  }
  if (ndead>0)			/* there exist entailed entries */
    gc_suspensions(w, index, lists_loc, tail);
}

static void fd_enqueue_list MAGIC (HIDDEN_PROTO
				   Argdecl,
				   int index,
				   TAGGED filter, TAGGED lists)
{
  TAGGED list, item, status, entvar, functor;

  list = CTagToArg(lists,index);
  while (TagIsLST(list)) {
    item = CTagToCar(list);
    list = CTagToCdr(list);
    entvar = CTagToArg(item,4);
    DerefSwitch(entvar,goto live;);
    continue;
  live:
    functor = TagToHeadfunctor(item);
    status = RefMutable(CTagToArg(item,3));
    if (functor==functor_daemon5) {
      if ((status & IStep(12)) != IStep(8)) /* STATUS: not current or not idempotent */
	fd_enqueue_daemon(w, item);
    } else {
      if (!(status & IStep(1))) { /* STATUS: not enqueued */
	if (functor==functor_ix7) {
	  if (entvar!=filter)
	    fd_enqueue_prepend(w, item, 0x0);
	} else {
	  fd_enqueue_prepend(w, item, (index==3 ? 0x3 : 0x2)); /* DOM or MINMAX */
	}
      }
    }
  }
}


static int fd_enqueue_val MAGIC (HIDDEN_PROTO
				 struct worker *w,
				 int index,
				 TAGGED filter,
				 TAGGED lists,
				 TAGGED *stack)
{
  TAGGED list0, list, pair, item, functor, vars, var, entvar, status, *h;
  int n=0;
  int iff_count=0;
  LetShadowGlobal;

  list = CTagToArg(lists,index);
  list0 = list;
  h = w->global_top;
  while (TagIsLST(list)) {
    pair = CTagToCar(list);
    list = CTagToCdr(list);
    vars = CTagToArg(pair,1); DerefNonvar(vars);
    item = CTagToArg(pair,2); DerefNonvar(item);
    functor = TagToHeadfunctor(item);
    entvar = CTagToArg(item,4);
    DerefSwitch(entvar,goto live;);
    continue;
  live:
    var = atom_nil;
    while (TagIsLST(vars) && !IsVar(var)) {
      DerefCar(var,vars);
      DerefCdr(vars,vars);
    }
    if (IsVar(var)) {	/* fd_val_link(t1, vars, item) later */
      n++;
      iff_count += (functor==functor_iff4);
      HeapPush(h,pair);
      HeapPush(h,*stack);
      *stack = MakeList(h-2);
    } else if (functor==functor_daemon5) {
      status = RefMutable(CTagToArg(item,3));
      if ((status & IStep(12)) != IStep(8)) /* STATUS: not current or not idempotent */
	fd_enqueue_daemon(w, item);
    } else if (functor==functor_ix7) {
      if (entvar!=filter)
	fd_enqueue_prepend(w, item, 0x0);
    } else if (functor==functor_iff4) {
      TAGGED bvar, key;

      iff_count++;
      bvar = CTagToArg(item,2);
      key = CTagToArg(item,3);
      DerefNonvar(bvar);
      if (bvar==key) {
	DerefArg(item,item,1);
	fd_enqueue_prepend(w, item, 0x0);
      }
    } else {
      status = RefMutable(CTagToArg(item,3));
      if (!(status & IStep(1))) { /* STATUS: not enqueued */
	fd_enqueue_prepend(w, item, 0x1);
      }
    }
    if (iff_count==0)
      list0 = list;
  }
  w->global_top = h;
  /* now disable all iff(Ix,B,Key,A) items we encountered */
  while (TagIsLST(list0)) {
    pair = CTagToCar(list0);
    list0 = CTagToCdr(list0);
    item = CTagToArg(pair,2); DerefNonvar(item);
    DerefArg(entvar,item,4);
    if (IsVar(entvar) && TagToHeadfunctor(item)==functor_iff4) {	/* iff(Ix,B,Key,A) */
      TAGGED key = CTagToArg(item,3);

      BindHVA(entvar,key);
    }
  }
  return n;
}


/* assuming bits > 0 */
/* X(1), X(2) must survive over this */
void fd_enqueue_all MAGIC (HIDDEN_PROTO
			   Argdecl,
			   int bits,
			   TAGGED filter, TAGGED lists_loc)
{
  TAGGED t1;
  int nsusp;
  
  /* each head and tail of each list 
     is dereferenced */
  t1 = RefMutable(lists_loc);	/* get suspension lists */
  t1 = CTagToArg(t1,1);		/* get suspension count */
  nsusp = GetSmall(t1);
  RequireHeap2((nsusp<<1)+8,filter,lists_loc,EVAL_ARITY); /* 2 * #suspensions + lists/7 */
  if (bits & MASK_SINGLETON) {
    TAGGED lists = RefMutable(lists_loc);
    int n=0;

    /* 3.9: enqueue all constraints when ground,
       (a) to maximize entailment detection,
       (b) to handle co-references */
    bits |= GetSmall(CTagToArg(lists,2)); /* bitmask of susp. lists */
    X(3) = atom_nil;		/* accumulator for fd_enqueue_val */
    /* we are about to prepend---prepend slowest first! */
    if (bits & MASK_DOM)
      fd_enqueue_list(w, 3, filter, lists);
    if (bits & MASK_MIN)
      fd_enqueue_list(w, 4, filter, lists);
    if (bits & MASK_MAX)
      fd_enqueue_list(w, 5, filter, lists);
    if (bits & MASK_MINMAX)
      fd_enqueue_list(w, 6, filter, lists);
    if (bits & MASK_VAL)
      n = fd_enqueue_val(w, 7, filter, lists, &X(3));
    if (n>0) {
      TAGGED pair, var, vars, item;
      TAGGED *h;

      RequireHeap(13*n, 4);
      while (TagIsLST(X(3))) {
	pair = CTagToCar(X(3));  
	X(3) = CTagToCdr(X(3)); 
	vars = CTagToArg(pair,1); DerefNonvar(vars);
	item = CTagToArg(pair,2); DerefNonvar(item);
	/* unwrap any iff(Ix,B,Key,A) item */
	if (TagToHeadfunctor(item)==functor_iff4) {
	  TAGGED bvar, key;

	  bvar = CTagToArg(item,2);
	  key = CTagToArg(item,3);
	  DerefNonvar(bvar);
	  if (bvar!=key)
	    continue;
	  DerefArg(item,item,1);
	}

	do {
	  DerefCar(var,vars);
	  DerefCdr(vars,vars);
	} while (!IsVar(var));
	h = w->global_top;
	HeapPush(h,functor_minus);
	HeapPush(h,vars);
	HeapPush(h,item);
	w->global_top = h;
	fd_link(w, var, fd.functor_val, MakeStructure(h-3));
      }
    }
  } else {
    if (bits & MASK_MINMAX)
      fd_enqueue_list_gc(w, 6, filter, lists_loc);
    if (bits & MASK_MAX)
      fd_enqueue_list_gc(w, 5, filter, lists_loc);
    if (bits & MASK_MIN)
      fd_enqueue_list_gc(w, 4, filter, lists_loc);
    if (bits & MASK_DOM)
      fd_enqueue_list_gc(w, 3, filter, lists_loc);
  }
}


/* implies $fd_begin */
void SPCDECL
prolog_fd_global_enqueue MAGIC (HIDDEN_PROTO SP_term_ref TermR)
{
  WAMENV;
  TAGGED term=RefTerm(TermR);
  TAGGED mutable;

  SP_MANGLE(prolog_fd_begin)(HIDDEN_ARG);
  DerefNonvar(term);
  mutable = CTagToArg(term,3);
  update_mutable(w,RefMutable(mutable)|IStep(9),mutable); /* STATUS: enqueued, current */
  /*** done in Prolog
  fd_sync(Arg);
  fd_enqueue(w, term, 0x3);
  ***/
}


/* implies $fd_begin */
void SPCDECL
prolog_fd_enqueue_all MAGIC (HIDDEN_PROTO SP_term_ref ListsM)
{
  WAMENV;
  TAGGED t1;
  int bits;

/*    X(0) = RefTerm(ListsM); */
  (void)ListsM;
  SP_MANGLE(prolog_fd_begin)(HIDDEN_ARG);
  DerefNonvar(X(0));
  t1 = RefMutable(X(0));	/* get suspension lists */
  bits = GetSmall(CTagToArg(t1,2)); /* get filter */
  if (bits > 0) {
    fd_sync(Arg);
    fd_enqueue_all(w,bits+MASK_SINGLETON,atom_nil,X(0));
  }
}

/* Support for managing incumbents:
   '$fd_update_incumbent'(+Ptr, +Value, +Vertex).
	store a new incumbent vertex and value
   '$fd_incumbent_bound'(+Ptr, -Value).
	retrieve the current incumbent value.
   [MC] 4.0: Ptr is passed as PointerToTerm.
*/

void SPCDECL
prolog_fd_update_incumbent MAGIC (HIDDEN_PROTO
				  SP_term_ref PtrR,
				  SP_term_ref ValueR,
				  SP_term_ref VertexR)
{
  WAMENV;
  struct instance *ins = (struct instance *)TermToPointer(RefTerm(PtrR));
  int i;
  TAGGED vertex;
  /* 3.9 assert scheme 
     => value is at offset no_cells-4,
        vertex is at offset no_cells-7, -9, ...
     3.10 assert scheme 
     => value is at offset 1,
        vertex is at offset 3, 5, ...
  */

#if SICSTUS_MAJOR_VERSION < 4
  int no_cells = ((ins->objsize-sizeof(struct instance))>>LogSizeOfWord) + 1;
  DEREF(ins->code[no_cells-4],RefTerm(ValueR));
  DEREF(vertex,RefTerm(VertexR));
  for (i=no_cells-7; TagIsLST(vertex); i-=2) {
    DerefCar(ins->code[i],vertex);
    DerefCdr(vertex,vertex);
  }
#else
  DEREF(ins->code[1],RefTerm(ValueR));
  DEREF(vertex,RefTerm(VertexR));
  for (i=3; TagIsLST(vertex); i+=2) {
    DerefCar(ins->code[i],vertex);
    DerefCdr(vertex,vertex);
  }
#endif
}


void SPCDECL
prolog_fd_incumbent_bound MAGIC (HIDDEN_PROTO
				 SP_term_ref PtrR,
				 SP_term_ref ValueR)
{
  WAMENV;
  struct instance *ins = (struct instance *)TermToPointer(RefTerm(PtrR));
  /* 3.9 assert scheme 
     => value is at offset no_cells-4,
        vertex is at offset no_cells-7, -9, ...
     3.10 assert scheme 
     => value is at offset 1,
        vertex is at offset 3, 5, ...
  */
#if SICSTUS_MAJOR_VERSION < 4
  int no_cells = ((ins->objsize-sizeof(struct instance))>>LogSizeOfWord) + 1;
  RefTerm(ValueR) = ins->code[no_cells-4];
#else
  RefTerm(ValueR) = ins->code[1];
#endif
}


void SPCDECL prolog_fd_minint_maxint MAGIC (HIDDEN_PROTO
					    long *minp, long *maxp)
{
  (void)HIDDEN_ARG_OR_ZERO;
  *minp = -HighInt;
  *maxp = HighInt-1L;
}


/* Heap routines from Cormen et al. */

void heap_init(struct heap *h)
{
  h->size = 0;
}


void heap_insert(struct heap *h,
		 void *item,
		 HeapFun cmpfun)
{
  int i = ++h->size;

  while (i>1 && (*cmpfun)(h->item[i>>1],item) > 0) {
    h->item[i] = h->item[i>>1];
    i >>= 1;
  }
  h->item[i] = item;
}


/* Repair the heap property
   i.e. h->item[i>>1].key <= h->item[i].key
*/
void heapify(struct heap *h,
	     HeapFun cmpfun)
{
  int i = 1;

  for (;;) {
    int l = i<<1;
    int r = l+1;
    int smallest;
    
    if (l <= h->size && (*cmpfun)(h->item[l],h->item[i]) < 0)
      smallest = l;
    else
      smallest = i;
    if (r <= h->size && (*cmpfun)(h->item[r],h->item[smallest]) < 0)
      smallest = r;
    if (smallest != i) {
      void *tmp = h->item[i];
      
      h->item[i] = h->item[smallest];
      h->item[smallest] = tmp;
      i = smallest;
    } else
      break;
  }
}


void *heap_extract_min(struct heap *h,
		       HeapFun cmpfun)
{
  void *it = h->item[1];

  if (h->size==0)
    return NULL;
  h->item[1] = h->item[h->size];
  --h->size;
  heapify(h,cmpfun);

  return it;
}

/* Support for Palloc/Pfree. */

void *fd_perm_alloc MAGIC (HIDDEN_PROTO
			   Argdecl,
			   int nbytes,
			   TAGGED handle) /* HVA to bind */
{
  void *ptr;
  TAGGED tptr;
  TAGGED *h;
  TAGGED inner, outer;
  LetShadowGlobal;

  ptr = fd_malloc(nbytes);
  tptr = PointerToTerm(ptr);


  RequireHeap(4,EVAL_ARITY);
  h = w->global_top;
  inner = MakeStructure(h);
  *h++ = functor_Dfree;
  *h++ = tptr;
  outer = MakeList(h);
  Load0HVA(h);
  *h++ = inner;
  w->global_top = h;
  TrailPushCheck(outer);
  BindHVA(handle,outer);
  return ptr;
}


void *fd_perm_data(TAGGED handle) /* [Flag | '$free'(Ptr)] */
{
  handle = CTagToCdr(handle);
  handle = CTagToArg(handle,1);
  return TermToPointer(handle);
}


/* A mutable item on the trail is redundant if:
   1. the mutable is in H+, or
   2. there is a previous item in TR+, or
   3. the new and old values are the same.
   */
#define RedundantMutableItem(Mutp,Item) \
((Mutp) >= GLOBAL_UNCOND || (Item)[2] >= TrailToInt(w->node->trail_top) || (Mutp)[1]==(Item)[1])

#define PrevMutItem(Item) \
(w->trail_start + GetSmall((Item)[2]))


void fd_perm_free MAGIC (HIDDEN_PROTO
			 Argdecl)
{
  struct node *nd;
  void (SPCDECL *destructor)(void *);
  ANYPOINTER frame;
  TAGGED handle;		/* [Flag | '$free'(Ptr)] */
  TAGGED flag;
  SP_BOOL committed;
  LetShadowGlobal;
  
  static_output_state(w,&handle,&committed);
  flag = CTagToCar(handle);
  if (committed) {
    BindHVA(flag,TaggedZero);	/* disable cleanup */
    frame = TermToPointer(CTagToArg(CTagToCdr(handle),1));
    destructor = *(void (SPCDECL **)(void*))frame;
    (*destructor)(frame);
    if (w->trail_top[-1]==handle)
      w->trail_top--;
  } else {
    BindHVA(flag,TaggedOne);	/* enable cleanup */
    for (nd = w->node;
	 ChoiceYounger(nd,w->choice_start) && !ChoiceptTestCleanup(nd);
	 nd = ChoiceptPrevious(nd))
      ChoiceptMarkCleanup(nd);
  }
}


TAGGED
daemon_copy_state MAGIC (HIDDEN_PROTO
			 Argdecl,
			 TAGGED *global)
{
  TAGGED *s, *h, tstate;
  int ar, i;
  LetShadowGlobal;
  
  tstate = RefMutable(CTagToArg(*global,1));
  ar = Arity(TagToHeadfunctor(tstate));
  s = TagToArg(tstate,0);
  if (s >= GLOBAL_UNCOND) {
    s[ar] += IStep(1);		/* increment stamp */
  } else {
    RequireHeap2(ar+1,*global,tstate,EVAL_ARITY);
    s = TagToArg(tstate,0);
    h = w->global_top;
    for (i=ar; i>=0; i--)
      h[i] = s[i];
    w->global_top = h+ar+1;
    h[ar] += IStep(1);		/* increment stamp */
    tstate = MakeStructure(h);
    update_mutable(w,tstate,CTagToArg(*global,1));
  }
  return tstate;
}

/* Most propagators have arguments (+State0, -State), ... where
   State0 = F(......,Handle,Stamp), is left dereferenced,
   State  = copy of State0 with Stamp incremented.
   Also, check if this execution step can be backtracked over or not.
*/
TAGGED unify_output_state MAGIC(HIDDEN_PROTO
				Argdecl,
				TAGGED *phandle,
				long *pstamp,
				SP_BOOL *pcommitted)
{
  TAGGED handle, t1, *s, *h;
  int ar, i;
  LetShadowGlobal;
  
  DerefNonvar(X(0));
  ar = Arity(TagToHeadfunctor(X(0)));
  DerefArg(handle,X(0),ar-1);
  DerefArg(t1,X(0),ar);
  *pstamp = GetSmall(t1);
  *pcommitted = (IsVar(handle) ? TRUE : TagToLST(handle) >= GLOBAL_UNCOND);
  s = TagToArg(X(0),0);
  if (s >= GLOBAL_UNCOND) {
    s[ar] += IStep(1);		/* increment stamp */
  } else {
    RequireHeap1(ar+1,handle,EVAL_ARITY);
    s = TagToArg(X(0),0);
    h = w->global_top;
    for (i=0; i<ar+1; i++)
      h[i] = s[i];
    w->global_top = h+ar+1;
    h[ar] += IStep(1);		/* increment stamp */
    X(0) = MakeStructure(h);
  }
  *phandle = handle;
  return X(0);
}

/* Some propagators have arguments (+State0, -State), ... where
   State0 = F(......,Handle), is left dereferenced,
   State  = State0,
   Also, check if this execution step can be backtracked over or not.
*/
TAGGED static_output_state(Argdecl,
			   TAGGED *phandle,
			   SP_BOOL *pcommitted)
{
  TAGGED handle;
  int ar;
  LetShadowGlobal;
  
  DerefNonvar(X(0));
  ar = Arity(TagToHeadfunctor(X(0)));
  DerefArg(handle,X(0),ar-1);
  *phandle = handle;
  *pcommitted = (IsVar(handle) ? TRUE : TagToLST(handle) >= GLOBAL_UNCOND);
  return X(0);
}

/* length(+List,-Length) */
int 
list_length(TAGGED tvec)
{
  int nvars=0;
  
  while (TagIsLST(tvec)) {
    DerefCdr(tvec,tvec);
    nvars++;
  }
  return nvars;
}

/* store var & attr in global term refs */
void
get_var_and_attr(TAGGED term, SP_globref ref)
{
  TAGGED t1;
  
  DerefArg(t1,term,1);	/* get domain var */
  RefGlob(ref+1) = t1;
  DerefArg(t1,term,2);	/* get attribute */
  RefGlob(ref) = t1;
}


/* for qsorting by ascending long */
static int cmp_asc_long(long *l1, long *l2)
{
  long val1 = *l1;
  long val2 = *l2;

  return CMP(val1,val2);
}

#define QType long
#define QCmp  cmp_asc_long
#define QSort qsort_asc_long1
#include "qsort.ic"

void
qsort_asc_long MAGIC (HIDDEN_PROTO
		      long *l1, int n)
{
  qsort_asc_long1(l1,n);
}

void *
fd_malloc MAGIC (HIDDEN_PROTO
                 size_t size)
{
  void *p;
  /* [PM] 4.0 FIXME: update all callers to check the return value from Malloc/SP_malloc, then remove the lazy-check */
  LAZY_NULL_CHECK(p = SP_malloc(size));
  return p;
}

