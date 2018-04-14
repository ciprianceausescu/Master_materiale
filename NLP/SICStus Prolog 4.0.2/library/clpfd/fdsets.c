/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#if MULTI_SP_AWARE
#endif /* MULTI_SP_AWARE */

#define EMIT_INTERVAL(H,B,E)			\
      NumstackAlloc(4,H);			\
      *valuep = MakeList(H);			\
      H[0] = MakeList(H+2);			\
      valuep = H+1;				\
      H[2] = (B);				\
      H[3] = (E);				\

#define EMIT_RANGE(H,R)				\
      NumstackAlloc(2,H);			\
      *valuep = MakeList(H);			\
      H[0] = (R);				\
      valuep = H+1;				\

#define SIZEOF_INTERVAL(B,E)			\
	(GetSmall(E)-GetSmall(B)+1)		\

TAGGED fd_min(TAGGED set)
{
  return RangeMin(CTagToCar(set));
}


TAGGED fd_max(TAGGED set)
{
  TAGGED r1;

  do {
    r1 = CTagToCar(set);
    set = CTagToCdr(set);
  } while (TagIsLST(set));  
  return RangeMax(r1);
}


void fd_first_and_rest MAGIC (HIDDEN_PROTO Argdecl,
			      TAGGED d1, TAGGED *firstp, TAGGED *restp)
{
  TAGGED r1, b, e;

  r1 = CTagToCar(d1); d1 = CTagToCdr(d1);
  b = RangeMin(r1); e = RangeMax(r1);
  *firstp = b;
  if (b==e)
    *restp  = d1;
  else {
    TAGGED *h, *valuep = restp;

    EMIT_INTERVAL(h,FDincr(b),e);
    *valuep = d1;
  }
}


/* Build a copy of old on the numstack.
   May cannibalize parts of old which are already on the numstack.
*/
TAGGED fd_localize MAGIC (HIDDEN_PROTO
			  Argdecl,
			  TAGGED old)
{
  TAGGED value = old;
  TAGGED *h, *valuep = &value;
  TAGGED r1;

  while (old!=EmptySet) {
    r1 = CTagToCar(old);
    if (OnHeap(TagToLST(old))) {
      EMIT_INTERVAL(h,RangeMin(r1),RangeMax(r1));
      old = CTagToCdr(old);
    } else {
      if (OnHeap(TagToLST(r1))) {
	NumstackAlloc(2,h);
	CTagToCar(old) = MakeList(h);
	h[0] = RangeMin(r1);
	h[1] = RangeMax(r1);
      }
      valuep = &CTagToCdr(old);
      old = *valuep;
    }
  }
  *valuep = old;
  return value;
}


/* set is assumed to be finite */
int fd_size(TAGGED set)
{
  TAGGED r;
  int sum=0;
  
  while (set!=EmptySet) {
    r = CTagToCar(set);
    set = CTagToCdr(set);
    sum += GetSmall(RangeMax(r) - RangeMin(r) + TaggedZero) + 1;
  }
  return sum;
}


SP_BOOL fd_singleton(TAGGED set)
{
  if (TagIsLST(set)) {
    TAGGED head = CTagToCar(set);
    
    if (CTagToCdr(set)==EmptySet && RangeMin(head)==RangeMax(head))
      return TRUE;
  }
  return FALSE;
}


int point_vs_interval(TAGGED p, TAGGED min, TAGGED max)
{
  if (!Are3Small(p,min,max)) {
    if (!TagIsSIN(p))
      p = (p==Inf ? InfAsINT : SupAsINT);
    if (min==Inf)
      min = InfAsINT;
    if (max==Sup)
      max = SupAsINT;
  }
  return (Tlt(p,min) ? CMP_BEFORE : Tgt(p,max) ? CMP_AFTER : CMP_INSIDE);
}


int val_vs_interval(TAGGED p, TAGGED min, TAGGED max)
{
  if (!AreSmall(min,max)) {
    if (min==Inf)
      min = InfAsINT;
    if (max==Sup)
      max = SupAsINT;
  }
  return (Tlt(p,min) ? CMP_BEFORE : Tgt(p,max) ? CMP_AFTER : CMP_INSIDE);
}


/* Compute the temporal relation between two intervals. */
unsigned int fd_interval_cmp(TAGGED b1, TAGGED e1,
			     TAGGED b2, TAGGED e2)
{
  if (b1==e1 && b2==e2) {	/* about 25% of the time */
    if (Tlt(b1,b2))
      return (Teq(FDincr(b1),b2) ? FD_MEETS : FD_BEFORE);
    else if (Teq(b1,b2))
      return FD_EQUALS;
    else
      return (Teq(b1,FDincr(b2)) ? FD_MET_BY : FD_AFTER);
  }
				/* extend <,=,> to Inf and Sup */
  if (b1==Inf)
    b1 = InfAsINT;
  if (e1==Sup)
    e1 = SupAsINT;
  else
    e1 += IStep(1);		/* want [b1,e1), [b2,e2) */
  if (b2==Inf)
    b2 = InfAsINT;
  if (e2==Sup)
    e2 = SupAsINT;
  else
    e2 += IStep(1);		/* want [b1,e1), [b2,e2) */
  if (Tlt(b1,b2)) {
    if (Tle(e1,b2))
      return (Teq(e1,b2) ? FD_MEETS : FD_BEFORE);
    else if (Tlt(e1,e2))
      return FD_OVERLAPS;
    else if (Teq(e1,e2))
      return FD_FINISHED_BY;
    else
      return FD_CONTAINS;
  } else if (Teq(b1,b2)) {
    if (Tlt(e1,e2))
      return FD_STARTS;
    else if (Teq(e1,e2))
      return FD_EQUALS;
    else
      return FD_STARTED_BY;
  } else {
    if (Tlt(e1,e2))
      return FD_DURING;
    else if (Teq(e1,e2))
      return FD_FINISHES;
    else if (Tlt(b1,e2))
      return FD_OVERLAPPED_BY;
    else
      return (Teq(b1,e2) ? FD_MET_BY : FD_AFTER);
  }
}


/* assuming checked nonempty interval */
TAGGED fd_interval MAGIC (HIDDEN_PROTO Argdecl,
			  TAGGED min, TAGGED max)
{
  TAGGED value, *h;
  TAGGED *valuep = &value;
  
  EMIT_INTERVAL(h,min,max);
  *valuep = EmptySet;
  return value;
}



TAGGED fd_pair MAGIC (HIDDEN_PROTO
		      Argdecl,
		      TAGGED t1, TAGGED t2)
{
  TAGGED value, *h, t3;
  TAGGED *valuep = &value;

  if (Tgt(t1,t2)) {
    t3=t1; t1=t2; t2=t3;
  }
  if (Tge(FDincr(t1),t2)) {
    EMIT_INTERVAL(h,t1,t2);
  } else {
    EMIT_INTERVAL(h,t1,t1);
    EMIT_INTERVAL(h,t2,t2);
  }
  *valuep = EmptySet;
  return value;
}



/* assuming checked nonempty interval */
TAGGED fd_compl_interval MAGIC (HIDDEN_PROTO
				Argdecl,
				TAGGED min, TAGGED max)
{
  TAGGED value, *h;
  TAGGED *valuep = &value;
  
  if (min!=Inf) {
      EMIT_INTERVAL(h,Inf,FDdecr(min));
  }
  if (max!=Sup) {
      EMIT_INTERVAL(h,FDincr(max),Sup);
  }
  *valuep = EmptySet;
  return value;
}

/* 3.9.1: use overflow arithmetic */
TAGGED fd_lsh MAGIC (HIDDEN_PROTO
		     Argdecl,
		     TAGGED d1,
		     TAGGED toffset)
{
  TAGGED value, *h, r1, b, e;
  TAGGED *valuep = &value;
  
  while (d1!=EmptySet) {
    r1 = CTagToCar(d1); d1 = CTagToCdr(d1);
    b = safe_plus(RangeMin(r1),toffset);
    e = safe_plus(RangeMax(r1),toffset);
    EMIT_INTERVAL(h,b,e);
  }
  *valuep = EmptySet;
  return value;
}


TAGGED *fd_neg_internal MAGIC (HIDDEN_PROTO
			       Argdecl,
			       TAGGED d1,
			       TAGGED toffset,
			       TAGGED *valuep)
{
  if (d1!=EmptySet) {
    TAGGED *h, r1, b1, e1;
      
    valuep = fd_neg_internal(w,CTagToCdr(d1),toffset,valuep);
    r1 = CTagToCar(d1);
    b1 = safe_minus(toffset,RangeMax(r1));
    e1 = safe_minus(toffset,RangeMin(r1));

    EMIT_INTERVAL(h,b1,e1);
  }
  return valuep;
}

TAGGED fd_neg_offset MAGIC (HIDDEN_PROTO
			    Argdecl,
			    TAGGED d1,
			    TAGGED toffset)
{
  TAGGED value;
  
  *fd_neg_internal(w,d1,toffset,&value) = EmptySet;
  return value;
}

TAGGED fd_subtract MAGIC (HIDDEN_PROTO
			  Argdecl,
			  TAGGED d1, TAGGED d2)
{
  TAGGED r1, b1, e1, r2, b2, e2;
  TAGGED *h, value;
  TAGGED *valuep = &value;
  
list_and_list:
  if (d1==EmptySet)
    goto null_and_list;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_list:
  if (d2==EmptySet)
    goto head_tail_and_null;
  r2 = CTagToCar(d2);  d2 = CTagToCdr(d2);
  b2 = RangeMin(r2);   e2 = RangeMax(r2);
  goto head_tail_and_head_tail;
list_and_head_tail:
  if (d1==EmptySet)
    goto null_and_head_tail;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_head_tail:
 switch_fd_interval_cmp(b1,e1,b2,e2,
			/*BEFORE*/ {
                          EMIT_INTERVAL(h,b1,e1);
			  goto list_and_head_tail;
                        },
			/*MEETS*/ {
                          EMIT_INTERVAL(h,b1,e1);
			  goto list_and_head_tail;
                        },
			/*OVERLAPS*/ {
			  EMIT_INTERVAL(h,b1,FDdecr(b2));
			  goto list_and_head_tail;
			},
			/*FINISHED_BY*/ {
			  EMIT_INTERVAL(h,b1,FDdecr(b2));
			  goto list_and_list;
			},
			/*CONTAINS*/ {
			  EMIT_INTERVAL(h,b1,FDdecr(b2));
			  b1 = FDincr(e2);
			  goto head_tail_and_list;
			},
			/*STARTS*/ goto list_and_head_tail;,
			/*EQUALS*/ goto list_and_list;,
			/*STARTED_BY*/ {
			  b1 = FDincr(e2);
			  goto head_tail_and_list;
			},
			/*DURING*/ goto list_and_head_tail;,
			/*FINISHES*/ goto list_and_list;,
			/*OVERLAPPED_BY*/ {
			  b1 = FDincr(e2);
			  goto head_tail_and_list;
			},
			/*MET_BY*/ goto head_tail_and_list;,
			/*AFTER*/ goto head_tail_and_list;
			);
 null_and_head_tail:
 null_and_list:
  *valuep = EmptySet;
  return value;
 head_tail_and_null:
  EMIT_INTERVAL(h,b1,e1);
  *valuep = d1;
  return value;
}


TAGGED fd_interval_subtract MAGIC (HIDDEN_PROTO
				   Argdecl,
				   TAGGED b1, TAGGED e1, TAGGED d2)
{
  TAGGED r2, b2, e2;
  TAGGED *h, value;
  TAGGED *valuep = &value;
  
list:
  if (d2==EmptySet)
    goto head_tail_and_null;
  r2 = CTagToCar(d2);  d2 = CTagToCdr(d2);
  b2 = RangeMin(r2);   e2 = RangeMax(r2);
  switch_fd_interval_cmp(b1,e1,b2,e2,
			 /*BEFORE*/ EMIT_INTERVAL(h,b1,e1);,
			 /*MEETS*/  EMIT_INTERVAL(h,b1,e1);,
			 /*OVERLAPS*/    EMIT_INTERVAL(h,b1,FDdecr(b2));,
			 /*FINISHED_BY*/ EMIT_INTERVAL(h,b1,FDdecr(b2));,
			 /*CONTAINS*/ {
			   EMIT_INTERVAL(h,b1,FDdecr(b2));
			   b1 = FDincr(e2);
			   goto list;
			 },
			 /*STARTS*/ ;,
			 /*EQUALS*/ ;,
			 /*STARTED_BY*/ {
			   b1 = FDincr(e2);
			   goto list;
			 },
			 /*DURING*/ ;,
			 /*FINISHES*/ ;,
			 /*OVERLAPPED_BY*/ {
			   b1 = FDincr(e2);
			   goto list;
			 },
			 /*MET_BY*/ goto list;,
			 /*AFTER*/ goto list;
			 );
  *valuep = EmptySet;
  return value;
head_tail_and_null:
  EMIT_INTERVAL(h,b1,e1);
  *valuep = EmptySet;
  return value;
}


TAGGED fd_subtract_interval MAGIC (HIDDEN_PROTO
				   Argdecl,
				   TAGGED d1, TAGGED b2, TAGGED e2)
{
  TAGGED r1, b1, e1;
  TAGGED *h, value;
  TAGGED *valuep = &value;
  
list:
  if (d1==EmptySet)
    goto list_and_null;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
  switch_fd_interval_cmp(b1,e1,b2,e2,
			 /*BEFORE*/ {
                           EMIT_RANGE(h,r1);
			   goto list;
                         },
			 /*MEETS*/ {
			   EMIT_RANGE(h,r1);
			   goto list;
			 },
			 /*OVERLAPS*/ {
			   EMIT_INTERVAL(h,b1,FDdecr(b2));
			   goto list;
			 },
			 /*FINISHED_BY*/ {
			   EMIT_INTERVAL(h,b1,FDdecr(b2));
			   goto list_and_null;
			 },
			 /*CONTAINS*/ {
			   EMIT_INTERVAL(h,b1,FDdecr(b2));
			   b1 = FDincr(e2);
			 },
			 /*STARTS*/ goto list;,
			 /*EQUALS*/ goto list_and_null;,
			 /*STARTED_BY*/ b1 = FDincr(e2);,
			 /*DURING*/ goto list;,
			 /*FINISHES*/ goto list_and_null;,
			 /*OVERLAPPED_BY*/ b1 = FDincr(e2);,
			 /*MET_BY*/ ;,
			 /*AFTER*/ ;
			 );
  EMIT_INTERVAL(h,b1,e1);
list_and_null:
  *valuep = d1;
  return value;
}


TAGGED fd_complement MAGIC (HIDDEN_PROTO
			    Argdecl,
			    TAGGED d2)
{
  return fd_interval_subtract(w,Inf,Sup,d2);
}


/* same as fd_subtract(d1,t2..t2) */
TAGGED fd_delete MAGIC (HIDDEN_PROTO
			Argdecl,
			TAGGED d1, TAGGED t2)
{
  return fd_subtract_interval(w,d1,t2,t2);
}


TAGGED fd_intersection MAGIC (HIDDEN_PROTO
		     Argdecl,
		     TAGGED d1, TAGGED d2)
{
  TAGGED r1, b1, e1, r2, b2, e2;
  TAGGED *h, value;
  TAGGED *valuep = &value;
  
list_and_list:
  if (d1==EmptySet)
    goto null_and_list;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_list:
  if (d2==EmptySet)
    goto head_tail_and_null;
  r2 = CTagToCar(d2);  d2 = CTagToCdr(d2);
  b2 = RangeMin(r2);   e2 = RangeMax(r2);
  goto head_tail_and_head_tail;
list_and_head_tail:
  if (d1==EmptySet)
    goto null_and_head_tail;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_head_tail:
  switch_fd_interval_cmp(b1,e1,b2,e2,
			 /*BEFORE*/ goto list_and_head_tail;,
			 /*MEETS*/ goto list_and_head_tail;,
			 /*OVERLAPS*/ {
			   EMIT_INTERVAL(h,b2,e1);
			   b2 = FDincr(e1);
			   goto list_and_head_tail;
			 },
			 /*FINISHED_BY*/ {
			   EMIT_INTERVAL(h,b2,e2);
			   goto list_and_list;
			 },
			 /*CONTAINS*/ {
			   EMIT_INTERVAL(h,b2,e2);
			   b1 = FDincr(e2);
			   goto head_tail_and_list;
			 },
			 /*STARTS*/ {
			   EMIT_INTERVAL(h,b1,e1);
			   b2 = FDincr(e1);
			   goto list_and_head_tail;
			 },
			 /*EQUALS*/ {
			   EMIT_INTERVAL(h,b1,e1);
			   goto list_and_list;
			 },
			 /*STARTED_BY*/ {
			   EMIT_INTERVAL(h,b2,e2);
			   b1 = FDincr(e2);
			   goto head_tail_and_list;
			 },
			 /*DURING*/ {
			   EMIT_INTERVAL(h,b1,e1);
			   b2 = FDincr(e1);
			   goto list_and_head_tail;
			 },
			 /*FINISHES*/ {
			   EMIT_INTERVAL(h,b1,e1);
			   goto list_and_list;
			 },
			 /*OVERLAPPED_BY*/ {
			   EMIT_INTERVAL(h,b1,e2);
			   b1 = FDincr(e2);
			   goto head_tail_and_list;
			 },
			 /*MET_BY*/ goto head_tail_and_list;,
			 /*AFTER*/ goto head_tail_and_list;
			 );
null_and_head_tail:
null_and_list:
head_tail_and_null:
  *valuep = EmptySet;
  return value;
}

long fd_intersection_size(TAGGED d1, TAGGED d2)
{
  TAGGED r1, b1, e1, r2, b2, e2;
  long size = 0;
  
list_and_list:
  if (d1==EmptySet)
    goto null_and_list;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_list:
  if (d2==EmptySet)
    goto head_tail_and_null;
  r2 = CTagToCar(d2);  d2 = CTagToCdr(d2);
  b2 = RangeMin(r2);   e2 = RangeMax(r2);
  goto head_tail_and_head_tail;
list_and_head_tail:
  if (d1==EmptySet)
    goto null_and_head_tail;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_head_tail:
  switch_fd_interval_cmp(b1,e1,b2,e2,
			 /*BEFORE*/ goto list_and_head_tail;,
			 /*MEETS*/ goto list_and_head_tail;,
			 /*OVERLAPS*/ {
			   size += SIZEOF_INTERVAL(b2,e1);
			   b2 = FDincr(e1);
			   goto list_and_head_tail;
			 },
			 /*FINISHED_BY*/ {
			   size += SIZEOF_INTERVAL(b2,e2);
			   goto list_and_list;
			 },
			 /*CONTAINS*/ {
			   size += SIZEOF_INTERVAL(b2,e2);
			   b1 = FDincr(e2);
			   goto head_tail_and_list;
			 },
			 /*STARTS*/ {
			   size += SIZEOF_INTERVAL(b1,e1);
			   b2 = FDincr(e1);
			   goto list_and_head_tail;
			 },
			 /*EQUALS*/ {
			   size += SIZEOF_INTERVAL(b1,e1);
			   goto list_and_list;
			 },
			 /*STARTED_BY*/ {
			   size += SIZEOF_INTERVAL(b2,e2);
			   b1 = FDincr(e2);
			   goto head_tail_and_list;
			 },
			 /*DURING*/ {
			   size += SIZEOF_INTERVAL(b1,e1);
			   b2 = FDincr(e1);
			   goto list_and_head_tail;
			 },
			 /*FINISHES*/ {
			   size += SIZEOF_INTERVAL(b1,e1);
			   goto list_and_list;
			 },
			 /*OVERLAPPED_BY*/ {
			   size += SIZEOF_INTERVAL(b1,e2);
			   b1 = FDincr(e2);
			   goto head_tail_and_list;
			 },
			 /*MET_BY*/ goto head_tail_and_list;,
			 /*AFTER*/ goto head_tail_and_list;
			 );
null_and_head_tail:
null_and_list:
head_tail_and_null:
  return size;
}

#define EMIT(l,u)				\
if (!x1) {					\
  x1 = l;					\
  if (Tlt(l,u)) {				\
    x2 = FDincr(l);				\
    goto ret;					\
  }						\
} else {					\
  x2 = l;					\
  goto ret;					\
}
    

TAGGED fd_intersection_min2(TAGGED d1, TAGGED d2, TAGGED *elt2)
{
  TAGGED r1, b1, e1, r2, b2, e2;
  TAGGED x1=ERRORTAG;
  TAGGED x2=ERRORTAG;
  
list_and_list:
  if (d1==EmptySet)
    goto ret;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_list:
  if (d2==EmptySet)
    goto ret;
  r2 = CTagToCar(d2);  d2 = CTagToCdr(d2);
  b2 = RangeMin(r2);   e2 = RangeMax(r2);
  goto head_tail_and_head_tail;
list_and_head_tail:
  if (d1==EmptySet)
    goto ret;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_head_tail:
  switch_fd_interval_cmp(b1,e1,b2,e2,
			 /*BEFORE*/ goto list_and_head_tail;,
			 /*MEETS*/ goto list_and_head_tail;,
			 /*OVERLAPS*/ {
			   EMIT(b2,e1);
			   b2 = FDincr(e1);
			   goto list_and_head_tail;
			 },
			 /*FINISHED_BY*/ {
			   EMIT(b2,e2);
			   goto list_and_list;
			 },
			 /*CONTAINS*/ {
			   EMIT(b2,e2);
			   b1 = FDincr(e2);
			   goto head_tail_and_list;
			 },
			 /*STARTS*/ {
			   EMIT(b1,e1);
			   b2 = FDincr(e1);
			   goto list_and_head_tail;
			 },
			 /*EQUALS*/ {
			   EMIT(b1,e1);
			   goto list_and_list;
			 },
			 /*STARTED_BY*/ {
			   EMIT(b2,e2);
			   b1 = FDincr(e2);
			   goto head_tail_and_list;
			 },
			 /*DURING*/ {
			   EMIT(b1,e1);
			   b2 = FDincr(e1);
			   goto list_and_head_tail;
			 },
			 /*FINISHES*/ {
			   EMIT(b1,e1);
			   goto list_and_list;
			 },
			 /*OVERLAPPED_BY*/ {
			   EMIT(b1,e2);
			   b1 = FDincr(e2);
			   goto head_tail_and_list;
			 },
			 /*MET_BY*/ goto head_tail_and_list;,
			 /*AFTER*/ goto head_tail_and_list;
			 );
ret:
  *elt2 = x2;
  return x1;
}


TAGGED fd_intersection_interval MAGIC (HIDDEN_PROTO
			      Argdecl,
			      TAGGED d1, TAGGED b2, TAGGED e2)
{
  TAGGED r1, b1, e1;
  TAGGED *h, value;
  TAGGED *valuep = &value;
  
list:
  if (d1==EmptySet)
    goto null;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
  switch_fd_interval_cmp(b1,e1,b2,e2,
			 /*BEFORE*/ goto list;,
			 /*MEETS*/ goto list;,
			 /*OVERLAPS*/ {
			   EMIT_INTERVAL(h,b2,e1);
			   b2 = FDincr(e1);
			   goto list;
			 },
			 /*FINISHED_BY*/ EMIT_INTERVAL(h,b2,e2);,
			 /*CONTAINS*/ EMIT_INTERVAL(h,b2,e2);,
			 /*STARTS*/ {
			   EMIT_RANGE(h,r1);
			   b2 = FDincr(e1);
			   goto list;
			 },
			 /*EQUALS*/ EMIT_RANGE(h,r1);,
			 /*STARTED_BY*/ EMIT_INTERVAL(h,b2,e2);,
			 /*DURING*/ {
			   EMIT_RANGE(h,r1);
			   b2 = FDincr(e1);
			   goto list;
			 },
			 /*FINISHES*/ EMIT_RANGE(h,r1);,
			 /*OVERLAPPED_BY*/ EMIT_INTERVAL(h,b1,e2);,
			 /*MET_BY*/ ;,
			 /*AFTER*/ ;
			 );
null:
  *valuep = EmptySet;
  return value;
}

TAGGED fd_intersection_min(TAGGED d1, TAGGED d2)
{
  TAGGED r1, b1, e1, r2, b2, e2;
  
  if (d1==EmptySet)
    goto null;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_list:
  if (d2==EmptySet)
    goto null;
  r2 = CTagToCar(d2);  d2 = CTagToCdr(d2);
  b2 = RangeMin(r2);   e2 = RangeMax(r2);
  goto head_tail_and_head_tail;
list_and_head_tail:
  if (d1==EmptySet)
    goto null;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_head_tail:
  switch_fd_interval_cmp(b1,e1,b2,e2,
			 /*BEFORE*/
			 goto list_and_head_tail;,
			 /*MEETS*/
			 goto list_and_head_tail;,
			 /*OVERLAPS*/
			 return b2;,
			 /*FINISHED_BY*/
			 return b2;,
			 /*CONTAINS*/
			 return b2;,
			 /*STARTS*/
			 return b1;,
			 /*EQUALS*/
			 return b1;,
			 /*STARTED_BY*/
			 return b2;,
			 /*DURING*/
			 return b1;,
			 /*FINISHES*/
			 return b1;,
			 /*OVERLAPPED_BY*/
			 return b1;,
			 /*MET_BY*/
			 goto head_tail_and_list;,
			 /*AFTER*/
			 goto head_tail_and_list;
			 );
 null:
  return ERRORTAG;
}

TAGGED fd_intersection_max(TAGGED d1, TAGGED d2)
{
  TAGGED r1, b1, e1, r2, b2, e2;
  TAGGED max = 0;
  
list_and_list:
  if (d1==EmptySet)
    goto ret;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_list:
  if (d2==EmptySet)
    goto ret;
  r2 = CTagToCar(d2);  d2 = CTagToCdr(d2);
  b2 = RangeMin(r2);   e2 = RangeMax(r2);
  goto head_tail_and_head_tail;
list_and_head_tail:
  if (d1==EmptySet)
    goto ret;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_head_tail:
  switch_fd_interval_cmp(b1,e1,b2,e2,
			 /*BEFORE*/ goto list_and_head_tail;,
			 /*MEETS*/ goto list_and_head_tail;,
			 /*OVERLAPS*/ goto during;,
			 /*FINISHED_BY*/ {
			   max = e2;
			   goto list_and_list;
			 },
			 /*CONTAINS*/ goto overlapped_by;,
			 /*STARTS*/ goto during;,
			 /*EQUALS*/ {
			   max = e1;
			   goto list_and_list;
			 },
			 /*STARTED_BY*/ goto overlapped_by;,
			 /*DURING*/ {
			 during:
			   max = e1;
			   b2 = FDincr(e1);
			   goto list_and_head_tail;
			 },
			 /*FINISHES*/ {
			   max = e1;
			   goto list_and_list;
			 },
			 /*OVERLAPPED_BY*/ {
			 overlapped_by:
			   max = e2;
			   b1 = FDincr(e2);
			   goto head_tail_and_list;
			 },
			 /*MET_BY*/ goto head_tail_and_list;,
			 /*AFTER*/ goto head_tail_and_list;
			 );
 ret:
  return max;
}

TAGGED fd_union MAGIC (HIDDEN_PROTO
		    Argdecl,
		    TAGGED d1, TAGGED d2)
{
  TAGGED r1, b1, e1, r2, b2, e2;
  TAGGED *h, value;
  TAGGED *valuep = &value;
  
list_and_list:
  if (d1==EmptySet)
    goto null_and_list;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_list:
  if (d2==EmptySet)
    goto head_tail_and_null;
  r2 = CTagToCar(d2);  d2 = CTagToCdr(d2);
  b2 = RangeMin(r2);   e2 = RangeMax(r2);
  goto head_tail_and_head_tail;
list_and_head_tail:
  if (d1==EmptySet)
    goto null_and_head_tail;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_head_tail:
  switch_fd_interval_cmp(b1,e1,b2,e2,
			 /*BEFORE*/ {
                           EMIT_INTERVAL(h,b1,e1);
			   goto list_and_head_tail;
                         },
			 /*MEETS*/ {
                           b2 = b1;
			   goto list_and_head_tail;
                         },
			 /*OVERLAPS*/ {
			   b2 = b1;
			   goto list_and_head_tail;
			 },
			 /*FINISHED_BY*/ {
			   EMIT_INTERVAL(h,b1,e1);
			   goto list_and_list;
			 },
			 /*CONTAINS*/ goto head_tail_and_list;,
			 /*STARTS*/ goto list_and_head_tail;,
			 /*EQUALS*/ {
			   EMIT_INTERVAL(h,b1,e1);
			   goto list_and_list;
			 },
			 /*STARTED_BY*/ goto head_tail_and_list;,
			 /*DURING*/ goto list_and_head_tail;,
			 /*FINISHES*/ {
			   EMIT_INTERVAL(h,b2,e2);
			   goto list_and_list;
			 },
			 /*OVERLAPPED_BY*/ {
			   b1 = b2;
			   goto head_tail_and_list;
			 },
			 /*MET_BY*/ {
			   b1 = b2;
			   goto head_tail_and_list;
			 },
			 /*AFTER*/ {
			   EMIT_INTERVAL(h,b2,e2);
			   goto head_tail_and_list;
			 }
			 );
null_and_head_tail:
  EMIT_INTERVAL(h,b2,e2);
null_and_list:
  *valuep = d2;
  return value;
head_tail_and_null:
  EMIT_INTERVAL(h,b1,e1);
  *valuep = d1;
  return value;
}


TAGGED fd_union_interval MAGIC (HIDDEN_PROTO
			     Argdecl,
			     TAGGED d1, TAGGED b2, TAGGED e2)
{
  TAGGED r1, b1, e1;
  TAGGED *h, value;
  TAGGED *valuep = &value;
  
list:
  if (d1==EmptySet)
    goto null_and_head_tail;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
  switch_fd_interval_cmp(b1,e1,b2,e2,
			 /*BEFORE*/ {
                           EMIT_RANGE(h,r1);
			   goto list;
                         },
			 /*MEETS*/ {
                           b2 = b1;
			   goto list;
                         },
			 /*OVERLAPS*/ {
			   b2 = b1;
			   goto list;
			 },
			 /*FINISHED_BY*/ {
			   EMIT_RANGE(h,r1);
			   goto list_and_null;
			 },
			 /*CONTAINS*/ goto head_tail_and_null;,
			 /*STARTS*/ goto list;,
			 /*EQUALS*/ {
			   EMIT_RANGE(h,r1);
			   goto list_and_null;
			 },
			 /*STARTED_BY*/ goto head_tail_and_null;,
			 /*DURING*/ goto list;,
			 /*FINISHES*/ {
			   EMIT_INTERVAL(h,b2,e2);
			   goto list_and_null;
			 },
			 /*OVERLAPPED_BY*/ {
			   b1 = b2;
			   goto head_tail_and_null;
			 },
			 /*MET_BY*/ {
			   b1 = b2;
			   goto head_tail_and_null;
			 },
			 /*AFTER*/ {
			   EMIT_INTERVAL(h,b2,e2);
			   goto head_tail_and_null;
			 }
			 );
null_and_head_tail:
  EMIT_INTERVAL(h,b2,e2);
  *valuep = EmptySet;
  return value;
head_tail_and_null:
  EMIT_INTERVAL(h,b1,e1);
list_and_null:
  *valuep = d1;
  return value;
}


/* same as fd_merge_into(t1..t1,d2) */
TAGGED fd_insert_into MAGIC (HIDDEN_PROTO
			     Argdecl,
			     TAGGED t1, TAGGED d2)
{
  if (!fd_member(t1,d2))
    d2 = fd_union_interval(w,d2,t1,t1);

  return d2;
}

/* Comparing fdset P vith fdset Q:
   Let PnQ be the intersection.

   FDI_EQUAL    - PnQ==P, PnQ==Q
   FDI_SUBSET   - PnQ==P, PnQ!=Q
   FDI_SUPERSET - PnQ!=P, PnQ==Q
   FDI_DISJOINT - PnQ!=P, PnQ!=Q, PnQ==[]
   FDI_INTERSECT- PnQ!=P, PnQ!=Q, PnQ!=[]
*/

#define FDC_SUBSET 01
#define FDC_SUPERSET 02
#define FDC_INTERSECTION 04

int fd_compare(TAGGED d1, TAGGED d2)
{
  TAGGED r1, b1, e1, r2, b2, e2;
  unsigned int state = 0;
  
list_and_list:
  if (d1==EmptySet)
    goto null_and_list;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_list:
  if (d2==EmptySet)
    goto head_tail_and_null;
  r2 = CTagToCar(d2);  d2 = CTagToCdr(d2);
  b2 = RangeMin(r2);   e2 = RangeMax(r2);
  goto head_tail_and_head_tail;
list_and_head_tail:
  if (d1==EmptySet)
    goto null_and_head_tail;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
head_tail_and_head_tail:
  if (state == FDC_SUBSET+FDC_SUPERSET+FDC_INTERSECTION)
    return FDI_INTERSECT;
  switch_fd_interval_cmp(b1,e1,b2,e2,
			 /*BEFORE*/ {
                           state |= FDC_SUPERSET;
			   goto list_and_head_tail;
                         },
			 /*MEETS*/ {
                           state |= FDC_SUPERSET;
			   goto list_and_head_tail;
                         },
			 /*OVERLAPS*/ return FDI_INTERSECT;,
			 /*FINISHED_BY*/ {
			   state |= FDC_SUPERSET+FDC_INTERSECTION;
			   goto list_and_list;
			 },
			 /*CONTAINS*/ {
			   state |= FDC_SUPERSET+FDC_INTERSECTION;
			   goto head_tail_and_list;
			 },
			 /*STARTS*/ {
			   state |= FDC_SUBSET+FDC_INTERSECTION;
			   goto list_and_head_tail;
			 },
			 /*EQUALS*/ {
			   state |= FDC_INTERSECTION;
			   goto list_and_list;
			 },
			 /*STARTED_BY*/ {
			   state |= FDC_SUPERSET+FDC_INTERSECTION;
			   goto head_tail_and_list;
			 },
			 /*DURING*/ {
			   state |= FDC_SUBSET+FDC_INTERSECTION;
			   goto list_and_head_tail;
			 },
			 /*FINISHES*/ {
			   state |= FDC_SUBSET+FDC_INTERSECTION;
			   goto list_and_list;
			 },
			 /*OVERLAPPED_BY*/ return FDI_INTERSECT;,
			 /*MET_BY*/ {
			   state |= FDC_SUBSET;
			   goto head_tail_and_list;
			 },
			 /*AFTER*/ {
			   state |= FDC_SUBSET;
			   goto head_tail_and_list;
			 }
			 );
null_and_list:
  if (d2==EmptySet)
    goto ret;
null_and_head_tail:
  state |= FDC_SUBSET;
  goto ret;
head_tail_and_null:
  state |= FDC_SUPERSET;
ret:
  switch (state) {
  case 0:
  case FDC_INTERSECTION:
    return FDI_EQUAL;
  case FDC_SUBSET:
  case FDC_SUBSET+FDC_INTERSECTION:
    return FDI_SUBSET;
  case FDC_SUPERSET:
  case FDC_SUPERSET+FDC_INTERSECTION:
    return FDI_SUPERSET;
  case FDC_SUBSET+FDC_SUPERSET:
    return FDI_DISJOINT;
  default:
    return FDI_INTERSECT;
  }
}


int fd_compare_interval(TAGGED d1, TAGGED b2, TAGGED e2)
{
  TAGGED r1, b1, e1;
  unsigned int state = 0;
  
list:
  if (d1==EmptySet)
    goto null;
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b1 = RangeMin(r1);   e1 = RangeMax(r1);
  if (state == FDC_SUBSET+FDC_SUPERSET+FDC_INTERSECTION)
    return FDI_INTERSECT;
  switch_fd_interval_cmp(b1,e1,b2,e2,
			 /*BEFORE*/ {
                           state |= FDC_SUPERSET;
			   goto list;
                         },
			 /*MEETS*/ {
                           state |= FDC_SUPERSET;
			   goto list;
                         },
			 /*OVERLAPS*/ return FDI_INTERSECT;,
			 /*FINISHED_BY*/ {
			   state |= FDC_SUPERSET+FDC_INTERSECTION;
			   goto ret;
			 },
			 /*CONTAINS*/ {
			   state |= FDC_SUPERSET+FDC_INTERSECTION;
			   goto ret;
			 },
			 /*STARTS*/ {
			   state |= FDC_SUBSET+FDC_INTERSECTION;
			   goto list;
			 },
			 /*EQUALS*/ {
			   state |= FDC_INTERSECTION;
			   goto list_and_null;
			 },
			 /*STARTED_BY*/ {
			   state |= FDC_SUPERSET+FDC_INTERSECTION;
			   goto ret;
			 },
			 /*DURING*/ {
			   state |= FDC_SUBSET+FDC_INTERSECTION;
			   goto list;
			 },
			 /*FINISHES*/ {
			   state |= FDC_SUBSET+FDC_INTERSECTION;
			   goto list_and_null;
			 },
			 /*OVERLAPPED_BY*/ return FDI_INTERSECT;,
			 /*MET_BY*/ {
			   state |= FDC_SUBSET;
			   goto more_and_null;
			 },
			 /*AFTER*/ {
			   state |= FDC_SUBSET;
			   goto more_and_null;
			 }
			 );
null:
  state |= FDC_SUBSET;
  goto ret;
list_and_null:
  if (d1==EmptySet)
    goto ret;
more_and_null:
  state |= FDC_SUPERSET;
ret:
  switch (state) {
  case 0:
  case FDC_INTERSECTION:
    return FDI_EQUAL;
  case FDC_SUBSET:
  case FDC_SUBSET+FDC_INTERSECTION:
    return FDI_SUBSET;
  case FDC_SUPERSET:
  case FDC_SUPERSET+FDC_INTERSECTION:
    return FDI_SUPERSET;
  case FDC_SUBSET+FDC_SUPERSET:
    return FDI_DISJOINT;
  default:
    return FDI_INTERSECT;
  }
}

int fd_compare_intervals(TAGGED b1, TAGGED e1,
			 TAGGED b2, TAGGED e2)
{
  if (!TagIsSIN(b1))
    b1 = InfAsINT;	/* extend <,=,> to Inf and Sup */
  if (!TagIsSIN(b2))
    b2 = InfAsINT;
  if (!TagIsSIN(e1))
    e1 = SupAsINT;
  if (!TagIsSIN(e2))
    e2 = SupAsINT;
  if (Tlt(e1,b2) || Tlt(e2,b1))
    return FDI_DISJOINT;
  else if (Teq(b1,b2)) {
    if (Tlt(e1,e2))
      return FDI_SUPERSET;
    else if (Tgt(e1,e2))
      return FDI_SUBSET;
    else
      return FDI_EQUAL;
  } else if (Teq(e1,e2)) {
    if (Tlt(b1,b2))
      return FDI_SUPERSET;
    else
      return FDI_SUBSET;
  } else
    return FDI_INTERSECT;
}


TAGGED fd_merge_into MAGIC (HIDDEN_PROTO
			    Argdecl,
			    TAGGED d1, TAGGED d2)
{
  switch (fd_compare(d1,d2)) {
  case FDI_DISJOINT:
  case FDI_INTERSECT:
  case FDI_SUPERSET:
    d2 = fd_union(w,d1,d2);
  }
  return d2;
}

/*** destructive variant of fd_union ***/

#define IntervalGt(a,b) FDgt(RangeMin(CTagToCar(a)),RangeMin(CTagToCar(b)))

#define IntervalCompare(a,b)											\
fd_interval_cmp(RangeMin(CTagToCar(a)),RangeMax(CTagToCar(a)),RangeMin(CTagToCar(b)),RangeMax(CTagToCar(b)))

static TAGGED fd_union_run(TAGGED *ap)
{
  TAGGED list = *ap;
  TAGGED c = list;
  TAGGED a = CTagToCdr(list);

  while (a != atom_nil) {
    switch (IntervalCompare(c,a)) {
    case FD_BEFORE:
      CTagToCdr(c) = a;
      c = a;
      a = CTagToCdr(a);
      break;
    case FD_MEETS:
    case FD_OVERLAPS:
    case FD_STARTS:
      RangeMax(CTagToCar(c)) = RangeMax(CTagToCar(a));
    case FD_FINISHED_BY:
    case FD_CONTAINS:
    case FD_EQUALS:
    case FD_STARTED_BY:
      a = CTagToCdr(a);
      break;
    case FD_DURING:
    case FD_FINISHES:
    case FD_OVERLAPPED_BY:
    case FD_MET_BY:
    case FD_AFTER:
      goto out;
    }
  }
 out:
  CTagToCdr(c) = atom_nil;
  *ap = a;
  return list;
}

TAGGED fd_union_dest(TAGGED a, TAGGED b)
{
  TAGGED t, u, v;

  if (a == atom_nil)
    return b;
  if (b == atom_nil)
    return a;
  if (IntervalGt(a,b)) {
    t = b;
    b = CTagToCdr(b);
  } else {
    t = a;
    a = CTagToCdr(a);
  }
  u = t;

  while (a != atom_nil && b != atom_nil) {
    if (IntervalGt(a,b)) {
      v = b;
      b = CTagToCdr(b);
    } else {
      v = a;
      a = CTagToCdr(a);
    }
    switch (IntervalCompare(t,v)) {
    case FD_BEFORE:
      CTagToCdr(t) = v;
      t = v;
      break;
    case FD_MEETS:
    case FD_OVERLAPS:
    case FD_STARTS:
      RangeMax(CTagToCar(t)) = RangeMax(CTagToCar(v));
    default:
      break;
    }
  }
  a = (b == atom_nil ? a : b);
  while (a != atom_nil) {
    v = a;
    a = CTagToCdr(a);
    switch (IntervalCompare(t,v)) {
    case FD_BEFORE:
      CTagToCdr(t) = v;
      return u;
    case FD_MEETS:
    case FD_OVERLAPS:
    case FD_STARTS:
      RangeMax(CTagToCar(t)) = RangeMax(CTagToCar(v));
    default:
      break;
    }
  }
  CTagToCdr(t) = atom_nil;
  return u;
}

TAGGED fd_union_sort(TAGGED list)
{
  TAGGED run;
  int depth=0, count=0, c;
  TAGGED stack[32];	

  if (list == atom_nil)
    return atom_nil;
  while (list != atom_nil) {
    run = fd_union_run(&list);
    count++;
    for (c = count; depth > 0 && !(c&1); c >>= 1) {
      run = fd_union_dest(stack[--depth], run);
    }
    stack[depth++] = run;
  }
  run = stack[--depth];
  while (depth > 0)
    run = fd_union_dest(stack[--depth], run);

  return run;
}



/*** support for iterators ***/

void
fditer_init(FDITER *it, TAGGED d)
{
  TAGGED r;
  
  if (d==EmptySet)
    it->cur = SupAsINT;
  else {
    r = CTagToCar(d);
    it->cur = RangeMin(r);
    it->max = RangeMax(r);
    it->tail = CTagToCdr(d);
  }
}


TAGGED
fditer_next(FDITER *it)
{
  TAGGED next = it->cur;
  TAGGED d = it->tail;
  TAGGED r;

  if (Tlt(it->cur,it->max))
    it->cur = FDincr(next);
  else if (d==EmptySet)
    it->cur = SupAsINT;
  else {
    r = CTagToCar(d);
    it->cur = RangeMin(r);
    it->max = RangeMax(r);
    it->tail = CTagToCdr(d);
  }

  return next;
}

/* ensure next elt will be > t */
void
fditer_skip(FDITER *it, TAGGED t)
{
  TAGGED d, r;

 start:
  d = it->tail;
  if (Tlt(t,it->cur))
    ;				/* already skipped */
  else if (Tlt(t,it->max))
    it->cur = FDincr(t);
  else if (d==EmptySet)
    it->cur = SupAsINT;
  else {
    r = CTagToCar(d);
    it->cur = RangeMin(r);
    it->max = RangeMax(r);
    it->tail = CTagToCdr(d);
    goto start;
  }
}

/*** constructors ***/

void
fdcons_init(FDCONS *cons)
{
  cons->head = EmptySet;
  cons->cur = 0;
  cons->size = 0;
}

void
fdcons_add MAGIC (HIDDEN_PROTO
		  FDCONS *cons, Argdecl, TAGGED t)
{
  TAGGED *h = TagToLST(cons->cur);
  
  cons->size++;
  if (cons->cur && h[3] == FDdecr(t))
    h[3] = t;
  else {
    TAGGED *valuep = cons->cur ? h+1 : &cons->head;
    
    EMIT_INTERVAL(h,t,t);
    h[1] = EmptySet;
    cons->cur = MakeList(h);
  }
}

void
fdcons_add_interval MAGIC (HIDDEN_PROTO
			   FDCONS *cons, Argdecl, TAGGED t, TAGGED u)
{
  TAGGED *h = TagToLST(cons->cur);
  
  cons->size += SIZEOF_INTERVAL(t,u);
  if (cons->cur && h[3] == FDdecr(t))
    h[3] = u;
  else {
    TAGGED *valuep = cons->cur ? h+1 : &cons->head;
    
    EMIT_INTERVAL(h,t,u);
    h[1] = EmptySet;
    cons->cur = MakeList(h);
  }
}


/*** predicates ***/

/* $fd_size(+Set, -Size) :-
   Set is a valid FD set with size Size, which may be infinite.
   */
long SPCDECL
prolog_fd_size MAGIC (HIDDEN_PROTO
		      SP_term_ref SetR,
		      SP_term_ref SizeR)
{
  WAMENV;
  TAGGED set = RefTerm(SetR);
  TAGGED size = TaggedZero;
  TAGGED range, a, b, blast = Inf;

  DerefSwitch(set,return FALSE;);
  while (TagIsLST(set) && blast!=Sup) {
    range = CTagToCar(set);
    set = CTagToCdr(set);
    if (!TagIsLST(range))
      return FALSE;
    a = RangeMin(range);
    b = RangeMax(range);
    if (AreSmall(a,b)) {
      if ((TagIsSmall(blast) && Tge(blast,a)) || Tgt(a,b))
	return FALSE;
      blast = FDincr(b);
      if (TagIsSmall(size)) {
	TAGGED delta = (blast-a)+TaggedZero;
	TADDCHK(size,delta);
      }
    } else if (TagIsSmall(b)) {
      if (Tnez(size) || a!=Inf)
	return FALSE;
      size = Sup;
      blast = FDincr(b);
    } else if (TagIsSmall(a)) {
      if ((TagIsSmall(blast) && Tge(blast,a)) || b!=Sup)
	return FALSE;
      size = Sup;
      blast = Sup;
    } else {
      if (Tnez(size) || a!=Inf || b!=Sup)
	return FALSE;
      size = Sup;
      blast = Sup;
    }
  }
  if (set!=EmptySet)
    return FALSE;
  RefTerm(SizeR) = size;
  return TRUE;
}

/* $fd_range(+A, +B, -Set) :-
   Set is the FD set representing the interval A..B
   (which may be empty)
   */
long SPCDECL
prolog_fd_range MAGIC (HIDDEN_PROTO
		       SP_term_ref AR,
		       SP_term_ref BR,
		       SP_term_ref SetR)
{
  WAMENV;
  TAGGED a, b, *h, range=EmptySet;

  a = RefTerm(AR);
  b = RefTerm(BR);
  DerefSwitch(a,return FALSE;);
  DerefSwitch(b,return FALSE;);
  if ((!TagIsSmall(a) && a!=Inf && a!=Sup) ||
      (!TagIsSmall(b) && b!=Inf && b!=Sup))
    return FALSE;
  if (!EmptyInterval(a,b)) {
    h = w->global_top;
    range = MakeList(h);
    *h++ = range+WD(2);
    *h++ = EmptySet;
    *h++ = a;
    *h++ = b;
    w->global_top = h;
  }
  RefTerm(SetR) = range;
  return TRUE;
}

/* $fd_cons(+A, +B, +Rest, -Set) :-
   Set is the FD set representing the interval union(A..B,Rest)
   and A..B is non-empty and all elements of Rest are greater than B+1.
   */
long SPCDECL
prolog_fd_cons MAGIC (HIDDEN_PROTO
		      SP_term_ref AR,
		      SP_term_ref BR,
		      SP_term_ref RestR,
		      SP_term_ref SetR)
{
  WAMENV;
  TAGGED a, b, rest, *h, set;

  a = RefTerm(AR);
  b = RefTerm(BR);
  rest = RefTerm(RestR);
  DerefSwitch(a,return FALSE;);
  DerefSwitch(b,return FALSE;);
  DerefNonvar(rest);
  if (  (!TagIsSmall(a) && a!=Inf) || (!TagIsSmall(b) && b!=Sup)
     || EmptyIntervalSafe(a,b)
     || (rest!=EmptySet && Tge(FDincr(b),RangeMin(CTagToCar(rest)))))
    return FALSE;
  h = w->global_top;
  set = MakeList(h);
  *h++ = set+WD(2);
  *h++ = rest;
  *h++ = a;
  *h++ = b;
  w->global_top = h;
  RefTerm(SetR) = set;
  return TRUE;
}

/* $fd_dom_complement(+D1, -D) */
void SPCDECL
prolog_fd_dom_complement MAGIC (HIDDEN_PROTO
				SP_term_ref D1R,
				SP_term_ref DR)
{
  WAMENV;
  TAGGED d1;

  d1 = RefTerm(D1R);
  DerefNonvar(d1);
  w->numstack_end = NULL;
  fd.fd_overflow = FALSE;
  RefTerm(DR) = fd_globalize(w,fd_localize(w,fd_complement(w,d1)),3,2);
  if (fd.fd_overflow) {
    TAGGED *h = w->global_top;
    TAGGED goal = MakeStructure(h);
    *h++ = SetArity(SP_atom_from_string("fdset_complement"),2);
    Load0HVA(h);
    Load0HVA(h);
    w->global_top = h;
    if (!fd_check_overflow(goal))
      SP_fail();
  }
}


/* $fd_dom_subtract(+D1, +D2, -D) */
void SPCDECL
prolog_fd_dom_subtract MAGIC (HIDDEN_PROTO
			      SP_term_ref D1R,
			      SP_term_ref D2R,
			      SP_term_ref DR)
{
  WAMENV;
  TAGGED d1, d2;
  
  d1 = RefTerm(D1R);  
  d2 = RefTerm(D2R);  
  DerefNonvar(d1);
  DerefNonvar(d2);
  w->numstack_end = NULL;
  RefTerm(DR) = fd_globalize(w,fd_localize(w,fd_subtract(w,d1,d2)),0,3);
}


/* $fd_dom_intersection(+D1, +D2, -D) */
void SPCDECL
prolog_fd_dom_intersection MAGIC (HIDDEN_PROTO
				  SP_term_ref D1R,
				  SP_term_ref D2R,
				  SP_term_ref DR)
{
  WAMENV;
  TAGGED d1, d2;
  
  d1 = RefTerm(D1R);  
  d2 = RefTerm(D2R);  
  DerefNonvar(d1);
  DerefNonvar(d2);
  w->numstack_end = NULL;
  RefTerm(DR) = fd_globalize(w,fd_localize(w,fd_intersection(w,d1,d2)),0,3);
}


/* $fd_dom_union(+D1, +D2, -D) */
void SPCDECL
prolog_fd_dom_union MAGIC (HIDDEN_PROTO
			   SP_term_ref D1R,
			   SP_term_ref D2R,
			   SP_term_ref DR)
{
  WAMENV;
  TAGGED d1, d2;
  
  d1 = RefTerm(D1R);  
  d2 = RefTerm(D2R);  
  DerefNonvar(d1);
  DerefNonvar(d2);
  w->numstack_end = NULL;
  RefTerm(DR) = fd_globalize(w,fd_localize(w,fd_union(w,d1,d2)),0,3);
}


void SPCDECL
prolog_fd_dom_contains MAGIC (HIDDEN_PROTO
			      SP_term_ref D1R,
			      SP_term_ref D2R)
{
  WAMENV;
  TAGGED d = RefTerm(D1R);
  TAGGED t = RefTerm(D2R);
  TAGGED r;

  DerefNonvar(t);
  DerefNonvar(d);
  if (d==atom_nil)
    SP_fail();
  else {
    r = CTagToCar(d), d = CTagToCdr(d);
    while (d!=atom_nil && val_vs_range(t,r)==CMP_AFTER)
      r = CTagToCar(d), d = CTagToCdr(d);
    if (val_vs_range(t,r)!=CMP_INSIDE)
      SP_fail();
  }
}


/* $fd_dom_insert(+D1, +Elt, -D) */
void SPCDECL
prolog_fd_dom_insert MAGIC (HIDDEN_PROTO
			    SP_term_ref D1R,
			    SP_term_ref D2R,
			    SP_term_ref DR)
{
  WAMENV;
  TAGGED d1, d2;
  
  d1 = RefTerm(D1R);  
  d2 = RefTerm(D2R);  
  DerefNonvar(d1);
  DerefNonvar(d2);
  w->numstack_end = NULL;
  RefTerm(DR) = fd_globalize(w,fd_localize(w,fd_insert_into(w,d2,d1)),0,3);
}


/* $fd_dom_delete(+D1, +Elt, -D) */
void SPCDECL
prolog_fd_dom_delete MAGIC (HIDDEN_PROTO
			    SP_term_ref D1R,
			    SP_term_ref D2R,
			    SP_term_ref DR)
{
  WAMENV;
  TAGGED d1, d2;
  
  d1 = RefTerm(D1R);  
  d2 = RefTerm(D2R);  
  DerefNonvar(d1);
  DerefNonvar(d2);
  w->numstack_end = NULL;
  RefTerm(DR) = fd_globalize(w,fd_localize(w,fd_delete(w,d1,d2)),0,3);
}


/* $fd_dom_intersect(+D1, +D2, -Value), 1=disjoint, 2=subset, 3=intersect */
long SPCDECL
prolog_fd_dom_intersect MAGIC (HIDDEN_PROTO
			       SP_term_ref D1R,
			       SP_term_ref D2R)
{
  TAGGED d1, d2;
  WAMENV;
  
  d1 = RefTerm(D1R);  
  d2 = RefTerm(D2R);  
  DerefNonvar(d1);
  DerefNonvar(d2);
  switch (fd_compare(d1,d2)) {
  case FDI_DISJOINT:
    return 1;
  case FDI_SUBSET:
  case FDI_EQUAL:
    return 2;
  default:
    return 3;
  }
}


/* $fd_negate(+D1, +Const, -D2) :- D2 is Const-D1 (pointwise difference) */
void SPCDECL
prolog_fd_negate MAGIC (HIDDEN_PROTO
			SP_term_ref D1R,
			SP_term_ref D2R,
			SP_term_ref DR)
{
  TAGGED t1, d1, d2;
  WAMENV;
  
  d1 = RefTerm(D1R);  
  d2 = RefTerm(D2R);  
  DerefNonvar(d1);
  DerefNonvar(d2);
  w->numstack_end = NULL;
  *fd_neg_internal(w,d1,d2,&t1) = EmptySet;
  RefTerm(DR) = fd_globalize(w,t1,0,3);
}


