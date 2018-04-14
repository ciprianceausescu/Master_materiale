/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "datadefs.h"
#include "support.h"
#include "predtyp.h"
#include "clpfd_glue.h"         /* [PM] 3.9b4 splfr-generated   */

#ifdef sp_make_integer          /* [PM] 3.10.1 */
#ifdef make_integer
#error "Did not expect make_integer to be #defined here/PM"
#endif /* make_integer */
/* make_integer (as used by, e.g., MakeInteger) is
   no longer exported from the support.h API. Instead use the new
   sp_make_integer with identical functionality. */
#define make_integer sp_make_integer
#endif /* sp_make_integer */

/* [PM] 3.9.2b1 SPRM 3610
   FD_CONSTRAINT predicates need to be destructed using free_fd_info.

   Prior to 3.9.2b1 the pointer to the destructed was kept with the
   FD_CONSTRAINT predicate. However, the pointer was invalid when used
   by prolog_restore to undefine all predicates since the clpfd
   resource had by then been unloaded. It "used to work" because the
   clpfd resource (shared object) apparently always ended up at the
   same address when re-loaded.

   The bug surfaced when the SPEnv stored in the clpfd resource
   descriptor got munged so that free_fd_info got a segfault trying to
   use the SP API dispatch table.

   The fix is to tell SP about the destructor separately (using
   SP_install_fd_hooks). This will also save two words for each
   indexical_info struct.

   The fix is enabled by setting SP_FD_GLOBAL_DESTRUCTOR to 1 (see
   support.h). It should be unconditionally enabled eventually.

 */


#define MAGIC                   /* empty */

#if MULTI_SP_AWARE
#if !ENABLE_CLPFD_MULTI_SP
#error "Inconsistency. SICStus must be compiled with ENABLE_CLPFD_MULTI_SP to support clpfd with MULTI_SP_AWARE/PM"
#endif

#if !SP_FOREIGN_SPENV
#error "Multi SP clpfd only supports SPEnv-style dispatch argument"
#endif /* !SP_FOREIGN_SPENV */
/* 
   [PM] 3.9b4 The following outlines a method for passing the hidded
   multi SP context argument. This method requres that all procedures
   use prototypes.

   The method has the advantage that you do not have to modify the
   actual code, you just wrap each function body with a sequence of
   defines. Another advantage at least when debugging is that all
   procedures retain their name, alternatively you can introduce name
   mangling similar to SP_MANGLE to ensure link-time consistency
   checking. Another advantage is that modifying the original sources
   to support this method can be done with emacs
   query-replace-regexp/keyboard macros and so is not very
   error-prone. The method is is also easily maintainable and CVS
   merge will work between older code that does not use the new
   defines and new code that does (since the meat of the procedure
   bodies are unaffected.) A disadvantage is that you have to wrap
   each function body in an ugly sequence of defines.  (This could be
   made much nicer if a more powerful macro processor us used, sed
   would do.).

   Except for foreign predicates, each static or extern procedure is
   redefined to take a HIDDEN_ARG. When in a prototype we let
   HIDDEN_ARG expand to a declaration of the extra argument, when in a
   procedure body we let HIDDEN_ARG expand to the name of the extra
   argument. The foreign predicate procedures are name mangled in the
   splfr-generated glue header and must be treated specially.
   
   As an example assuming a function int foo (long, double):

   define foo(A1,A2) foo(HIDDEN_ARG, A1, A2)

   For all foreign procedures and for all procedures with a #define
   above you should:

   Ensure that when the procedure prototype appears (either
   declaration or part of the definition) then the following two
   defines must be in effect.

      #define HIDDEN_ARG HIDDEN_ARG_PROTO

   The prototyped definition
   static int foo(long x, double y) {... }
   then expands to:
   static int foo (HIDDEN_ARG_PROTO, long x, double y) {...}"

   When inside a procedure body the following two defines must be in
   effect:

      #define HIDDEN_ARG HIDDEN_ARG_NAME

   This can be achived by wrapping each body
   { // procedure body start
      ... the code of the body
   } // procedure body end
   as
   { // procedure body start
      #undef HIDDEN_ARG
      #define HIDDEN_ARG HIDDEN_ARG_NAME

      ... // ordinary body

      #undef HIDDEN_ARG
      #define HIDDEN_ARG HIDDEN_ARG_PROTO
   } // procedure body end

   A call foo(43, 3.14) in the body then expands to foo(HIDDEN_ARG_NAME, 3, 3.14).

   MC expressed dislike towards the pairs
   #undef HIDDEN_ARG
   #define HIDDEN_ARG ... that wrap every function body.

   so we use a different technique. It uses some of the
   less well known aspects of the C pre-processor. The key are the
   following defines that holds throughout the file.
*/ 

#define HIDDEN_ARG_PROTO SPAPI_ARG_PROTO_DECL0
#define HIDDEN_ARG_NAME SPAPI_ARG0
#define HIDDEN_ARG_COMMA HIDDEN_ARG,
#define HIDDEN_ARG_OR_ZERO HIDDEN_ARG
#define HIDDEN_ARG HIDDEN_ARG_NAME
#define HIDDEN_PROTO HIDDEN_ARG_PROTO, /* for arity >=1 */
#define HIDDEN_PROTO_VOID HIDDEN_ARG_PROTO /* for arity zero, e.g., foo(void) */

/* When the SPEnv* does not come as an extra argument (for instance,
   when it is hidden inside some other data) you need to extract the
   SPEnv and make the appropriate variable point to the SPEnv so that
   the clpfd global state and the SICStus API functions can be
   accessed.

  Use this before accessing any struct fd global state and before
  doing any callbacks to SICStus. See alldiff_destructor for an
  example.

  Currently FD_SETUP_SPENV are used by the destructor methods. The
  destructors are all (except struct indexical_info.destructor_fun)
  called with a pointer to the memory block they should destruct. The
  memory block contains a pointer to the SPEnv. All these destructors
  are called from '$free'/1 (a.k.a. prolog_free()). The special case
  is indexical_info.destructor_fun that is called from
  Emulator/indexing.c scan_info() with a pointer to a pointer to the
  memory block.

  FD_SETUP_SPENV(<a pointer to an SPEnv>) should not be followed by a
  semicolon (it will be harmless unless there are more declarations
  following it).
  
*/
#define FD_SETUP_SPENV(SPENV) SPAPI_ARG_PROTO_DECL0=(SPENV); /* Requires SP_FOREIGN_SPENV */
/* use this to initialize the field in the memory block, e.g. FD_STORE_SPENV(foo->spenv); */
#define FD_STORE_SPENV(SPENV) (SPENV)=HIDDEN_ARG


#else  /* !MULTI_SP_AWARE */

#define FD_SETUP_SPENV(SPENV)
#define FD_STORE_SPENV(SPENV)

#define HIDDEN_ARG
#define HIDDEN_ARG_COMMA
#define HIDDEN_ARG_OR_ZERO 0
#define HIDDEN_PROTO /* empty, for arity >=1 */
#define HIDDEN_PROTO_VOID void /* for arity zero, e.g., foo(void) */

#endif /* !MULTI_SP_AWARE */

#if MULTI_SP_AWARE
/* redefines for all extern procedures */
/* fdsets.c */
#define fd_first_and_rest(A1,A2,A3,A4) fd_first_and_rest(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_localize(A1,A2) fd_localize(HIDDEN_ARG, A1,A2)
#define fd_interval(A1,A2,A3) fd_interval(HIDDEN_ARG, A1,A2,A3)
#define fd_pair(A1,A2,A3) fd_pair(HIDDEN_ARG, A1,A2,A3)
#define fd_compl_interval(A1,A2,A3) fd_compl_interval(HIDDEN_ARG, A1,A2,A3)
#define fd_lsh(A1,A2,A3) fd_lsh(HIDDEN_ARG, A1,A2,A3)
#define fd_neg_internal(A1,A2,A3,A4) fd_neg_internal(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_neg_offset(A1,A2,A3) fd_neg_offset(HIDDEN_ARG, A1,A2,A3)
#define fd_subtract(A1,A2,A3) fd_subtract(HIDDEN_ARG, A1,A2,A3)
#define fd_interval_subtract(A1,A2,A3,A4) fd_interval_subtract(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_subtract_interval(A1,A2,A3,A4) fd_subtract_interval(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_complement(A1,A2) fd_complement(HIDDEN_ARG, A1,A2)
#define fd_delete(A1,A2,A3) fd_delete(HIDDEN_ARG, A1,A2,A3)
#define fd_intersection(A1,A2,A3) fd_intersection(HIDDEN_ARG, A1,A2,A3)
#define fd_intersection_interval(A1,A2,A3,A4) fd_intersection_interval(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_union(A1,A2,A3) fd_union(HIDDEN_ARG, A1,A2,A3)
#define fd_union_interval(A1,A2,A3,A4) fd_union_interval(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_insert_into(A1,A2,A3) fd_insert_into(HIDDEN_ARG, A1,A2,A3)
#define fd_merge_into(A1,A2,A3) fd_merge_into(HIDDEN_ARG, A1,A2,A3)
#define fdcons_add(A1,A2,A3) fdcons_add(HIDDEN_ARG, A1,A2,A3)
#define fdcons_add_interval(A1,A2,A3,A4) fdcons_add_interval(HIDDEN_ARG, A1,A2,A3,A4)
/* indexical.c */
#define fd_tell_value(A2,A3) fd_tell_value(HIDDEN_ARG, A2,A3)
#define fd_tell_interval(A2,A3,A4,A5) fd_tell_interval(HIDDEN_ARG, A2,A3,A4,A5)
#define fd_tell(A2,A3) fd_tell(HIDDEN_ARG, A2,A3)
#define fd_tell_unsafe(A2,A3) fd_tell_unsafe(HIDDEN_ARG, A2,A3)
#define fd_told(A2,A3,A4) fd_told(HIDDEN_ARG, A2,A3,A4)
/* profile.c */
#define init_profile() init_profile(HIDDEN_ARG)
#define profile_cons(A1,A2,A3,A4,A5) profile_cons(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define profile_dispose(A1) profile_dispose(HIDDEN_ARG, A1)
#define profile_update(A1,A2,A3,A4,A5) profile_update(HIDDEN_ARG, A1,A2,A3,A4,A5)
/* support.c */
#define check_argument(A1,A2,A3,A4,A5) check_argument(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define fd_check_overflow(A1) fd_check_overflow(HIDDEN_ARG, A1)
#define fd_globalize(A1,A2,A3,A4) fd_globalize(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_globalize_unsafe(A1,A2,A3,A4) fd_globalize_unsafe(HIDDEN_ARG, A1,A2,A3,A4)
#define update_mutable(A1,A2,A3) update_mutable(HIDDEN_ARG, A1,A2,A3)
#define fd_link(A1,A2,A3,A4) fd_link(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_sync(A1) fd_sync(HIDDEN_ARG, A1)
#define fd_end(A1) fd_end(HIDDEN_ARG, A1)
#define fd_dealloc() fd_dealloc(HIDDEN_ARG)
#define daemon_copy_state(A1,A2) daemon_copy_state(HIDDEN_ARG, A1,A2)
#define fd_enqueue_general(A1,A2,A3) fd_enqueue_general(HIDDEN_ARG, A1,A2,A3)
#define fd_enqueue_global(A1,A2,A3) fd_enqueue_global(HIDDEN_ARG, A1,A2,A3)
#define fd_dequeue(A1,A2) fd_dequeue(HIDDEN_ARG, A1,A2)
#define fd_enqueue_all(A1,A2,A3,A4) fd_enqueue_all(HIDDEN_ARG, A1,A2,A3,A4)
#define fd_perm_alloc(A1,A2,A3) fd_perm_alloc(HIDDEN_ARG, A1,A2,A3)
#define fd_perm_free(A1) fd_perm_free(HIDDEN_ARG, A1)
#define unify_output_state(A1,A2,A3,A4) unify_output_state(HIDDEN_ARG, A1,A2,A3,A4)
#define qsort_asc_long(A1,A2) qsort_asc_long(HIDDEN_ARG, A1,A2)
#define fd_malloc(A1) fd_malloc(HIDDEN_ARG, A1)
#endif /* MULTI_SP_AWARE */

extern void *foo (Argdecl,int,TAGGED);

/* new persistent storage allocation */
/* Handle is a term [Flag | '$free'(Ptr)] */
#define Palloc(What,Extra,Handle) ((What *)fd_perm_alloc(w,sizeof(What)+(Extra),Handle))

#define Pdata(What,Handle) ((What *)fd_perm_data(Handle))

#define Pfree fd_perm_free(w)

#define Pcommitted(Handle) (TagToLST(Handle) >= GLOBAL_UNCOND)

extern void *fd_perm_alloc MAGIC (HIDDEN_PROTO Argdecl,int,TAGGED);
extern void *fd_perm_data(TAGGED);
extern void fd_perm_free MAGIC (HIDDEN_PROTO Argdecl);

extern TAGGED
unify_output_state MAGIC (HIDDEN_PROTO Argdecl, TAGGED *phandle, long *pstamp, SP_BOOL *pcommitted);

extern TAGGED
static_output_state(Argdecl, TAGGED *phandle, SP_BOOL *pcommitted);

extern int
list_length(TAGGED tvec);

extern void
get_var_and_attr(TAGGED term, SP_globref ref);

/* [PM] 4.0 Like SP_malloc but does MEMORY_FAULT instead of returning NULL on out-of-memory */
extern void *
fd_malloc MAGIC (HIDDEN_PROTO size_t size);

/* temporary storage allocation */
#define Malloc(N,What) ((What *)fd_malloc((N)*sizeof(What)))
#define Free(Ptr) SP_free(Ptr)

#define EVAL_ARITY 3

#define EmptySet atom_nil

#define Inf atom_inf

#define Sup atom_sup

/* something smaller than all smallints */
#define InfAsINT (TaggedLow-1)

/* something greater than all smallints */
#define SupAsINT (TaggedHigh+1)

#define CLPFD_MAXINT  ((long)(((TAGGED)(-1L)) >> 1))
#define CLPFD_MAXINT2 ((long)(((TAGGED)(-1L)) >> 2))

/* an FD Set is [First|Rest] */

/* a range is [Min|Max] */
#define RangeMin(X) CTagToCar(X)
#define RangeMax(X) CTagToCdr(X)

/* a domain is dom(Set,Min,Max,Size) */
#define DomainSet(D) CTagToArg(D,1)
#define DomainMin(D) CTagToArg(D,2)
#define DomainMax(D) CTagToArg(D,3)
#define DomainSize(D) CTagToArg(D,4)

#define FDdecr(T) ((T)!=TaggedLow  ? (T)-IStep(1) : Inf)

#define FDincr(T) ((T)!=TaggedHigh ? (T)+IStep(1) : Sup)

/* A can't be Sup and B can't be Inf */
#define EmptyIntervalSafe(A,B) (Tgt(A,B) && AreSmall(A,B))

#define EmptyInterval(A,B) \
(EmptyIntervalSafe(A,B) || (A)==Sup || (B)==Inf)

#define InInterval(X,A,B) \
(!FDgt(A,X) && !FDgt(X,B))

#define FDlt(A,B) EmptyInterval(B,A)
#define FDgt(A,B) EmptyInterval(A,B)
#define FDle(A,B) (!FDgt(A,B))
#define FDge(A,B) (!FDlt(A,B))

#define   CMP(U,V) ((U)<(V) ? -1 : (U)>(V) ? 1 : 0)
#define  TCMP(U,V) (Tlt(U,V) ? -1 : Tgt(U,V) ? 1 : 0)
#define FDCMP(U,V) (FDlt(U,V) ? -1 : FDgt(U,V) ? 1 : 0)

#define DerefNonvar(X) \
{while (IsVar(X)) (X) = CTagToPointer(X);}

#define CMP_BEFORE -1
#define CMP_INSIDE 0
#define CMP_AFTER  1


/*	 v(1,0,'$mutable'(dom([[inf|sup]],inf,sup,sup),0),
	       '$mutable'(lists(0,0,[],[],[],[],[]),0))
*/
#define FD_ATTR_DOM_OFFSET 5
#define FD_ATTR_SUSPS_OFFSET 8
#define FD_ATTR_MIN_OFFSET 13
#define FD_ATTR_MAX_OFFSET 14
#define FD_ATTR_SIZE_OFFSET 15
#define FD_ATTR_V_ARITY 4

/* support unification of dvars */
#define RefMutable(Mut) CTagToArg((Mut),1)

#define AttrToSuspM(Attr,To)				\
{							\
  TAGGED m_prev = (Attr);				\
  (To)=RefMutable(CTagToArg(m_prev,3));			\
  while (TagToHeadfunctor(To)==functor_v4) {		\
    m_prev=(To); (To)=RefMutable(CTagToArg(m_prev,3));	\
  }							\
  (To)=CTagToArg(m_prev,4);				\
}

#define AttrToDomM(Attr,To)				\
{							\
  TAGGED m_prev = (Attr);				\
  (To)=RefMutable(CTagToArg(m_prev,3));			\
  while (TagToHeadfunctor(To)==functor_v4) {		\
    m_prev=(To); (To)=RefMutable(CTagToArg(m_prev,3));	\
  }							\
  (To)=CTagToArg(m_prev,3);				\
}

#define DerefAttribute(To,Attr)			\
{						\
  (To)=RefMutable(CTagToArg((Attr),3));		\
  while (TagToHeadfunctor(To)==functor_v4) {	\
    (To)=RefMutable(CTagToArg((To),3));		\
  }						\
}

/* suspension mask bits */
#define MASK_DOM 0x1
#define MASK_MIN 0x2
#define MASK_MAX 0x4
#define MASK_MINMAX 0x8
#define MASK_VAL 0x10
#define MASK_SINGLETON 0x20

/* What is the relation between two FD sets. */
/* N.B. idiom <= FDI_SUPERSET captures "equal or superset"! */
#define FDI_EQUAL 1
#define FDI_SUPERSET 2
#define FDI_SUBSET 3
#define FDI_DISJOINT 4
#define FDI_INTERSECT 5

/* The temporal relations between two intervals. */
#define FD_BEFORE 0
#define FD_MEETS 1
#define FD_OVERLAPS 2
#define FD_FINISHED_BY 3
#define FD_CONTAINS 4
#define FD_STARTS 5
#define FD_EQUALS 6
#define FD_STARTED_BY 7
#define FD_DURING 8
#define FD_FINISHES 9
#define FD_OVERLAPPED_BY 10
#define FD_MET_BY 11
#define FD_AFTER 12

#define FD_INLINE 1

#if FD_INLINE

#define switch_fd_interval_cmp(B1,E1,B2,E2,				\
			       CASE_BEFORE,				\
			       CASE_MEETS,				\
			       CASE_OVERLAPS,				\
			       CASE_FINISHED_BY,			\
			       CASE_CONTAINS,				\
			       CASE_STARTS,				\
			       CASE_EQUALS,				\
			       CASE_STARTED_BY,				\
			       CASE_DURING,				\
			       CASE_FINISHES,				\
			       CASE_OVERLAPPED_BY,			\
			       CASE_MET_BY,				\
			       CASE_AFTER)				\
{									\
  TAGGED m_b1 = (B1);							\
  TAGGED m_e1 = (E1);							\
  TAGGED m_b2 = (B2);							\
  TAGGED m_e2 = (E2);							\
  if (m_b1==Inf)							\
    m_b1 = InfAsINT;							\
  if (m_e1==Sup)							\
    m_e1 = SupAsINT;							\
  else									\
    m_e1 += IStep(1);		/* want [m_b1,m_e1), [m_b2,m_e2) */	\
  if (m_b2==Inf)							\
    m_b2 = InfAsINT;							\
  if (m_e2==Sup)							\
    m_e2 = SupAsINT;							\
  else									\
    m_e2 += IStep(1);		/* want [m_b1,m_e1), [m_b2,m_e2) */	\
  if (Tlt(m_b1,m_b2)) {							\
    if (Tle(m_e1,m_b2)) {						\
      if (Teq(m_e1,m_b2)) {						\
	CASE_MEETS;							\
      } else {								\
	CASE_BEFORE;							\
      }									\
    } else if (Tlt(m_e1,m_e2)) {					\
      CASE_OVERLAPS;							\
    } else if (Teq(m_e1,m_e2)) {					\
      CASE_FINISHED_BY;							\
    } else {								\
      CASE_CONTAINS;							\
    }									\
  } else if (Teq(m_b1,m_b2)) {						\
    if (Tlt(m_e1,m_e2)) {						\
      CASE_STARTS;							\
    } else if (Teq(m_e1,m_e2)) {					\
      CASE_EQUALS;							\
    } else {								\
      CASE_STARTED_BY;							\
    }									\
  } else {								\
    if (Tlt(m_e1,m_e2)) {						\
      CASE_DURING;							\
    } else if (Teq(m_e1,m_e2)) {					\
      CASE_FINISHES;							\
    } else if (Tlt(m_b1,m_e2)) {					\
      CASE_OVERLAPPED_BY;						\
    } else {								\
      if (Teq(m_b1,m_e2)) {						\
	CASE_MET_BY;							\
      } else {								\
	CASE_AFTER;							\
      }									\
    }									\
  }									\
}

#else /* FD_INLINE */

#define switch_fd_interval_cmp(B1,E1,B2,E2,		\
			       CASE_BEFORE,		\
			       CASE_MEETS,		\
			       CASE_OVERLAPS,		\
			       CASE_FINISHED_BY,	\
			       CASE_CONTAINS,		\
			       CASE_STARTS,		\
			       CASE_EQUALS,		\
			       CASE_STARTED_BY,		\
			       CASE_DURING,		\
			       CASE_FINISHES,		\
			       CASE_OVERLAPPED_BY,	\
			       CASE_MET_BY,		\
			       CASE_AFTER)		\
     switch (fd_interval_cmp(B1,E1,B2,E2)) {		\
     case FD_BEFORE: CASE_BEFORE; break;		\
     case FD_MEETS: CASE_MEETS; break;			\
     case FD_OVERLAPS: CASE_OVERLAPS; break;		\
     case FD_FINISHED_BY: CASE_FINISHED_BY; break;	\
     case FD_CONTAINS: CASE_CONTAINS; break;		\
     case FD_STARTS: CASE_STARTS; break;		\
     case FD_EQUALS: CASE_EQUALS; break;		\
     case FD_STARTED_BY: CASE_STARTED_BY; break;	\
     case FD_DURING: CASE_DURING; break;		\
     case FD_FINISHES: CASE_FINISHES; break;		\
     case FD_OVERLAPPED_BY: CASE_OVERLAPPED_BY; break;	\
     case FD_MET_BY: CASE_MET_BY; break;		\
     case FD_AFTER: CASE_AFTER; break;			\
     }							\


#endif


#define FLOORDIV(Over,Under) \
((Under)==1 ? (Over) : \
 ((Over)>=0) ? (Over)/(Under) : -((-(Over)-1)/(Under)+1))
#define  CEILDIV(Over,Under) \
((Under)==1 ? (Over) : \
 ((Over)<=0) ? (Over)/(Under) : ((Over)-1)/(Under)+1)

#define point_vs_range(p, r) point_vs_interval(p, RangeMin(r), RangeMax(r))
#define val_vs_range(p, r) val_vs_interval(p, RangeMin(r), RangeMax(r))

typedef unsigned long ix_byte;

typedef struct profile *PROFILE;

#define FD_NB_QUEUES 5

struct prop_queue {
  TAGGED *items;
  int first;
  int last;
  int size;
};

struct propagator {
  struct propagator *next;
#if DBG
  TAGGED chpt;			/* [PRM 8883] to detect nondet propagation */
#endif
  struct prop_queue queue[FD_NB_QUEUES];	/* 0=indexical, 1=val, 2=minmax, 3=dom, 4=daemon */
  int hint;			/* first queue that can be non-empty */
};

struct fd_state {
#if DBG                         /* [PM] 3.9.2b1 */
  int generation;               /* a unique ID for this instance of the clpfd resource (roughly: incremented for each load_foreign_resource(clpfd)) */
#endif /* DBG */

  void *gdata;			/* of various types */
  unsigned long debugging;	/* 0=off, 1=on */
  unsigned long overflowing;	/* 0=fail, 1=error */
  unsigned long resumptions;
  unsigned long entailments;
  unsigned long prunings;
  unsigned long failures;
  unsigned long constraints;
  PROFILE profile;
  struct profile *profile_pool;
  struct mod_def *fd_module;

#if !SP_FD_GLOBAL_DESTRUCTOR
  SP_FD_FreeFun *fd_destructor_fun; /* [PM] 3.9b4 replaced the fd_destructor field */
#endif /* !SP_FD_GLOBAL_DESTRUCTOR */

  SP_pred_ref call_action1;
  SP_pred_ref overflow_action1;
  SP_BOOL fd_overflow;
  TAGGED functor_dom4;
  TAGGED functor_lists7;
  TAGGED functor_in_set2;
  TAGGED functor_dom;
  TAGGED functor_min;
  TAGGED functor_max;
  TAGGED functor_minmax;
  TAGGED functor_val;
  TAGGED functor_none;
  TAGGED functor_call;
  TAGGED functor_eq;
  TAGGED functor_leqc;
  TAGGED token_a;
  TAGGED token_d;
  TAGGED token_h;
  TAGGED token_l;
  TAGGED token_t;
  TAGGED linkage_keys[8];
  struct propagator *current_propagator;
  struct propagator *free_propagators;
  struct sw_on_key *dispatch;
};

#if !MULTI_SP_AWARE  
extern struct fd_state fd;
#endif /* !MULTI_SP_AWARE */

#if MULTI_SP_AWARE
#define fd (*(struct fd_state*)*(SP_foreign_stash()))
#endif /* MULTI_SP_AWARE */


/* The literals array below consists of items tagged:
   CONST - integers, inf or sup
   STRUCT - FD sets with a bignum header, in its own mem
   LIST - hash table, in its own mem
*/
struct indexical_info {

#if !SP_FD_GLOBAL_DESTRUCTOR
  /* void (SPCDECL *destructor)(); */
#if 1                           /* [PM] 3.9b4 Make it a direct pointer to free_fd_info_hook */
  SP_FD_FreeFun *destructor_fun;
#else
  struct definition *destructor;
#endif
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
#endif /* !SP_FD_GLOBAL_DESTRUCTOR */

#if DBG                         /* [PM] 3.9.2b1 */
  int identifier;               /* the clpfd_generation that was used to create this struct */
#endif

  struct definition *pred;
  ix_byte *linkage;		/* each is (var<<8)+index */
  unsigned int length_of_linkage;
  unsigned int length_of_bytecode;
  unsigned int length_of_literals;
  unsigned int pruned:8;
  unsigned int checking:1;	/* checking as opp. to pruning */
  unsigned int truth_value:1;	/* positive as opp. to negative */
  ix_byte *code;
  TAGGED *literals;
  struct indexical_info *next;
};

#define TADD(X,Y) ((X) + ((Y)-TaggedZero))
#define TSUB(X,Y) ((X) - ((Y)-TaggedZero))

#if SICSTUS_MAJOR_VERSION < 4
# define LetShadowregs
# define GLOBAL_UNCOND (w->global_uncond)
# define TADDCHK(X,Y) {				\
  (X) += ((Y)-TaggedZero);			\
  if (!TagIsSmall(X))				\
    (X) = Tltz(X) ? Inf : Sup;			\
}
# define TSUBCHK(X,Y) {				\
  (X) -= ((Y)-TaggedZero);			\
  if (!TagIsSmall(X))				\
    (X) = Tltz(X) ? Inf : Sup;			\
}
#else
/* Converting between LIST_TAG and INT_TAG */
# define LIST_INT(X) ((X)+1)
# define INT_LIST(X) ((X)-1)
# define TADDCHK(X,Y) {					\
  TAGGED m_z = (X)+((Y)-TaggedZero);			\
  if ((long)((X)^(Y))>=0 && (long)((X)^m_z)<0)		\
    m_z = Tltz(X) ? Inf : Sup;				\
  (X) = m_z;						\
}
# define TSUBCHK(X,Y) {					\
  TAGGED m_z = (X)-((Y)-TaggedZero);			\
  if ((long)((X)^(Y))<0 && (long)((X)^m_z)<0)		\
    m_z = Tltz(X) ? Inf : Sup;				\
  (X) = m_z;						\
}
#endif

#define FD_ATTRIBUTE_SIZE 26
#define INT_ATTRIBUTE_SIZE 15
extern TAGGED fd_attribute[];

extern TAGGED safe_mul (TAGGED t1,TAGGED t2);
extern TAGGED safe_divu (TAGGED t1,TAGGED t2);
extern TAGGED safe_divd (TAGGED t1,TAGGED t2);
extern TAGGED safe_plus (TAGGED t1,TAGGED t2);
extern TAGGED safe_minus (TAGGED t1,TAGGED t2);
extern TAGGED fd_min(TAGGED set);
extern TAGGED fd_max(TAGGED set);
extern void fd_first_and_rest MAGIC (HIDDEN_PROTO Argdecl, TAGGED d1, TAGGED *firstp, TAGGED *restp);
extern TAGGED fd_localize MAGIC (HIDDEN_PROTO Argdecl, TAGGED old);
extern int fd_size(TAGGED set);
extern SP_BOOL fd_singleton(TAGGED set);
extern int point_vs_interval(TAGGED p, TAGGED min, TAGGED max);
extern int val_vs_interval(TAGGED p, TAGGED min, TAGGED max);
extern unsigned int fd_interval_cmp(TAGGED b1,TAGGED e1,TAGGED b2,TAGGED e2);
extern TAGGED fd_interval MAGIC (HIDDEN_PROTO Argdecl, TAGGED min, TAGGED max);
extern TAGGED fd_compl_interval MAGIC (HIDDEN_PROTO Argdecl, TAGGED min, TAGGED max);
extern TAGGED fd_pair MAGIC (HIDDEN_PROTO Argdecl, TAGGED t1, TAGGED t2);
extern TAGGED fd_lsh MAGIC (HIDDEN_PROTO Argdecl, TAGGED d1, TAGGED toffset);
extern TAGGED *fd_neg_internal MAGIC (HIDDEN_PROTO Argdecl, TAGGED d1, TAGGED toffset, TAGGED *valuep);
extern TAGGED fd_neg_offset MAGIC (HIDDEN_PROTO Argdecl, TAGGED d1, TAGGED t2);
extern TAGGED fd_delete MAGIC (HIDDEN_PROTO Argdecl, TAGGED d1, TAGGED t2);
extern TAGGED fd_subtract MAGIC (HIDDEN_PROTO Argdecl, TAGGED d1, TAGGED t2);
extern TAGGED fd_interval_subtract MAGIC (HIDDEN_PROTO Argdecl, TAGGED b, TAGGED e, TAGGED d2);
extern TAGGED fd_subtract_interval MAGIC (HIDDEN_PROTO Argdecl, TAGGED d, TAGGED b, TAGGED e);
extern TAGGED fd_complement MAGIC (HIDDEN_PROTO Argdecl, TAGGED d);
extern TAGGED fd_intersection MAGIC (HIDDEN_PROTO Argdecl, TAGGED d1, TAGGED d2);
extern long   fd_intersection_size(TAGGED d1, TAGGED d2);
extern TAGGED fd_intersection_interval MAGIC (HIDDEN_PROTO Argdecl, TAGGED d, TAGGED b, TAGGED e);
extern TAGGED fd_intersection_min(TAGGED d1, TAGGED d2);
extern TAGGED fd_intersection_max(TAGGED d1, TAGGED d2);
extern TAGGED fd_intersection_min2(TAGGED d1, TAGGED d2, TAGGED *elt);
extern TAGGED fd_union MAGIC (HIDDEN_PROTO Argdecl, TAGGED d1, TAGGED d2);
extern TAGGED fd_union_interval MAGIC (HIDDEN_PROTO Argdecl, TAGGED d1, TAGGED b, TAGGED e);
extern TAGGED fd_union_dest(TAGGED a, TAGGED b);
extern TAGGED fd_union_sort(TAGGED list);

extern TAGGED check_argument MAGIC (HIDDEN_PROTO Argdecl, TAGGED argument, TAGGED min, TAGGED max, TAGGED size);
extern int fd_compare(TAGGED d1, TAGGED d2);
extern int fd_compare_interval(TAGGED d1, TAGGED min, TAGGED max);
extern int fd_compare_intervals(TAGGED b1, TAGGED e1, TAGGED mb2, TAGGED e2);
extern TAGGED fd_merge_into MAGIC (HIDDEN_PROTO Argdecl, TAGGED d1, TAGGED d2);
extern TAGGED fd_insert_into MAGIC (HIDDEN_PROTO Argdecl, TAGGED t, TAGGED d);
extern SP_BOOL fd_member(TAGGED x, TAGGED set);
extern SP_BOOL fd_check_overflow MAGIC (HIDDEN_PROTO TAGGED goal);
extern TAGGED fd_globalize MAGIC (HIDDEN_PROTO Argdecl, TAGGED old, long req, int ar);
extern TAGGED fd_globalize_unsafe MAGIC (HIDDEN_PROTO Argdecl, TAGGED old, long req, int ar);
extern void update_mutable MAGIC (HIDDEN_PROTO Argdecl, TAGGED new_value, TAGGED mutable);
extern void fd_sync MAGIC (HIDDEN_PROTO Argdecl);
extern void fd_end MAGIC (HIDDEN_PROTO Argdecl);
extern void fd_dealloc MAGIC (HIDDEN_PROTO_VOID);
extern TAGGED daemon_copy_state MAGIC (HIDDEN_PROTO Argdecl, TAGGED *global);
extern void fd_enqueue_general MAGIC (HIDDEN_PROTO Argdecl, TAGGED item, int where);
extern void fd_enqueue_global MAGIC (HIDDEN_PROTO Argdecl, TAGGED item, int where);
extern int  fd_dequeue MAGIC (HIDDEN_PROTO Argdecl, TAGGED *item);
extern void fd_enqueue_all MAGIC (HIDDEN_PROTO Argdecl, int bits, TAGGED filter, TAGGED lists_loc);
extern int fd_tell_value MAGIC (HIDDEN_PROTO Argdecl, TAGGED value);
extern int fd_tell_interval MAGIC (HIDDEN_PROTO Argdecl, TAGGED min, TAGGED max, int why);
extern int fd_tell MAGIC (HIDDEN_PROTO Argdecl, TAGGED new);
extern int fd_tell_unsafe MAGIC (HIDDEN_PROTO Argdecl, TAGGED new);
extern void fd_told MAGIC (HIDDEN_PROTO Argdecl, int why, TAGGED filter);
extern void fd_link MAGIC (HIDDEN_PROTO Argdecl, TAGGED var, long key, TAGGED item);

#define fd_enqueue_prepend(w,item,ix)					\
{									\
  struct propagator *cur = fd.current_propagator;			\
  struct prop_queue *q = &cur->queue[ix];				\
  if (((q->last-q->first+1) & (q->size-1))==0) {/* must expand */	\
    fd_enqueue_general(w, item, (ix<<1)/*prepend*/);			\
  } else {								\
    {								\
      TAGGED mutable = CTagToArg(item,3);				\
      update_mutable(w,RefMutable(mutable)|IStep(1),mutable); /* STATUS: enqueued */ \
      if (cur->hint > ix)							\
        cur->hint = ix;							\
    }									\
    if (q->first==0) q->first = q->size;				\
    q->items[--q->first] = item;					\
  }									\
}

#define fd_enqueue_append(w,item,ix)					\
{									\
  struct propagator *cur = fd.current_propagator;			\
  struct prop_queue *q = &cur->queue[ix];				\
  if (((q->last-q->first+1) & (q->size-1))==0) {/* must expand */	\
    fd_enqueue_general(w, item, (ix<<1)+0x1/*append*/);			\
  } else {								\
    {								\
      TAGGED mutable = CTagToArg(item,3); 				\
      update_mutable(w,RefMutable(mutable)|IStep(1),mutable); /* STATUS: enqueued */ \
      if (cur->hint > ix)							\
        cur->hint = ix;							\
    }									\
    q->items[q->last++] = item;						\
    if (q->last==q->size) q->last = 0;					\
  }									\
}

#define fd_enqueue_daemon(w,item)					\
{									\
  struct propagator *cur = fd.current_propagator;			\
  struct prop_queue *q = &cur->queue[4];				\
  if (((q->last-q->first+1) & (q->size-1))==0) {/* must expand */	\
    fd_enqueue_general(w, item, 9/*append*/);				\
  } else {								\
    q->items[q->last++] = item;						\
    if (q->last==q->size) q->last = 0;					\
  }									\
}

struct profile {
  long begin;
  long end;
  long erg;
  struct profile *next;
};

extern void init_profile MAGIC (HIDDEN_PROTO_VOID);
extern PROFILE empty_profile(void);
extern PROFILE profile_cons MAGIC (HIDDEN_PROTO Argdecl, long begin, long end, long erg, PROFILE next);
extern void profile_dispose MAGIC (HIDDEN_PROTO PROFILE cons);
extern PROFILE profile_update MAGIC (HIDDEN_PROTO Argdecl, PROFILE p, long b2, long e2, long y2);
extern SP_BOOL profile_zero_at(PROFILE p, long b2, long e2, long *wit);
extern SP_BOOL profile_nonzero(PROFILE p, long b2, long e2);
extern SP_BOOL profile_next(PROFILE prof, long *bp, long *ep, long *hp, PROFILE *nextp);

/* support for save & restore */

extern void SPCDECL free_fd_info_hook (struct indexical_info **infop);

#if MULTI_SP_AWARE
#define SP_HOOKS_COOKIE_PROTO HIDDEN_ARG_PROTO
#define SP_HOOKS_COOKIE_PROTO_COMMA SP_HOOKS_COOKIE_PROTO ,
#define SP_HOOKS_COOKIE_USE   (void)HIDDEN_ARG
#else  /* !MULTI_SP_AWARE */

/* xref support.h */
#if ENABLE_CLPFD_MULTI_SP

/* 3.9 when compiling clpfd as a static resource. SPEnv is passed but ignored.
  More precisely: This happens when compiling clpfd as a static
  foreign resource in a SICStus where (dynamic) clpfd supports
  multiple SICStus run-times. */
#define SP_HOOKS_COOKIE_PROTO SPEnv *ignore_cookie
#define SP_HOOKS_COOKIE_PROTO_COMMA SP_HOOKS_COOKIE_PROTO ,
#define SP_HOOKS_COOKIE_USE   (void)ignore_cookie

#else /* !ENABLE_CLPFD_MULTI_SP */

/* Happens when clpfd support for multiple run-times has been switched
   off completely (should not happen except on WinCE). xref
   SP_HOOKS_COOKIE_COMMA in support.h */
/* [PM] WinCE Was:  
   #define SP_HOOKS_COOKIE_PROTO void *ignore_cookie
*/
#define SP_HOOKS_COOKIE_PROTO
#define SP_HOOKS_COOKIE_PROTO_COMMA

/* [PM] WinCE Was:
   #define SP_HOOKS_COOKIE_USE   (void)ignore_cookie
*/
#define SP_HOOKS_COOKIE_USE
#endif /* !ENABLE_CLPFD_MULTI_SP */

#endif /* !MULTI_SP_AWARE */

#ifndef SP_MANGLE
# define SP_MANGLE(FNAME) FNAME
#endif

extern void SPCDECL fd_manager_hook (SP_HOOKS_COOKIE_PROTO_COMMA Argdecl,int msg,TAGGED *ptr);
/* [PM] 3.9b4 Proper prototype for frd and fwr */
extern void SPCDECL fd_restore_hook(SP_HOOKS_COOKIE_PROTO_COMMA struct saverest_record *record, fread_longs_fun *frd, struct saverest_info *sr_info);
extern void SPCDECL fd_save_hook (SP_HOOKS_COOKIE_PROTO_COMMA struct saverest_record *record, struct definition *pred, fwrite_fun *fwr, struct saverest_info *sr_info);

/* [PM] 3.9.2b1 */
extern void SPCDECL fd_destructor_hook(SP_HOOKS_COOKIE_PROTO_COMMA struct indexical_info **infop);


/* Heap routines from Cormen et al., not in use */

typedef long (*HeapFun)(const void *, const void *);

struct heap {
  long size;
  void *item[1];		/* 1-based indexing! */
};

extern void heap_init(struct heap *h);
extern void heap_insert(struct heap *h, void *item, HeapFun cmpfun);
extern void heapify(struct heap *h, HeapFun cmpfun);
extern void *heap_extract_min(struct heap *h, HeapFun cmpfun);

/* Iterators */

typedef struct {
  TAGGED cur, max, tail;
} FDITER;

#define fditer_empty(it) ((it)->cur==SupAsINT)

extern void fditer_init(FDITER *it, TAGGED d);
extern TAGGED fditer_next(FDITER *it);
extern void fditer_skip(FDITER *it, TAGGED d);


/* Constructors */

typedef struct {
  TAGGED head, cur;
  int size;
} FDCONS;

#define fdcons_size(cons) ((cons)->size)
#define fdcons_set(cons) ((cons)->head)

extern void fdcons_init(FDCONS *cons);
extern void fdcons_add MAGIC (HIDDEN_PROTO FDCONS *cons, Argdecl, TAGGED t);
extern void fdcons_add_interval MAGIC (HIDDEN_PROTO FDCONS *cons, Argdecl, TAGGED t, TAGGED u);

/* radix (bucket?) sort */

#define KEYSORT(Inarray, Incount, Type, Keyarray, Keycount, Key, Outarray)	\
{										\
  int _i;									\
  Type _x;									\
  for (_i=(Keycount)-1; _i>=0; _i--)						\
    (Keyarray)[_i] = 0;								\
  for (_i=(Incount)-1; _i>=0; _i--)						\
    (Keyarray)[Key((Inarray)[_i])]++;						\
  for (_i=1; _i<(Keycount); _i++)						\
    (Keyarray)[_i] += (Keyarray)[_i-1];						\
  for (_i=(Incount)-1; _i>=0; _i--)						\
    _x = (Inarray)[_i],								\
    (Outarray)[--(Keyarray)[Key(_x)]] = _x;					\
}

extern void
qsort_asc_long MAGIC (HIDDEN_PROTO
		      long *l1, int n);

#if DBG
extern void
addstat(int n);

extern void
printstat(void);

extern void
clearstat(void);
#endif
