/* Copyright(C) 2002, Swedish Institute of Computer Science */

/* Domain variable abstraction */
/*******************************/

#define DV_PRUNED_DOM 0x1	/* some domain element was pruned */
#define DV_PRUNED_MIN 0x2	/* min was adjusted */
#define DV_PRUNED_MAX 0x4	/* max was adjusted */
#define DV_PRUNED_VAL 0x8	/* domain is/became a singleton */
#define DV_SET_OK     0x10	/* means set \subseteq min..max */
#define DV_INTERVAL   0x20	/* domain is an interval */
#define DV_EXPORTED   0x40	/* dv_export() called, so set is unsafe */

struct dvar {
  TAGGED min;			/* kept up to date */
  TAGGED max;			/* kept up to date */
  TAGGED set;
  TAGGED cookie;
  SP_globref attr_ref;
  SP_globref var_ref;
  unsigned int flags;
};

typedef struct dvar *Dvar;

typedef struct {
  TAGGED min, max, fdset;
} DVITER;

#if MULTI_SP_AWARE
#define dvar_assign(A1,A2,A3) dvar_assign(HIDDEN_ARG,A1,A2,A3)
#define dvar_set(A1,A2) dvar_set(HIDDEN_ARG,A1,A2)
#define dvar_compare_interval_t(A1,A2,A3,A4) dvar_compare_interval_t(HIDDEN_ARG,A1,A2,A3,A4)
#define dvar_compare_set(A1,A2,A3) dvar_compare_set(HIDDEN_ARG,A1,A2,A3)
#define dvar_prune_interval_t(A1,A2,A3,A4) dvar_prune_interval_t(HIDDEN_ARG,A1,A2,A3,A4)
#define dvar_prune_set(A1,A2,A3) dvar_prune_set(HIDDEN_ARG,A1,A2,A3)
#define dvar_fix_set(A1,A2,A3) dvar_fix_set(HIDDEN_ARG,A1,A2,A3)
#define dvar_pruning_done(A1,A2) dvar_pruning_done(HIDDEN_ARG,A1,A2)
#define dvar_export_do(A1,A2) dvar_export_do(HIDDEN_ARG,A1,A2)
#define dvar_export_ix(A1,A2,A3) dvar_export_ix(HIDDEN_ARG,A1,A2,A3)
#define dvar_export_equal(A1,A2,A3) dvar_export_equal(HIDDEN_ARG,A1,A2,A3)
#define dvar_export_leqc(A1,A2,A3,A4) dvar_export_leqc(HIDDEN_ARG,A1,A2,A3,A4)
#define dvar_export_done(A1,A2,A3) dvar_export_done(HIDDEN_ARG,A1,A2,A3)
#define dvar_attach_daemon(A1,A2,A3,A4,A5) dvar_attach_daemon(HIDDEN_ARG,A1,A2,A3,A4,A5)
#define dvar_validate() dvar_validate(HIDDEN_ARG)
#endif /* MULTI_SP_AWARE */

/* Purpose: 
     (Re)Initialize dvar when entering filtering alg.
   Synopsis:
     void dvar_init(dvar, attr_ref, var_ref);
   Arguments:
     Dvar dvar            - the dvar
     SP_globref attr_ref - SP_globref holding Prolog FD attribute
     SP_globref var_ref  - SP_globref holding Prolog FD varable
*/
extern void
dvar_init(Dvar dvar,
	  SP_globref attr_ref,
	  SP_globref var_ref);

extern void
dvar_init_ix(Dvar dvar,
	     TAGGED attr,
	     TAGGED var);

/* Purpose: 
     Get dereferenced domain variable.
   Synopsis:
     TAGGED dvar_get(dvar);
   Arguments:
     Dvar dvar            - the dvar
*/
extern TAGGED
dvar_get(Dvar dvar);

/* Purpose: 
     Clone a dvar, e.g. for constructive disjunction.
   Synopsis:
     void dvar_assign(dest, source, w);
   Arguments:
     Dvar dest            - destination dvar
     Dvar source          - source dvar
*/
extern void
dvar_assign MAGIC (HIDDEN_PROTO
		   Dvar dest,
		   Dvar source,
		   Argdecl);

/* Purpose:
     Access various dvar attributes.
   Synopsis:
     SP_BOOL   dvar_is_alias(dvar,dvar);
     SP_BOOL   dvar_is_integer(dvar);
     SP_BOOL   dvar_is_interval(dvar);
     long   dvar_min_l(dvar);
     long   dvar_max_l(dvar);
     TAGGED dvar_min_t(dvar);
     TAGGED dvar_max_t(dvar);
     TAGGED dvar_set(dvar,w);
     long   dvar_interval_count(dvar);
     long   dvar_value_count(dvar);
   Arguments:
     Dvar dvar          - the dvar
*/
#define dvar_is_pruned(DVAR)  ((DVAR)->flags & (DV_PRUNED_DOM|DV_PRUNED_MIN|DV_PRUNED_MAX))
#define dvar_is_integer(DVAR)  ((DVAR)->flags & DV_PRUNED_VAL)
#define dvar_is_integer_first(DVAR)  			\
((DVAR)->flags==(DV_PRUNED_VAL|DV_SET_OK|DV_INTERVAL))
#define dvar_is_interval(DVAR) ((DVAR)->flags & DV_INTERVAL)
#define dvar_min_t(DVAR) ((DVAR)->min)
#define dvar_min_l(DVAR) GetSmall((DVAR)->min)
#define dvar_max_t(DVAR) ((DVAR)->max)
#define dvar_max_l(DVAR) GetSmall((DVAR)->max)

extern SP_BOOL
dvar_is_alias(Dvar dv1,Dvar dv2);

extern TAGGED
dvar_set MAGIC (HIDDEN_PROTO
		Dvar dvar,
		Argdecl);


extern long
dvar_interval_count(Dvar dvar);


extern long
dvar_value_count(Dvar dvar);


/* Purpose:
     Compare a dvar with a set/interval/value.
   Synopsis:
     int    dvar_compare_set(dvar, set, w);
     int    dvar_compare_interval_l(dvar, min, max,w);
     int    dvar_compare_interval_t(dvar, tmin, tmax, w);
     SP_BOOL   dvar_contains_value_l(dvar, value);
     SP_BOOL   dvar_contains_value_t(dvar, tvalue);
   Arguments:
     Dvar dvar          - the dvar
     TAGGED set         - the set to compare with
     long min           - min. of the interval to compare with
     long max           - max. of the interval to compare with
     long value         - the value to test for membership
     TAGGED tmin        - min. of the interval to compare with
     TAGGED tmax        - max. of the interval to compare with
     TAGGED tvalue      - the value to test for membership
   int  values:
     FDI_EQUAL          - domain is equal to set/interval
     FDI_SUBSET         - domain is a struct subset of set/interval
     FDI_SUPERSET       - domain is a strict superset of set/interval
     FDI_DISJOINT       - domain does not intersect set/interval,
                          which is non-empty
     FDI_INTERSECT      - domain is neither subset nor superset,
                          but intersects set/interval
   SP_BOOL values:
     TRUE               - the value is in the domain
     FALSE              - the value is not in the domain
*/

extern int
dvar_compare_set MAGIC (HIDDEN_PROTO
			Dvar dvar, TAGGED set, Argdecl);

#define dvar_compare_interval_l(DVAR,MIN,MAX,W) \
        dvar_compare_interval_t(DVAR,MakeSmall(MIN),MakeSmall(MAX),W)

extern int
dvar_compare_interval_t MAGIC (HIDDEN_PROTO
			       Dvar dvar, TAGGED tmin, TAGGED tmax, Argdecl);

#define dvar_contains_value_l(DVAR,VALUE) \
        dvar_contains_value_t(DVAR,MakeSmall(VALUE))

extern SP_BOOL
dvar_contains_value_t(Dvar dvar, TAGGED tvalue);

/* Purpose:
     Get successor domain element.
   Synopsis:
     long   dvar_successor_l(dvar, val);
     TAGGED dvar_successor_t(dvar, tval);
   Arguments:
     Dvar dvar          - the dvar
     long val           - the predecessor element
                          -CLPFD_MAXINT means no predecessor
     TAGGED tval        - the predecessor element
                          Inf means no predecessor
   long   values:
     CLPFD_MAXINT       - no successor exists
     anything else      - successor
   TAGGED values:
     Sup                - no successor exists
     anything else      - successor
*/

extern long
dvar_successor_l(Dvar dvar,long val);

extern TAGGED
dvar_successor_t(Dvar dvar,TAGGED tval);

/* Purpose:
     Get predecessor domain element.
   Synopsis:
     long   dvar_predecessor_l(dvar, val);
     TAGGED dvar_predecessor_t(dvar, tval);
   Arguments:
     Dvar dvar          - the dvar
     long val           - the successor element
                          CLPFD_MAXINT means no successor
     TAGGED tval        - the successor element
                          Sup means no successor
   long   values:
     -CLPFD_MAXINT      - no predecessor exists
     anything else      - predecessor
   TAGGED values:
     Inf                - no predecessor exists
     anything else      - predecessor
*/

extern long
dvar_predecessor_l(Dvar dvar,long val);

extern TAGGED
dvar_predecessor_t(Dvar dvar,TAGGED tval);

/* Purpose:
     Iterate over domain intervals or values.
   Synopsis:
     void dviter_init(&it, dvar);
     SP_BOOL dviter_empty(&it);
     void dviter_next_interval_l(&it, &min, &max);
     void dviter_next_interval_t(&it, &tmin, &tmax);
     long dviter_next_value_l(&it);
     TAGGED dviter_next_value_t(&it);
     void dviter_skip_l(&it, value);
     void dviter_skip_t(&it, tvalue);
   Arguments:
     Dvar dvar          - the dvar
     DVITER it          - the iterator
     long   min         - min. of the next interval
     long   max         - max. of the next interval
     long   value       - the next value
     TAGGED tmin        - min. of the next interval
     TAGGED tmax        - max. of the next interval
     TAGGED tvalue      - the next value
     SP_BOOL values:
     TRUE               - no more intervals or values
     FALSE              - there are more intervals and values
*/

extern void 
dviter_init(DVITER *it, Dvar dvar);

#define dviter_empty(it) ((it)->fdset==EmptySet)

extern void
dviter_next_interval_l(DVITER *it, long *min, long *max);

extern void
dviter_next_interval_t(DVITER *it, TAGGED *tmin, TAGGED *tmax);

#define dviter_next_value_l(it) \
        GetSmall(dviter_next_value_t(it))

extern TAGGED
dviter_next_value_t(DVITER *it);

#define dviter_skip_l(it,l) dviter_skip_t(it,MakeSmall(l))

extern void
dviter_skip_t(DVITER *it, TAGGED t);

/* Purpose:
     Remove a set/interval/value from a dvar.
   Synopsis:
     int    dvar_prune_set(dvar, set,w);
     int    dvar_prune_interval_l(dvar, min, max,w);
     int    dvar_prune_interval_t(dvar, tmin, tmax,w);
     int    dvar_prune_value_l(dvar, value,w);
     int    dvar_prune_value_t(dvar, tvalue,w);
   Arguments:
     Dvar dvar          - the dvar to prune
     TAGGED set         - the set to remove
     long min           - min. of the interval to remove
     long max           - max. of the interval to remove
     long value         - the value to remove
     TAGGED tmin        - min. of the interval to remove
     TAGGED tmax        - max. of the interval to remove
     TAGGED tvalue      - the value to remove
   int  values:
     -1                 - failure (empty domain)
     >=0                - success, value is sum of DV_PRUNED_{DOM,MIN,MAX,VAL}
*/

#define dvar_prune_value_l(DVAR,L,W) dvar_prune_value_t(DVAR,MakeSmall(L),W)

#define dvar_prune_value_t(DVAR,T,W) dvar_prune_interval_t(DVAR,T,T,W)

#define dvar_prune_interval_l(DVAR,L,U,W) \
        dvar_prune_interval_t(DVAR,MakeSmall(L),MakeSmall(U),W)

extern int 
dvar_prune_interval_t MAGIC (HIDDEN_PROTO
			     Dvar dvar,TAGGED lbt,TAGGED ubt,Argdecl);

int 
dvar_prune_set MAGIC (HIDDEN_PROTO
		      Dvar dvar,TAGGED set,Argdecl);

/* Purpose:
     Fix a dvar to a set/interval/value.
   Synopsis:
     int    dvar_fix_set(dvar, set,w);
     int    dvar_fix_interval_l(dvar, min, max);
     int    dvar_fix_interval_t(dvar, tmin, tmax);
     int    dvar_fix_min_l(dvar, min);
     int    dvar_fix_min_t(dvar, tmin);
     int    dvar_fix_max_l(dvar, max);
     int    dvar_fix_max_t(dvar, tmax);
     int    dvar_fix_value_l(dvar, value);
     int    dvar_fix_value_t(dvar, tvalue);
   Arguments:
     Dvar dvar          - the dvar to prune
     TAGGED set         - the set to impose
     long min           - min. of the interval to impose
     long max           - max. of the interval to impose
     long value         - the value to impose
     TAGGED tmin        - min. of the interval to impose
     TAGGED tmax        - max. of the interval to impose
     TAGGED tvalue      - the value to impose
   int  values:
     -1                 - failure (empty domain)
     >=0                - success, value is sum of DV_PRUNED_{DOM,MIN,MAX,VAL}
*/

#define dvar_fix_value_l(DVAR,L) \
        dvar_fix_value_t(DVAR,MakeSmall(L))

#define dvar_fix_value_t(DVAR,T) \
        dvar_fix_interval_t(DVAR,T,T)

#define dvar_fix_interval_l(DVAR,L,U) \
        dvar_fix_interval_t(DVAR,MakeSmall(L),MakeSmall(U))

#define dvar_fix_min_l(DVAR,L) \
  dvar_fix_min_t(DVAR,MakeSmall(L))

#define dvar_fix_min_t(DVAR,TL) \
        dvar_fix_interval_t(DVAR,TL,Sup)

#define dvar_fix_max_l(DVAR,U) \
        dvar_fix_interval_t(DVAR,Inf,MakeSmall(U))

#define dvar_fix_max_t(DVAR,UL) \
        dvar_fix_interval_t(DVAR,Inf,UL)

extern int 
dvar_fix_interval_t(Dvar dvar,TAGGED lbt,TAGGED ubt);

extern int 
dvar_fix_set MAGIC (HIDDEN_PROTO
		    Dvar dvar,TAGGED set,Argdecl);

/* Purpose: 
     Export dvars to Prolog environment.
     Signal success/failure/entailment.
     1. For each dvar: dvar_pruning_done(w, dvar)
     2. dvar_export_start(w);
     3. For each dvar: dvar_export(w,dvar);
     4. dvar_export_done(w,actions, ent);
   Synopsis:
     void dvar_pruning_done(w, dvar);
     void dvar_export_start(w);
     void dvar_export(w,dvar);
     void dvar_export_done(w,actions, ent);
   Arguments:
     Dvar dvar            - the dvar
     int  ent             - the dvar
     SP_term_ref actions  - SP_term_ref holding actions list
*/

extern void
dvar_pruning_done MAGIC (HIDDEN_PROTO
			 Argdecl,
			 Dvar dvar);

#define dvar_export_start(w) 			\
        (X(EVAL_ARITY-1) = atom_nil)

#define dvar_export(w,dv)						\
        if ((dv)->flags & (DV_PRUNED_DOM|DV_PRUNED_MIN|DV_PRUNED_MAX))	\
          dvar_export_do(w,dv)

extern void
dvar_export_do MAGIC (HIDDEN_PROTO
		      Argdecl,
		      Dvar dvar);

extern void
dvar_export_ix MAGIC (HIDDEN_PROTO
		      Argdecl,
		      Dvar dvar, TAGGED filter);

extern void
dvar_export_equal MAGIC (HIDDEN_PROTO
			 Argdecl,
			 Dvar dv1, Dvar dv2);

extern void
dvar_export_leqc MAGIC (HIDDEN_PROTO
			Argdecl,
			Dvar dv1, Dvar dv2, int c);

extern void
dvar_export_done MAGIC (HIDDEN_PROTO
			Argdecl,
			SP_term_ref Actions, int ent);


/* attaching a daemon */
/* Term = daemon(Global,AttrRef,StatusM,Ent,RawHandle) */
extern void 
dvar_attach_daemon MAGIC (HIDDEN_PROTO
			  Argdecl,
			  Dvar dv,
			  void *handle,
			  TAGGED global,
			  TAGGED list_functor);


#if DBG
extern void dvar_dump(Dvar dv);
#endif /* DBG */

#if DBG
extern void dvar_validate MAGIC (HIDDEN_PROTO_VOID);
#endif


/* TODO:
   - handle inf/sup consistently for long typed ops
   - handle unification
*/
