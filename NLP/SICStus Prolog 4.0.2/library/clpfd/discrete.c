/* Copyright(C) 2004, Swedish Institute of Computer Science */

/****************************************************************

 Generic filtering algorithms for unary resources:
 edge finding, detectable precedences, energetic reasoning.

 ****************************************************************/

#include "fd.h"
#include "dvars.h"

#if MULTI_SP_AWARE
#define qsort_asc_eventsswap(A1,A2,A3) qsort_asc_eventsswap(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_eventsmed3(A1,A2,A3) qsort_asc_eventsmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_asc_events(A1,A2) qsort_asc_events(HIDDEN_ARG, A1,A2)
#define qsort_asc_event_ptrs(A1,A2) qsort_asc_event_ptrs(HIDDEN_ARG, A1,A2)
#define qsort_ti_upswap(A1,A2,A3) qsort_ti_upswap(HIDDEN_ARG, A1,A2,A3)
#define qsort_ti_upmed3(A1,A2,A3) qsort_ti_upmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_ti_up(A1,A2) qsort_ti_up(HIDDEN_ARG, A1,A2)
#define qsort_ti_downswap(A1,A2,A3) qsort_ti_downswap(HIDDEN_ARG, A1,A2,A3)
#define qsort_ti_downmed3(A1,A2,A3) qsort_ti_downmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_ti_down(A1,A2) qsort_ti_down(HIDDEN_ARG, A1,A2)
#define qsort_estswap(A1,A2,A3) qsort_estswap(HIDDEN_ARG, A1,A2,A3)
#define qsort_estmed3(A1,A2,A3) qsort_estmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_est(A1,A2) qsort_est(HIDDEN_ARG, A1,A2)
#define qsort_lctswap(A1,A2,A3) qsort_lctswap(HIDDEN_ARG, A1,A2,A3)
#define qsort_lctmed3(A1,A2,A3) qsort_lctmed3(HIDDEN_ARG, A1,A2,A3)
#define qsort_lct(A1,A2) qsort_lct(HIDDEN_ARG, A1,A2)
#define init_ranks(A1) init_ranks(HIDDEN_ARG, A1)
#define tt_init(A1) tt_init(HIDDEN_ARG, A1)
#define unary_detect_precedences(A1) unary_detect_precedences(HIDDEN_ARG, A1)
#define unary_detect_all_precedences(A1,A2,A3) unary_detect_all_precedences(HIDDEN_ARG, A1,A2,A3)
#define unary_not_first_last(A1) unary_not_first_last(HIDDEN_ARG, A1)
#define ti_build_index(A1,A2) ti_build_index(HIDDEN_ARG, A1,A2)
#define ti_collect_intervals(A1,A2) ti_collect_intervals(HIDDEN_ARG, A1,A2)
#define task_interval_filtering(A1,A2) task_interval_filtering(HIDDEN_ARG, A1,A2)
#define wolf_alloc(A1,A2,A3) wolf_alloc(HIDDEN_ARG, A1,A2,A3)
#define wolf_filtering(A1) wolf_filtering(HIDDEN_ARG, A1)
#define unary_alloc(A1,A2,A3,A4) unary_alloc(HIDDEN_ARG, A1,A2,A3,A4)
#define unary_filtering(A1,A2,A3,A4,A5,A6) unary_filtering(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define unary_filtering_rec(A1,A2,A3,A4,A5,A6) unary_filtering_rec(HIDDEN_ARG, A1,A2,A3,A4,A5,A6)
#define discrete_filtering(A1,A2,A3,A4,A5,A6,A7) discrete_filtering(HIDDEN_ARG, A1,A2,A3,A4,A5,A6,A7)
#define compulsory_parts(A1,A2,A3,A4,A5) compulsory_parts(HIDDEN_ARG, A1,A2,A3,A4,A5)
#define cp_ends(A1,A2) cp_ends(HIDDEN_ARG, A1,A2)
#define cp_starts(A1,A2) cp_starts(HIDDEN_ARG, A1,A2)
#define dr_cp_ends(A1,A2) dr_cp_ends(HIDDEN_ARG, A1,A2)
#define dr_cp_starts(A1,A2) dr_cp_starts(HIDDEN_ARG, A1,A2)
#endif

struct task {
  long est;
  long lct;
  long lctmax;
  long dur;
  long res;
  int  est_rank;
  int  lct_rank;
  int  enable;			/* 0=off, 1=optional, 2=on */
  int  id;			/* ordinal number in resp. global constraint */
};

#define dur(t) ((t)->dur)
#define est(t) ((t)->est)
#define ect(t) ((t)->est+(t)->dur)
#define lst(t) ((t)->lct-(t)->dur)
#define lct(t) ((t)->lct)
#define lctmax(t) ((t)->lctmax)
#define res(t) ((t)->res)
#define enable(t) ((t)->enable)
#define must_precede(ti,tj) (ect(tj) > lst(ti))

#define TERMIN (-1)
#define PREC(I) (pdata->prec + (I)*nbtasks)
#define SUCC(I) (pdata->succ + (I)*nbtasks)
#define STAMP(I) (pdata->stamp[I])

#define SYNCRC(t) (rc = ((ect(t)>lct(t) && enable(t)==2) ? 3 : 1))
#define SYNCLB(t,ti,bound)			\
      if (pdata->lb[ti] < (bound)) {		\
	pdata->lb[ti] = (bound);		\
        if (lst(t) < (bound) && enable(t)==2)	\
	  return FALSE;				\
      }
#define SYNCUB(t,ti,bound)			\
      if (pdata->ub[ti] > (bound)) {		\
	pdata->ub[ti] = (bound);		\
        if (ect(t) > (bound) && enable(t)==2)	\
	  return FALSE;				\
      }


struct event {
  long key;
  struct task *task;
};

struct comppart {
  long start;
  long height;
};

struct unary_data {
  int nbtasks;
  int flags;
  struct task *tasks;
  struct event *event;
  struct event **eventp;
  struct comppart *cp;
  struct comppart *cp2;
  long *id_map;
  /* theta-lambda trees */
  struct theta_lambda_node *tree;
  struct task **est_rank;
  struct task **lct_rank;
  struct task **aux_rank;
  long *tt_map;
  long *lb;
  long *ub;
  /* task intervals */
  int nbobltasks;
  int nbti;
  long *est;
  long *lct;
  struct task **obltasks;
  struct task **ti_tasks;
  struct ti_index *ti_index;
  struct task_interval *task_interval;
};

/* Sorting */

/* for qsorting events */
static int 
cmp_asc_events(struct event **p1, struct event **p2)
{
  struct event *l1 = *p1;
  struct event *l2 = *p2;
  
  int cmp = CMP(l1->key,l2->key);
  if (cmp==0)
    cmp = CMP(l1->task,l2->task);
  return cmp;
}


#define QType struct event *
#define QCmp  cmp_asc_events
#define QSort qsort_asc_events
#include "qsort.ic"

static void
qsort_asc_event_ptrs MAGIC(HIDDEN_PROTO
			   struct unary_data *pdata,
			   int n)
{
  int i;
  struct event *event = pdata->event;
  struct event **eventp = pdata->eventp;
  
  for (i=n-1; i>=0; i--)
    *eventp++ = event++;
  qsort_asc_events(pdata->eventp,n);
}

static int 
cmp_ti_up(struct task **l1, struct task **l2) 
{
  struct task *t1 = *l1;
  struct task *t2 = *l2;
  int cmp = CMP(est(t1),est(t2));
  if (cmp==0)
    cmp = -CMP(lct(t1),lct(t2));
  if (cmp==0)
    cmp = CMP(lst(t1),lst(t2));
  if (cmp==0) {
    long area1 = dur(t1)*res(t1);
    long area2 = dur(t2)*res(t2);
    cmp = -CMP(area1,area2);
  }
  if (cmp==0)
    cmp = CMP(t1,t2);
  return cmp;
}


#define QType struct task *
#define QCmp  cmp_ti_up
#define QSort qsort_ti_up
#include "qsort.ic"

static int 
cmp_ti_down(struct task **l1, struct task **l2)
{
  struct task *t1 = *l1;
  struct task *t2 = *l2;
  int cmp = -CMP(lct(t1),lct(t2));
  if (cmp==0)
    cmp = CMP(est(t1),est(t2));
  if (cmp==0)
    cmp = -CMP(ect(t1),ect(t2));
  if (cmp==0) {
    long area1 = dur(t1)*res(t1);
    long area2 = dur(t2)*res(t2);
    cmp = -CMP(area1,area2);
  }
  if (cmp==0)
    cmp = CMP(t1,t2);
  return cmp;
}

#define QType struct task *
#define QCmp  cmp_ti_down
#define QSort qsort_ti_down
#include "qsort.ic"


/* THETA TREES */

/* ThetaTree index macros */
#define LSON(i)     ((i)<<1)
#define RSON(i)     (((i)<<1) + 1)
#define PARENT(i)   ((i)>>1)
#define ROOT(i)     ((i) == 1)
#define ISNODE(i)   ((i) >= 1)
#define LEAF(i)     (LSON(i) > nbtasks)
#define LBROTHER(i) (!RBROTHER(i))
#define RBROTHER(i) ((i)&1)

#define tstate cplfd_tstate     /* [PM] 4.0 work around AIX name conflict */
enum tstate {
  tstate_empty=0,
  tstate_normal,
  tstate_gray
};

struct theta_node
{
  long dur;      /* Processing time of activity. 0 if node is empty */
  long est;      /* Est of activity */
  long ECT; /* maximalni earliest completition time podstromu */
  long DUR;   /* celkovy processing time podstromu */
  long EST;      /* Est_Omega, where ECT_Omega = ECT */
  enum tstate state;
};

struct lambda_node
{
  long ECT;			/* ECT of subtree, one gray node can be used */
  long DUR;			/* sum duration in subtree, one gray node can be used */
  int  grayECT;			/* gray node responsible for ECT, or -1 */
  int  grayDUR;			/* gray node responsible for DUR, or -1 */
  int  gray;			/* rank if gray, or -1 */
};

struct theta_lambda_node {
  struct theta_node theta;
  struct lambda_node lambda;
};

static int cmp_est(struct task **t1, struct task **t2)
{
  long val1 = est(*t1);
  long val2 = est(*t2);

  return CMP(val1,val2);
}

#define QType struct task *
#define QCmp  cmp_est
#define QSort qsort_est
#include "qsort.ic"

static int cmp_lct(struct task **t1, struct task **t2)
{
  long val1 = -lct(*t1);
  long val2 = -lct(*t2);

  return CMP(val1,val2);
}

#define QType struct task *
#define QCmp  cmp_lct
#define QSort qsort_lct
#include "qsort.ic"

static void
init_ranks MAGIC (HIDDEN_PROTO
		  struct unary_data *pdata)
{
  int i;
  int nbtasks = pdata->nbtasks;
  
  for (i=nbtasks-1; i>=0; i--) {
    pdata->est_rank[i] = pdata->tasks+i;
    pdata->lct_rank[i] = pdata->tasks+i;
  }
  qsort_est(pdata->est_rank,nbtasks);
  qsort_lct(pdata->lct_rank,nbtasks);
  for (i=nbtasks-1; i>=0; i--) {
    pdata->est_rank[i]->est_rank = i;
    pdata->lct_rank[i]->lct_rank = i;
  }
}

static void 
tn_recompute_leaf(struct theta_node *th)
{
  if (th->state==tstate_normal) {
    th->ECT = th->est + th->dur;
    th->EST = th->est;
    th->DUR = th->dur;
  } else {
    th->ECT = -CLPFD_MAXINT2;
    th->EST = -CLPFD_MAXINT2;
    th->DUR = 0;
  }
}

static void 
tn_recompute_node(struct theta_node *th,
		  struct theta_node *left,
		  struct theta_node *right)
{
  long ECTleft;
  long ECTthis;
  long ECTright;

  if (th->state==tstate_normal) {
    ECTleft = left->ECT + th->dur  + right->DUR;
    ECTthis = th->est + th->dur + right->DUR;
    th->DUR = left->DUR + th->dur + right->DUR;
  } else {
    ECTleft = left->ECT + right->DUR;
    ECTthis = -CLPFD_MAXINT2;
    th->DUR = left->DUR + right->DUR;
  }
  ECTright = right->ECT;

  if (ECTright >= ECTthis && ECTright >= ECTleft) {
    th->ECT = ECTright;
    th->EST = right->EST;
  } else if (ECTthis >= ECTleft) {
    th->ECT = ECTthis;
    th->EST = th->est;
  } else {
    th->ECT = ECTleft;
    th->EST = left->EST;
  }
}

static long 
compute_max_ect(long ECT1, long ECT2, long ECT3)
{
  if (ECT3 >= ECT2 && ECT3 >= ECT1) {
    return ECT3;
  } else if (ECT2 >= ECT1) {
    return ECT2;
  } else {
    return ECT1;
  }
}

/* max ECT of current Theta if an activity is removed */
static long 
tt_del_element_common(struct unary_data *pdata,
		      int rank,
		      long esta,
		      long dura)
{
  int nbtasks = pdata->nbtasks;
  int node = pdata->tt_map[rank];
  long ECT = -CLPFD_MAXINT2; /* fixed ECT of current node */

  if (pdata->tree[1].theta.EST > esta || pdata->tree[node].theta.state!=tstate_normal) {
    /* ECT in tree root doesn't use removed activity */
    return pdata->tree[1].theta.ECT;
  }

  if (!LEAF(node)) {
    ECT = compute_max_ect(
      pdata->tree[LSON(node)].theta.ECT + pdata->tree[node].theta.dur - dura + pdata->tree[RSON(node)].theta.DUR,
      pdata->tree[node].theta.dur == dura ? -CLPFD_MAXINT2 :
	(pdata->tree[node].theta.est + pdata->tree[node].theta.dur - dura + pdata->tree[RSON(node)].theta.DUR),
      pdata->tree[RSON(node)].theta.ECT);
  } else {
    ECT = pdata->tree[node].theta.dur == dura ? -CLPFD_MAXINT2 : (pdata->tree[node].theta.est + pdata->tree[node].theta.dur - dura);
  }

  while (!ROOT(node)) {
    int parent = PARENT(node);
    long pest, pdur;
    if (pdata->tree[parent].theta.state==tstate_normal) {
      pest = pdata->tree[parent].theta.est;
      pdur = pdata->tree[parent].theta.dur;
    } else {
      pest = -CLPFD_MAXINT2;
      pdur = 0;
    }
    if (LBROTHER(node)) {
      ECT = compute_max_ect(
	ECT + pdur + pdata->tree[RSON(parent)].theta.DUR,
	pest + pdur + pdata->tree[RSON(parent)].theta.DUR,
	pdata->tree[RSON(parent)].theta.ECT);
    } else {
      ECT = compute_max_ect(
	pdata->tree[LSON(parent)].theta.ECT + pdur + pdata->tree[RSON(parent)].theta.DUR - dura,
	pest + pdur + pdata->tree[RSON(parent)].theta.DUR - dura,
	ECT);
    }
    node = parent;
  }

  return ECT;
}

static long 
tt_del_element_ect(struct unary_data *pdata,
		   struct task *a)
{
  return tt_del_element_common(pdata,a->est_rank,est(a),dur(a));
}

static long 
tt_del_element_lst(struct unary_data *pdata,
		   struct task *a)
{
  return -tt_del_element_common(pdata,a->lct_rank,-lct(a),dur(a));
}


/* Compute the mapping from rank to node so that LSON and RSON work
   as expected.  E.g.
   nbtasks=1 tt_map={1}
   nbtasks=2 tt_map={2,1}
   nbtasks=3 tt_map={2,1,3}
   nbtasks=4 tt_map={4,2,1,3}
   nbtasks=5 tt_map={4,2,5,1,3}
   nbtasks=6 tt_map={4,2,5,1,6,3}
   nbtasks=7 tt_map={4,2,5,1,6,3,7}
*/
static void
tt_init_inorder(int node, long **mapp, int nbtasks)
{
  if (node <= nbtasks) {
    tt_init_inorder(LSON(node),mapp,nbtasks);
    *((*mapp)++) = node;
    tt_init_inorder(RSON(node),mapp,nbtasks);
  }			
}

static void
tt_init MAGIC (HIDDEN_PROTO
	       struct unary_data *pdata)
{
  long *map = pdata->tt_map;

  tt_init_inorder(1,&map,pdata->nbtasks);
  init_ranks(pdata);
}

/* THETA-LAMBDA-TREES */

static void 
tln_clear(struct theta_lambda_node *th)
{
  th->theta.state = tstate_empty;
  th->theta.dur = 0;
  th->theta.est = -CLPFD_MAXINT2;
  th->theta.DUR = 0;
  th->theta.EST = -CLPFD_MAXINT2;
  th->theta.ECT = -CLPFD_MAXINT2;
  th->lambda.ECT = -CLPFD_MAXINT2;
  th->lambda.DUR = 0;
  th->lambda.grayECT = -1;
  th->lambda.grayDUR = -1;
  th->lambda.gray = -1;
}

static void 
tln_recompute_leaf(struct theta_lambda_node *th)
{
  long est = th->theta.est;
  long dur = th->theta.dur;

  tn_recompute_leaf(&th->theta);
  if (th->theta.state==tstate_gray) {
    th->lambda.DUR = dur;
    th->lambda.ECT = est+dur;
    th->lambda.grayECT = th->lambda.gray;
    th->lambda.grayDUR = th->lambda.gray;
  } else {
    th->lambda.DUR = th->theta.DUR;
    th->lambda.ECT = th->theta.ECT;
    th->lambda.grayECT = -1;
    th->lambda.grayDUR = -1;
  }
}

static void 
tln_recompute_node(struct theta_lambda_node *th,
		   struct theta_lambda_node *left,
		   struct theta_lambda_node *right)
{
  long ECTleft1;
  long ECTleft2;
  long ECTthis;
  long ECT;
  long thdur, thelam, lamthe, thethethe;
  int  gray1;
  int  gray;
  
  tn_recompute_node(&th->theta, &left->theta, &right->theta);
  if (th->lambda.gray == -1 &&
      left->lambda.grayDUR == -1 && right->lambda.grayDUR == -1 &&
      left->lambda.grayECT == -1 && right->lambda.grayECT == -1) {
    th->lambda.DUR = th->theta.DUR;
    th->lambda.grayDUR = -1;
    th->lambda.ECT = th->theta.ECT;
    th->lambda.grayECT = -1;
    return;
  }
  thdur = th->theta.dur;
  thelam = left->theta.DUR + right->lambda.DUR;
  lamthe = left->lambda.DUR + right->theta.DUR;
  thethethe = left->theta.DUR + thdur + right->theta.DUR;
  switch (th->theta.state) {
  case tstate_empty:
    if (thelam >= lamthe) {
      th->lambda.DUR = thelam;
      th->lambda.grayDUR = right->lambda.grayDUR;
    } else {
      th->lambda.DUR = lamthe;
      th->lambda.grayDUR = left->lambda.grayDUR;
    }
    ECTleft1 = left->theta.ECT + right->lambda.DUR;
    gray1 = right->lambda.grayDUR;
    ECTleft2 = left->lambda.ECT + right->theta.DUR;
    ECTthis = -CLPFD_MAXINT2;
    break;
  case tstate_normal:
    if (thelam >= lamthe) {
      th->lambda.DUR = thdur + thelam;
      th->lambda.grayDUR = right->lambda.grayDUR;
    } else {
      th->lambda.DUR = thdur + lamthe;
      th->lambda.grayDUR = left->lambda.grayDUR;
    }
    ECTleft1 = left->theta.ECT + thdur + right->lambda.DUR;
    gray1 = right->lambda.grayDUR;
    ECTleft2 = left->lambda.ECT + thdur + right->theta.DUR;
    ECTthis = th->theta.est + thdur + right->lambda.DUR;
    break;
  case tstate_gray:
  default:
    if (thelam >= thethethe && thelam >= lamthe) {
      th->lambda.DUR = thelam;
      th->lambda.grayDUR = right->lambda.grayDUR;
    } else if (thethethe >= lamthe) {
      th->lambda.DUR = thethethe;
      th->lambda.grayDUR = th->lambda.gray;
    } else {
      th->lambda.DUR = lamthe;
      th->lambda.grayDUR = left->lambda.grayDUR;
    }
    ECTleft1 = left->theta.ECT + right->lambda.DUR;
    gray1 = right->lambda.grayDUR;
    if (thdur + right->theta.DUR > right->lambda.DUR) {
      ECTleft1 = left->theta.ECT + thdur + right->theta.DUR;
      gray1 = th->lambda.gray;
    }
    ECTleft2 = left->lambda.ECT + right->theta.DUR;
    ECTthis = th->theta.est + thdur + right->theta.DUR;
  }

  ECT = right->lambda.ECT;
  gray = right->lambda.grayECT;
  
  if (ECT < ECTthis) {
    ECT = ECTthis;
    if (th->theta.state==tstate_gray)
      gray = th->lambda.gray;
    else
      gray = right->lambda.grayDUR;
  }
  
  if (ECT < ECTleft2) {
    ECT = ECTleft2;
    gray = left->lambda.grayECT;
  }
  
  if (ECT < ECTleft1) {
    ECT = ECTleft1;
    gray = gray1;
  }
  
  th->lambda.ECT = ECT;
  th->lambda.grayECT = gray;
}

static void 
tln_recompute_gray(struct theta_lambda_node *th,
		   struct theta_lambda_node *left, 
		   struct theta_lambda_node *right,
		   int gray)
{
  long thdur, thelam, lamthe, thethethe;
  
  if (th->theta.ECT != th->lambda.ECT) {
    tln_recompute_node(th, left, right);
    return;
  }

  th->lambda.grayECT = gray;
  tn_recompute_node(&th->theta, &left->theta, &right->theta);
  thdur = th->theta.dur;
  thelam = left->theta.DUR + right->lambda.DUR;
  lamthe = left->lambda.DUR + right->theta.DUR;
  thethethe = left->theta.DUR + thdur + right->theta.DUR;

  switch (th->theta.state) {
  case tstate_empty:
    if (thelam >= lamthe) {
      th->lambda.DUR = thelam;
      th->lambda.grayDUR = right->lambda.grayDUR;
    } else {
      th->lambda.DUR = lamthe;
      th->lambda.grayDUR = left->lambda.grayDUR;
    }
    break;
  case tstate_normal:
    if (thelam >= lamthe) {
      th->lambda.DUR = thdur + thelam;
      th->lambda.grayDUR = right->lambda.grayDUR;
    } else {
      th->lambda.DUR = thdur + lamthe;
      th->lambda.grayDUR = left->lambda.grayDUR;
    }
    break;
  case tstate_gray:
    if (thelam >= thethethe && thelam >= lamthe) {
      th->lambda.DUR = thelam;
      th->lambda.grayDUR = right->lambda.grayDUR;
    } else if (thethethe >= lamthe) {
      th->lambda.DUR = thethethe;
      th->lambda.grayDUR = th->lambda.gray;
    } else {
      th->lambda.DUR = lamthe;
      th->lambda.grayDUR = left->lambda.grayDUR;
    }
  }
}

static void 
tlt_bottom_up(struct unary_data *pdata)
{
  int nbtasks = pdata->nbtasks;
  int i;
  
  for (i = nbtasks; LEAF(i); i--) {
    tln_recompute_leaf(&pdata->tree[i]);
  }
  for (; ISNODE(i); i--) {
    tln_recompute_node(&pdata->tree[i],
		       &pdata->tree[LSON(i)],
		       &pdata->tree[RSON(i)]);
  }
}

static void 
tlt_gray_activity(struct unary_data *pdata,
		  int rank)
{
  int nbtasks = pdata->nbtasks;
  int node = pdata->tt_map[rank];
  
  pdata->tree[node].theta.state = tstate_gray;
  pdata->tree[node].lambda.gray = rank;
  if (LEAF(node)) {
    tln_recompute_leaf(&pdata->tree[node]);
    node = PARENT(node);
  }
  while (ISNODE(node)) {
    tln_recompute_gray(&pdata->tree[node],
		       &pdata->tree[LSON(node)], 
		       &pdata->tree[RSON(node)],
		       rank);
    node = PARENT(node);
  }
}

static void 
tlt_clear(struct unary_data *pdata)
{
  int nbtheta = 2*pdata->nbtasks;
  int i;
  
  for (i=1; i<nbtheta; i++)
    tln_clear(&pdata->tree[i]);
}

static void
tlt_fix_path(struct unary_data *pdata,
	     int node)
{
  int nbtasks = pdata->nbtasks;
  if (LEAF(node)) {
    tln_recompute_leaf(&pdata->tree[node]);
    node = PARENT(node);
  }
  while (ISNODE(node)) {
    tln_recompute_node(&pdata->tree[node],
		       &pdata->tree[LSON(node)],
		       &pdata->tree[RSON(node)]);
    node = PARENT(node);
  }
}

static void 
tlt_place(struct unary_data *pdata,
	  int rank,
	  long est,
	  long dur,
	  enum tstate state)
{
  int node = pdata->tt_map[rank];
  struct theta_lambda_node *th = &pdata->tree[node];
  
  th->theta.state = tstate_normal;
  th->theta.est = est;
  th->theta.dur = dur;
  switch (state) {
  case tstate_normal:
    tlt_fix_path(pdata,node);
    break;
  case tstate_gray:
    tlt_gray_activity(pdata,rank);
  case tstate_empty:
    break;
  }
}

static void 
tlt_remove(struct unary_data *pdata,
	   int rank)
{
  int node = pdata->tt_map[rank];
  struct theta_lambda_node *th = &pdata->tree[node];
  
  tln_clear(th);
  tlt_fix_path(pdata,node);
}

/* max ECT of current Theta+(the activity rank)+(at most one activity of Lambda) */
static long 
tlt_del_element_common(struct unary_data *pdata,
		       int rank,
		       long esta,
		       long dura,
		       int *grayp)
{
  int node = pdata->tt_map[rank];
  long ECT = pdata->tree[1].lambda.ECT; /* from the root */
  int grayECT = pdata->tree[1].lambda.grayECT; /* from the root */

  if (pdata->tree[1].theta.EST <= esta && pdata->tree[node].theta.state==tstate_normal) {
    /* ECT in tree root uses the removed activity */
    tlt_remove(pdata, rank);
    ECT = pdata->tree[1].lambda.ECT; /* from the root */
    grayECT = pdata->tree[1].lambda.grayECT; /* from the root */
    tlt_place(pdata, rank, esta, dura, tstate_normal);
  }
  *grayp = grayECT;
  return ECT;
}

static long 
tlt_del_element_ect(struct unary_data *pdata,
		    struct task *a,
		    int *grayp)
{
  return tlt_del_element_common(pdata,a->est_rank,est(a),dur(a),grayp);
}

static long 
tlt_del_element_lst(struct unary_data *pdata,
		    struct task *a,
		    int *grayp)
{
  return -tlt_del_element_common(pdata,a->lct_rank,-lct(a),dur(a),grayp);
}


/* DETECTABLE PRECEDENCES */

#define CACHE_CLEAR c1=c2=NULL;

#define CACHE_ADD(T) c2=c1; c1=(T);

#define CACHE_MORE_THAN(T) (c2 || (c1 && c1!=(T)))

static SP_BOOL
unary_detect_precedences MAGIC (HIDDEN_PROTO
				struct unary_data *pdata)
{
  int nbtasks = pdata->nbtasks;
  struct event *event = pdata->event;
  struct task *tasks = pdata->tasks;
  int i, j;
  int nbevent = nbtasks<<1;
  struct task *c1, *c2;

  for (i=0, j=0; i<nbtasks; i++, j+=2) {
    event[j].key = (ect(tasks+i)<<1);
    event[j].task = (tasks+i);
    event[j+1].key = (lst(tasks+i)<<1)+1;
    event[j+1].task = (tasks+i);
  }
  qsort_asc_event_ptrs(pdata,nbevent);
  CACHE_CLEAR;
  tlt_clear(pdata);
  for (j=0; j<nbevent; j++) {
    struct event *ev = pdata->eventp[j];
    struct task *t = ev->task;
    
    if (ev->key & 1) { /* lst item */
      if (enable(t)<2) {
	tlt_place(pdata, t->est_rank, est(t), dur(t), tstate_gray);
      } else {
	CACHE_ADD(t);
	tlt_place(pdata, t->est_rank, est(t), dur(t), tstate_normal);
      }
    } else {
      int rank;
      
      if (CACHE_MORE_THAN(t)) { /* t is preceded by omega-t */
	int ti = t-tasks;
	long lb = tt_del_element_ect(pdata,t);
	SYNCLB(t,ti,lb);
      }

      if (enable(t)==2) {
	while (tlt_del_element_ect(pdata,t,&rank) > lst(t)) {
	  struct task *to = pdata->est_rank[rank];
	  tlt_remove(pdata,rank);
	  enable(to) = 0;
	}
      }
    }
  }
  CACHE_CLEAR;
  tlt_clear(pdata);
  for (j--; j>=0; j--) {
    struct event *ev = pdata->eventp[j];
    struct task *t = ev->task;
    
    if (!(ev->key & 1)) { /* ect item */
      if (enable(t)<2) {
	tlt_place(pdata, t->lct_rank, -lct(t), dur(t), tstate_gray);
      } else {
	CACHE_ADD(t);
	tlt_place(pdata, t->lct_rank, -lct(t), dur(t), tstate_normal);
      }
    } else {
      int rank;

      if (CACHE_MORE_THAN(t)) { /* t precedes omega-t */
	int ti = t-tasks;
	long ub = tt_del_element_lst(pdata,t);
	SYNCUB(t,ti,ub);
      }

      if (enable(t)==2) {
	while (tlt_del_element_lst(pdata,t,&rank) < ect(t)) {
	  struct task *to = pdata->lct_rank[rank];
	  tlt_remove(pdata,rank);
	  enable(to) = 0;
	}
      }
    }
  }
  return TRUE;
}


/* NOT-FIRST & NOT-LAST */

static SP_BOOL
unary_not_first_last MAGIC (HIDDEN_PROTO
			    struct unary_data *pdata)
{
  int nbtasks = pdata->nbtasks;
  struct event *event = pdata->event;
  struct task *tasks = pdata->tasks;
  int i, j;
  int nbevent = nbtasks<<1;
  long limt=0, limtl=0;
  struct task *c1, *c2;

  CACHE_CLEAR;
  tlt_clear(pdata);
  for (i=0, j=0; i<nbtasks; i++, j+=2) {
    event[j].key = (lct(tasks+i)<<1);
    event[j].task = (tasks+i);
    event[j+1].key = (lst(tasks+i)<<1)+1;
    event[j+1].task = (tasks+i);
  }
  qsort_asc_event_ptrs(pdata,nbevent);
  for (j=0; j<nbevent; j++) {
    struct event *ev = pdata->eventp[j];
    struct task *t = ev->task;
    if (ev->key & 1) { /* lst item */
      limtl = lst(t);
      if (enable(t)<2) {
	tlt_place(pdata, t->est_rank, est(t), dur(t), tstate_gray);
      } else {
	limt = limtl;
	CACHE_ADD(t);
	tlt_place(pdata, t->est_rank, est(t), dur(t), tstate_normal);
      }
    } else {
      int rank;

      if (CACHE_MORE_THAN(t)) {
	int ti = t-tasks;
	if (tt_del_element_ect(pdata,t) > lst(t)) /* t is not last among omega */
	  SYNCUB(t,ti,limt);
      }

      if (enable(t)==2 && limtl < ect(t)) {
	while (tlt_del_element_ect(pdata,t,&rank) > lst(t)) {
	  struct task *to = pdata->est_rank[rank];
	  tlt_remove(pdata,rank);
	  enable(to) = 0;
	}
      }
    }
  }
  CACHE_CLEAR;
  tlt_clear(pdata);
  for (i=0, j=0; i<nbtasks; i++, j+=2) {
    event[j].key = (est(tasks+i)<<1)+1;
    event[j].task = (tasks+i);
    event[j+1].key = (ect(tasks+i)<<1);
    event[j+1].task = (tasks+i);
  }
  qsort_asc_event_ptrs(pdata,nbevent);
  for (j--; j>=0; j--) {
    struct event *ev = pdata->eventp[j];
    struct task *t = ev->task;
    if (!(ev->key & 1)) { /* lst item */
      limtl = ect(t);
      if (enable(t)<2) {
	tlt_place(pdata, t->lct_rank, -lct(t), dur(t), tstate_gray);
      } else {
	limt = limtl;
	CACHE_ADD(t);
	tlt_place(pdata, t->lct_rank, -lct(t), dur(t), tstate_normal);
      }
    } else {
      int rank;

      if (CACHE_MORE_THAN(t)) {
	int ti = t-tasks;
	if (tt_del_element_lst(pdata,t) < ect(t)) /* t is not first among omega */
	  SYNCLB(t,ti,limt);
      }

      if (enable(t)==2 && limtl > lst(t)) {
	while (tlt_del_element_lst(pdata,t,&rank) < ect(t)) {
	  struct task *to = pdata->lct_rank[rank];
	  tlt_remove(pdata,rank);
	  enable(to) = 0;
	}
      }
    }
  }
  return TRUE;
}


/* FIRST & LAST (EDGE-FINDING) */

static SP_BOOL
unary_first_last(struct unary_data *pdata)
{
  int nbtasks = pdata->nbtasks;
  struct theta_lambda_node *root = &pdata->tree[1];
  int i, di, dj;
  
  tlt_clear(pdata);

  /* Put all activities into theta tree */
  for (i=0; i < nbtasks; i++) {
    struct task *t = &pdata->tasks[i];
    
    if (enable(t)==2)
      tlt_place(pdata, t->est_rank, est(t), dur(t), tstate_empty);
  }

  tlt_bottom_up(pdata);

  /* Form set of obligatory tasks by descending lct. */
  for (di=0, dj=0; di<nbtasks; di++) {
    struct task *t = pdata->lct_rank[di];
    
    if (enable(t)==2)
      pdata->aux_rank[dj++] = t;
  }

  /* In descending order by lct remove activities (ie make them gray) */
  for (di=0; di<dj-1; di++) {
    long lct;
    struct task *t = pdata->aux_rank[di];

    tlt_gray_activity(pdata,t->est_rank);
    
    lct = lct(pdata->aux_rank[di+1]);
    if (root->theta.ECT > lct)
      return FALSE;

    while (root->lambda.ECT > lct) {
      int rank = root->lambda.grayECT;
      struct task *t = pdata->est_rank[rank];
      int ti = t - pdata->tasks;
      long ect = root->theta.ECT;

      SYNCLB(t,ti,ect);
      tlt_remove(pdata,rank);
    }
  }
  
  tlt_clear(pdata);

  /* Put all activities into theta tree */
  for (i=0; i < nbtasks; i++) {
    struct task *t = &pdata->tasks[i];
    
    if (enable(t)==2)
      tlt_place(pdata, t->lct_rank, -lct(t), dur(t), tstate_empty);
  }

  tlt_bottom_up(pdata);

  /* Form set of obligatory tasks by descending lct. */
  for (di=0, dj=0; di<nbtasks; di++) {
    struct task *t = pdata->est_rank[di];
    
    if (enable(t)==2)
      pdata->aux_rank[dj++] = t;
  }

  /* In ascending order by est remove activities (ie make them gray) */
  for (di=0; di<dj-1; di++) {
    long est;
    struct task *t = pdata->aux_rank[di];

    tlt_gray_activity(pdata,t->lct_rank);
    
    est = est(pdata->aux_rank[di+1]);
    if (-root->theta.ECT < est)
      return FALSE;

    while (-root->lambda.ECT < est) {
      int rank = root->lambda.grayECT;
      struct task *t = pdata->lct_rank[rank];
      int ti = t - pdata->tasks;
      long lst = -root->theta.ECT;

      SYNCUB(t,ti,lst);
      tlt_remove(pdata,rank);
    }
  }
  return TRUE;
}


/* EXPLICIT PRECEDENCES AS SIDE-CONSTRAINTS */

struct diff_constraint {
  long si;
  long sj;
};

static SP_BOOL
unary_detect_all_precedences MAGIC (HIDDEN_PROTO
				    struct unary_data *pdata,
				    struct diff_constraint *dc,
				    int nbdiffs);


/* ENERGETIC REASONING WITHIN TASK INTERVALS */

struct ti_index {
  long key;
  struct ti_index *val;		/* subtree for prefix */
  struct ti_index *next;	/* to tree of the next prefix */
};

struct task_interval {
  long a;
  long b;
  long slack;
};

#define get_ti_index(ROW,COL) (pdata->ti_index+5*(ROW)+(COL))

#define ti_subsumes(TI1,TI2)								\
((TI1)->a >= (TI2)->a &&								\
 (TI1)->b <= (TI2)->b &&								\
 (((TI1)->b-(TI1)->a)-((TI2)->b-(TI2)->a)) * limit >= (TI1)->slack-(TI2)->slack)	\


/* compute area of <est,lct,dur> that must overlap [lb,ub) */
static long 
min_overlap(struct task *t, long lb, long ub)
{
  long ltmp1=ect(t)-lb, ltmp2=ub-lst(t);
  long r = (ltmp1<ltmp2 ? ltmp1 : ltmp2);

  if (r<=0)
    return 0;
  if (r>dur(t))
    r = dur(t);
  if (r>ub-lb)
    r = ub-lb;
  return r * res(t);
}

/* We need an O(N log N) algorithm for building a 4-level "indexing
   tree" of tasks.  

   For adjusting ends, the tasks should be sorted by
   [est(up)][lct(down)][lst(up)][area(down)].
   For adjusting starts, the tasks should be sorted by
   [lct(down)][est(up)][ect(down)][area(down)].
*/
static void 
ti_build_index MAGIC (HIDDEN_PROTO
		      struct unary_data *pdata, SP_BOOL up)
{
  int nbobltasks = pdata->nbobltasks;
  int i, j;

  for (i=0, j=0; i<nbobltasks; i++) {
    struct task *t = pdata->obltasks[i];
    if (lct(t)-est(t)>dur(t))
      pdata->ti_tasks[j++] = t;
  }
  nbobltasks = j;
  if (up)
    qsort_ti_up(pdata->ti_tasks,nbobltasks);
  else
    qsort_ti_down(pdata->ti_tasks,nbobltasks);
  for (i=0; i<nbobltasks; i++) {
    struct ti_index *tix = get_ti_index(i,0);
    struct task *t = pdata->ti_tasks[i];

    for (j=0; j<5; j++) {
      (tix+j)->val = j<4 ? (tix+j+1) : NULL;
      (tix+j)->next = (j==0 && i<nbobltasks-1) ? (tix+5) : NULL;
    }
    if (up) {
      (tix+0)->key = est(t);
      (tix+1)->key = lct(t);
      (tix+2)->key = lst(t);
      (tix+3)->key = dur(t)*res(t);
      (tix+4)->key = (long)t;
    } else {
      (tix+0)->key = lct(t);
      (tix+1)->key = est(t);
      (tix+2)->key = ect(t);
      (tix+3)->key = dur(t)*res(t);
      (tix+4)->key = (long)t;
    }
  }
  for (j=0; j<4; j++) {
    for (i=nbobltasks-2; i>=0; i--) {
      struct ti_index *tix = get_ti_index(i,j);

      if (tix->next && tix->key==tix->next->key) {
	tix->val->next = tix->next->val;
	tix->next = tix->next->next;
      }
    }
  }
}

static int 
ti_do_one_interval_up(struct unary_data *pdata,
		      long a,
		      long b,
		      long slack)
{
  int rc = 0;
  struct ti_index *tix1, *tix2, *tix3, *tix4, *tix5;
  
  for (tix1 = pdata->ti_index; tix1 && tix1->key < a; tix1 = tix1->next) {
    for (tix2 = tix1->val; tix2 && tix2->key > a; tix2 = tix2->next) {
      for (tix3 = tix2->val; tix3 && tix3->key < b; tix3 = tix3->next) {
	for (tix4 = tix3->val; tix4 && tix4->key > slack; tix4 = tix4->next) {
	  for (tix5 = tix4->val; tix5; tix5 = tix5->next) {
	    struct task *t = (struct task *)tix5->key;
	    long a2 = a;
	    long b2 = b;
	    long res = res(t);

	    slack += min_overlap(t,a,b);
	    if (b2 > lct(t))
	      b2 = lct(t);
	    if (a2 < lst(t))
	      a2 = lst(t);
	    if ((b2-a2)*res > slack && (lct(t)-a)*res > slack) {
	      lctmax(t) = a+slack/res;
	      SYNCRC(t);
	      if (rc>=2)
		return rc;
	    }
	  }
	}
      }
    }
  }
  return rc;
}

static int 
ti_do_one_interval_down(struct unary_data *pdata,
			long a,
			long b,
			long slack)
{
  int rc = 0;
  struct ti_index *tix1, *tix2, *tix3, *tix4, *tix5;
  
  for (tix1 = pdata->ti_index; tix1 && tix1->key > b; tix1 = tix1->next) {
    for (tix2 = tix1->val; tix2 && tix2->key < b; tix2 = tix2->next) {
      for (tix3 = tix2->val; tix3 && tix3->key > a; tix3 = tix3->next) {
	for (tix4 = tix3->val; tix4 && tix4->key > slack; tix4 = tix4->next) {
	  for (tix5 = tix4->val; tix5; tix5 = tix5->next) {
	    struct task *t = (struct task *)tix5->key;
	    long a2 = a;
	    long b2 = b;
	    long res = res(t);

	    slack += min_overlap(t,a,b);
	    if (b2 > ect(t))
	      b2 = ect(t);
	    if (a2 < est(t))
	      a2 = est(t);
	    if ((b2-a2)*res > slack && (b-est(t))*res > slack) {
	      est(t) = b-slack/res;
	      SYNCRC(t);
	      if (rc>=2)
		return rc;
	    }
	  }
	}
      }
    }
  }
  return rc;
}

/* An O(N^2) algorithm for computing all task intervals and their slacks. 
   Given fixed t and a, minoverlap(t,a,b) is a piecewise linear function in b
   with at most 3 segments: a plateau, a slope, and a plateau.

   Algorithm idea:
   For each distinct a=est(t):
     Build an event queue with events (break ties arbitrarily):
     LCT(00): event date is a unique b=lct(t) > a.
     INC(01): event date is the beginning of a slope.
     DEC(1.): event date is the end of a slope.
     Do the sweep.  For each LCT event, record a task interval <a,b,slack>.
*/
static SP_BOOL 
ti_collect_intervals MAGIC (HIDDEN_PROTO
			    struct unary_data *pdata,
			    long limit)
{
  int nbobltasks = pdata->nbobltasks;
  struct event *event = pdata->event;
  long term, maxarea, maxb;
  int i, j, k, n, coeff, nbest, nblct, nbevent;

  for (i=0, j=0; i<nbobltasks; i++) {
    struct task *t = pdata->obltasks[i];
    
    pdata->est[j] = est(t);
    pdata->lct[j++] = lct(t);
  }
  qsort_asc_long(pdata->est,nbobltasks);
  qsort_asc_long(pdata->lct,nbobltasks);
  for (i=1, j=1; i<nbobltasks; i++) {
    if (pdata->est[j-1]!=pdata->est[i])
      pdata->est[j++] = pdata->est[i];
  }
  nbest = j;
  for (i=1, j=1; i<nbobltasks; i++) {
    if (pdata->lct[j-1]!=pdata->lct[i])
      pdata->lct[j++] = pdata->lct[i];
  }
  nblct = j;

  n=0;
  for (i=0; i<nbest; i++) {
    long a = pdata->est[i];
    coeff = 0;
    term = 0;
    maxarea = 0;
    maxb = 0;
    for (j=0, k=0; j<nblct; j++) {
      long b = pdata->lct[j];
      if (a<b && (i>0 || j<nblct-1)) {
	(event+k)->key = maxb = b<<2;	/* LCT event */
	(event+k)->task = NULL;
	k++;
      }
    }
    for (j=0; j<nbobltasks; j++) {	/* I should sort... */
      struct task *t = pdata->obltasks[j];
      long area = dur(t)*res(t);
      long m;

      if (lct(t)>a && lct(t)-est(t)>dur(t) && maxarea<area)
	maxarea = area;
      if (a >= ect(t))
	continue;
      m = ect(t)-a;
      if (m > dur(t))
	m = dur(t);
      if (a >= lst(t)) {
	long key = ((a+m)<<2)+3; /* PLATEAU event, a >= lst(t) */
	coeff += res(t);	/* Total += res(t)*(b-a) */
	if (key < maxb) {
	  (event+k)->key = key;
	  (event+k)->task = t;
	  k++;
	}
      } else {
	long key = ((lst(t))<<2)+1; /* SLOPE event, a < lst(t) */
	if (key < maxb) {
	  (event+k)->key = key;
	  (event+k)->task = t;
	  k++;
	}
	key = ((lst(t)+m)<<2)+2; /* PLATEAU event, a < lst(t) */
	if (key < maxb) {
	  (event+k)->key = key;
	  (event+k)->task = t;
	  k++;
	}
      }
    }
    nbevent = k;
    qsort_asc_event_ptrs(pdata,nbevent);
    for (k=0; k<nbevent; k++) {
      struct event *ev = pdata->eventp[k];
      long key = ev->key;
      struct task *t = ev->task;
      long b = key>>2;
      long slack;

      switch (key & 0x3) {
      case 0:			/* LCT event */
	slack = (limit-coeff)*(b-a)-term;
	if (slack < 0) {
	  return FALSE;
	} else if (slack < maxarea) {
	  struct task_interval *ti = pdata->task_interval+n;
	  struct task_interval *ti0 = ti-1;
	  ti->a = a;
	  ti->b = b;
	  ti->slack = slack;
	  if (n>0 && ti_subsumes(ti0,ti))
	    ;
	  else if (n>0 && ti_subsumes(ti,ti0))
	    *ti0 = *ti;
	  else
	    n++;
	}
	break;
      case 1: 			/* SLOPE event, a < lst(t) */
	coeff += res(t);
	term += res(t)*(a - lst(t));
	break;
      case 2:			/* PLATEAU event, a < lst(t) */
      case 3:			/* PLATEAU event, a >= lst(t) */
      default:
	coeff -= res(t);
	term += res(t)*(b - a);
	break;
      }
    }
  }
  pdata->nbti = n;
  return TRUE;
}

static int
task_interval_filtering MAGIC (HIDDEN_PROTO
			       struct unary_data *pdata,
			       long limit)
{
  int i, j;
  int rc=0;
  int nbtasks = pdata->nbtasks;

  for (i=0, j=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;

    if (enable(t)==2)
      pdata->obltasks[j++] = t;
  }
  if (j<=2)
    return 0;
  pdata->nbobltasks = j;
  if (!ti_collect_intervals(pdata,limit))
    return 3;
  if (pdata->nbti>0) {
    ti_build_index(pdata,TRUE);
    for (i=0; i<pdata->nbti; i++) {
      struct task_interval *ti = pdata->task_interval+i;
      rc |= ti_do_one_interval_up(pdata, ti->a, ti->b, ti->slack);
      if (rc>=2)
	return rc;
    }
    ti_build_index(pdata,FALSE);
    for (i=0; i<pdata->nbti; i++) {
      struct task_interval *ti = pdata->task_interval+i;
      rc |= ti_do_one_interval_down(pdata, ti->a, ti->b, ti->slack);
      if (rc>=2)
	return rc;
    }
  }
  return rc;
}


/* BETTER PROPAGATION FOR NON-PREEMPTIVE SINGLE-RESOURCE CONSTRAINT PROBLEMS
   ARMIN WOLF, ERCIM WS 2004 */

struct wolf_data {
  char *in_set;
  char *forbidden;
  long *space_after;
  long *minmax;
  struct task *tasks;
  long  ect;
  int  size;
  int  n;
  SP_BOOL doing_est;
};

static struct wolf_data *
wolf_alloc MAGIC (HIDDEN_PROTO
		  int n,
		  struct task *tasks,
		  SP_BOOL doing_est)
{
  struct wolf_data *wolf;
  char *ptr;

  wolf = fd_malloc(sizeof(struct wolf_data) + n*(2*sizeof(long)+2));
  ptr = (char *)(wolf+1);
  wolf->in_set = ptr;
  ptr += n;
  wolf->forbidden = ptr;
  ptr += n;
  wolf->space_after = (long *)ptr;
  ptr += n*sizeof(long);
  wolf->minmax = (long *)ptr;
  ptr += n*sizeof(long);
  wolf->tasks = tasks;
  wolf->n = n;
  wolf->doing_est = doing_est;
  return wolf;
}

static void
wolf_init(struct wolf_data *wolf)
{
  int n = wolf->n;
  int i;
  
  for (i=n-1; i>=0; i--) {
    wolf->in_set[i] = 0;
    wolf->forbidden[i] = 0;
    wolf->space_after[i] = 0;
  }

  wolf->ect = -CLPFD_MAXINT2;
  wolf->size = 0;
}

static void
wolf_insert(struct wolf_data *wolf,
	    struct task *ti)
{
  struct task *tasks = wolf->tasks;
  long diff;
  int i = ti-tasks;
  int j;
  SP_BOOL doing_est = wolf->doing_est;

  long esti = (doing_est ? est(ti) : -lct(ti));
  long ecti = (doing_est ? ect(ti) : -lst(ti));
  long duri = dur(ti);
  long new_space = esti - wolf->ect;

  if (new_space > 0) {
    for (j=0; j<wolf->n; j++) {
      long estj = (doing_est ? est(tasks+j) : -lct(tasks+j));
      if (i==j || wolf->in_set[j] || wolf->forbidden[j] || enable(tasks+j)<2)
	continue;
      diff = estj - esti;
      if (diff < 0)
	wolf->space_after[j] += (new_space < -diff ? new_space : -diff);
    }
    wolf->ect = ecti;
  } else {
    long taken_space = wolf->space_after[i];
    if (taken_space > 0) {
      if (taken_space > duri)
	taken_space = duri;
      for (j=0; j<wolf->n; j++) {
	long estj = (doing_est ? est(tasks+j) : -lct(tasks+j));
	if (i==j || wolf->in_set[j] || wolf->forbidden[j] || enable(tasks+j)<2)
	  continue;
	diff = estj - esti;
	if (diff <= 0) {
	  wolf->space_after[j] -= taken_space;
	} else {
	  long decr = taken_space - (wolf->space_after[i] - wolf->space_after[j]);
	  if (decr > 0)
	    wolf->space_after[j] -= decr;
	}
      }
    }
    diff = wolf->space_after[i] - duri;
    if (diff < 0)
      wolf->ect -= diff;
  }
  wolf->in_set[i] = 1;
  wolf->size++;
}

static long
wolf_increase(struct wolf_data *wolf)
{
  long inf = CLPFD_MAXINT2;
  struct task *choice = NULL;
  long estc = CLPFD_MAXINT2;
  SP_BOOL doing_est = wolf->doing_est;
  long esti, ecti, duri, ect2;
  int i;

  for (i=0; i<wolf->n; i++) {
    struct task *ti = wolf->tasks+i;
    if (wolf->in_set[i] || wolf->forbidden[i] || enable(ti)<2)
      continue;
    esti = (doing_est ? est(ti) : -lct(ti));
    ecti = (doing_est ? ect(ti) : -lst(ti));
    duri = dur(ti);    
    if (esti > wolf->ect) {
      ect2 = ecti;
    } else if (wolf->space_after[i] < duri) {
      ect2 = wolf->ect + duri - wolf->space_after[i];
    } else {
      ect2 = wolf->ect;
    }

    if (ect2<inf || (ect2==inf && esti < estc)) {
      inf = ect2;
      choice = ti;
      estc = esti;
    }
  }

  wolf_insert(wolf,choice);

  return (doing_est ? wolf->ect : -wolf->ect);
}
    
static SP_BOOL
wolf_filtering MAGIC (HIDDEN_PROTO
		      struct unary_data *pdata)
{
  int n = pdata->nbtasks;
  struct task *tasks = pdata->tasks;

  struct wolf_data *min_before = wolf_alloc(n,tasks,TRUE);
  struct wolf_data *min_after = wolf_alloc(n,tasks,FALSE);
  int min_before_size, min_after_size;
  int i, j, nreq;
  SP_BOOL rc = TRUE;


  for (i = 0; i < n && rc; i++) {
    struct task *ti = tasks+i;
    long nest = pdata->lb[i];
    long nlct = pdata->ub[i];

    wolf_init(min_before);
    wolf_init(min_after);
    min_before->forbidden[i] = 1;
    min_after->forbidden[i] = 1;

    nreq = n;
    for (j = 0; j < n; j++) {
      if (i == j || enable(tasks+j)<2) {
	nreq--;
      } else if (must_precede(tasks+i,tasks+j)) { /* Precede(i, j) */
        wolf_insert(min_after,tasks+j);
        min_before->forbidden[j] = 1;
      } else if (must_precede(tasks+j,tasks+i)) { /* Precede(j, i) */
        wolf_insert(min_before,tasks+j);
        min_after->forbidden[j] = 1;
      }
    }

    if (nest < min_before->ect)
      nest = min_before->ect;
    if (nlct > -min_after->ect)
      nlct = -min_after->ect;

    min_before_size = min_before->size;
    min_after_size = min_after->size;

    min_before->minmax[min_before->size] = min_before->ect;
    min_after->minmax[min_after->size] = -min_after->ect;
    while (min_before->size < nreq - min_after_size && min_before->ect <= lst(ti)) {
      wolf_increase(min_before);
      min_before->minmax[min_before->size] = min_before->ect;
    }
    while (min_after->size < nreq - min_before_size && -min_after->ect >= ect(ti)) {
      wolf_increase(min_after);
      min_after->minmax[min_after->size] = -min_after->ect;
    }

    /* Before i, there can be min_before->size - 1 activities maximum. */
    /* Therefore, at least nreq - (min_before->size - 1) activities */
    /* must be after i. I.e:  lct(ti) <= min_after->minmax[n - min_before->size] */
    
    if (min_before->ect > lst(ti)) {
      if (nreq + 1 - min_before->size > min_after->size)
        nlct = -CLPFD_MAXINT2; /* FAIL */
      else if (nlct > min_after->minmax[nreq + 1 - min_before->size])
        nlct = min_after->minmax[nreq + 1 - min_before->size];
    }
    if (pdata->ub[i] > nlct) {
      pdata->ub[i] = nlct;
      if (ect(ti) > nlct && enable(ti)==2)
	rc = FALSE;
    }
    if (-min_after->ect < ect(ti)) {
      if (nreq + 1 - min_after->size > min_before->size)
        nest = CLPFD_MAXINT2; /* FAIL */
      else if (nest < min_before->minmax[nreq + 1 - min_after->size])
        nest = min_before->minmax[nreq + 1 - min_after->size];
    }
    if (pdata->lb[i] < nest) {
      pdata->lb[i] = nest;
      if (lst(ti) < nest && enable(ti)==2)
	rc = FALSE;
    }
  }
  Free(min_before);
  Free(min_after);
  return rc;
}




/* FILTERING */

static struct unary_data *
unary_alloc MAGIC (HIDDEN_PROTO
		   struct task *tasks,
		   int nbtasks,
		   int nbtotal,
		   int flags)
{
  struct unary_data *pdata;
  int nbt2 = nbtasks*nbtasks;
  char *ptr;
  int i, msize;

  msize = 3*nbtasks*sizeof(struct event)
        + 3*nbtasks*sizeof(struct event *)
        + (4*nbtasks+4)*sizeof(struct comppart)
        + 2*nbtasks*sizeof(struct theta_lambda_node)
        + nbtotal*sizeof(long)
        + 6*nbtasks*sizeof(long);
  if (flags & 2) {
    msize += 4*nbtasks*sizeof(long)
          +  5*nbtasks*sizeof(struct ti_index)
          +  nbt2*sizeof(struct task_interval);
  }
  ptr = fd_malloc(sizeof(struct unary_data) + msize);
  pdata = (struct unary_data *)ptr;
  ptr += sizeof(struct unary_data);
  pdata->tasks = tasks;
  pdata->nbtasks = nbtasks;
  pdata->flags = flags;
  pdata->event = (struct event *)ptr;
  ptr += 3*nbtasks*sizeof(struct event);
  pdata->eventp = (struct event **)ptr;
  ptr += 3*nbtasks*sizeof(struct event *);
  pdata->cp = (struct comppart *)ptr;
  pdata->cp2 = pdata->cp + 2*nbtasks+2;
  ptr += (4*nbtasks+4)*sizeof(struct comppart);
  pdata->id_map = (long *)ptr;
  ptr += nbtotal*sizeof(long);
  pdata->tree = (struct theta_lambda_node *)ptr;
  ptr += 2*nbtasks*sizeof(struct theta_lambda_node);
  pdata->est_rank = (struct task **)ptr;
  ptr += nbtasks*sizeof(struct task *);
  pdata->lct_rank = (struct task **)ptr;
  ptr += nbtasks*sizeof(struct task *);
  pdata->aux_rank = (struct task **)ptr;
  ptr += nbtasks*sizeof(struct task *);
  pdata->tt_map = (long *)ptr;
  ptr += nbtasks*sizeof(long);
  pdata->lb = (long *)ptr;
  ptr += nbtasks*sizeof(long);
  pdata->ub = (long *)ptr;
  ptr += nbtasks*sizeof(long);
  if (flags&2) {		/* extra for task intervals */
    pdata->est = (long *)ptr;
    ptr += nbtasks*sizeof(long);
    pdata->lct = (long *)ptr;
    ptr += nbtasks*sizeof(long);
    pdata->obltasks = (struct task **)ptr;
    ptr += nbtasks*sizeof(struct task *);
    pdata->ti_tasks = (struct task **)ptr;
    ptr += nbtasks*sizeof(struct task *);
    pdata->ti_index = (struct ti_index *)ptr;
    ptr += 5*nbtasks*sizeof(struct ti_index);
    pdata->task_interval = (struct task_interval *)ptr;
    ptr += nbt2*sizeof(struct task_interval);
  }
#if DBG
  if (ptr != (char *)(pdata+1)+msize)
    printf("SHOULDN'T HAPPEN: expected ptr=%p, got %p\n",
	   (char *)(pdata+1)+msize, ptr);
#endif

  if (nbtotal>0) {
    for (i=nbtotal-1; i>=0; i--)
      pdata->id_map[i] = -1;
    
    for (i=nbtasks-1; i>=0; i--)
      pdata->id_map[(tasks+i)->id] = i;
  }

  return pdata;
}

static int
unary_apply(struct unary_data *pdata)
{
  int nbtasks = pdata->nbtasks;
  int i, rc=0;

  for (i=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    long lb = pdata->lb[i];
    long ub = pdata->ub[i];

    switch (enable(t)) {
    case 2:			/* normal task */
      if (lb>est(t) || ub<lctmax(t)) {
	est(t) = lb;
	lctmax(t) = ub;
	if (lct(t)>ub)
	  lct(t) = ub;
	rc = 1;
      }
      break;
    case 1:			/* still optional */
      if (ub-lb < lctmax(t)-lst(t)) {
	est(t) = lb;
	lctmax(t) = ub;
	rc = 1;
      }
      break;
    case 0:			/* forced inactive */
      rc = 1;
    }      
  }
  return rc;
}

/* DOES NOT find a fixpoint */
static int
unary_filtering MAGIC (HIDDEN_PROTO
		       struct task *tasks,
		       int nbtasks,
		       int nbtotal,
		       int flags,
		       struct diff_constraint *dc,
		       int nbdiffs)
{
  struct unary_data *pdata;
  int rc, i;

  if (nbtasks<=1)
    return 0;
  pdata = unary_alloc(tasks,nbtasks,nbtotal,flags);
  for (i=nbtasks-1; i>=0; i--) {
    pdata->lb[i] = est(tasks+i);
    pdata->ub[i] = lctmax(tasks+i);
  }
  tt_init(pdata);
  if (!(unary_detect_precedences(pdata) &&
	unary_not_first_last(pdata) &&
	unary_first_last(pdata)))
    goto fail;
  rc = unary_apply(pdata);
  if (rc==0 && (flags&2)) {
    if (!(wolf_filtering(pdata)))
      goto fail;
    rc = unary_apply(pdata);
  }
  if (rc==0 && nbdiffs>0) {
    if (!(unary_detect_all_precedences(pdata,dc,nbdiffs)))
      goto fail;
    rc = unary_apply(pdata);
  }
  Free(pdata);
  return rc;
 fail:
  Free(pdata);
  return 3;
}


/* Normalization rules:
   - First segment is <-infty,0>
   - Last  segment is <+infty,0>
   - No consecutive segments of the same height, except
     last but one may have zero height.
*/
static SP_BOOL
compulsory_parts MAGIC (HIDDEN_PROTO
			struct unary_data *pdata,
			struct task *except,
			long start,
			long end,
			Dvar limitvar)
{
  int nbtasks = pdata->nbtasks;
  struct task *tasks = pdata->tasks;
  struct event *event = pdata->event;
  struct comppart *cp = pdata->cp;
  long height0, height;
  int i, j, nbev;

  for (i=0, j=0; i<nbtasks; i++) {
    struct task *t = tasks+i;
    
    if (t!=except &&
	enable(t)==2 &&
	lst(t) < ect(t) &&
	lst(t) < end &&
	ect(t) > start) {
      event[j].key = ((lst(t) > start ? lst(t) : start)<<1)+1;
      event[j].task = t;
      event[j+1].key = ((ect(t) < end ? ect(t) : end)<<1);
      event[j+1].task = t;
      j += 2;      
    }
  }
  nbev = j;
  start = -CLPFD_MAXINT;
  height0 = -CLPFD_MAXINT;
  height = 0;
  qsort_asc_event_ptrs(pdata,nbev);
  for (i=0, j=0; j<nbev; j++) {
    struct event *ev = pdata->eventp[j];
    long key = ev->key;
    long evdate = key>>1;
    struct task *t = ev->task;

    if (start<evdate && height0!=height) {
      if (dvar_fix_min_l(limitvar,height)<0)
	return FALSE;
      cp[i].start = start;
      cp[i].height = height;
      i++;
      height0 = height;
    }
    start = evdate;
    if (key & 1)		/* start event */
      height += res(t);
    else			/* end event */
      height -= res(t);
  }
  if (height0!=height) {
    cp[i].start = start;
    cp[i].height = height;
    i++;
  }
  cp[i].start = CLPFD_MAXINT;
  cp[i].height = 0;
  i++;
  return TRUE;
}

static int
comppart_emit(long start,
	     long height,
	     struct comppart *cp,
	     int e2)
{
  cp[e2].start = start;
  cp[e2++].height = height;
  if (e2>=2 && cp[e2-2].start==cp[e2-1].start) {
    e2--;
    cp[e2-1].height = cp[e2].height;
  }
  if (e2>=2 && cp[e2-2].height==cp[e2-1].height) {
    e2--;
  }
  return e2;
}


static void
compulsory_part_delete(struct unary_data *pdata,
		       long start,
		       long end,
		       long delta)
{
  struct comppart *cp = pdata->cp;
  struct comppart *cp2 = pdata->cp2;
  int e1, e2;

  e1 = 0;
  e2 = 0;
  while (cp[e1+1].start <= start) {
    e2 = comppart_emit(cp[e1].start, cp[e1].height, cp2, e2);
    e1++;
  }

  /* emit crumb before start (maybe empty) */
  e2 = comppart_emit(cp[e1].start, cp[e1].height, cp2, e2);

  /* emit subtracted except last part */
  while (cp[e1+1].start <= end) {
    e2 = comppart_emit(start, cp[e1].height - delta, cp2, e2);
    start = cp[e1+1].start;
    e1++;
  }

  /* emit subtracted, last part (maybe empty) */
  e2 = comppart_emit(start, cp[e1].height - delta, cp2, e2);
  start = end;

  /* emit everything after subtracted */
  while (start < CLPFD_MAXINT) {
    e2 = comppart_emit(start, cp[e1].height, cp2, e2);
    start = cp[e1+1].start;
    e1++;
  }
  cp2[e2++] = cp[e1++];
}


static int
cp_ends MAGIC (HIDDEN_PROTO
	       struct unary_data *pdata,
	       Dvar limitvar)
{
  long limit = dvar_max_l(limitvar);
  int nbtasks = pdata->nbtasks;
  int i, rc=0;

  if (!compulsory_parts(pdata,NULL,-CLPFD_MAXINT,CLPFD_MAXINT,limitvar))
    return 3;
  for (i=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    long limt = limit - res(t);
    struct comppart *cp;
    int e1;
    long ub;
    
    if (ect(t)==lctmax(t))
      continue;
    if (lst(t) < ect(t) && enable(t)==2) {
      compulsory_part_delete(pdata,lst(t),ect(t),res(t));
      cp = pdata->cp2;
    } else {
      cp = pdata->cp;
    }
    
    e1 = 0;
    ub = lctmax(t);
    while (cp[e1+1].start < ub)
      e1++;
    /* cp[e1].start < lctmax(t) <= cp[e1+1].start */
    while (cp[e1+1].start > ub-dur(t)) {
      if (cp[e1].height > limt)
	ub = cp[e1].start;
      e1--;
    }
    if (lctmax(t) > ub) {
      lctmax(t) = ub;
      SYNCRC(t);
    }
  }
  return rc;
}

static int
cp_starts MAGIC (HIDDEN_PROTO
		 struct unary_data *pdata,
		 Dvar limitvar)
{
  long limit = dvar_max_l(limitvar);
  int nbtasks = pdata->nbtasks;
  int i, rc=0;

  if (!compulsory_parts(pdata,NULL,-CLPFD_MAXINT,CLPFD_MAXINT,limitvar))
    return 3;
  for (i=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    long limt = limit - res(t);
    struct comppart *cp;
    int e1;
    long lb;
    
    if (ect(t)==lctmax(t))
      continue;
    if (lst(t) < ect(t) && enable(t)==2) {
      compulsory_part_delete(pdata,lst(t),ect(t),res(t));
      cp = pdata->cp2;
    } else {
      cp = pdata->cp;
    }
    
    e1 = 0;
    lb = est(t);
    while (cp[e1+1].start <= est(t))
      e1++;
    /* cp[e1].start <= est(t) < cp[e1+1].start */
    while (cp[e1].start < lb+dur(t)) {
      if (cp[e1].height > limt)
	lb = cp[e1+1].start;
      e1++;
    }
    if (est(t) < lb) {
      est(t) = lb;
      SYNCRC(t);
    }
  }
  return rc;
}


static SP_BOOL
can_place(struct task *t,
	  struct comppart *cp,
	  long limtu)
{
  int e1 = 0;
  long lb = est(t);
  
  while (cp[e1+1].start <= est(t))
    e1++;
  /* cp[e1].start <= est(t) < cp[e1+1].start */
  while (cp[e1].start < lb+dur(t)) {
    if (cp[e1].height > limtu)
      lb = cp[e1+1].start;
    e1++;
  }
  return (lst(t) >= lb);
}      

static int 
dr_cp_ends MAGIC (HIDDEN_PROTO
		  struct unary_data *pdata,
		  Dvar limitvar)
{
  long limit = dvar_max_l(limitvar);
  int nbtasks = pdata->nbtasks;
  struct comppart *cp = pdata->cp;
  int i, j, rc=0;

  for (i=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    
    if (ect(t) == lct(t))
      continue;
    if (!compulsory_parts(pdata,t,lst(t),lct(t),limitvar))
      return 3;
    for (j=0; j<nbtasks && rc==0; j++) {
      struct task *u = pdata->tasks+j;
      long limtu;
      
      if (enable(u)<2 || u==t || ect(u) <= lst(t) || lst(u) >= lct(t))
	continue;
      limtu = limit - res(t) - res(u);
      if (limtu<0) {
	lctmax(t) = lst(u);
	SYNCRC(t);
      } else if (lst(u) >= ect(u) && !can_place(u,cp,limtu)) {
	int e1 = 0;
	long ub = lctmax(t);

	/* same iteration as in cp_ends, except ub must be at least lst(u) */

	while (cp[e1+1].start < ub)
	  e1++;
	/* cp[e1].start < lctmax(t) <= cp[e1+1].start */
	while (cp[e1+1].start > ub-dur(t) && cp[e1+1].start > lst(u)) {
	  if (cp[e1].height > limtu)
	    ub = cp[e1].start > lst(u) ? cp[e1].start : lst(u);
	  e1--;
	}
	lctmax(t) = ub;
	SYNCRC(t);
      }
    }
  }
  return rc;
}

static int 
dr_cp_starts MAGIC (HIDDEN_PROTO
		    struct unary_data *pdata,
		    Dvar limitvar)
{
  long limit = dvar_max_l(limitvar);
  int nbtasks = pdata->nbtasks;
  struct comppart *cp = pdata->cp;
  int i, j, rc=0;

  for (i=0; i<nbtasks; i++) {
    struct task *t = pdata->tasks+i;
    
    if (ect(t) == lct(t))
      continue;
    if (!compulsory_parts(pdata,t,est(t),ect(t),limitvar))
      return 3;
    for (j=0; j<nbtasks && rc==0; j++) {
      struct task *u = pdata->tasks+j;
      long limtu;
      
      if (enable(u)<2 || u==t || ect(u) <= est(t) || lst(u) >= ect(t))
	continue;
      limtu = limit - res(t) - res(u);
      if (limtu<0) {
	est(t) = ect(u);
	SYNCRC(t);
      } else if (lst(u) >= ect(u) && !can_place(u,cp,limtu)) {
	int e1 = 0;
	long lb = est(t);

	/* same iteration as in cp_starts, except lb must be at most ect(u) */

	while (cp[e1+1].start <= est(t))
	  e1++;
	/* cp[e1].start <= est(t) < cp[e1+1].start */
	while (cp[e1].start < lb+dur(t) && cp[e1].start < ect(u)) {
	  if (cp[e1].height > limtu)
	    lb = cp[e1+1].start < ect(u) ? cp[e1+1].start : ect(u);
	  e1++;
	}
	est(t) = lb;
	SYNCRC(t);
      }
    }
  }
  return rc;
}



/* FILTERING */

/* use unary methods from inside discrete filtering */
static int
disjunctive_tasks(struct task *tasks,
		  int nbtasks,
		  struct task **map,
		  Dvar limitvar)
{
  long limit = dvar_max_l(limitvar);
  long hlim = limit>>1;
  long minres = limit;
  int i, j;

  for (i=0, j=0; i<nbtasks; i++) {
    long res = tasks[i].res;
    
    if (res > hlim) {
      map[j++] = &tasks[i];
      if (minres > res)
	minres = res;
    }
  }
  /* now add task t | limit-minres < res(t) <= hlim AND dur(t) is maximal */
  if (j < nbtasks && limit-minres < hlim) {
    struct task *cand = NULL;
    for (i=0; i<nbtasks; i++) {
      long res = tasks[i].res;
    
      if (limit-minres < res && res <= hlim)
	if (!cand || cand->dur < tasks[i].dur)
	  cand = &tasks[i];
    }
    if (cand)
      map[j++] = cand;
  }
  return j;
}

/* DOES NOT find a fixpoint */
static int
unary_filtering_rec MAGIC (HIDDEN_PROTO
			   struct task **map,
			   int nbtasks,
			   int nbtotal,
			   int flags,
			   struct diff_constraint *dc,
			   int nbdiffs)
{
  struct task *tasks;
  int i, rc;
  
  if (nbtasks<=1)
    return 0;
  tasks = Malloc(nbtasks,struct task);

  for (i=nbtasks-1; i>=0; i--) {
    tasks[i] = *map[i];
    tasks[i].res = 1;
  }
  rc = unary_filtering(tasks,nbtasks,nbtotal,flags,dc,nbdiffs);
  if (rc==1)
    for (i=nbtasks-1; i>=0; i--) {
      map[i]->est = tasks[i].est;
      map[i]->lct = tasks[i].lct;
      map[i]->lctmax = tasks[i].lctmax;
      map[i]->enable = tasks[i].enable;
    }
  Free(tasks);
  return rc;
}


/* DOES NOT find a fixpoint */
static int
discrete_filtering MAGIC (HIDDEN_PROTO
			  struct task *tasks,
			  int nbtasks,
			  int nbtotal,
			  int flags,
			  Dvar limitvar,
			  struct diff_constraint *dc,
			  int nbdiffs)
{
  int rc;
  struct unary_data *pdata=NULL;
  struct task **map;
  int nbutasks;

  map = Malloc(nbtasks,struct task *);
  nbutasks = disjunctive_tasks(tasks,nbtasks,map,limitvar);
  rc = unary_filtering_rec(map,nbutasks,nbtotal,flags,dc,nbdiffs);
  if (rc==0 && nbutasks<nbtasks) {
    pdata = unary_alloc(tasks,nbtasks,nbtotal,(flags & ~1));
    rc = cp_ends(pdata,limitvar);
    if (rc<2)
      rc |= cp_starts(pdata,limitvar);
    if (rc==0)
      rc |= dr_cp_ends(pdata,limitvar);
    if (rc==0)
      rc |= dr_cp_starts(pdata,limitvar);
    if (rc==0 && (flags&2))
      rc |= task_interval_filtering(pdata,dvar_max_l(limitvar));
    Free(pdata);
  }
  Free(map);
  return rc;
}


/* DISJOINT1 REVISITED */

typedef long SHIFTED_ITEM;

typedef long ITEM;

typedef long MARGIN;

#define STATUS_SOURCE 0x1
#define STATUS_TARGET 0x2
#define STATUS_CONNECTED 0x4
#define STATUS_SOURCE_LATER 0x10
#define STATUS_TARGET_LATER 0x20
#define TARGET(i) (pdata->target[i])
#define STATUS(it) (pdata->item.status[it])
#define TYPE(it) (pdata->item.type[it])
#define ORIGVAR(T) (pdata->origvar + (T))
#define DURVAR(T) (pdata->durvar + (T))
#define DUR(T) dvar_min_l(DURVAR(T))
#define DURmax(T) dvar_max_l(DURVAR(T))
#define EST(T) dvar_min_l(ORIGVAR(T))
#define LaST(T) dvar_max_l(ORIGVAR(T))
#define ECT(T) (EST(T)+DUR(T))
#define LCT(T) (LaST(T)+DUR(T))
#define LCTmax(T) (LaST(T)+DURmax(T))

#define FORBIDDEN_REGION1D(TARGET, SOURCE, X1, X2) \
(X1) = LaST(pdata->shifted_item.item[(SOURCE)])+pdata->shifted_item.amount[(SOURCE)] - DUR(TARGET) + 1; \
(X2) = EST(pdata->shifted_item.item[(SOURCE)])+pdata->shifted_item.amount[(SOURCE)]  + DUR(pdata->shifted_item.item[(SOURCE)]) - 1;

/* The constraint frame. */
struct disjoint1_data {
  void (SPCDECL *destructor)(void*);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  SP_globref refbase;		/* static */
  long stamp;
  int nitems;			/* static */
  int nshifted_items;		/* nitems * (1 or 3), static */
  int ntargets;			/* #items that may be targets, := nitems */
  int nsources;			/* #items that may be sources only, := 0 */
  int nmargs;			/* exact number */
  int nmargs_aligned;		/* power of 2, static */
  int flags;			/* static */
  long lborder;			/* static */
  long rborder;			/* static */
  long maxmargin;		/* static */
  ITEM *target;			/* [nitems], qsorted */
  SHIFTED_ITEM *source;		/* [nshifted_items], perhaps qsorted, volatile */
  MARGIN *margtab;		/* [nmargs_aligned] */
  Dvar origvar;
  Dvar durvar;
  struct {
    TAGGED *type;		/* static */
    long *status;
  } item;			/* each [nitem] */
  struct {
    ITEM *item;			/* volatile */
    long *amount;		/* volatile */
  } shifted_item;		/* each [nshifted_items] */
  struct {
    TAGGED *type1;
    TAGGED *type2;
    long *amount;
    MARGIN *next;		/* terminated by TERMIN */
  } margin;			/* each [nmargs] */
  /* space for the above arrays */
};

/* subtract from *p1 margin from target to source;
   add to *p2 margin from source to target.
*/
static int extend_forbidden_region1d(struct disjoint1_data *pdata,
				     ITEM target, ITEM source,
				     long *p1, long *p2)
{
  TAGGED t1 = TYPE(target);
  TAGGED t2 = TYPE(source);
  TAGGED key = ((t1^t2)>>LogSizeOfWord) & (pdata->nmargs_aligned-1);
  MARGIN m = pdata->margtab[key];
  int hits = 0;

  while (m > TERMIN && hits<2) {
    if ((pdata->margin.type1[m]==t1) && (pdata->margin.type2[m]==t2)) { /* target -> source margin found */
      hits++;
      *p1 -= pdata->margin.amount[m];
    }
    if ((pdata->margin.type1[m]==t2) && (pdata->margin.type2[m]==t1)) { /* source -> target margin found */
      hits++;
      *p2 += pdata->margin.amount[m];
    }
    m = pdata->margin.next[m];
  }
  return hits;
}

static void SPCDECL disjoint1_destructor(void *pdata_v)
{
  struct disjoint1_data *pdata = (struct disjoint1_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,4*pdata->nitems);
  SP_free(pdata);
}

/* '$fd_disjoint1'(+State0, -State, -Actions) :-
   State0 is f(N,Opt,Items,NTargets,NSources,Handle,Stamp),
   State  similarly,
   Actions is a list of prunings etc.
   Opt = opt(Flags,LBorder,RBorder,Margins) where
               0x1 denotes decomposition (unused!)
               0x2 denotes wrap-around over the interval LBorder..RBorder
               0x4 denotes existence of margins
               0x8 denotes global reasoning (task intervals light)
               0x20 denotes lean (avoid heavy work in incremental case)
               Margins = list of margin(Type1,Type2,Diff) = list of extra margins
   Items is a list of N items item(X,XMut,XDur,XDurMut,Type).
*/
void SPCDECL
prolog_fd_disjoint1 MAGIC (HIDDEN_PROTO
			   SP_term_ref State0,
			   SP_term_ref State,
			   SP_term_ref Actions)
{
  WAMENV;
  int i, j, ent = -1;		/* disentailed */
  int nitems;			/* #items */
  int nshifted_items;		/* #shifted items if wrap-around */
  int nmargs = 0;
  int nmargs_aligned = 0;
  int flags;
  int nactive_items;		/* caches pdata->ntargets + pdata->nsources */
  int nactive_shifted;		/* the above * 1 or 3 */
  long l, total_size, state_stamp;
  TAGGED tmp, opt, item, items;
  TAGGED handle;
  SP_BOOL committed, change;
  struct disjoint1_data *pdata;
  struct task *tasks;
  char *ptr;
  
  w->numstack_end = NULL;

/*    X(0) = RefTerm(State0); */
  (void)State0;
  dvar_export_start(w);
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct disjoint1_data,handle);
    fd.gdata = pdata;
    flags = pdata->flags;
    nitems = pdata->nitems;
    nshifted_items = pdata->nshifted_items;
  } else {			/* build persistent state */
				/* compute flags, nitems, nshifted_items */
    DerefArg(tmp,X(0),1);	/* get N */
    nshifted_items = nitems = GetSmall(tmp);
    if (nitems==0) {
      ent = 1;
      goto ret1;
    }
    DerefArg(opt,X(0),2);		/* get Opt */
    DerefArg(tmp,opt,1);
    flags = GetSmall(tmp);
    if (flags & 0x2)
      nshifted_items *= 3;
    if (flags & 0x4) {
      DerefArg(items,opt,4);
      while (TagIsLST(items)) {
	nmargs++;
	DerefCar(item,items);
	DerefCdr(items,items);
      }
      for (nmargs_aligned=1; nmargs_aligned<nmargs; nmargs_aligned <<= 1)
	;
    }

    total_size =
      2*nitems*sizeof(struct dvar) +
      (3*nitems + 4*nmargs + nmargs_aligned)*sizeof(long);
    pdata = Palloc(struct disjoint1_data, total_size, handle);
    fd.gdata = pdata;
    ptr = (char *)(pdata+1);
    pdata->origvar = (Dvar)ptr;
    ptr += nitems*sizeof(struct dvar);
    pdata->durvar = (Dvar)ptr;
    ptr += nitems*sizeof(struct dvar);
    pdata->target = (ITEM *)ptr;
    ptr = (char *)(pdata->target+nitems);
    pdata->margtab = (MARGIN *)ptr;
    ptr = (char *)(pdata->margtab+nmargs_aligned);
    pdata->item.type = (TAGGED *)ptr;
    ptr = (char *)(pdata->item.type+nitems);
    pdata->item.status = (long *)ptr;
    ptr = (char *)(pdata->item.status+nitems);
    pdata->margin.type1 = (TAGGED *)ptr;
    ptr = (char *)(pdata->margin.type1+nmargs);
    pdata->margin.type2 = (TAGGED *)ptr;
    ptr = (char *)(pdata->margin.type2+nmargs);
    pdata->margin.amount = (long *)ptr;
    ptr = (char *)(pdata->margin.amount+nmargs);
    pdata->margin.next = (MARGIN *)ptr;
    ptr = (char *)(pdata->margin.next+nmargs);
#if DBG
    if (ptr != (char *)(pdata+1)+total_size)
      printf("SHOULDN'T HAPPEN: expected ptr=%p, got %p\n",
	     (char *)(pdata+1)+total_size, ptr);
#endif
    pdata->destructor = disjoint1_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(4*nitems);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    pdata->nitems = nitems;
    pdata->nshifted_items = nshifted_items;
    pdata->nmargs = nmargs;
    pdata->nmargs_aligned = nmargs_aligned;
    pdata->flags = flags;
    pdata->lborder = 0;
    pdata->rborder = 0;
    pdata->maxmargin = 0;
    if (flags & 0x2) {
      DerefArg(tmp,opt,2);
      pdata->lborder = GetSmall(tmp);
      DerefArg(tmp,opt,3);
      pdata->rborder = GetSmall(tmp);
    }
				/* wrap-around can triple the #forbidden regions */

    if (flags & 0x4) {		/* build margin table */
      DerefArg(items,opt,4);
      i = 0;
      while (TagIsLST(items)) {
	DerefCar(item,items);
	DerefCdr(items,items);
	DerefArg(pdata->margin.type1[i],item,1);
	DerefArg(pdata->margin.type2[i],item,2);
	DerefArg(tmp,item,3);
	pdata->margin.amount[i] = l =
	  (TagIsSmall(tmp) ? GetSmall(tmp) : CLPFD_MAXINT>>1); /* TODO: cheating */
	if (pdata->maxmargin<l)
	  pdata->maxmargin = l;
	i++;
      }
      pdata->nmargs = i;
      for (nmargs_aligned=1; nmargs_aligned<i; nmargs_aligned <<= 1)
	;
      for (i=nmargs_aligned-1; i>=0; i--)
	pdata->margtab[i] = TERMIN;
      for (i=nmargs-1; i>=0; i--) {
	TAGGED key = ((pdata->margin.type1[i]^pdata->margin.type2[i])>>LogSizeOfWord) & (nmargs_aligned-1);
	MARGIN m = pdata->margtab[key];

	pdata->margtab[key] = i;
	pdata->margin.next[i] = m;
      }
    }
				/* transfer all the items */
    DerefArg(items,X(0),3);
    for (i=0; i<nitems; i++) {
      SP_globref ref = pdata->refbase+(i<<2);

      TARGET(i) = i;
      DerefCar(item,items);
      DerefCdr(items,items);
      get_var_and_attr(item,ref);
      get_var_and_attr(item+WD(2),ref+2);
      DerefArg(TYPE(i),item,5);	/* type */
      STATUS(i) = (STATUS_SOURCE+STATUS_TARGET)<<4;
    }
    CTagToArg(X(0),3) = atom_nil; /* [MC] 3.12: free for GC */
  }

				/* RESUME HERE */
  ptr = fd_malloc((3*nshifted_items)*sizeof(long));
  pdata->source = (ITEM *)ptr; /* volatile */
  ptr = (char *)(pdata->source+nshifted_items);
  pdata->shifted_item.item = (ITEM *)ptr; /* volatile */
  ptr = (char *)(pdata->shifted_item.item+nshifted_items);
  pdata->shifted_item.amount = (long *)ptr; /* volatile */
  ptr = (char *)(pdata->shifted_item.amount+nshifted_items);
  tasks = Malloc(nshifted_items,struct task);

  DerefArg(tmp,X(0),4);
  pdata->ntargets = GetSmall(tmp);
  DerefArg(tmp,X(0),5);
  pdata->nsources = GetSmall(tmp);
  nactive_items = pdata->nsources + pdata->ntargets;
  if (state_stamp != pdata->stamp) { /* trust nothing */
    for (i=0; i<pdata->ntargets; i++) {
      ITEM it = TARGET(i);
      
      STATUS(it) |= STATUS(it)>>4;
    }
    for (; i<nactive_items; i++) { /* 3.9 */
      ITEM it = TARGET(i);

      STATUS(it) |= STATUS_SOURCE;
    }
  }
  for (i=0; i<pdata->ntargets; i++) {
    ITEM it = TARGET(i);
      
    if (STATUS(it)&STATUS_TARGET) { /* always true? */
      SP_globref ref = pdata->refbase+(it<<2);
      
      dvar_init(ORIGVAR(it), ref, ref+1);
      dvar_init(DURVAR(it), ref+2, ref+3);
    }
  }

  pdata->stamp = state_stamp+1;

  /* populate pdata->source array */
  for (i=j=0; i<nactive_items; i++) {
    ITEM var = TARGET(i);
    
    pdata->source[j] = j;
    pdata->shifted_item.item[j] = var;
    pdata->shifted_item.amount[j++] = 0;
    if (flags & 0x2) {
      pdata->source[j] = j;
      pdata->shifted_item.item[j] = var;
      pdata->shifted_item.amount[j++] = pdata->lborder - pdata->rborder;
      pdata->source[j] = j;
      pdata->shifted_item.item[j] = var;
      pdata->shifted_item.amount[j++] = pdata->rborder - pdata->lborder;
    }
  }

  nactive_shifted = j;
  do {
    change = FALSE;
    for (i=j=0; i<nactive_shifted; i++) {
      SHIFTED_ITEM src = pdata->source[i];
      ITEM sitem = pdata->shifted_item.item[src];
      long amount = pdata->shifted_item.amount[src];
      
      if (DURmax(sitem)>0) {
	est(tasks+j) = EST(sitem)+amount;
	lct(tasks+j) = LCT(sitem)+amount;
	lctmax(tasks+j) = LCTmax(sitem)+amount;
	dur(tasks+j) = DUR(sitem);
	res(tasks+j) = 1;
	enable(tasks+j) = 2;
	(tasks+j)->id = sitem;
	if (dur(tasks+j)==0) {
	  long inc = dvar_successor_l(DURVAR(sitem),0);
	  lct(tasks+j) += inc;
	  dur(tasks+j) += inc;
	  enable(tasks+j) = 1;
	}
	j++;
      }
    }
    switch (unary_filtering(tasks,j,0,(flags&0x8)>>2,NULL,0)) {
    case 3:
    case 2:
      goto ret;
    case 0:
      break;
    default:
      for (i=j=0; i<nactive_shifted; i++) {
	SHIFTED_ITEM src = pdata->source[i];
	ITEM sitem = pdata->shifted_item.item[src];
	long amount = pdata->shifted_item.amount[src];
    
	if (DURmax(sitem)>0) {
	  if (STATUS(sitem)&STATUS_TARGET) {
	    switch (enable(tasks+j)) {
	    case 1:
	      if (lctmax(tasks+j) > est(tasks+j)) {
		switch (dvar_fix_max_l(DURVAR(sitem),lctmax(tasks+j)-est(tasks+j))) {
		case -1:
		  goto ret;
		case 0:
		  break;
		default:
		  change = TRUE;
		}
		break;
	      }
	    case 0:
	      switch (dvar_fix_max_l(DURVAR(sitem),0)) {
	      case -1:
		goto ret;
	      case 0:
		break;
	      default:
		change = TRUE;
	      }
	      break;
	    case 2:
	      switch (dvar_fix_interval_l(ORIGVAR(sitem), est(tasks+j)-amount, lst(tasks+j)-amount)) {
	      case -1:
		goto ret;
	      case 0:
		break;
	      default:
		change = TRUE;
	      }
	      if (!dvar_is_integer(DURVAR(sitem)))
		switch (dvar_fix_max_l(DURVAR(sitem),lctmax(tasks+j)-est(tasks+j))) {
		case -1:
		  goto ret;
		case 0:
		  break;
		default:
		  change = TRUE;
		}
	    }
	  }
	  j++;
	}
      }
    }
    if (flags & 0x4) { /* margins - check all pairs for now */
      for (i=0; i<pdata->ntargets; i++) {
	ITEM t = pdata->target[i];
	long m1, m2;
      
	for (j=0; j<nactive_shifted; j++) {
	  SHIFTED_ITEM src = pdata->source[j];
	  ITEM s = pdata->shifted_item.item[src];
	  long amount = pdata->shifted_item.amount[src];

	  FORBIDDEN_REGION1D(t,src,m1,m2);
	  if ((s!=t || amount!=0) &&
	      extend_forbidden_region1d(pdata,t,s,&m1,&m2)>0) {
	    if (EST(t)>=m1 && EST(t)<=m2) {
	      change = TRUE;
	      if (dvar_fix_min_l(ORIGVAR(t),m2+1)<0)
		goto ret;
	    }
	    if (LaST(t)>=m1 && LaST(t)<=m2) {
	      change = TRUE;
	      /* TODO: more eager pruning of lctmax */
	      if (dvar_fix_max_l(ORIGVAR(t),m1-1)<0)
		goto ret;
	    }
	  }
	}
      }
    }
  } while (change);

  for (i=0; i<nactive_shifted; i++) {
    SHIFTED_ITEM src = pdata->source[i];
    ITEM sitem = pdata->shifted_item.item[src];
    
    if ((STATUS(sitem) & STATUS_TARGET) &&
	dvar_is_integer(ORIGVAR(sitem)) &&
	dvar_is_integer(DURVAR(sitem)))
      STATUS(sitem) &= ~STATUS_TARGET;
  }

  {
    long est = CLPFD_MAXINT;
    long lct = -CLPFD_MAXINT;
    long sstart, send;

    /* compute bounding box of targets */
    for (j=0; j<nactive_shifted; j++) {
      SHIFTED_ITEM src = pdata->source[j];
      ITEM sitem = pdata->shifted_item.item[src];

      sstart = EST(sitem) + pdata->shifted_item.amount[src] - pdata->maxmargin;
      send = LCTmax(sitem) + pdata->shifted_item.amount[src] + pdata->maxmargin;
      if (STATUS(sitem)&STATUS_TARGET) {
	if (est>sstart)
	  est = sstart;
	if (lct<send)
	  lct = send;
      }
    }

    /* forget sources that can no longer prune */
    for (i=0; i<nactive_items; i++) {
      ITEM item = TARGET(i);
    
      if (!(STATUS(item)&STATUS_TARGET) &&
	  (LCTmax(item)<=est || lct<=EST(item))) {
	STATUS(item) &= ~STATUS_SOURCE;
      }
    }
  }

  ent = 1;
  for (i=0; i<pdata->ntargets; i++) {
    ITEM it = TARGET(i);

    if (STATUS(it)&STATUS_TARGET)
      ent = 0;
    dvar_export(w,ORIGVAR(it));
    dvar_export(w,DURVAR(it));
  }
  if (ent)
    goto ret;

  /* partition into SOURCE+TARGET and SOURCE */

  {
    int delta;
    int inf = 0;
    int sup = pdata->ntargets - 1;
    ITEM held = TARGET(sup); /* sup is the hole */
    ITEM current = TARGET(inf);
    
    while (inf<=sup) {
      if (STATUS(current) & STATUS_TARGET) {
	TARGET(inf) = current;
	inf++;
	current = (inf>=sup ? held : TARGET(inf));
      } else {
	TARGET(sup) = current;
	sup--;
	current = (inf>=sup ? held : TARGET(sup));
      }
    }
    delta = pdata->ntargets - inf;
    pdata->ntargets -= delta;
    pdata->nsources += delta;
  }
  
  /* find the last real SOURCE */

  {
    int delta, sup;
    
    for (sup=nactive_items; sup>pdata->ntargets; --sup)
      if (STATUS(TARGET(sup-1)) & STATUS_SOURCE)
	break;
    delta = nactive_items - sup;
    pdata->nsources -= delta;
    nactive_items -= delta;
  }

  if (committed)
    for (i=pdata->ntargets; i<nactive_items; i++) {
      ITEM it = TARGET(i);
      
      STATUS(it) &= ~(STATUS_TARGET<<4);
    }
  
 ret:
  Free(pdata->source);
  Free(tasks);
  CTagToArg(X(0),4) = MakeSmall(pdata->ntargets);
  CTagToArg(X(0),5) = MakeSmall(pdata->nsources);
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(w,Actions, ent);
}


/* CUMULATIVE REVISITED */
typedef long TASK;

#undef DUR
#undef DURmax
#undef STATUS
#undef EST
#undef LaST
#undef ECT
#undef LCT
#undef LCTmax
#undef TARGET
#undef ORIGVAR
#undef DURVAR

#define TARGET(I)   (pdata->target[I])
#define STATUS(t) (pdata->status[t])
#define ORIGVAR(t) (pdata->origvar+(t))
#define DURVAR(t) (pdata->durvar+(t))
#define ENDVAR(t) (pdata->endvar+(t))
#define USEVAR(t) (pdata->usevar+(t))
#define DURmin(T) dvar_min_l(DURVAR(T))
#define DURmax(T) dvar_max_l(DURVAR(T))
#define USEmin(T) dvar_min_l(USEVAR(T))
#define USEmax(T) dvar_max_l(USEVAR(T))
#define EST(T) dvar_min_l(ORIGVAR(T))
#define LaST(T) dvar_max_l(ORIGVAR(T))
#define ECT(T) dvar_min_l(ENDVAR(T))
#define LCTmax(T) dvar_max_l(ENDVAR(T))
#define LCTmin(T) (LaST(T)+DURmin(T))
#define RefOrigAttr(T) (pdata->refbase + ((T)<<3) + 2)
#define RefOrig(T) (pdata->refbase + ((T)<<3) + 3)
#define RefDurAttr(T) (pdata->refbase + ((T)<<3) + 4)
#define RefDur(T) (pdata->refbase + ((T)<<3) + 5)
#define RefEndAttr(T) (pdata->refbase + ((T)<<3) + 6)
#define RefEnd(T) (pdata->refbase + ((T)<<3) + 7)
#define RefUseAttr(T) (pdata->refbase + ((T)<<3) + 8)
#define RefUse(T) (pdata->refbase + ((T)<<3) + 9)
#define RefLim    (pdata->refbase + 1)
#define RefLimAttr    (pdata->refbase)
#define DIFFVAR(t) (pdata->diffvar+(t))
#define RefDiffAttr(T) (pdata->drefbase + ((T)<<1))
#define RefDiff(T) (pdata->drefbase + ((T)<<1) + 1)

/* The constraint frame. */
struct cumulative_data {
  void (SPCDECL *destructor)(void *);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  int nrefs;			/* static, 2 + 8*nbtasks + nbdiffs */
  SP_globref refbase;		/* static */
  SP_globref drefbase;		/* static */
  long stamp;
  int nbdiffs;
  int nbtasks;			/* static, #tasks */
  int ntargets;			/* #tasks that may be targets, := nbtasks */
  int nsources;			/* #tasks that may be sources only, := 0 */
  int flags;			/* static */
  SP_BOOL change;
  long use_limit;		/* capacity limit */
  Dvar limitvar;
  Dvar origvar;
  Dvar durvar;
  Dvar endvar;
  Dvar usevar;
  TASK *target;			/* [nbtasks] */
  long *status;			/* [nbtasks] */
  Dvar diffvar;
  struct diff_constraint *dc;
  /* space for the above arrays */
};

static void SPCDECL cumulative_destructor(void *pdata_v)
{
  struct cumulative_data *pdata = (struct cumulative_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}


static void decompose(struct cumulative_data *pdata)
{
  long est = CLPFD_MAXINT;
  long lct = -CLPFD_MAXINT;
  int nbtasks = pdata->nbtasks;
  int nbdiffs = pdata->nbdiffs;
  int ntargets = pdata->ntargets;
  int nactive_items = ntargets + pdata->nsources;
  int i;
  
  for (i=nactive_items-1; i>=0; i--) {
    TASK si = TARGET(i);
    
    if (STATUS(si)&STATUS_TARGET) {
      if (est>EST(si))
	est = EST(si);
      if (lct<LCTmax(si))
	lct = LCTmax(si);
    }
  }

  /* forget sources that can no longer prune */
  for (i=ntargets; i<nactive_items; i++) {
    TASK si = TARGET(i);
    STATUS(si) &= ~STATUS_CONNECTED;
  }
  for (i=nbdiffs-1; i>=0; i--) {
    struct diff_constraint *dc = pdata->dc+i;

    if (STATUS(nbtasks+i) & STATUS_TARGET) {
      STATUS(dc->si) |= STATUS_CONNECTED;
      STATUS(dc->sj) |= STATUS_CONNECTED;
    }
  }
  for (i=ntargets; i<nactive_items; i++) {
    TASK si = TARGET(i);
    
    if ((STATUS(si)&(STATUS_SOURCE|STATUS_TARGET|STATUS_CONNECTED))==STATUS_SOURCE &&
	(LCTmax(si)<=est || lct<=EST(si)))
      STATUS(si) -= STATUS_SOURCE;
  }
}


static SP_BOOL 
task_renormalize(struct cumulative_data *pdata,TASK t) /* maintain origin + duration = end */
{
  for (;;) {
    long est = EST(t);
    long lst = LaST(t);
    long ect = ECT(t);
    long lct = LCTmax(t);
    long mindur = DURmin(t);
    long maxdur = DURmax(t);
    long lb1 = ect-maxdur;
    long ub1 = lct-mindur;
    long lb2 = ect-lst;
    long ub2 = lct-est;
    long lb3 = est+mindur;
    long ub3 = lst+maxdur;

    if (mindur>0 && (dvar_fix_min_l(pdata->limitvar, USEmin(t))<0 ||
		     dvar_fix_max_l(USEVAR(t), pdata->use_limit)<0))
      return FALSE;
    
    if (!(est<lb1 || lst>ub1 ||
	  mindur<lb2 || maxdur>ub2 ||
	  ect<lb3 || lct>ub3))
      return TRUE;
    
    /* origin in min(end)-max(dur) ... max(end)-min(dur) */
    if (dvar_fix_interval_l(ORIGVAR(t), lb1, ub1)<0)
      return FALSE;
  
  /* dur in min(end)-max(origin) ... max(end)-min(origin) */
    if (dvar_fix_interval_l(DURVAR(t), lb2, ub2)<0)
      return FALSE;
  
  /* end in min(origin)+min(dur) ... max(origin)-max(dur) */
    if (dvar_fix_interval_l(ENDVAR(t), lb3, ub3)<0)
      return FALSE;
  }
}

static int
diff_renormalize(struct cumulative_data *pdata,int i) /* maintain Si-Sj = Dij */
{
  TASK si = (pdata->dc+i)->si;
  TASK sj = (pdata->dc+i)->sj;
  Dvar sivar = ORIGVAR(si);
  Dvar sjvar = ORIGVAR(sj);
  Dvar dijvar = DIFFVAR(i);
  int rc = 0;
  
  for (;;) {
    long minsi = dvar_min_l(sivar);
    long maxsi = dvar_max_l(sivar);
    long minsj = dvar_min_l(sjvar);
    long maxsj = dvar_max_l(sjvar);
    long mindij = dvar_min_l(dijvar);
    long maxdij = dvar_max_l(dijvar);
    long lb1 = minsj-maxdij;
    long ub1 = maxsj-mindij;
    long lb2 = minsj-maxsi;
    long ub2 = maxsj-minsi;
    long lb3 = minsi+mindij;
    long ub3 = maxsi+maxdij;

    if (!(minsi<lb1 || maxsi>ub1 ||
	  mindij<lb2 || maxdij>ub2 ||
	  minsj<lb3 || maxsj>ub3))
      return rc;
    
    /* Si in min(Sj)-max(dij) ... max(Sj)-min(dij) */
    switch (dvar_fix_interval_l(sivar, lb1, ub1)) {
    case -1:
      return -1;
    case 0:
      break;
    default:
      rc = 1;
      if (!task_renormalize(pdata,si))
	return -1;
    }
  
    /* dij in min(Sj)-max(Si) ... max(Sj)-min(Si) */
    if (dvar_fix_interval_l(dijvar, lb2, ub2)<0)
      return -1;
  
    /* Sj in min(Si)+min(dij) ... max(Si)-max(dij) */
    switch (dvar_fix_interval_l(sjvar, lb3, ub3)) {
    case -1:
      return -1;
    case 0:
      break;
    default:
      rc = 1;
      if (!task_renormalize(pdata,sj))
	return -1;
    }
  }
}





/* '$fd_cumulative'(+State0, -State, -Actions).

   Filtering algorithm for serialized/[2,3] and cumulative/[4,5].

   State0 = State = f(N,Tasks,Diffs,Limit,Flags,NTargets,NSources,Handle,Stamp).

   Tasks are the tasks task(Si,SMi,Di,DMi,Ri,RMi,i,_) to be scheduled,
   where i in 0..N-1.

   Diffs is a list of d(i,j,Mij) where Mij is a mutable whose value is a
   domain (a..-Dj)\/(Di..b) if tasks i and j are disjunctive or have a
   precedence relation; otherwise, 0 may be in the domain.

   Flags include: 0x1 - precedences
                  0x2 - task intervals rule

   No unbounded domains!

   The following invariants hold when a fixpoint is reached:

   [0. cumulative]
	   The total task load does not exceed limit anywhere.

   [1. differences]
	   Si + dij = Sj

   [3. mutual exclusion]

	   If tasks i and j are exclusive, then
		   Di=0 | Dj=0 | Si+Di =< Sj | Sj+Dj =< Si
	   Hence
		   Si+Di =< max(Sj) | Si >= min(Sj)+Dj
		      ##                            ##
		   Sj+Dj =< max(Si) | Sj >= min(Si)+Di
		      ##                            ##

   Can be tightened by replacing ## by values from relevant Dij variable.

   [4. resource restriction]

	   Bi0*R0 +...+ Bin*Rm =< L,  at all points in time i, where

	   Bij = 1, if Sj =< i < Sj+Dj
	   Bij = 0, otherwise

*/
static SP_BOOL
add_precedence(struct unary_data *pdata,
	       int i,
	       int j,
	       char *precedence)
{
  int nbtasks = pdata->nbtasks;
  struct task *tasks = pdata->tasks;
  int irow = i*nbtasks;
  int jrow = j*nbtasks;
  int a, b;
  
  if (!precedence[irow+j] && !must_precede(tasks+i,tasks+j)) {
    if (must_precede(tasks+j,tasks+i) || precedence[jrow+i])
      return FALSE;
    precedence[irow+j] = 1;
    /* Transitive closure: precedence a->b is added iff one of following: */
    /* a->i->j->b     */
    /*  a=i->j->b */
    /* a->i->j=b */
    for (a = 0; a < nbtasks; a++) {
      int arow = a*nbtasks;
      for (b = 0; b < nbtasks; b++) {
	if (a==b || precedence[arow+b])
	  continue;
	if (a == i && b == j)
	  continue;
	if (a != i && !precedence[arow+i])
	  continue;
	if (b != j && !precedence[jrow+b])
	  continue;
	precedence[arow+b] = 1;
      }
    }
  }
  return TRUE;
}    

static SP_BOOL
unary_detect_all_precedences MAGIC (HIDDEN_PROTO
				    struct unary_data *pdata,
				    struct diff_constraint *dc,
				    int nbdiffs)
{
  int nbtasks = pdata->nbtasks;
  int nbprec = 0;
  struct task *tasks = pdata->tasks;
  char *precedence = Malloc(nbtasks*nbtasks,char);
  struct cumulative_data *cdata = fd.gdata;
  long ao;
  int i, j, k;

  for (i=nbtasks-1; i>=0; i--) {
    int irow = i*nbtasks;
    for (j=nbtasks-1; j>=0; j--)
      precedence[irow+j] = 0;
  }

  for (k=0; k<nbdiffs; k++) {
    i = pdata->id_map[(dc+k)->si];
    j = pdata->id_map[(dc+k)->sj];
    if (i>=0 && j>=0) {
      if (dvar_min_l(cdata->diffvar+k) > (enable(tasks+j)<2 ? 0 : -dur(tasks+j))) {
	nbprec++;
	if (!add_precedence(pdata,i,j,precedence))
	  goto fail;
      }
      if (dvar_max_l(cdata->diffvar+k) < (enable(tasks+i)<2 ? 0 : dur(tasks+i))) {
	nbprec++;
	if (!add_precedence(pdata,j,i,precedence))
	  goto fail;
      }
    }
  }

  if (nbprec>0) {
  
    /* adjust ESTs */
    for (i = 0; i < nbtasks; i++) {
      struct task *ti = tasks+i;
      if (enable(ti)==0)
	continue;
    
      /* \Omega = \emptyset */
      ao = -CLPFD_MAXINT; /* "after" \Omega, future EST(i) */

      /* Put all activities which have to be processed BEFORE  */
      /* the activity i into the set \Omega. */
      for (j = 0; j < nbtasks; j++) {
	struct task *tj = pdata->est_rank[j];
	int tji = tj-tasks;

	if (ti==tj || enable(tj)<2 || !(precedence[tji*nbtasks+i] || must_precede(tj,ti)))
	  continue;

	/* Add activity j into the \Omega */
	if (ao < est(tj)) {
	  ao = est(tj) + dur(tj);
	} else {
	  ao += dur(tj);
	}
      }

      if (pdata->lb[i] < ao) {
	pdata->lb[i] = ao; 
      }
    }

    /* adjust LCTs */
    for (i = 0; i < nbtasks; i++) {
      struct task *ti = tasks+i;
      if (enable(ti)==0)
	continue;
    
      /* \Omega = \emptyset */
      ao = CLPFD_MAXINT; /* "before" \Omega, future LCT(i) */

      /* Put all activities which have to be processed AFTER  */
      /* the activity i into the set \Omega. */
      for (j = 0; j < nbtasks; j++) {
	struct task *tj = pdata->lct_rank[j];
	int tji = tj-tasks;

	if (ti==tj || enable(tj)<2 || !(precedence[i*nbtasks+tji] || must_precede(ti,tj)))
	  continue;

	/* Add activity j into the \Omega */
	if (ao > lct(tj)) {
	  ao = lct(tj) - dur(tj);
	} else {
	  ao -= dur(tj);
	}
      }

      if (pdata->ub[i] > ao) {
	pdata->ub[i] = ao; 
      }
    }

    /* back propagate precedences */
    for (k=0; k<nbdiffs; k++) {
      i = pdata->id_map[(dc+k)->si];
      j = pdata->id_map[(dc+k)->sj];
      if (i>=0 && j>=0) {
	struct task *ti = tasks+i;
	struct task *tj = tasks+j;
	if (enable(ti)==2 && enable(tj)==2) {
	  if (must_precede(ti,tj)) {
            long t1_dur = dur(ti);
	    switch (dvar_fix_min_l((cdata->diffvar+k), (t1_dur))) {
	    case -1:
	      goto fail;
	    case 0:
	      break;
	    default:
	      cdata->change = TRUE;
	    }
	  }
	  if (must_precede(tj,ti)) {
	    switch (dvar_fix_max_l(cdata->diffvar+k, -dur(tj))) {
	    case -1:
	      goto fail;
	    case 0:
	      break;
	    default:
	      cdata->change = TRUE;
	    }
	  }
	}
      }
    }
  }
  
  Free(precedence);
  return TRUE;
 fail:
  Free(precedence);
  return FALSE;
}

void SPCDECL
prolog_fd_cumulative MAGIC (HIDDEN_PROTO
			    SP_term_ref State0,
			    SP_term_ref State,
			    SP_term_ref Actions)
{
  WAMENV;
  TAGGED tasks, diffs, handle, tmp;
  int i, j, k, nbtasks, nbdiffs;
  int ent = -1;			/* disentailed unless otherwise */
  SP_BOOL committed;
  int flags;
  int nactive_items;		/* caches pdata->ntargets + pdata->nsources */
  long total_size, state_stamp, use_limit;
  struct cumulative_data *pdata;
  struct task *dtasks;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

  w->numstack_end = NULL;

/*    X(0) = RefTerm(State0); */
  dvar_export_start(w);
  RefTerm(State) = unify_output_state(w,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct cumulative_data,handle);
    fd.gdata = pdata;
    flags = pdata->flags;
    nbtasks = pdata->nbtasks;
    nbdiffs = pdata->nbdiffs;
  } else {			/* build persistent state */
				/* compute flags, nbtasks */
    DerefArg(tmp,X(0),1);		/* get N */
    nbtasks = GetSmall(tmp);
    if (nbtasks==0) {
      ent = 1;
      goto ret1;
    }
    DerefArg(tmp,X(0),3);		/* get Diffs */
    nbdiffs = list_length(tmp);
    DerefArg(tmp,X(0),5);		/* get Flags */
    flags = GetSmall(tmp);
    total_size = (2*nbtasks + nbdiffs)*sizeof(long) +
                 (1 + 4*nbtasks + nbdiffs)*sizeof(struct dvar) +
                 nbdiffs*sizeof(struct diff_constraint);
    pdata = Palloc(struct cumulative_data, total_size, handle);
    fd.gdata = pdata;
    ptr = (char *)(pdata+1);
    pdata->limitvar = (Dvar)ptr;
    ptr += sizeof(struct dvar);
    pdata->origvar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->durvar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->endvar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->usevar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->target = (TASK *)ptr;
    ptr = (char *)(pdata->target+nbtasks);
    pdata->status = (long *)ptr;
    ptr = (char *)(pdata->status+nbtasks+nbdiffs);
    pdata->diffvar = (Dvar)ptr;
    ptr += nbdiffs*sizeof(struct dvar);
    pdata->dc = (struct diff_constraint *)ptr;
    ptr += nbdiffs*sizeof(struct diff_constraint);
#if DBG
    if (ptr != (char *)(pdata+1)+total_size)
      printf("SHOULDN'T HAPPEN: expected ptr=%p, got %p\n",
	     (char *)(pdata+1)+total_size, ptr);
#endif
    pdata->destructor = cumulative_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->nrefs = 2*nbdiffs + 8*nbtasks + 2;
    pdata->refbase = SP_alloc_globrefs(pdata->nrefs);
    pdata->drefbase = pdata->refbase + 8*nbtasks + 2;
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    pdata->nbtasks = nbtasks;
    pdata->nbdiffs = nbdiffs;
    pdata->flags = flags;

    DerefArg(tmp,X(0),4);	/* get Limit */
    get_var_and_attr(tmp,RefLimAttr);
    DerefArg(tasks,X(0),2);	/* get Tasks */
    for (i=0; i<nbtasks; i++) {
      TASK si = i;
      TAGGED elt;
      DerefCar(elt,tasks);
      DerefCdr(tasks,tasks);
      DerefArg(tmp,elt,1);
      get_var_and_attr(tmp,RefOrigAttr(si));
      DerefArg(tmp,elt,2);
      get_var_and_attr(tmp,RefDurAttr(si));
      DerefArg(tmp,elt,3);
      get_var_and_attr(tmp,RefEndAttr(si));
      DerefArg(tmp,elt,4);
      get_var_and_attr(tmp,RefUseAttr(si));
      DerefArg(tmp,elt,5);
      TARGET(i) = GetSmall(tmp);
      STATUS(si) = (STATUS_SOURCE_LATER|STATUS_TARGET_LATER);
    }
    
    DerefArg(diffs,X(0),3);
    for (i=0; TagIsLST(diffs); i++) {
      struct diff_constraint *dc = pdata->dc+i;
      TAGGED delt, tmp;
      DerefCar(delt,diffs);	/* get d/3: d(J,I,DIJ): Oi+Dij = Oj */
      DerefCdr(diffs,diffs);
      DerefArg(tmp,delt,1);	/* get task J's ID */
      j = GetSmall(tmp);
      for (k=0; k<nbtasks; k++)
	if (TARGET(k)==j)
	  break;
      dc->sj = k;
      DerefArg(tmp,delt,2);	/* get task I's ID */
      j = GetSmall(tmp);
      for (k=0; k<nbtasks; k++)
	if (TARGET(k)==j)
	  break;
      dc->si = k;
      DerefArg(tmp,delt,3);	/* get Dij pair */
      get_var_and_attr(tmp,RefDiffAttr(i));
    }

    for (i=nbtasks-1; i>=0; i--)
      TARGET(i) = i;
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),3) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  fd.gdata = pdata;
  dvar_init(pdata->limitvar, RefLimAttr, RefLim);
  use_limit = dvar_max_l(pdata->limitvar);

  DerefArg(tmp,X(0),6);
  pdata->ntargets = GetSmall(tmp);
  DerefArg(tmp,X(0),7);
  pdata->nsources = GetSmall(tmp);
  nactive_items = pdata->ntargets + pdata->nsources;

  if (state_stamp != pdata->stamp ||
      use_limit != pdata->use_limit) { /* trust nothing */

    pdata->nsources = 0;
    pdata->ntargets = nbtasks;
    nactive_items = nbtasks;

    /* refresh all task variables */
  
    for (i=nactive_items-1; i>=0; i--) {
      TASK si = TARGET(i);
      
      STATUS(si) |= STATUS(si)>>4;
    }
  }
  pdata->use_limit = use_limit;
  pdata->stamp = state_stamp+1;
  
  for (i=0; i<nactive_items; i++) {
    TASK si = TARGET(i);
    if (STATUS(si) & STATUS_TARGET) {
      dvar_init(ORIGVAR(si), RefOrigAttr(si), RefOrig(si));
      dvar_init(DURVAR(si),  RefDurAttr(si), RefDur(si));
      dvar_init(ENDVAR(si),  RefEndAttr(si), RefEnd(si));
      dvar_init(USEVAR(si),  RefUseAttr(si), RefUse(si));
      if (USEmin(si)>pdata->use_limit && dvar_fix_max_l(DURVAR(si),0)<0)
	goto ret1;
      if (!task_renormalize(pdata,si))
	goto ret1;
    }
  }
  for (i=0; i<nbdiffs; i++) {
    struct diff_constraint *dc = pdata->dc+i;
    STATUS(nbtasks+i) = STATUS(dc->si)|STATUS(dc->sj);
    if (STATUS(nbtasks+i) & STATUS_TARGET) { /* ensure that bounds are fixed, which is a
					       prerequisite for diff_renormalize */
      long minsi = dvar_min_l(ORIGVAR(dc->si));
      long maxsi = dvar_max_l(ORIGVAR(dc->si));
      long minsj = dvar_min_l(ORIGVAR(dc->sj));
      long maxsj = dvar_max_l(ORIGVAR(dc->sj));
      dvar_init(DIFFVAR(i), RefDiffAttr(i), RefDiff(i));
      if (dvar_fix_interval_l(DIFFVAR(i), minsj-maxsi, maxsj-minsi)<0)
	goto ret1;
    }
  }

  dtasks = Malloc(nactive_items,struct task);
  
  /* END OF RESUMPTION */

  do {
    pdata->change = FALSE;

    for (i=0; i<nbdiffs; i++) {
      if (STATUS(nbtasks+i) & STATUS_TARGET) {
	switch (diff_renormalize(pdata,i)) {
	case -1:
	  goto ret;
	case 1:
	  pdata->change = TRUE;
	}
      }
    }
    if (pdata->change)
      continue;

    for (i=j=0; i<nactive_items; i++) {
      TASK si = TARGET(i);

      if (DURmax(si)>0 && USEmin(si)>0) {
	est(dtasks+j) = EST(si);
	lct(dtasks+j) = LCTmin(si);
	lctmax(dtasks+j) = LCTmax(si);
	dur(dtasks+j) = DURmin(si);
	res(dtasks+j) = USEmin(si);
	enable(dtasks+j) = 2;
	(dtasks+j)->id = si;
	if (dur(dtasks+j)==0) {
	  long inc = dvar_successor_l(DURVAR(si),0);
	  lct(dtasks+j) += inc;
	  if (lct(dtasks+j) > lctmax(dtasks+j))
	    lct(dtasks+j) = lctmax(dtasks+j);
	  dur(dtasks+j) += inc;
	  enable(dtasks+j) = 1;
	}
	j++;
      }
    }
    switch (discrete_filtering(dtasks,j,nbtasks,flags,pdata->limitvar,pdata->dc,nbdiffs)) {
    case 3:
    case 2:
      goto ret;
    case 0:
      break;
    default:
      for (i=j=0; i<nactive_items; i++) {
	TASK si = TARGET(i);
    
	if (DURmax(si)>0 && USEmin(si)>0) {
	  if (STATUS(si)&STATUS_TARGET) {
	    SP_BOOL pruned = FALSE;
	    switch (enable(dtasks+j)) {
	    case 1:
	      if (lctmax(dtasks+j) > est(dtasks+j)) {
		switch (dvar_fix_max_l(DURVAR(si),lctmax(dtasks+j)-est(dtasks+j))) {
		case -1:
		  goto ret;
		case 0:
		  break;
		default:
		  pdata->change = pruned = TRUE;
		}
		break;
	      }
	    case 0:
	      switch (dvar_fix_max_l(DURVAR(si),0)) {
	      case -1:
		goto ret;
	      case 0:
		break;
	      default:
		pdata->change = pruned = TRUE;
	      }
	      break;
	    case 2:
              {
                long dtask_j_est = est(dtasks+j);
                switch (dvar_fix_min_l(ORIGVAR(si), dtask_j_est)) {
                case -1:
                  goto ret;
                case 0:
                  break;
                default:
                  pdata->change = pruned = TRUE;
                }
              }
	      switch (dvar_fix_max_l(ENDVAR(si), lctmax(dtasks+j))) {
	      case -1:
		goto ret;
	      case 0:
		break;
	      default:
		pdata->change = pruned = TRUE;
	      }
	      if (!dvar_is_integer(DURVAR(si)))
		switch (dvar_fix_max_l(DURVAR(si),lctmax(dtasks+j)-est(dtasks+j))) {
		case -1:
		  goto ret;
		case 0:
		  break;
		default:
		  pdata->change = pruned = TRUE;
		}
	    }
	    if (pruned && !task_renormalize(pdata,si))
	      goto ret;
	  }
	  j++;
	}
      }
    }
  } while (pdata->change);

  for (i=nactive_items-1; i>=0; i--) {
    TASK si = TARGET(i);
    
    if ((STATUS(si) & STATUS_TARGET) &&
	dvar_is_integer(ORIGVAR(si)) &&
	dvar_is_integer(DURVAR(si)) &&
	dvar_is_integer(ENDVAR(si)) &&
	dvar_is_integer(USEVAR(si)))
      STATUS(si) &= ~STATUS_TARGET;
  }
  decompose(pdata);

  for (i=nbdiffs-1; i>=0; i--) {
    if (STATUS(nbtasks+i) & STATUS_TARGET)
      dvar_pruning_done(w,DIFFVAR(i));
  }
  
  for (i=pdata->ntargets-1; i>=0; i--) {
    dvar_pruning_done(w,ORIGVAR(TARGET(i)));
    dvar_pruning_done(w,DURVAR(TARGET(i)));
    dvar_pruning_done(w,ENDVAR(TARGET(i)));
    dvar_pruning_done(w,USEVAR(TARGET(i)));
  }    
  dvar_pruning_done(w,pdata->limitvar);

  /* OK to GC from here */
  
  ent = dvar_is_integer(pdata->limitvar);
  for (i=pdata->ntargets-1; i>=0; i--) {
    TASK si = TARGET(i);

    if (STATUS(si) & STATUS_TARGET)
      ent = 0;
    dvar_export(w,ORIGVAR(si));
    dvar_export(w,DURVAR(si));
    dvar_export(w,ENDVAR(si));
    dvar_export(w,USEVAR(si));
  }
  dvar_export(w,pdata->limitvar);

  for (i=nbdiffs-1; i>=0; i--) {
    if (STATUS(nbtasks+i) & STATUS_TARGET)
      dvar_export(w,DIFFVAR(i));
  }

  if (ent)
    goto ret;

  /* partition into SOURCE+TARGET and SOURCE */

  {
    int delta;
    int inf = 0;
    int sup = pdata->ntargets-1;
    TASK held = TARGET(sup); /* sup is the hole */
    TASK current = TARGET(inf);
    
    while (inf<=sup) {
      if (STATUS(current) & STATUS_TARGET) {
	TARGET(inf) = current;
	inf++;
	current = (inf>=sup ? held : TARGET(inf));
      } else {
	TARGET(sup) = current;
	sup--;
	current = (inf>=sup ? held : TARGET(sup));
      }
    }
    delta = pdata->ntargets - inf;
    pdata->ntargets -= delta;
    pdata->nsources += delta;
  }
  
  /* find the last SOURCE */

  {
    int delta, sup;
    
    for (sup=nactive_items; sup>pdata->ntargets; --sup)
      if (STATUS(TARGET(sup-1)) & STATUS_SOURCE)
        break;
    delta = nactive_items - sup;
    pdata->nsources -= delta;
    nactive_items -= delta;
  }

  if (committed && dvar_is_integer(pdata->limitvar))
    for (i=pdata->ntargets; i<nactive_items; i++) {
      TASK si = TARGET(i);
      
      STATUS(si) &= ~(STATUS_TARGET<<4);
    }

 ret:
  Free(dtasks);
  CTagToArg(X(0),6) = MakeSmall(pdata->ntargets);
  CTagToArg(X(0),7) = MakeSmall(pdata->nsources);
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(w,Actions, ent);
}

