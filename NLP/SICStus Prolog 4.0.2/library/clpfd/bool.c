/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

#define CAND 0
#define CANDN 1
#define CNAND 2
#define COR 3
#define CORN 4
#define CNOR 5
#define CXOR 6
#define CXORN 7

#define COMB(F,X,Y,Z) (((((((F)*3)+(X))*3)+(Y))*3)+(Z))

#define TZ TaggedZero
#define TO TaggedOne

#if FD_EAGER_ALIAS
# define FD_ALIAS(X,Y) {			\
    dvar_export_equal(w,(X),(Y));		\
    ent = 1;					\
}
#else
# define FD_ALIAS(X,Y)
#endif



/* Solve the eight essential Boolean constraints. */
/* State = state(X,XMut,Y,YMut,Z,ZMut,Fun) */
void SPCDECL
prolog_fd_bool MAGIC (HIDDEN_PROTO
		      SP_term_ref State,
		      SP_term_ref NewState,
		      SP_term_ref Actions)
{
  WAMENV;
  int xsig, ysig, zsig;
  int ent=0;
  long fun;
  TAGGED xv, yv, zv, t;
  SP_globref ref = SP_alloc_globrefs(6);
  struct dvar localvar[3];
  Dvar dvx = &localvar[0];
  Dvar dvy = &localvar[1];
  Dvar dvz = &localvar[2];

  RefTerm(NewState) = RefTerm(State);
  dvar_export_start(w);
  DerefNonvar(X(0));
  get_var_and_attr(X(0),ref+0);
  xv = RefGlob(ref+1);
  get_var_and_attr(X(0)+WD(2),ref+2);
  yv = RefGlob(ref+3);
  get_var_and_attr(X(0)+WD(4),ref+4);
  zv = RefGlob(ref+5);
  DerefArg(t,X(0),7);
  fun = GetSmall(t);
  xsig = (IsVar(xv) ? 2 : GetSmall(xv));
  ysig = (IsVar(yv) ? 2 : GetSmall(yv));
  zsig = (IsVar(zv) ? 2 : GetSmall(zv));
  dvar_init(dvx, ref+0, ref+1);
  dvar_init(dvy, ref+2, ref+3);
  dvar_init(dvz, ref+4, ref+5);
  switch (COMB(fun,xsig,ysig,zsig)) {
  case COMB(CAND,1,2,2):
  case COMB(COR,0,2,2):
  case COMB(CXOR,0,2,2):
  case COMB(CXORN,1,2,2):
    FD_ALIAS(dvy, dvz);
    break;
  case COMB(CAND,2,1,2):
  case COMB(CANDN,2,0,2):
  case COMB(COR,2,0,2):
  case COMB(CORN,2,1,2):
  case COMB(CXOR,2,0,2):
  case COMB(CXORN,2,1,2):
  x_equals_z:
    FD_ALIAS(dvx, dvz);
    break;
  case COMB(CXOR,2,2,0):
  case COMB(CXORN,2,2,1):
    FD_ALIAS(dvx, dvy);
    break;
  case COMB(CAND,2,2,0):
  case COMB(CNAND,2,2,1):
    if (xv==yv)
      goto x_equals_0;
    else
      break;
  case COMB(CAND,2,2,2):
  case COMB(COR,2,2,2):
    if (xv==yv)
      goto x_equals_z;
    else
      break;
  case COMB(CANDN,2,2,0):
  case COMB(CORN,2,2,1):
    break;
  case COMB(CANDN,2,2,2):
    if (xv==yv)
      goto z_equals_0;
    else if (yv==zv)
      goto x_equals_0_y_equals_0;
    else
      break;
  case COMB(CXOR,2,2,2):
    if (xv==yv)
      goto z_equals_0;
    else if (xv==zv)
      goto y_equals_0;
    else if (yv==zv)
      goto x_equals_0;
    else
      break;
  case COMB(COR,2,2,1):
  case COMB(CNOR,2,2,0):
    if (xv==yv)
      goto x_equals_1;
    else
      break;
  case COMB(CORN,2,2,2):
    if (xv==yv)
      goto z_equals_1;
    else if (yv==zv)
      goto x_equals_1_y_equals_1;
    else
      break;
  case COMB(CXORN,2,2,2):
    if (xv==yv)
      goto z_equals_1;
    else if (xv==zv)
      goto y_equals_1;
    else if (yv==zv)
      goto x_equals_1;
    else
      break;
  case COMB(CXOR,2,2,1):
  case COMB(CXORN,2,2,0):
    if (xv==yv)
      goto fail;
    else
      break;
  case COMB(CNAND,2,1,2):
  case COMB(CNOR,2,0,2):
  case COMB(CXOR,2,1,2):
  case COMB(CXORN,2,0,2):
    if (xv==zv)
      goto fail;
    else
      break;
  case COMB(CNAND,2,2,2):
    if (xv==zv)
      goto x_equals_1_y_equals_0;
    else if (yv==zv)
      goto x_equals_0_y_equals_1;
    else
      break;
  case COMB(CNOR,2,2,2):
    if (xv==zv)
      goto x_equals_0_y_equals_1;
    else if (yv==zv)
      goto x_equals_1_y_equals_0;
    else
      break;
  case COMB(CANDN,1,2,2):
  case COMB(CNAND,1,2,2):
  case COMB(CORN,0,2,2):
  case COMB(CNOR,0,2,2):
  case COMB(CXOR,1,2,2):
  case COMB(CXORN,0,2,2):
    if (yv==zv)
      goto fail;
    else
      break;

  case COMB(CAND,0,0,0):
  case COMB(CAND,0,1,0):
  case COMB(CAND,0,2,0):
  case COMB(CAND,1,0,0):
  case COMB(CAND,1,1,1):
  case COMB(CAND,2,0,0):
  case COMB(CANDN,0,0,0):
  case COMB(CANDN,0,1,0):
  case COMB(CANDN,0,2,0):
  case COMB(CANDN,1,0,1):
  case COMB(CANDN,1,1,0):
  case COMB(CANDN,2,1,0):
  case COMB(CNAND,0,0,1):
  case COMB(CNAND,0,1,1):
  case COMB(CNAND,0,2,1):
  case COMB(CNAND,1,0,1):
  case COMB(CNAND,1,1,0):
  case COMB(CNAND,2,0,1):
  case COMB(COR,0,0,0):
  case COMB(COR,0,1,1):
  case COMB(COR,1,0,1):
  case COMB(COR,1,1,1):
  case COMB(COR,1,2,1):
  case COMB(COR,2,1,1):
  case COMB(CORN,0,0,1):
  case COMB(CORN,0,1,0):
  case COMB(CORN,1,0,1):
  case COMB(CORN,1,1,1):
  case COMB(CORN,1,2,1):
  case COMB(CORN,2,0,1):
  case COMB(CNOR,0,0,1):
  case COMB(CNOR,0,1,0):
  case COMB(CNOR,1,0,0):
  case COMB(CNOR,1,1,0):
  case COMB(CNOR,1,2,0):
  case COMB(CNOR,2,1,0):
  case COMB(CXOR,0,0,0):
  case COMB(CXOR,0,1,1):
  case COMB(CXOR,1,0,1):
  case COMB(CXOR,1,1,0):
  case COMB(CXORN,0,0,1):
  case COMB(CXORN,0,1,0):
  case COMB(CXORN,1,0,0):
  case COMB(CXORN,1,1,1):
    ent = 1;
    break;
  case COMB(CAND,0,0,1):
  case COMB(CAND,0,1,1):
  case COMB(CAND,0,2,1):
  case COMB(CAND,1,0,1):
  case COMB(CAND,1,1,0):
  case COMB(CAND,2,0,1):
  case COMB(CANDN,0,0,1):
  case COMB(CANDN,0,1,1):
  case COMB(CANDN,0,2,1):
  case COMB(CANDN,1,0,0):
  case COMB(CANDN,1,1,1):
  case COMB(CANDN,2,1,1):
  case COMB(CNAND,0,0,0):
  case COMB(CNAND,0,1,0):
  case COMB(CNAND,0,2,0):
  case COMB(CNAND,1,0,0):
  case COMB(CNAND,1,1,1):
  case COMB(CNAND,2,0,0):
  case COMB(COR,0,0,1):
  case COMB(COR,0,1,0):
  case COMB(COR,1,0,0):
  case COMB(COR,1,1,0):
  case COMB(COR,1,2,0):
  case COMB(COR,2,1,0):
  case COMB(CORN,0,0,0):
  case COMB(CORN,0,1,1):
  case COMB(CORN,1,0,0):
  case COMB(CORN,1,1,0):
  case COMB(CORN,1,2,0):
  case COMB(CORN,2,0,0):
  case COMB(CNOR,0,0,0):
  case COMB(CNOR,0,1,1):
  case COMB(CNOR,1,0,1):
  case COMB(CNOR,1,1,1):
  case COMB(CNOR,1,2,1):
  case COMB(CNOR,2,1,1):
  case COMB(CXOR,0,0,1):
  case COMB(CXOR,0,1,0):
  case COMB(CXOR,1,0,0):
  case COMB(CXOR,1,1,1):
  case COMB(CXORN,0,0,0):
  case COMB(CXORN,0,1,1):
  case COMB(CXORN,1,0,1):
  case COMB(CXORN,1,1,0):
  fail:
    ent = -1;
    break;
  case COMB(CAND,2,1,0):
  case COMB(CANDN,2,0,0):
  case COMB(CNAND,2,1,1):
  case COMB(COR,2,0,0):
  case COMB(CORN,2,1,0):
  case COMB(CNOR,2,0,1):
  case COMB(CXOR,2,0,0):
  case COMB(CXOR,2,1,1):
  case COMB(CXORN,2,0,1):
  case COMB(CXORN,2,1,0):
				/* do([x=0,exit]); */
  x_equals_0:
    dvar_fix_value_t(dvx, TZ);
    ent = 1;
    break;
  case COMB(COR,2,2,0):
  case COMB(CNOR,2,2,1):
				/* do([x=0,y=0,exit]); */
  x_equals_0_y_equals_0:
    dvar_fix_value_t(dvx, TZ);
    dvar_fix_value_t(dvy, TZ);
    ent = 1;
    break;
  case COMB(CORN,2,2,0):
				/* do([x=0,y=1,exit]); */
  x_equals_0_y_equals_1:
    dvar_fix_value_t(dvx, TZ);
    dvar_fix_value_t(dvy, TO);
    ent = 1;
    break;
  case COMB(CAND,2,1,1):
  case COMB(CANDN,2,0,1):
  case COMB(CNAND,2,1,0):
  case COMB(COR,2,0,1):
  case COMB(CORN,2,1,1):
  case COMB(CNOR,2,0,0):
  case COMB(CXOR,2,0,1):
  case COMB(CXOR,2,1,0):
  case COMB(CXORN,2,0,0):
  case COMB(CXORN,2,1,1):
				/* do([x=1,exit]); */
  x_equals_1:
    dvar_fix_value_t(dvx, TO);
    ent = 1;
    break;
  case COMB(CANDN,2,2,1):
				/* do([x=1,y=0,exit]); */
  x_equals_1_y_equals_0:
    dvar_fix_value_t(dvx, TO);
    dvar_fix_value_t(dvy, TZ);
    ent = 1;
    break;
  case COMB(CAND,2,2,1):
  case COMB(CNAND,2,2,0):
				/* do([x=1,y=1,exit]); */
  x_equals_1_y_equals_1:
    dvar_fix_value_t(dvx, TO);
    dvar_fix_value_t(dvy, TO);
    ent = 1;
    break;
  case COMB(CAND,1,2,0):
  case COMB(CANDN,1,2,1):
  case COMB(CNAND,1,2,1):
  case COMB(COR,0,2,0):
  case COMB(CORN,0,2,1):
  case COMB(CNOR,0,2,1):
  case COMB(CXOR,0,2,0):
  case COMB(CXOR,1,2,1):
  case COMB(CXORN,0,2,1):
  case COMB(CXORN,1,2,0):
				/* do([y=0,exit]); */
  y_equals_0:
    dvar_fix_value_t(dvy, TZ);
    ent = 1;
    break;
  case COMB(CAND,1,2,1):
  case COMB(CANDN,1,2,0):
  case COMB(CNAND,1,2,0):
  case COMB(COR,0,2,1):
  case COMB(CORN,0,2,0):
  case COMB(CNOR,0,2,0):
  case COMB(CXOR,0,2,1):
  case COMB(CXOR,1,2,0):
  case COMB(CXORN,0,2,0):
  case COMB(CXORN,1,2,1):
				/* do([y=1,exit]); */
  y_equals_1:
    dvar_fix_value_t(dvy, TO);
    ent = 1;
    break;
  case COMB(CAND,0,0,2):
  case COMB(CAND,0,1,2):
  case COMB(CAND,0,2,2):
  case COMB(CAND,1,0,2):
  case COMB(CAND,2,0,2):
  case COMB(CANDN,0,0,2):
  case COMB(CANDN,0,1,2):
  case COMB(CANDN,0,2,2):
  case COMB(CANDN,1,1,2):
  case COMB(CANDN,2,1,2):
  case COMB(CNAND,1,1,2):
  case COMB(COR,0,0,2):
  case COMB(CORN,0,1,2):
  case COMB(CNOR,0,1,2):
  case COMB(CNOR,1,0,2):
  case COMB(CNOR,1,1,2):
  case COMB(CNOR,1,2,2):
  case COMB(CNOR,2,1,2):
  case COMB(CXOR,0,0,2):
  case COMB(CXOR,1,1,2):
  case COMB(CXORN,0,1,2):
  case COMB(CXORN,1,0,2):
				/* do([z=0,exit]); */
  z_equals_0:
    dvar_fix_value_t(dvz, TZ);
    ent = 1;
    break;
  case COMB(CAND,1,1,2):
  case COMB(CANDN,1,0,2):
  case COMB(CNAND,0,0,2):
  case COMB(CNAND,0,1,2):
  case COMB(CNAND,0,2,2):
  case COMB(CNAND,1,0,2):
  case COMB(CNAND,2,0,2):
  case COMB(COR,0,1,2):
  case COMB(COR,1,0,2):
  case COMB(COR,1,1,2):
  case COMB(COR,1,2,2):
  case COMB(COR,2,1,2):
  case COMB(CORN,0,0,2):
  case COMB(CORN,1,0,2):
  case COMB(CORN,1,1,2):
  case COMB(CORN,1,2,2):
  case COMB(CORN,2,0,2):
  case COMB(CNOR,0,0,2):
  case COMB(CXOR,0,1,2):
  case COMB(CXOR,1,0,2):
  case COMB(CXORN,0,0,2):
  case COMB(CXORN,1,1,2):
				/* do([z=1,exit]); */
  z_equals_1:
    dvar_fix_value_t(dvz, TO);
    ent = 1;
    break;
  }
  dvar_export(w,dvx);
  dvar_export(w,dvy);
  dvar_export(w,dvz);
  dvar_export_done(w,Actions, ent);
  SP_free_globrefs(ref,6);
}
