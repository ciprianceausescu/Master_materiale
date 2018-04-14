/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#if DBG>1
#include "dvars.h"              /* dvar_validate */
#endif

#if MULTI_SP_AWARE
#define fd_define_predicates(A1) fd_define_predicates(HIDDEN_ARG, A1)
#endif /* MULTI_SP_AWARE */

#if !MULTI_SP_AWARE
struct fd_state fd;
#endif /* !MULTI_SP_AWARE */

TAGGED fd_attribute[] = {
/*  0 */  functor_v4,
	   TaggedOne,
           TaggedZero,
	   TagOffset(STRUCT_TAG,5),
	   TagOffset(STRUCT_TAG,8),
/*  5 */  functor_Dmutable,
	   TagOffset(STRUCT_TAG,11),
	   TaggedZero,
/*  8 */  functor_Dmutable,
	   TagOffset(STRUCT_TAG,16),
	   TaggedZero,
/* 11 */  0, /*functor_dom4,*/
	   TagOffset(LIST_TAG,24),
	   0,			/* min = inf or 0 */
	   0,			/* max = sup or 1 */
	   0,			/* size = sup or 2 */
/* 16 */  0, /*functor_lists7,*/
	   TaggedZero,
	   TaggedZero,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
	   atom_nil,
/* 24 */   TagOffset(LIST_TAG,13),
	   atom_nil
};

/* Initialization etc.  Must be last.  */

static void fd_define_predicates MAGIC (HIDDEN_PROTO int install)
{
  /* Hooks for abolish, gc, save & restore */
  {
#if MULTI_SP_AWARE
    SPEnv *cookie;
    FD_STORE_SPENV(cookie);
#else/* !MULTI_SP_AWARE */
    void *cookie = NULL;
#endif/* !MULTI_SP_AWARE */
#if DBG
    {
      SP_FD_RestoreFun *x = fd_restore_hook; /* [PM] trigger compile time type error here if prolog_fd_restore is the wrong type */
      (void)x;
    }
#endif                      /* DBG */

    {
      int res;
      
      res  = SP_install_fd_hooks(cookie, fd_manager_hook, fd_restore_hook, fd_save_hook,
#if SP_FD_GLOBAL_DESTRUCTOR
                                 fd_destructor_hook, 
#else /* !SP_FD_GLOBAL_DESTRUCTOR */
                                 NULL,
#endif /* !SP_FD_GLOBAL_DESTRUCTOR */
                                 install);
#if DBG
      fd.generation = res;
#if DBG>1
      fprintf(stderr, "%s: generation==%d (%s)\n", __FILE__, (int)fd.generation, ( install ? "install" : "uninstall"));fflush(stderr);
#endif
#else
      (void)res;
#endif
    }
  }

}



struct pred_spec {
  char *name;
  int arity;
  void (SPCDECL *function)(HIDDEN_PROTO SP_term_ref State, SP_term_ref NewState, SP_term_ref Actions);
};

static struct pred_spec pred_table[] = {
  {"in_set_iff",3,SP_MANGLE(prolog_fd_in_set_iff)},
  {"eq_iff",3,SP_MANGLE(prolog_fd_eq_iff)},
  {"le_iff",3,SP_MANGLE(prolog_fd_le_iff)},
  {"oneof",3,SP_MANGLE(prolog_fd_oneof)},
  {"abs",2,SP_MANGLE(prolog_fd_abs)},
  {"x*x=y",2,SP_MANGLE(prolog_fd_square)},
  {"x*y=z",3,SP_MANGLE(prolog_fd_product)},
  {"x/y=z",3,SP_MANGLE(prolog_fd_quotient)},
  {"x mod y=z",3,SP_MANGLE(prolog_fd_modulo)},
  {"scalar_product",4,SP_MANGLE(prolog_fd_linear)},
  {"ac_linear",3,SP_MANGLE(prolog_fd_ac_linear)},
  {"all_different",1,SP_MANGLE(prolog_fd_all_different)},
  {"all_distinct",1,SP_MANGLE(prolog_fd_all_distinct)},
  {"bc_alldiff",1,SP_MANGLE(prolog_fd_bc_alldiff)},
  {"pairing",2,SP_MANGLE(prolog_fd_pairing)},
  {"sorting",3,SP_MANGLE(prolog_fd_sorting)},
  {"assignment",2,SP_MANGLE(prolog_fd_assignment)},
  {"assignment_helper",2,SP_MANGLE(prolog_fd_assignment_helper)},
  {"circuit",1,SP_MANGLE(prolog_fd_circuit)},
  {"element",3,SP_MANGLE(prolog_fd_element)},
  {"relation",3,SP_MANGLE(prolog_fd_relation)},
  {"bool",4,SP_MANGLE(prolog_fd_bool)},
  {"cumulative",2,SP_MANGLE(prolog_fd_cumulative)},
  {"disjoint1",2,SP_MANGLE(prolog_fd_disjoint1)},
  {"disjoint2",2,SP_MANGLE(prolog_fd_disjoint2)},
  {"case",4,SP_MANGLE(prolog_fd_case)},
  {"cumulatives",3,SP_MANGLE(prolog_fd_cumulatives)},
  {"global_cardinality",3,SP_MANGLE(prolog_fd_gcc)},
  {"global_cardinality_helper",2,SP_MANGLE(prolog_fd_gcc_helper)},
  {"local_cardinality",2,SP_MANGLE(prolog_fd_lcc)},
  {"lex_chain",2,SP_MANGLE(prolog_fd_lex_chain)},
  {"minimum",2,SP_MANGLE(prolog_fd_minmax)},
  {"maximum",2,SP_MANGLE(prolog_fd_minmax)},
  {"nvalue",2,SP_MANGLE(prolog_fd_nvalue)},
  {NULL,0,NULL}
};

/* Initialization upon load_foreign_resource, save, or restore. */
void SPCDECL fd_initialize MAGIC (HIDDEN_PROTO int when)
{
  TAGGED *table;
  struct pred_spec *pt;

  (void)when;
#if MULTI_SP_AWARE
  (*SP_foreign_stash()) = fd_malloc(sizeof(struct fd_state));
#endif /* MULTI_SP_AWARE */

  table = fd.linkage_keys+1;
  fd.debugging = 0;
  fd.overflowing = 1;
  fd.resumptions = 0;
  fd.entailments = 0;
  fd.prunings = 0;
  fd.failures = 0;
  fd.constraints = 0;
  fd.current_propagator = NULL;
  fd.free_propagators = NULL;
  fd.functor_dom4    = fd_attribute[FD_ATTR_MIN_OFFSET-2]
    = SetArity(SP_atom_from_string("dom"),4);
  (void)SP_register_atom(fd.functor_dom4);
  fd.functor_in_set2 = SetArity(SP_atom_from_string("in_set"),2);
  (void)SP_register_atom(fd.functor_in_set2);
  fd.functor_dom     = SetArity(SP_atom_from_string("dom"),1);
  (void)SP_register_atom(fd.functor_dom);
  fd.functor_min     = SetArity(SP_atom_from_string("min"),1);
  (void)SP_register_atom(fd.functor_min);
  fd.functor_max     = SetArity(SP_atom_from_string("max"),1);
  (void)SP_register_atom(fd.functor_max);
  fd.functor_minmax  = SetArity(SP_atom_from_string("minmax"),1);
  (void)SP_register_atom(fd.functor_minmax);
  fd.functor_val     = SetArity(SP_atom_from_string("val"),1);
  (void)SP_register_atom(fd.functor_val);
  fd.functor_none     = SetArity(SP_atom_from_string("none"),1);
  (void)SP_register_atom(fd.functor_none);
  fd.functor_call    = SetArity(SP_atom_from_string("call"),1);
  (void)SP_register_atom(fd.functor_call);
  fd.functor_eq      = SetArity(SP_atom_from_string("="),2);
  (void)SP_register_atom(fd.functor_eq);
  fd.functor_lists7  = fd_attribute[FD_ATTR_MIN_OFFSET+3]
    = SetArity(SP_atom_from_string("$fdlists"),7);
  (void)SP_register_atom(fd.functor_lists7);
  fd.functor_leqc  = SetArity(SP_atom_from_string("t=<u+c"),3);
  (void)SP_register_atom(fd.functor_leqc);
  *table++ = fd.functor_dom;
  *table++ = fd.functor_min;
  *table++ = fd.functor_dom;
  *table++ = fd.functor_max;
  *table++ = fd.functor_dom;
  *table++ = fd.functor_minmax;
  *table++ = fd.functor_dom;
  fd.token_a         = SetArity(SP_atom_from_string("a"),1);
  (void)SP_register_atom(fd.token_a);
  fd.token_d         = SetArity(SP_atom_from_string("d"),1);
  (void)SP_register_atom(fd.token_d);
  fd.token_h         = SetArity(SP_atom_from_string("h"),1);
  (void)SP_register_atom(fd.token_h);
  fd.token_l         = SetArity(SP_atom_from_string("l"),1);
  (void)SP_register_atom(fd.token_l);
  fd.token_t         = SetArity(SP_atom_from_string("t"),1);
  (void)SP_register_atom(fd.token_t);
  fd.fd_module = find_module(SP_atom_from_string("clpfd"),TRUE);
  fd_define_predicates(1);

#if !SP_FD_GLOBAL_DESTRUCTOR   /* [PM] 3.9.2b1 */
  {
#if 0
    {
      TAGGED *junk;
      fd.fd_destructor = find_definition(fd.fd_module,SP_atom_from_string("$fd_abolish"),&junk,TRUE);
    }
#else  /* [PM] 3.9b4 */
    fd.fd_destructor_fun = free_fd_info_hook; /* [PM] should not even need a variable for this!? */
#endif
  }
#endif/* !SP_FD_GLOBAL_DESTRUCTOR */

  fd.call_action1 = SP_predicate("call_action", 1, "clpfd");
  fd.overflow_action1 = SP_predicate("overflow_action", 1, "clpfd");
  fd.dispatch = new_switch_on_key(32,NULL);
  (void)prolog_fd_evaluate_indexical(HIDDEN_ARG_COMMA 0);
  for (pt = pred_table; pt->name != NULL; pt++) {
    TAGGED key = SetArity(SP_atom_from_string(pt->name),pt->arity);
    
    SP_register_atom(key);
    dyn_puthash(&fd.dispatch,key)->value.arities = (unsigned long)pt->function;
  }
#if DBG>1
  dvar_validate();
#endif
}



/* Deinitialization upon unload_foreign_resource or before save/restore. */
void SPCDECL fd_deinitialize MAGIC (HIDDEN_PROTO int when)
{
  struct pred_spec *pt;
  
  (void)when;
  for (pt = pred_table; pt->name != NULL; pt++) {
    TAGGED key = SetArity(SP_atom_from_string(pt->name),pt->arity);
    
    SP_unregister_atom(key);
  }
  dispose_switch_on_key(fd.dispatch);
  (void)SP_unregister_atom(fd.functor_dom4);
  (void)SP_unregister_atom(fd.functor_in_set2);
  (void)SP_unregister_atom(fd.functor_dom);
  (void)SP_unregister_atom(fd.functor_min);
  (void)SP_unregister_atom(fd.functor_max);
  (void)SP_unregister_atom(fd.functor_minmax);
  (void)SP_unregister_atom(fd.functor_val);
  (void)SP_unregister_atom(fd.functor_call);
  (void)SP_unregister_atom(fd.functor_eq);
  (void)SP_unregister_atom(fd.functor_lists7);
  (void)SP_unregister_atom(fd.token_a);
  (void)SP_unregister_atom(fd.token_d);
  (void)SP_unregister_atom(fd.token_h);
  (void)SP_unregister_atom(fd.token_l);
  (void)SP_unregister_atom(fd.token_t);
  fd_define_predicates(0);
  fd_dealloc();
  
#if MULTI_SP_AWARE
  {
    void *p = *SP_foreign_stash();
    *SP_foreign_stash()=NULL;
    SP_free(p);
  }
#endif /* MULTI_SP_AWARE */
}

