/* Copyright (C) 1993 Swedish Institute of Computer Science */

/* Definitions and headers for fast term I/O. */

#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */
#include <stdlib.h>
#include <stdio.h>
#define INIT_BUFSIZE 256

/* [PD] SICStus 3.11.1/Quintus 3.5 Assume Quintus if not SICStus. */
#ifndef SICSTUS
#define QUINTUS 1
#endif

#if QUINTUS
/* [PD] 3.5 See the comment at the beginning of fastrw.c for an explanation of
   this flag. */
#define INDIRECT_TERMREF_HACK 1
#endif

#if 0
#define INDIRECT_TERMREF_HACK 1	/* *** [PD] Temporary testing */
#endif

#if INDIRECT_TERMREF_HACK
#define XP_TERM_REF_DECL(t) XP_term_par t##ind
#define MAKE_INDIRECT_TERM(t) XP_term t = XP_store_indirect_term_ref(SPAPI_ARG t##ind)
#define UNMAKE_INDIRECT_TERM(t) t##ind = local.ids.stack[t]
#else
#define XP_TERM_REF_DECL(t) XP_term t
#define MAKE_INDIRECT_TERM(t)
#define UNMAKE_INDIRECT_TERM(t)
#endif

#if SICSTUS

#include <sicstus/sicstus.h>
#include "fastrw_glue.h"

#define Malloc(Size) SP_malloc(Size)
#define Realloc(Ptr,OldSize,NewSize) SP_realloc((char *)(Ptr),NewSize)
#define Free(Ptr,OldSize) SP_free((char *)(Ptr))

#if INDIRECT_TERMREF_HACK	/* This is for testing with SICStus */
#define XP_new_indirect_term_ref(A) XP_store_indirect_term_ref(SPAPI_ARG YP_new_term_ref()) 
#define YP_new_term_ref()        SP_new_term_ref()
#define YP_term_ref SP_term_ref
#define XP_new_term_ref()        XP_new_indirect_term_ref(SPAPI_ARG0)
#define XP_new_term_refs(arity)  XP_new_indirect_term_refs(SPAPI_ARG arity)
#define XP_init_term(T)         (T) = XP_new_indirect_term_ref(SPAPI_ARG0)
#define YP_deref_term_id(ID)    (local.ids.stack[(ID)])
#define XP_term XP_indirect_term_ref
#define XP_term_par SP_term_ref
#define XP_term_type(TERM) SP_term_type(YP_deref_term_id((TERM)))

#define XP_stream SP_stream
#define XP_TYPE_VARIABLE SP_TYPE_VARIABLE
#define XP_TYPE_ATOM SP_TYPE_ATOM
#define XP_TYPE_COMPOUND SP_TYPE_COMPOUND
#define XP_TYPE_INTEGER SP_TYPE_INTEGER
#define XP_TYPE_FLOAT SP_TYPE_FLOAT
#define XP_atom_from_string(STR) SP_atom_from_string(STR)
#define XP_string_from_atom(ATM) SP_string_from_atom(ATM)

#define XP_put_term(DEST,SRC) SP_put_term((YP_deref_term_id((DEST))),(YP_deref_term_id((SRC))))
#define XP_put_atom(TERM,ATM) SP_put_atom((YP_deref_term_id((TERM))),ATM)
#define XP_put_string(TERM,STR) SP_put_string((YP_deref_term_id((TERM))),STR)
#define XP_put_list(TERM) SP_put_list(YP_deref_term_id((TERM)))
#define XP_put_functor(TERM,ATM,ARITY) SP_put_functor(YP_deref_term_id((TERM)),ATM,ARITY)
#define XP_put_integer(TERM,INT) SP_put_integer((YP_deref_term_id((TERM))),INT)
#define XP_put_integer_chars(TERM,STR) SP_put_number_chars((YP_deref_term_id((TERM))),STR)
#define XP_put_float_chars(TERM,STR) SP_put_number_chars((YP_deref_term_id((TERM))),STR)
#define XP_get_atom(TERM,ATM) SP_get_atom(YP_deref_term_id((TERM)),&(ATM))
#define XP_get_string(TERM,STR) SP_get_string(YP_deref_term_id((TERM)),&(STR))
#define XP_get_functor(TERM,ATM,ARITY) SP_get_functor(YP_deref_term_id((TERM)),&(ATM),&(ARITY))
#define XP_get_arg(I,TERM,ARG) SP_get_arg(I,YP_deref_term_id((TERM)),(YP_deref_term_id((ARG))))
#define XP_get_integer(TERM,INT) SP_get_integer(YP_deref_term_id((TERM)),&(INT))
#define XP_get_integer_chars(TERM,STR) SP_get_number_chars(YP_deref_term_id((TERM)),&(STR))
#define XP_get_float_chars(TERM,STR) SP_get_number_chars(YP_deref_term_id((TERM)),&(STR))
#define XP_putc(STREAM,CHAR) ( SPIO_FAILED(SP_put_byte((STREAM),(CHAR))) ? -1 : 0 )
#define XP_getc(STREAM) frw_getc(STREAM)
#define XP_ungetc(STREAM,C) frw_ungetc(STREAM,C)
#define XP_unify(X,Y) SP_unify(YP_deref_term_id((X)),YP_deref_term_id((Y)))
#define XP_register_atom(X) (void)SP_register_atom(X)
#define XP_unregister_atom(X) (void)SP_unregister_atom(X)
#else  /* !INDIRECT_TERMREF_HACK */
#define XP_new_term_refs(arity)  SP_new_term_refs(arity) /* [PD] 3.11.1 */
#define XP_init_term(T)         (T) = SP_new_term_ref()
#define XP_term SP_term_ref
#define XP_term_type SP_term_type

#define XP_stream SP_stream
#define XP_TYPE_VARIABLE SP_TYPE_VARIABLE
#define XP_TYPE_ATOM SP_TYPE_ATOM
#define XP_TYPE_COMPOUND SP_TYPE_COMPOUND
#define XP_TYPE_INTEGER SP_TYPE_INTEGER
#define XP_TYPE_FLOAT SP_TYPE_FLOAT
#define XP_atom_from_string(STR) SP_atom_from_string(STR)
#define XP_string_from_atom(ATM) SP_string_from_atom(ATM)

#define XP_put_term(DEST,SRC) SP_put_term((DEST),(SRC))
#define XP_put_atom(TERM,ATM) SP_put_atom((TERM),ATM)
#define XP_put_string(TERM,STR) SP_put_string((TERM),STR)
#define XP_put_list(TERM) SP_put_list(TERM)
#define XP_put_functor(TERM,ATM,ARITY) SP_put_functor(TERM,ATM,ARITY)
#define XP_put_integer(TERM,INT) SP_put_integer((TERM),INT)
#define XP_put_integer_chars(TERM,STR) SP_put_number_codes((TERM),STR)
#define XP_put_float_chars(TERM,STR) SP_put_number_codes((TERM),STR)
#define XP_get_atom(TERM,ATM) SP_get_atom(TERM,&(ATM))
#define XP_get_string(TERM,STR) SP_get_string(TERM,&(STR))
#define XP_get_functor(TERM,ATM,ARITY) SP_get_functor(TERM,&(ATM),&(ARITY))
#define XP_get_arg(I,TERM,ARG) SP_get_arg(I,TERM,(ARG))
#define XP_get_integer(TERM,INT) SP_get_integer(TERM,&(INT))
#define XP_get_integer_chars(TERM,STR) SP_get_number_codes(TERM,&(STR))
#define XP_get_float_chars(TERM,STR) SP_get_number_codes(TERM,&(STR))
#define XP_putc(STREAM,CHAR) ( SPIO_FAILED(SP_put_byte((STREAM),(CHAR))) ? -1 : 0 )
#define XP_getc frw_getc
#define XP_ungetc frw_ungetc
#define XP_unify(X,Y) SP_unify(X,Y)
#define XP_register_atom(X) (void)SP_register_atom(X)
#define XP_unregister_atom(X) (void)SP_unregister_atom(X)
#endif /* !INDIRECT_TERMREF_HACK */

#endif /* SICSTUS */

#if QUINTUS

/* #include <quintus/quintus.h> */ /* [PD] 3.5 */
#include "quintus.h"		   /* [PD] 3.5 */

#define SPAPI_ARG_PROTO_DECL
#define SPAPI_ARG_PROTO_DECL0
#define SPAPI_ARG
#define SPAPI_ARG0
#define SPCDECL

/* [PM] 3.11.2 use system headers!
  extern double atof();
*/

static char *QP_realloc();
#define Malloc(Size) QP_malloc(Size)
#define Realloc(Ptr,OldSize,NewSize) QP_realloc((char *)(Ptr),OldSize,NewSize)
#define Free(Ptr,OldSize) QP_free((char *)(Ptr))

#if INDIRECT_TERMREF_HACK
#define XP_new_indirect_term_ref(A) XP_store_indirect_term_ref(SPAPI_ARG YP_new_term_ref()) 
#define YP_new_term_ref()        QP_new_term_ref()
#define YP_term_ref QP_term_ref
#define XP_new_term_ref()        XP_new_indirect_term_ref(SPAPI_ARG0)
#define XP_new_term_refs(arity)  XP_new_indirect_term_refs(SPAPI_ARG arity)
#define XP_init_term(T)         (T) = XP_new_indirect_term_ref(SPAPI_ARG0)
#define XP_term XP_indirect_term_ref
#define XP_term_par QP_term_ref
#define XP_stream QP_stream
#define XP_term_type(TERM) QP_term_type(YP_deref_term_id((TERM)))
#define XP_TYPE_VARIABLE QP_VARIABLE
#define XP_TYPE_ATOM QP_ATOM
#define XP_TYPE_COMPOUND QP_COMPOUND
#define XP_TYPE_INTEGER QP_INTEGER
#define XP_TYPE_FLOAT QP_FLOAT
#define XP_atom_from_string(STR) QP_atom_from_string(STR)
#define XP_string_from_atom(ATM) QP_string_from_atom(ATM)
#define XP_put_term(DEST,SRC) QP_put_term((YP_deref_term_id((DEST))),(YP_deref_term_id((SRC))))
#define XP_put_atom(TERM,ATM) QP_put_atom((YP_deref_term_id((TERM))),ATM)
#define XP_put_string(TERM,STR) QP_put_atom((YP_deref_term_id((TERM))),QP_atom_from_string(STR))
#define XP_put_list(TERM) QP_put_list(YP_deref_term_id((TERM)))
#define XP_put_functor(TERM,ATM,ARITY) QP_put_functor(YP_deref_term_id((TERM)),ATM,ARITY)
#define XP_put_integer(TERM,INT) QP_put_integer((YP_deref_term_id((TERM))),INT)
/* #define XP_put_integer_chars(TERM,STR) QP_put_integer((YP_deref_term_id((TERM))),atoi(STR)) */
#define XP_put_integer_chars(TERM,STR) QP_put_number_chars((YP_deref_term_id((TERM))),STR)
/* #define XP_put_float_chars(TERM,STR) QP_put_float((YP_deref_term_id((TERM))),atof(STR)) */
#define XP_put_float_chars(TERM,STR) QP_put_float_chars((YP_deref_term_id((TERM))),STR)
#define XP_get_atom(TERM,ATM) QP_get_atom(YP_deref_term_id((TERM)),&(ATM))
#define XP_get_string(TERM,STR) QP_get_string(YP_deref_term_id((TERM)),&(STR))
#define XP_get_functor(TERM,ATM,ARITY) QP_get_functor(YP_deref_term_id((TERM)),&(ATM),&(ARITY))
#define XP_get_arg(I,TERM,ARG) QP_get_arg(I,YP_deref_term_id((TERM)),(YP_deref_term_id((ARG))))
#define XP_get_integer(TERM,INT) QP_get_integer(YP_deref_term_id((TERM)),&(INT))
#define XP_get_integer_chars(TERM,STR) QP_get_integer_chars(YP_deref_term_id((TERM)),&(STR))
#define XP_get_float_chars(TERM,STR) QP_get_float_chars(YP_deref_term_id((TERM)),&(STR))
#error "[PM] 4.0 change QP XP_putc to check for errors"
#define XP_putc(STREAM,CHAR) ( QP_fputc(CHAR,STREAM) ? 0 : 0 )
#if 1                           /* [PM] 3.5+ XP_getc must return -1 on EOF (contrary to QP_getc) */
#define XP_getc(STREAM) frw_getc(STREAM)
#else
#define XP_getc(STREAM) QP_getc(STREAM)
#endif
#define XP_ungetc(STREAM,C) QP_ungetc(C,STREAM)
#define XP_unify(X,Y) QP_unify(YP_deref_term_id((X)),YP_deref_term_id((Y)))
#define XP_register_atom(X) (void)QP_register_atom(X)
#define XP_unregister_atom(X) (void)QP_unregister_atom(X)
#else  /* !INDIRECT_TERMREF_HACK */    /* [PD] 3.5 This won't work. */
#define XP_init_term(T)         (T) = QP_new_term_ref()
#define XP_term QP_term_ref
#define XP_stream QP_stream
#define XP_term_type QP_term_type
#define XP_TYPE_VARIABLE QP_VARIABLE
#define XP_TYPE_ATOM QP_ATOM
#define XP_TYPE_COMPOUND QP_COMPOUND
#define XP_TYPE_INTEGER QP_INTEGER
#define XP_TYPE_FLOAT QP_FLOAT
#define XP_atom_from_string(STR) QP_atom_from_string(STR)
#define XP_string_from_atom(ATM) QP_string_from_atom(ATM)
#define XP_put_term(DEST,SRC) QP_put_term(DEST,SRC)
#define XP_put_atom(TERM,ATM) QP_put_atom(TERM,ATM)
#define XP_put_string(TERM,STR) QP_put_atom(TERM,QP_atom_from_string(STR))
#define XP_put_list(TERM) QP_put_list(TERM)
#define XP_put_functor(TERM,ATM,ARITY) QP_put_functor(TERM,ATM,ARITY)
#define XP_put_integer(TERM,INT) QP_put_integer(TERM,INT)
/* #define XP_put_integer_chars(TERM,STR) QP_put_integer(TERM,atoi(STR)) */
#define XP_put_integer_chars(TERM,STR) QP_put_number_chars(TERM,STR)
/* #define XP_put_float_chars(TERM,STR) QP_put_float(TERM,atof(STR)) */
#define XP_put_float_chars(TERM,STR) QP_put_float_chars(TERM,STR)
#define XP_get_atom(TERM,ATM) QP_get_atom(TERM,&(ATM))
#define XP_get_string(TERM,STR) QP_get_string(TERM,&(STR))
#define XP_get_functor(TERM,ATM,ARITY) QP_get_functor(TERM,&(ATM),&(ARITY))
#define XP_get_arg(I,TERM,ARG) QP_get_arg(I,TERM,ARG)
#define XP_get_integer(TERM,INT) QP_get_integer(TERM,&(INT))
#define XP_get_integer_chars(TERM,STR) QP_get_integer_chars(TERM,&(STR))
#define XP_get_float_chars(TERM,STR) QP_get_float_chars(TERM,&(STR))
#error "[PM] 4.0 change QP XP_putc to check for errors"
#define XP_putc(STREAM,CHAR) ( QP_fputc(CHAR,STREAM) ? 0 : 0 )

#if 1                           /* [PM] 3.5+ XP_getc must return -1 on EOF (contrary to QP_getc) */
#define XP_getc(STREAM) frw_getc(STREAM)
#else
#define XP_getc(STREAM) QP_getc(STREAM)
#endif
#define XP_ungetc(STREAM,C) QP_ungetc(C,STREAM)
#define XP_unify(X,Y) QP_unify(X,Y)
#define XP_register_atom(X) (void)QP_register_atom(X)
#define XP_unregister_atom(X) (void)QP_unregister_atom(X)
#endif /* !INDIRECT_TERMREF_HACK */

#endif /* QUINTUS */
