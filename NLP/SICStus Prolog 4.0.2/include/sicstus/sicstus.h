/* Copyright (C) 1995, Swedish Institute of Computer Science. */

/* Declarations & definitions for the SICStus Prolog C interface. */

/*----------------------------------------------------------------------*/


#ifndef INCLUDED_SICSTUS_H
#define INCLUDED_SICSTUS_H

#if _MSC_VER > 1000
#pragma once
#endif /* _MSC_VER > 1000 */

#ifdef __GNUC__
#define SP_HAVE_GCC_VERSION(MAJOR,MINOR) ((__GNUC__ > (MAJOR)) || (__GNUC__ == (MAJOR) && __GNUC_MINOR__ >= (MINOR)))
#else  /* !__GNUC__ */
#define SP_HAVE_GCC_VERSION(MAJOR,MINOR) 0
#endif  /* !__GNUC__ */

#ifndef SICSTUS_OLDHOOKS
#define SICSTUS_OLDHOOKS 1      /* [PM] 4.0 Should make this zero for SP4 release */
#endif  /* SICSTUS_OLDHOOKS */

#ifdef __cplusplus
#define SP_BEGIN_DECL extern "C" {
#define SP_END_DECL }
#else  /* !__cplusplus */
#define SP_BEGIN_DECL
#define SP_END_DECL
#endif

SP_BEGIN_DECL

  /* [PM] 3.9 platform and version dependant info now goes into the
     public part of include/sicstus/config.h.
     
     The run-time system gets the non-public defines by not defining
     SP_NO_PRIVATE_CONFIG when including config.h.

   */
#define SP_NO_PRIVATE_CONFIG 1
#include "config.h"
  /* make sure subsequent explicit include of config.h does define everything */
#undef SP_NO_PRIVATE_CONFIG

#if SP_STATIC_FOREIGN_RESOURCE

/* [PM] Try to clean this up. It does not really belong here.*/

/* [PM] 3.9b4 SP_SINGLE_THREADED does not make sense for a static
   foreign resource linked to a single sicstus run-time. It would also
   cause some problems. */

#if SP_SINGLE_THREADED
#error "SP_SINGLE_THREADED is not compatible with static foreign resources"
#endif

#undef SP_SINGLE_THREADED
#endif /* SP_STATIC_FOREIGN_RESOURCE */

/* [PM] Split export declaration for foreign resources DLLs and sprt.dll */

   /* For declaring functions exported from sicstus runtime (DLL). */
      /* API functions accessed via dispatch table, not exported from DLL */
#if 0                           /* [PM] 4.0 SPEXP() should never be seen by C compiler */
#define SPEXP(API_NO) /* API_NO is extracted by transhdr.pl */
#endif  /* 0 */
  
#if SPDLL /* True if a dynamic (DLL) foreign resource, e.g., from splfr  */

      /* For *declaring* functions exported from glue code of a foreign function DLL 
         Used by spld glue */
#if defined(_MSC_VER)
#define SPGLUEEXP __declspec(dllexport)
#elif SP_USE_GCC_VISIBILITY && SP_HAVE_GCC_VERSION(4,0)
#define SPGLUEEXP __attribute__ ((__visibility__ ("default")))
#else
#define SPGLUEEXP
#endif

      /* For *defining* functions exported from glue code of a foreign function DLL */
#if defined(_MSC_VER)
#define SPGLUEEXP1 __declspec(dllexport)
#else
#define SPGLUEEXP1
#endif
#define SPGLUEEXP2

#else /* static foreign resource (or (static or dynamic) sicstus runtime) */
#define SPGLUEEXP
#define SPGLUEEXP1
#define SPGLUEEXP2
#endif

/*----------------------------------------------------------------------*/
/* Common */

#define SP_TYPE_VARIABLE 1
#define SP_TYPE_INTEGER 2
#define SP_TYPE_ATOM 3
#define SP_TYPE_FLOAT 4
#define SP_TYPE_COMPOUND 5

#define SP_SUCCESS 1
#define SP_FAILURE 0
#ifndef SP_ERROR
#define SP_ERROR (-1)
#endif

/* [PM] 3.9.1 SP_WHEN_ERROR, ... < 0 (never seen by (de)init functions) */
#define SP_WHEN_EXPLICIT 0
/* [PM] 4.0 We no longer unload foreign resources when saving:
   #define SP_WHEN_SAVE 1
*/
#define SP_WHEN_RESTORE 1
#define SP_WHEN_EXIT 2

#include <stddef.h>       /* [PM] 3.9.1 size_t has now made into the SICStus API prototypes */
#include <stdarg.h>

#include "sicstus_spio.h"


/* [PM] 3.9 PROTOTYPE/PROTOARGS are tags used by transhdr.pl to
            extract information about API functions. */

/* [PM] 3.9 Use PROTOTYPE when only arg types are specified */
#define PROTOTYPE(argl) argl
/* [PM] 3.9 Use PROTOARGS when arg types and (all) argnames are
            specified.  In this case the argname must be last
*/
#define PROTOARGS(argl) argl

#ifndef SP_FLI_CONST
#define SP_FLI_CONST const      /* used in splfr-generated glue */
#endif  /* !SP_FLI_CONST */

#define ANYPOINTER void *

/* [PM] 3.9.2 provide a default so only DS and Extended Runtimes need to care about this */
#ifndef SPLD_DSP
/* Kind of run-time; 0 RT, 1 DS, 2 Extende RT (3.9.2) */
#define SPLD_DSP 0
#endif /* SPLD_DSP */

#define SP_GLUE_INITIALIZE_OPTION_RESTORE   0x0001
#define SP_GLUE_INITIALIZE_OPTION_ARGV_UTF8 0x0100

#define SP_initialize(Argc,Argv,Reserved) \
  sp_glue_initialize((Argc),(Argv),(Reserved),sp_pre_linkage,sp_pre_map,SPLD_DSP,SP_GLUE_INITIALIZE_OPTION_RESTORE)

#define SP_errno SP_get_errno()
#define SP_stdin SP_get_stdin()
#define SP_stdout SP_get_stdout()
#define SP_stderr SP_get_stderr()
#define SP_curin SP_get_curin()
#define SP_curout SP_get_curout()
  
typedef unsigned long int SP_term;


/*----------------------------------------------------------------------*/

/* [PM] 4.0 For internal use only */

/*
   Call this to cause a memory fault when you cannot be bothered to
   handle NULL from SP_malloc/SP_realloc/SP_calloc/SP_strdup etc

   MSG may be NULL

   Can be called from foreign resources etc. 
*/
#define LAZY_MEMORY_FAULT(MSG) do {             \
  SP_ASSERT(0);                                 \
  sp_memory_fault((MSG),-1);                    \
} while (0)

/* [PM] 4.0 use this like LAZY_NULL_CHECK(p = SP_malloc(...)); when you cannot be bother to handle out-of-memory
   
Can be called from foreign resources etc. 
*/
#define LAZY_NULL_CHECK(EXPR) do{               \
  if ((EXPR) == NULL) LAZY_MEMORY_FAULT(NULL);  \
} while (0)


/*----------------------------------------------------------------------*/
/* Types */

typedef unsigned long   SP_atom;
typedef long            SP_integer;
typedef long SP_qid;		/* Choice offset */
struct SP_pred_ref_;            /* forward declaration */
typedef struct SP_pred_ref_ *SP_pred_ref;
typedef int SP_term_ref;
typedef double *SP_globref;	/* sicstus4 */

typedef struct { void *mutex; } SP_mutex;

/* Use this like:
static SP_mutex my_mutex = SP_MUTEX_INITIALIZER;
...
SP_mutex_lock(&my_mutex);
...
SP_mutex_unlock(&my_mutex);
*/
#define SP_MUTEX_INITIALIZER {0}


/*** External Object types */

typedef struct SP_external_object_info_ *SP_external_object_type;
typedef struct SP_external_object_link_ SP_external_object_link;

typedef void (SPCDECL *SP_external_object_finalizer)(void *obj, void* type_data);

/* Should wrap with a structure where the first arg is put by SPPutExternalObjectLink */
typedef int (SPCDECL *SP_external_object_putter)(SP_term_ref, SP_external_object_link*);


typedef int (SPCDECL SP_EventFun) (ANYPOINTER);
/* typedef void (SPCDECL SP_VoidFun) (void); */
typedef void (SPCDECL SP_SigFun) (int, ANYPOINTER);
typedef void (SPCDECL SP_SetWindowTitleHook) (void *, char const *title_ienc);
typedef /* ienc */ char const * (SPCDECL SP_GetWindowTitleHook) (void *);

/* return zero if there were no work to be done, return one if it did some work */
typedef int (SPCDECL SP_idle_hook) (void *cookie);


  /* Foreign resources and glue functions */

struct SICSTUS_API_STRUCT;      /* forward declaration, real def in spaux.h */
typedef struct SICSTUS_API_STRUCT SPEnv;


#if MULTI_SP_AWARE
/* Name mangling is used for link time checking. If the foreign
   declaration says 'foo' then the function called by the generated
   glue-code will be 'foo_multi_sp' if MULTI_SP_AWARE is set when
   compiling.
*/

#define SP_MANGLE(FNAME) FNAME ## _spenv_multi_sp

#define SPAPI_ARG_NAME spenv_arg
#define SPAPI_STASH_NAME (&((SPAPI_ARG_NAME)->stash))
#define SPAPI_ARG0 SPAPI_ARG_NAME
#define SPAPI_ARG SPAPI_ARG0,
#define SPAPI_ARG_PROTO_DECL0 SPEnv *SPAPI_ARG_NAME
#define SPAPI_ARG_LOCAL_DECL SPEnv *SPAPI_ARG_NAME;
#define SPAPI_ARG_PROTO_DECL SPAPI_ARG_PROTO_DECL0, 
#define SPAPI_ARG_IGNORE do{(void)SPAPI_ARG_NAME;}while(0)

/* This is where SP_put_list et al find the dispatch table, it is
   passed as an argument to all functions that use the SICStus API. */
#ifndef SICStusDISPATCHVAR      /* allow it to be overridden by user */
#define SICStusDISPATCHVAR SPAPI_ARG_NAME
#endif /* SICStusDISPATCHVAR */


#else /* !MULTI_SP_AWARE */
#define SPAPI_ARG0
#define SPAPI_ARG
#define SPAPI_ARG_PROTO_DECL0 void
#define SPAPI_ARG_LOCAL_DECL
#define SPAPI_ARG_PROTO_DECL
#define SPAPI_ARG_IGNORE

/* Use the definition of SICStusDISPATCHVAR from spaux.h */
#endif /* !MULTI_SP_AWARE */

/* Used by glue code */
struct worker;

/* [PM] 3.9b4 
   The (type of the) glue function actually called by the SICStus
   run-time when a function that implements a predicate in a foreign
   resource is to be invoked (Not to be confused with SP_RTPred) */

typedef int (SPCDECL SP_GlueFun) PROTOTYPE((SPEnv *spenv));
typedef SP_GlueFun *SP_GlueFunPtr;

/* Used with SP_define_c_predicate */
typedef int (SPCDECL SP_CPredFun)(SP_term_ref goal, void *stash);

typedef void (SPCDECL SP_InitFunPlain) (int);
typedef void (SPCDECL SP_InitFunMulti) (SPEnv *, int);

typedef union {
  SP_InitFunPlain *plain;
  SP_InitFunMulti *multi;
} SP_InitFunUnion;



/* Type of foreign resource set-up function (generated by splfr) */

struct SICSTUS_DISPATCH_TABLE_STRUCT;

/* [PM] 3.10 If SP_DISPATCH_API_VERSION_MAJOR differs then
   sp_main_helper may not find the SP API functions it needs. This was
   a real issue in SP 3.10. */
#define SP_MAINFUN_PARAMS_VERSION (((SP_DISPATCH_API_VERSION_MAJOR)<<16) + 1) /* The LSB is for real main-fun param changes */


struct SP_MAINFUN_PARAMS_STRUCT;
typedef struct SP_MAINFUN_PARAMS_STRUCT SP_MAINFUN_PARAMS;

typedef int (SPCDECL SP_MainFun)(SP_MAINFUN_PARAMS *params);

/*----------------------------------------------------------------------*/

/* Wide character encoding (WCX) */

#define SP_WCX_FLAG       -1

#define CHT_LAYOUT_CHAR    1
#define CHT_SMALL_LETTER   2
#define CHT_CAPITAL_LETTER 3
#define CHT_SOLO_CHAR      4
#define CHT_SYMBOL_CHAR    5
#define CHT_UNDECIDED      6
#define CHT_OTHER          7

  /* WCX hook setup */
#if SICSTUS_OLDHOOKS || !SICSTUS_SPIO
typedef void  (SPCDECL SP_WcxOpenHook) PROTOTYPE((SP_stream *s, SP_atom option, int where));
typedef void  (SPCDECL SP_WcxCloseHook) PROTOTYPE((SP_stream *s));
typedef int  (SPCDECL SP_WcxGetcHook) PROTOTYPE((int first_byte, SP_stream *s, long *pbyte_count));
typedef int (SPCDECL SP_WcxPutcHook) PROTOTYPE((int char_code, SP_stream *s, long *pbyte_count));
typedef int  (SPCDECL SP_WcxCharTypeHook) PROTOTYPE((int char_code));

typedef char const * (SPCDECL SP_WcxConvHook) PROTOTYPE((char const *string, int context));
#endif  /* SICSTUS_OLDHOOKS || !SICSTUS_SPIO */

  /* WCX usage defaults */


#define WCX_USE_LATIN1          0
#define WCX_USE_EUC             1
#define WCX_USE_UTF8            2
#define WCX_USE_UNDEF           3

#define WCX_OS_8BIT             0
#define WCX_OS_UTF8         0x100
				
#define WCX_PRESERVES_ASCII     0
#define WCX_CHANGES_ASCII  0x4000

  /* WCX conversion contexts */

#define WCX_FILE		1
#define WCX_OPTION		2
#define WCX_WINDOW_TITLE	3
#define WCX_C_CODE		4

  /* WCI (alias Ienc) utilities for use in the foreign code */


#define WCI_MAX_BYTES 6

/*----------------------------------------------------------------------*/
/* Global vars defined in the "resource table" file used by Runtime
   Systems. */

extern SP_MainFun *sp_pre_linkage[];
extern char *sp_pre_map[];

/* WCX */
/* The parameter which should be 0, 1, or 2, denoting user_input,
user_output, and user_error respectively.  

The post-hook is called after the streams have been created and
enable the user to modify the default streams.
*/

#define SP_STREAMHOOK_STDIN   0x00
#define SP_STREAMHOOK_STDOUT  0x01
#define SP_STREAMHOOK_STDERR  0x02
#define SP_STREAMHOOK_BIN     0x40
#define SP_STREAMHOOK_WCI     0x41
#define SP_STREAMHOOK_NULL    0x80
#define SP_STREAMHOOK_OPEN    0x81
#define SP_STREAMHOOK_LIB     0x90   
#define SP_STREAMHOOK_C       0xff

typedef SP_stream * (SPCDECL SP_UserStreamHook) (void *, int which);
typedef void (SPCDECL SP_UserStreamPostHook) (void *, int which, SP_stream *);


/* Alloc */

typedef int (SPCDECL SP_InitAllocHook) (size_t alignment, void *cookie);
typedef void (SPCDECL SP_DeinitAllocHook) (void *cookie);
typedef void* (SPCDECL SP_AllocHook) (size_t size, size_t *actual_sizep, void *cookie);
typedef int (SPCDECL SP_FreeHook) (void *ptr, size_t size, int force, void *cookie);

#define SP_on_fault(Stmt,Reason,CleanupStmt)	\
{						\
  jmp_buf m__buf;				\
						\
  if (!setjmp(m__buf))				\
    {						\
      sp_set_abort_env(&m__buf);		\
      {Stmt};					\
      sp_set_abort_env(NULL);			\
    }						\
  else						\
    {						\
      (Reason) = sp_get_abort_err_ienc();	\
      {CleanupStmt};				\
    }						\
}

/*----------------------------------------------------------------------*/
/* Error codes used to raise_exception from C-code */
/* xref Bips/intrins1.pl */
#define INSTANTIATION_ERROR 0
#define TYPE_ERROR 1
#define DOMAIN_ERROR 2
#define REPRESENTATION_ERROR 3
#define EXISTENCE_ERROR 4
#define SYSTEM_ERROR 5
#define PERMISSION_ERROR 6
#define EVALUATION_ERROR 7
#define CONSISTENCY_ERROR 8
#define INTERNAL_ERROR 9        /* [PM] 4.0 Never seen by prolog, only used with DBG */


/* Some types, domains, etc. */
/* xref Bips/intrins1.pl */
#define TYPE_ATOMIC (1<<8)
#define TYPE_ATOM (2<<8)
#define TYPE_NUMBER (3<<8)
#define TYPE_INTEGER (4<<8)
#define TYPE_COMPOUND (5<<8)
#define TYPE_VAR (6<<8)
#define TYPE_SPCODES (7<<8)
#define TYPE_FLOAT (8<<8)

#define DOMAIN_1_ARITY (1<<8)	/* unused */
#define DOMAIN_CHAR (2<<8)      /* [PM] 4.0 now used */
#define DOMAIN_NONZERO (3<<8)
#define DOMAIN_GT_ZERO (4<<8)
#define DOMAIN_GE_ZERO (5<<8)
#define DOMAIN_EXPRESSION (6<<8)
#define DOMAIN_LIST (7<<8)
#define DOMAIN_MUTABLE (8<<8)	/* unused */
#define DOMAIN_STREAM (9<<8)
#define DOMAIN_INPUT_STREAM (10<<8) /* unused */
#define DOMAIN_OUTPUT_STREAM (11<<8)
#define DOMAIN_ATTR_MODULE (12<<8) /* unused */
#define DOMAIN_IO_DESCR_STREAM (13<<8)
#define DOMAIN_ABS_LE_ONE (14<<8)
#define DOMAIN_ABS_GE_ONE (15<<8)
#define DOMAIN_GE_ONE (16<<8)
#define DOMAIN_GT_FZERO (17<<8)
#define DOMAIN_GE_FZERO (18<<8)
  
#define REPR_MAXARITY (1<<8)	/* unused */
#define REPR_PATHLEN (2<<8)	/* unused */
#define REPR_NAN (3<<8)
#define REPR_WORD (4<<8)
#define REPR_MISENC (5<<8)
#define REPR_MAXATOM (6<<8)

#define EXIST_EOF (1<<8)
#define EXIST_PREDICATE (2<<8)	/* unused */
#define EXIST_FUNCTION (3<<8)	/* unused */
#define EXIST_RESOURCE (4<<8)
#define EXIST_FILE (5<<8)

#define PERM_READ_STREAM (1<<8)
#define PERM_WRITE_STREAM (2<<8)
#define PERM_READ_FILE (3<<8)
#define PERM_WRITE_FILE (4<<8)
#define PERM_SEEK (5<<8)

#define CONS_RESTORE (1<<8)

#define EVAL_FLOAT_OFLO (1<<8)
#define EVAL_UNDEFINED (2<<8)
#define EVAL_ZERO_DIV (3<<8)	/* unused */
#define EVAL_FLOAT_NOT_A_NUMBER (4<<8)

#if SP_WIN32
#define SP_syserror_win32(PRED,CALL) sp_raise_win32_error(GetLastError(), PRED, CALL)
#else  /* !SP_WIN32 (a.k.a. UNIX)  */
#define SP_syserror_errno(PRED,CALL) sp_raise_errno_error(errno, PRED, CALL)
#endif  /* !SP_WIN32 */

#if 0
   /* SP_syserror_*() raises a system_error/1 with error information from
      the appropriate api, e.g. 'errno' in addition to the args. Must
      convert to string locally to dll. */
   #define SP_syserror_win32(PRED,CALL) sp_syserror(API_WIN32, NULL, PRED, CALL)

   #define API_CLIB 0
   #define API_WIN32 1
#endif  /* 0 */

/*----------------------------------------------------------------------*/

/* Options for SP_expand_file_name */
/* Return slash terminated path */
#define SP_EXPAND_FILE_NAME_OPTION_DIR        0x0001
/* do not accept relative path as input */
#define SP_EXPAND_FILE_NAME_OPTION_NO_CWD     0x0002
/* Do not expand environment variable */
#define SP_EXPAND_FILE_NAME_OPTION_NO_ENV     0x0004
/* Do not expand ~ or ~user */
#define SP_EXPAND_FILE_NAME_OPTION_NO_HOME    0x0008
/* If !SP_EXPAND_FILE_NAME_OPTION_DIR root dir becomes "/.", "c:/." etc */
#define SP_EXPAND_FILE_NAME_OPTION_ROOT_DOT   0x00010
/* If !SP_EXPAND_FILE_NAME_OPTION_DIR root dir gives error */
#define SP_EXPAND_FILE_NAME_OPTION_ROOT_SLASH 0x0020

/* Options for SP_get_stream_counts */
#define SP_GET_STREAM_COUNTS_OPTION_READ  0x0001
#define SP_GET_STREAM_COUNTS_OPTION_WRITE 0x0002


/*----------------------------------------------------------------------*/

/* [PM] 3.9 Run-time system entry point (as called by spld-generated code)*/
extern int SPCDECL user_main(int argc, char *argv[]);
/* [PM] 3.9 xref charmain.c. Called from spld-generated code */
extern int SPCDECL sp_main_internal(int argc, char **argv);

#if SICSTUS_OLDHOOKS
/* [PM] 3.9 Run-time system user hook (as called by spld-generated code) */
extern int SPCDECL SU_initialize(int argc, char *argv[]);
#endif  /* SICSTUS_OLDHOOKS */

/* [PM] 3.11.2 WinCE options to SP_set_initial_options */
enum SP_option_type {
  SP_option_type_invalid=0,
  SP_option_type_null=1,
  SP_option_type_environment_property=2
};

/* type=SP_option_type_environment_property */
struct SP_option_environment_property {
  char const *key;              /* SICStus internal encoding (UTF-8) */
  char const *value;            /* SICStus internal encoding (UTF-8) */
};

struct SP_option {
  enum SP_option_type type;
  union SP_option_ {
    void *reserved;
    struct SP_option_environment_property prop;
  } u;
};

struct SP_options {
  unsigned long version;
  size_t noptions;
  struct SP_option *options; /* noptions elements */
};
#define SP_OPTIONS_VERSION_4_0_1 0x040001 /* introduced in 4.0.1 */
#define SP_OPTIONS_VERSION SP_OPTIONS_VERSION_4_0_1
#define SP_OPTIONS_STATIC_INITIALIZER {SP_OPTIONS_VERSION, 0, 0}


/*----------------------------------------------------------------------*/
/* Undocumented. The following definitions should be avoided if possible. */

     /* Misc. prolog actions */
#if 0
#define	SP_ACTION_TRACE		1 /* Switch to trace mode */
#define SP_ACTION_DEBUG		2 /* Switch to debug mode */
#define SP_ACTION_HALT		5 /* Halt Prolog execution */
#define SP_ACTION_ABORT		6 /* Generate a Prolog abort */
#endif
     /* Inquire prolog */
#if 0
#define SP_INQUIRY_ARGC		1 /* Return main() argc */
#define SP_INQUIRY_ARGV		2 /* Return main() argv */
#define SP_INQUIRY_SYMBOLFILE	3 /* Not implemented */
#endif


/*----------------------------------------------------------------------*/

struct SICSTUS_API_STRUCT;      /* forward declaration */
/*
  The following typedef for SP_get_dispatch_type expose more than it
  should. This is due to a need to forward declare this type before
  SICSTUS_API_STRUCT_TYPE is defined. You should pretend that it
  instead looked like:

typedef SICSTUS_API_STRUCT_TYPE * SPCDECL SP_get_dispatch_type(void *reserved);
  
*/
typedef struct SICSTUS_API_STRUCT * SPCDECL SP_get_dispatch_type(void *reserved /* should be NULL */);

struct SP_multi_sp_state;   /* [PM] 3.9b4 forward declaration */


/*----------------------------------------------------------------------*/
/* External function declarations, spaux.h contains alternative
   definitions which are necessary for some platforms. Because of
   this, *ALL FUNCTION DECLARATIONS*, and *NOTHING ELSE*, must be inside
   the following else-endif.
   In particular, any typedefs or macros need to go above.

   [PM]: Mars 2000 We might as well keep the convention that
         everything except function declarations should go above.
         (there are some special cases for SP_DISPATCH_API api)
*/

   /* spaux declares all functions either directly or via dispatch so
      the extern declarations below are never seen by any code,
      they are just input to transhdr.pl */
#if LOCAL_INCLUDES
#include "spaux.h"
#else /* ! LOCAL_INCLUDES */
#include <sicstus/spaux.h>
#endif /* ! LOCAL_INCLUDES */

/* [PM] 3.9 Moved this here instead of duplicating it in generated
            glue code. Has to go after spaux.h so that
            SICSTUS_API_STRUCT_TYPE is defined (if SP_DISPATCH_API)
*/
/* Structure to pass info to glue functions. */
struct sp_fli_info {
  SP_term *fli_stack_start;	/* Copy of pointer in worker */
  SP_term *term;                /* Pointer into worker X-regs */
  int excp_flag;
};

  struct SP_MAINFUN_PARAMS_STRUCT {
    unsigned long version;        /* This field must be first (SP_MAINFUN_PARAMS_VERSION) */
    /* The rest of the fields may vary when version is changed */
    SPEnv *spenv;               /* contains api and stash */

    int flags;                  /* in, out */
#define SP_MAINFUN_PARAM_FLAG_ENTER 0x1 /* in -- about to load the resource */
#define SP_MAINFUN_PARAM_FLAG_EXIT  0x2 /* in -- about to unload the resource */

#define SP_MAINFUN_PARAM_FLAG_PLAIN 0x8  /* out -- foreign functions does not take a stash argument */
#define SP_MAINFUN_PARAM_FLAG_MULTI 0x10 /* out -- foreign functions takes a stash argument (i.e., multi-sp-aware) */


    const SP_GlueFunPtr  *funcs;    /* out */
    char const*          *prednames; /* out */
    const int            *arities;  /* out */

#if 1
    SP_InitFunUnion init_fun;   /* out */
    SP_InitFunUnion deinit_fun; /* out */
#else  /* 0 */
    SP_InitFun*        init_fun;  /* out */
    SP_InitFun*        deinit_fun; /* out */
#endif  /* 0 */
  };


extern
#if INCLUDED_FROM_RUNTIME
#if defined(_MSC_VER)
__declspec(dllexport)
#elif SP_USE_GCC_VISIBILITY && SP_HAVE_GCC_VERSION(4,0)
__attribute__ ((__visibility__ ("default")))
#endif
#endif /* INCLUDED_FROM_RUNTIME */
SP_get_dispatch_type SP_get_dispatch;

#if !(SP_DISPATCH_API  || 1 /* 3.9 always on */) /* 3.8 API */

#if SPDLL

#if LOCAL_INCLUDES
#include "spaux.h"
#else /* ! LOCAL_INCLUDES */
#include <sicstus/spaux.h>
#endif /* ! LOCAL_INCLUDES */

#else /* ! SPDLL */

#if (defined(_MSC_VER) || (defined(__GNUC__) && defined(__WIN32__)))
/* Defines funcs to have leading underscore (undecorated) to be
   compatible with Watcom and Borland. */
#if (defined(INCLUDED_DATADEFS_H) || LOCAL_INCLUDES)
#include "spmsc.h"
#else  /* ! (defined(INCLUDED_DATADEFS_H) || LOCAL_INCLUDES) */
#include <sicstus/spmsc.h>
#endif /* ! (defined(INCLUDED_DATADEFS_H) || LOCAL_INCLUDES) */
#endif /* (defined(_MSC_VER) || (defined(__GNUC__) && defined(__WIN32__))) */

/*----------------------------------------------------------------------*/
/* Preparation */

/* [PM] 3.11.2 WinCE call this before SP_initialize/sp_glue_initialize to set-up options */
extern SPEXP(123) SPEXPFLAG_PREINIT SPEXPFLAG_HIDDEN SPEXPFLAG_OLDHOOKS int SPCDECL SP_set_initial_options PROTOTYPE((struct SP_options *));

/* Should not be hidded */
extern SPEXP(1) SPEXPFLAG_PREINIT int SPCDECL sp_glue_initialize PROTOTYPE((int, char **, void *, SP_MainFun **, char **, int, int));

extern SPEXP(2) SPEXPFLAG_PREINIT void SPCDECL SP_deinitialize PROTOTYPE((void));
extern SPEXP(3) SPEXPFLAG_PREINIT SPEXPFLAG_OLDHOOKS void SPCDECL SP_force_interactive PROTOTYPE((void));
extern SPEXP(4) SPEXPFLAG_UNAVAILABLE SPEXPFLAG_HIDDEN int SPCDECL SP_is_interactive PROTOTYPE((void));
extern SPEXP(5) int SPCDECL SP_load PROTOTYPE((char const *));
extern SPEXP(6) SP_pred_ref SPCDECL SP_predicate PROTOTYPE((char const *,long,char const *));
extern SPEXP(7) SP_pred_ref SPCDECL SP_pred PROTOTYPE((SP_atom,long,SP_atom));
extern SPEXP(8) SP_term_ref SPCDECL SP_new_term_ref PROTOTYPE((void));
extern SPEXP(9) int SPCDECL SP_new_term_refs PROTOTYPE((int));
extern SPEXP(10) SPEXPFLAG_HIDDEN void SPCDECL SP_reset_term_refs PROTOTYPE((int));

extern SPEXP(11) SPEXPFLAG_HIDDEN SP_globref SPCDECL SP_alloc_globrefs PROTOTYPE((int));
extern SPEXP(12) SPEXPFLAG_HIDDEN void SPCDECL SP_free_globrefs PROTOTYPE((SP_globref,int));


/*----------------------------------------------------------------------*/
/* Control */

extern SPEXP(13) int SPCDECL SP_query PROTOARGS((SP_pred_ref predicate, ...));
extern SPEXP(14) int SPCDECL SP_query_cut_fail PROTOARGS((SP_pred_ref predicate, ...));
extern SPEXP(15) SP_qid SPCDECL SP_open_query PROTOARGS((SP_pred_ref predicate, ...));
extern SPEXP(16) SP_qid SPCDECL SP_open_query_array PROTOTYPE((SP_pred_ref, SP_term_ref *));
extern SPEXP(17) int SPCDECL SP_next_solution PROTOTYPE((SP_qid));
extern SPEXP(18) int SPCDECL SP_cut_query PROTOTYPE((SP_qid));
extern SPEXP(19) int SPCDECL SP_close_query PROTOTYPE((SP_qid));
extern SPEXP(20) int SPCDECL SP_event PROTOTYPE((SP_EventFun *, ANYPOINTER));
extern SPEXP(22) SPEXPFLAG_HIDDEN SP_term SPCDECL sp_ref_term PROTOARGS((SP_term_ref ref));

/*----------------------------------------------------------------------*/
/* SP_put_* */

extern SPEXP(26) int SPCDECL SP_put_variable PROTOTYPE((SP_term_ref));
extern SPEXP(27) int SPCDECL SP_put_term PROTOTYPE((SP_term_ref, SP_term_ref));
extern SPEXP(28) int SPCDECL SP_put_integer PROTOTYPE((SP_term_ref, long));
extern SPEXP(29) int SPCDECL SP_put_float PROTOTYPE((SP_term_ref, double));
extern SPEXP(30) int SPCDECL SP_put_atom PROTOTYPE((SP_term_ref, SP_atom));
extern SPEXP(31) int SPCDECL SP_put_string PROTOTYPE((SP_term_ref, char const *));
extern SPEXP(32) int SPCDECL SP_put_address PROTOTYPE((SP_term_ref, ANYPOINTER));
extern SPEXP(33) int SPCDECL SP_put_list_codes PROTOTYPE((SP_term_ref, SP_term_ref, char const *));
extern SPEXP(34) int SPCDECL SP_put_number_codes PROTOTYPE((SP_term_ref, char const *));
extern SPEXP(35) int SPCDECL SP_put_functor PROTOTYPE((SP_term_ref, SP_atom, int));
extern SPEXP(36) int SPCDECL SP_put_list PROTOTYPE((SP_term_ref));
extern SPEXP(37) int SPCDECL SP_cons_functor PROTOARGS((SP_term_ref term, SP_atom name, int arity, ...));
extern SPEXP(38) int SPCDECL SP_cons_list PROTOTYPE((SP_term_ref, SP_term_ref, SP_term_ref));

extern SPEXP(39) int SPCDECL SP_read_from_string PROTOARGS((SP_term_ref term, char const *string, SP_term_ref *values));

/*----------------------------------------------------------------------*/
/* SP_get_* */

extern SPEXP(40) int SPCDECL SP_get_integer PROTOTYPE((SP_term_ref, long *));
extern SPEXP(41) int SPCDECL SP_get_float PROTOTYPE((SP_term_ref, double *));
extern SPEXP(42) int SPCDECL SP_get_atom PROTOTYPE((SP_term_ref, SP_atom *));
extern SPEXP(43) int SPCDECL SP_get_string PROTOTYPE((SP_term_ref, char const **));
extern SPEXP(44) int SPCDECL SP_get_address PROTOTYPE((SP_term_ref, ANYPOINTER *));
extern SPEXP(45) int SPCDECL SP_get_list_codes PROTOTYPE((SP_term_ref, char const **));
extern SPEXP(46) int SPCDECL SP_get_number_codes PROTOTYPE((SP_term_ref, char const **));
extern SPEXP(47) int SPCDECL SP_get_functor PROTOTYPE((SP_term_ref, SP_atom *, int *));
extern SPEXP(48) int SPCDECL SP_get_list PROTOTYPE((SP_term_ref, SP_term_ref, SP_term_ref));
extern SPEXP(49) int SPCDECL SP_get_arg PROTOTYPE((int, SP_term_ref, SP_term_ref));



/*----------------------------------------------------------------------*/
/* SP_is_* */

extern SPEXP(50) int SPCDECL SP_term_type PROTOTYPE((SP_term_ref));
extern SPEXP(51) int SPCDECL SP_is_variable PROTOTYPE((SP_term_ref));
extern SPEXP(52) int SPCDECL SP_is_integer PROTOTYPE((SP_term_ref));
extern SPEXP(53) int SPCDECL SP_is_float PROTOTYPE((SP_term_ref));
extern SPEXP(54) int SPCDECL SP_is_atom PROTOTYPE((SP_term_ref));
extern SPEXP(55) int SPCDECL SP_is_compound PROTOTYPE((SP_term_ref));
extern SPEXP(56) int SPCDECL SP_is_list PROTOTYPE((SP_term_ref));
extern SPEXP(57) int SPCDECL SP_is_atomic PROTOTYPE((SP_term_ref));
extern SPEXP(58) int SPCDECL SP_is_number PROTOTYPE((SP_term_ref));


/*----------------------------------------------------------------------*/
/* Others */

extern SPEXP(59) SP_atom SPCDECL SP_atom_from_string PROTOTYPE((char const *));
extern SPEXP(60) SP_atom SPCDECL SP_existing_atom_from_string PROTOTYPE((char const *));
extern SPEXP(61) char const* SPCDECL SP_string_from_atom PROTOTYPE((SP_atom));
extern SPEXP(62) int SPCDECL SP_unify PROTOTYPE((SP_term_ref,SP_term_ref));
extern SPEXP(63) int SPCDECL SP_compare PROTOTYPE((SP_term_ref,SP_term_ref));
extern SPEXP(64) int SPCDECL SP_exception_term PROTOTYPE((SP_term_ref));
extern SPEXP(65) void SPCDECL SP_raise_exception PROTOTYPE((SP_term_ref));
extern SPEXP(66) void SPCDECL SP_fail PROTOTYPE((void));
extern SPEXP(67) char const * SPCDECL SP_error_message PROTOTYPE((int));
extern SPEXP(68) int SPCDECL SP_get_errno PROTOTYPE((void));


/*----------------------------------------------------------------------*/
/* Streams */

extern SPEXP(69) SP_stream* SPCDECL SP_get_stdin PROTOTYPE((void));
extern SPEXP(70) SP_stream* SPCDECL SP_get_stdout PROTOTYPE((void));
extern SPEXP(71) SP_stream* SPCDECL SP_get_stderr PROTOTYPE((void));
extern SPEXP(72) SP_stream* SPCDECL SP_get_curin PROTOTYPE((void));
extern SPEXP(73) SP_stream* SPCDECL SP_get_curout PROTOTYPE((void));


extern SPEXP(74) SPEXPFLAG_PREINIT SPEXPFLAG_OLDHOOKS SP_UserStreamHook * SPCDECL SP_set_user_stream_hook PROTOTYPE((SP_UserStreamHook *, void *));
extern SPEXP(75) SPEXPFLAG_PREINIT SPEXPFLAG_OLDHOOKS SP_UserStreamPostHook * SPCDECL SP_set_user_stream_post_hook PROTOTYPE((SP_UserStreamPostHook *, void *));

extern SPEXP(76) SPEXPFLAG_PREINIT SPEXPFLAG_HIDDEN SPEXPFLAG_UNAVAILABLE SPEXPFLAG_OLDHOOKS SP_ErrprintfHook * SPCDECL SP_set_errprintf_hook PROTOTYPE((SP_ErrprintfHook *, void *));

extern SPEXP(77) SPEXPFLAG_PREINIT SPEXPFLAG_OLDHOOKS int SPCDECL 
SP_set_memalloc_hooks PROTOARGS((int hints,
				 SP_InitAllocHook *init_alloc_hook,
				 SP_DeinitAllocHook *deinit_alloc_hook,
				 SP_AllocHook *alloc_hook,
				 SP_FreeHook *free_hook,
                                 void *cookie));

/* SPIO stream API */

extern SPEXP(78) SPEXPFLAG_SPIO
       spio_t_error_code SPCDECL SP_create_stream PROTOARGS((void *user_data,
                                                             void const *user_class,
                                                             spio_t_simple_device_read *user_read,
                                                             spio_t_simple_device_write *user_write,
                                                             spio_t_simple_device_flush_output *user_flush_output,
                                                             spio_t_simple_device_seek *user_seek,
                                                             spio_t_simple_device_close *user_close,
                                                             spio_t_simple_device_interrupt *user_interrupt,
                                                             spio_t_simple_device_ioctl *user_ioctl,
                                                             void *args,
                                                             spio_t_bits create_options,
                                                             SP_stream **pstream));

extern SPEXP(24) SPEXPFLAG_SPIO
       spio_t_error_code SPCDECL SP_get_stream_user_data PROTOARGS((SP_stream *stream, void const *user_class, void **puser_data));


extern SPEXP(25) SPEXPFLAG_SPIO
       spio_t_error_code SPCDECL SP_get_stream_counts PROTOARGS((SP_stream *stream,
                                                                 spio_t_offset *item_offset,
                                                                 spio_t_offset *newline_count,
                                                                 spio_t_offset *line_length,
                                                                 spio_t_bits option));
                                                                 

extern SPEXP(23) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_create_os_stream PROTOARGS((spio_t_os_file_handle hFile, SP_stream **pstream, void *arg, spio_t_bits options));

/* These are used by SP_{get,put}_{byte,code} and should not be hidded (but are not publicly documented) */
extern SPEXP(79) SPEXPFLAG_SPIO spio_t_error_code SPCDECL sp_get_byte_helper PROTOARGS((SP_stream *s, spio_t_bits options));
extern SPEXP(80) SPEXPFLAG_SPIO spio_t_error_code SPCDECL sp_put_byte_helper PROTOARGS((SP_stream *s, spio_t_uint8 byte, spio_t_bits options));
extern SPEXP(81) SPEXPFLAG_SPIO spio_t_error_code SPCDECL sp_get_code_helper PROTOARGS((SP_stream *s, spio_t_bits options));
extern SPEXP(82) SPEXPFLAG_SPIO spio_t_error_code SPCDECL sp_put_code_helper PROTOARGS((SP_stream *s, spio_t_wchar code, spio_t_bits options));

extern SPEXP(83) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_printf PROTOARGS((char const *fmt, ...));
extern SPEXP(84) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_fprintf PROTOARGS((SP_stream *stream, char const *fmt, ...));
extern SPEXP(85) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL sp_ttyprintf PROTOARGS((char const *fmt, ...));
extern SPEXP(184) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL sp_ttyflush PROTOARGS((void));

extern SPEXP(86) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_put_bytes PROTOARGS((SP_stream *s, spio_t_uint8 const *bytes, size_t byte_count, spio_t_bits options));
extern SPEXP(87) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_put_codes PROTOARGS((SP_stream *s, spio_t_wchar const *codes, size_t code_count, spio_t_bits options));
extern SPEXP(88) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_put_encoded_string PROTOARGS((SP_stream *s, char const *string, spio_t_bits options));

extern SPEXP(89) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_flush_output PROTOARGS((SP_stream *s, spio_t_bits options));
extern SPEXP(21) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_fopen PROTOARGS((char const *path_ienc, void *reserved, spio_t_bits options, SP_stream **ps));
extern SPEXP(90) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_fclose PROTOARGS((SP_stream *s, spio_t_bits options));
extern SPEXP(91) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_unget_code PROTOTYPE((SP_stream*, int));
extern SPEXP(92) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_unget_byte PROTOTYPE((SP_stream*, int));

extern SPEXP(138) SPEXPFLAG_SPIO spio_t_error_code SPCDECL SP_next_stream PROTOARGS((SP_stream *s, SP_stream **pnext));

extern SPEXP(139) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN SPEXPFLAG_UNIX spio_t_error_code SPCDECL
                  SP_select PROTOARGS((int read_fds[], size_t *pnread_fds,
                                       int write_fds[], size_t *pnwrite_fds,
                                       SP_stream * read_streams[], size_t *pnread_streams,
                                       SP_stream * write_streams[], size_t *pnwrite_streams,
                                       spio_t_timespec *timeout,
                                       spio_t_bits options
                                       ));

extern SPEXP(139) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN SPEXPFLAG_WIN32 spio_t_error_code SPCDECL
                  SP_select PROTOARGS((spio_t_os_file_handle os_events[], size_t *pnos_events,
                                       SP_stream * read_streams[], size_t *pnread_streams,
                                       SP_stream * write_streams[], size_t *pnwrite_streams,
                                       spio_t_timespec *timeout,
                                       spio_t_bits options
                                       ));

extern SPEXP(140) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_open_socket_stream PROTOARGS((char const *nodename, char const *servname, void *args, SP_stream **pstream, spio_t_bits options));



/*----------------------------------------------------------------------*/

/* signal() replacement */
extern SPEXP(93) SP_SigFun* SPCDECL SP_signal PROTOTYPE((int, SP_SigFun *, ANYPOINTER));

/* [PM] WinCE tell SP that a signal happened. Not yet public. (change
   name to SP_raise_signal when going public) */ 
extern SPEXP(94) SPEXPFLAG_HIDDEN int SPCDECL sp_raise_signal PROTOARGS((int signo));

extern SPEXP(95) int SPCDECL SP_define_c_predicate PROTOARGS((char const *name, int arity, char const *module, SP_CPredFun *proc, void *stash));

/* [PM] See library/system.c for how to use this, if at all */
extern SPEXP(96) char** SPCDECL SP_copy_environ PROTOTYPE((void));
/*----------------------------------------------------------------------*/
/* Undocumented. The following definitions should be avoided if possible. */

extern SPEXP(97) SPEXPFLAG_HIDDEN SPEXPFLAG_UNAVAILABLE int SPCDECL SP_action PROTOTYPE((int, ANYPOINTER)); /* Perform some action */
extern SPEXP(98) SPEXPFLAG_HIDDEN SPEXPFLAG_UNAVAILABLE long SPCDECL SP_inquiry PROTOTYPE((int)); /* Get information from Prolog */
  /* extern SPEXP int SPCDECL SP_choice_offset PROTOTYPE((int)); */
  /* extern SPEXP void SPCDECL SP_put_choice_argument PROTOTYPE((int offset,int,SP_term_ref)); */

extern SPEXP(99) SPEXPFLAG_HIDDEN void SPCDECL SP_write_term PROTOTYPE((SP_term_ref));


/*----------------------------------------------------------------------*/
/* Misc */

extern SPEXP(100) SPEXPFLAG_UNAVAILABLE SPEXPFLAG_HIDDEN int SPCDECL SP_isatty PROTOTYPE((SP_stream *));
extern SPEXP(101) ANYPOINTER SPCDECL SP_malloc PROTOTYPE((size_t));
extern SPEXP(102) ANYPOINTER SPCDECL SP_realloc PROTOTYPE((ANYPOINTER, size_t));
extern SPEXP(103) void SPCDECL SP_free PROTOTYPE((ANYPOINTER));

/* [PM] 4.0 gone */
extern SPEXP(104) SPEXPFLAG_UNAVAILABLE void * SPCDECL SP_memmove PROTOARGS((void *dest, void *src, int n));

/*----------------------------------------------------------------------*/
/* Raising standard errors from C-code */

/* SP_save_error() followed by SP_raise_error() will raise a standard
   error exception according to the constants defined below.
*/

extern SPEXP(105) void SPCDECL SP_save_error PROTOARGS((int error_type, char const *error_msg_ienc, SP_term_ref culprit));
extern SPEXP(106) void SPCDECL SP_raise_error PROTOARGS((char const *name, int arity, int argn));
extern SPEXP(107) SPEXPFLAG_HIDDEN SPEXPFLAG_UNIX void SPCDECL sp_raise_errno_error PROTOARGS((int errno_value, char const *ienc_pred, char const *ienc_syscall_name));
extern SPEXP(107) SPEXPFLAG_HIDDEN SPEXPFLAG_WIN32 void SPCDECL sp_raise_win32_error PROTOARGS((int gle_value, char const *ienc_pred, char const *ienc_syscall_name));

extern SPEXP(108) int SPCDECL SP_atom_length PROTOTYPE((SP_atom));
extern SPEXP(188) int SPCDECL SP_put_list_n_bytes PROTOTYPE((SP_term_ref, SP_term_ref, size_t, unsigned char const *));
extern SPEXP(189) int SPCDECL SP_get_list_n_bytes PROTOTYPE((SP_term_ref, SP_term_ref, size_t, size_t *, unsigned char *));
extern SPEXP(109) int SPCDECL SP_put_list_n_codes PROTOTYPE((SP_term_ref, SP_term_ref, size_t, char const *));
extern SPEXP(110) int SPCDECL SP_get_list_n_codes PROTOTYPE((SP_term_ref, SP_term_ref, size_t, size_t *, char *));
extern SPEXP(111) int SPCDECL SP_register_atom PROTOTYPE((SP_atom));
extern SPEXP(112) int SPCDECL SP_unregister_atom PROTOTYPE((SP_atom));

extern SPEXP(113) int SPCDECL SP_cons_functor_array PROTOARGS((SP_term_ref term, SP_atom name, int arity, SP_term_ref *args));
extern SPEXP(114) int SPCDECL SP_restore PROTOTYPE((char const *));
/* extern SPEXP(115) int SPCDECL SP_chdir PROTOTYPE((char const *)); */
/* extern SPEXP(116) char * SPCDECL SP_getcwd PROTOTYPE((char *,unsigned int)); */
/* [PM] 4.0 path uses internal (UTF-8) encoding. */
extern SPEXP(115) spio_t_error_code SPCDECL SP_set_current_dir PROTOTYPE((char const *));
/* [PM] 4.0 Returned path uses internal (UTF-8) encoding and is slash terminated. Returns NULL on failure. */
extern SPEXP(116) char * SPCDECL SP_get_current_dir PROTOTYPE((void));
/* [PM] 4.0 expand relative path */
extern SPEXP(174) spio_t_error_code SPCDECL SP_expand_file_name PROTOARGS((char const *relpath, char const *cwd, spio_t_bits options, char **pabspath));


/* support for SP_on_fault */
extern SPEXP(117) void SPCDECL SP_raise_fault PROTOARGS((char const *message_ienc));
/* Should not be hidded but is not documented */
extern SPEXP(118) void SPCDECL sp_set_abort_env PROTOTYPE((ANYPOINTER));
/* Should not be hidded but is not documented */
extern SPEXP(119) char const * SPCDECL sp_get_abort_err_ienc PROTOTYPE((void));

extern SPEXP(120) SPEXPFLAG_HIDDEN char const * SPCDECL sp_get_boot_path PROTOTYPE((void));
  /* extern SPEXP ANYPOINTER SPCDECL sp_get_self PROTOTYPE((void)); */
  /* extern SPEXP void SPCDECL sp_sigemptyset PROTOTYPE((void)); */

/* Returns a pointer to the version string */
extern SPEXP(121) char const * SPCDECL SP_get_emulator_version PROTOTYPE((void));
/* [PM] 3.9b4 Returns a pointer to the version specific path component, e.g., sicstus-3.9.0beta4 */
extern SPEXP(122) char const * SPCDECL SP_get_emulator_dir PROTOTYPE((void));

extern SPEXP(124) SPEXPFLAG_PREINIT SPEXPFLAG_HIDDEN SPEXPFLAG_OLDHOOKS SP_SetWindowTitleHook * SPCDECL SP_set_set_window_title_hook PROTOTYPE((SP_SetWindowTitleHook *, void *));
extern SPEXP(125) SPEXPFLAG_PREINIT SPEXPFLAG_HIDDEN SPEXPFLAG_OLDHOOKS SP_GetWindowTitleHook * SPCDECL SP_set_get_window_title_hook PROTOTYPE((SP_GetWindowTitleHook *, void *));

  /* Internal API functions */
extern SPEXP(126) SPEXPFLAG_HIDDEN ANYPOINTER SPCDECL sp_get_engine_global PROTOTYPE((void));
extern SPEXP(127) SPEXPFLAG_HIDDEN void SPCDECL sp_reset_ctrlc_pending PROTOTYPE((void));
extern SPEXP(128) SPEXPFLAG_HIDDEN int SPCDECL sp_get_ctrlc_pending PROTOTYPE((void));

extern SPEXP(129) SPEXPFLAG_HIDDEN int SPCDECL sp_qload_or_restore PROTOTYPE((char const *,int));
extern SPEXP(130) SPEXPFLAG_HIDDEN void SPCDECL sp_variable_to_string PROTOTYPE((SP_term_ref,char *));
extern SPEXP(131) SPEXPFLAG_HIDDEN int SPCDECL sp_get_mem_usage PROTOTYPE((void));

/* [PM] 4.0 sp_get_classpath() now returns a SP_malloc'd path, caller should SP_free it */
extern SPEXP(132) SPEXPFLAG_HIDDEN char * SPCDECL sp_get_classpath PROTOTYPE((void));
/* [PM] 4.0 sp_get_failed_bootpath should be hidden but that does not work due to how sicstus.h is included in spld-generated glue */
extern SPEXP(133) char const * SPCDECL sp_get_failed_bootpath PROTOTYPE((void));

extern SPEXP(134) SPEXPFLAG_HIDDEN int SPCDECL sp_prolog_initialized PROTOTYPE((void));
extern SPEXP(135) SPEXPFLAG_HIDDEN int SPCDECL sp_is_development_system PROTOTYPE((void));

extern SPEXP(136) void * SPCDECL SP_calloc PROTOTYPE((size_t, size_t));
extern SPEXP(137) char * SPCDECL SP_strdup PROTOTYPE((char const *));


  /* WCI (alias Ienc) utilities for use in the foreign code */
extern SPEXP(141) SPEXPFLAG_HIDDEN char const * SPCDECL SP_to_os PROTOARGS((char const *ienc, int context));
extern SPEXP(142) SPEXPFLAG_HIDDEN char const * SPCDECL SP_from_os PROTOARGS((char const *senc, int context));

extern SPEXP(143) int SPCDECL SP_wci_code PROTOARGS((int *pcode, char const *wci));

  /* SP_wci_code() determines the number of bytes that comprise the
     internally encoded character pointed to by wci. Also, if pcode is not
     a null pointer, SP_wci_code() converts the internally encoded
     character to a wide character code and places the result in the object
     pointed to by pcode. (The value of the wide character corresponding to
     the NUL character is zero.)  At most WCI_MAX_BYTES bytes will be
     examined, starting at the byte pointed to by wci, but note that no
     bytes following a NUL character will be examined.

     If wci is a null pointer, SP_wci_code() simply returns 0.  If wci is
     not a null pointer, then, if wci points to the NUL character,
     SP_wci_code() returns 0; if the next bytes form a valid internally
     encoded character, SP_wci_code() returns the number of bytes that
     comprise the converted internally encoded character; otherwise, wci
     does not point to a valid internally encoded character and
     SP_wci_code() returns the negated length of the invalid byte sequence.
     This latter case will not happen, if wci points to the beginning of a
     Prolog atom string, or to a position within such a string reached by
     repeated addition of lengths (SP_wci_len) of the preceding encoded
     wide characters */


extern SPEXP(144) int SPCDECL SP_wci_len PROTOARGS((char const *wci));
  /*
     SP_wci_len() determines the number of bytes comprising the multi-
     byte character pointed to by wci.  It is equivalent to

          SP_wci_code(NULL, wci);
  */


extern SPEXP(145) int SPCDECL SP_code_wci PROTOARGS((char *wci, int code));

  /*
     SP_code_wci() determines the number of bytes needed to  represent
     the  internally encoded  character  corresponding  to  the code whose
     value is code, and, if wci is not a null pointer, stores  the
     internally encoded  character  representation in the array pointed to
     by wci.  At most WCI_MAX_BYTES bytes are stored.

     If wci is a null pointer, SP_code_wci() simply returns 0.  If wci  is
     not  a  null  pointer,  SP_code_wci()  returns -1 if the value of
     code does not correspond to a  valid  internally encoded  character;
     otherwise  it  returns the number of bytes that comprise the
     internally encoded character corresponding to the value of code.
  */

  /* A utility that may be useful in wcx_chartype implementation */

/* [PM] 4.0.1 was visible in 4.0.0 by mistake */
extern SPEXP(146) SPEXPFLAG_HIDDEN int SPCDECL SP_latin1_chartype PROTOARGS((int char_code));

/* extern SPEXP(175) SPEXPFLAG_SPIO SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_encode_chars PROTOARGS((void const *from_buf, size_t *psize_from, spio_t_charset_enum from_charset, void *to_buf, size_t *psize_to, spio_t_charset_enum to_charset, void *args, spio_t_bits options)); */


/*
  [PM] 3.9b4 Support for loading multiple SICStus run-times into the same process.
 */

/* 
   Mutexes are typically initialized by static initialization to 0
   We could have defined :
   int SP_init_mutex(SP_mutex *pstore)
   {
      (*pstore) = 0;
      return 1;
   }
*/
extern SPEXP(147) int SPCDECL SP_mutex_lock PROTOARGS((SP_mutex *pmutex));
extern SPEXP(148) int SPCDECL SP_mutex_unlock PROTOARGS((SP_mutex *pmutex));
extern SPEXP(149) int SPCDECL SP_mutex_destroy PROTOARGS((SP_mutex *pmutex));
extern SPEXP(150) int SPCDECL SP_load_sicstus_run_time PROTOARGS((SP_get_dispatch_type **ppfunc, void **phandle));

extern SPEXP(151) SPEXPFLAG_HIDDEN int SPCDECL SP_set_multi_sp_state PROTOTYPE((struct SP_multi_sp_state *));


/* [PM] Mar 2000 External Object Functions.

   THE EXTERNAL OBJECTS API WILL CHANGE BEFORE SICSTUS 3.9 FINAL!!

   The external object routines provides a mechanism for encoding
   pointers to data external to the prolog system in such a way that a
   finalizer will be run when the data can no longer be referenced by
   prolog code. The major trick is to encode the pointer as an atom
   and to use the atom GC to detect when the atom is no longer
   referenced by prolog.

*/

/* Use this to put an external object into a term ref */
extern SPEXP(152) int SPCDECL SP_put_external_object PROTOARGS((SP_term_ref tObj, SP_external_object_link *pObj));

/* Use this to get an external object into a term ref */
extern SPEXP(153) int SPCDECL SP_get_external_object PROTOARGS((SP_term_ref tObj, SP_external_object_link **ppObj));


/* For the simple cases use this to put by wrapping in unary compound
   with FUNCTOR as constructor. */
extern SPEXP(154) int SPCDECL SP_external_object_default_putter_helper PROTOARGS((SP_term_ref tr, SP_external_object_link *obj, unsigned long functor));

/* For complex cases use this to put the object pointer as the first
   arg of the compound term. See external_object_default_putter_helper 
   for how to use this.  */
extern SPEXP(155) int SPCDECL SP_put_external_object_link PROTOARGS((SP_term_ref tObj, SP_external_object_link *pObj));

/* Put a term representation of the link that will not prevent garbage collection. */
extern SPEXP(156) int SPCDECL SP_put_weak_external_object_link PROTOARGS((SP_term_ref tObj, SP_external_object_link *pObj));

/* Register a new type of external object */
extern SPEXP(157) SP_external_object_type SPCDECL SP_register_external_object_type PROTOARGS((SP_external_object_finalizer finalizer, SP_external_object_putter putter, void *type_data));
/* wrap some kind of pointer as an external object of a certain type */
extern SPEXP(158) SP_external_object_link * SPCDECL SP_register_external_object PROTOARGS((void* data,SP_external_object_type object_type));

extern SPEXP(159) void SPCDECL SP_unlink_external_object PROTOARGS((SP_external_object_link *pObj, int finalize));
extern SPEXP(160) void * SPCDECL SP_get_external_object_data PROTOTYPE((SP_external_object_link *));
extern SPEXP(161) SP_external_object_type SPCDECL SP_get_external_object_type PROTOTYPE((SP_external_object_link *));
extern SPEXP(162) void SPCDECL SP_finalize_external_object PROTOARGS((SP_term_ref tObj, long *existed));
extern SPEXP(163) long SPCDECL SP_garbage_collect_external_objects PROTOARGS((SP_external_object_type object_type));

/* [PM] 3.9b5 These are not public, they are here so that winmain.c and tkterm.c can use them */
extern SPEXP(164) SPEXPFLAG_HIDDEN void SPCDECL SP_ctrlc_action PROTOTYPE((void));
extern SPEXP(165) SPEXPFLAG_HIDDEN void SPCDECL SP_ctrlbreak_action PROTOTYPE((void));

extern SPEXP(166) SPEXPFLAG_HIDDEN int SPCDECL sp_set_jasper_magic PROTOTYPE((void*, int));
extern SPEXP(167) SPEXPFLAG_HIDDEN void* SPCDECL sp_get_jasper_magic PROTOTYPE((int));
extern SPEXP(168) SPEXPFLAG_HIDDEN int SPCDECL sp_set_jasper_threadservermode PROTOTYPE((int, int));
extern SPEXP(169) SPEXPFLAG_HIDDEN int SPCDECL sp_get_jasper_threadservermode PROTOTYPE((int*, int));

/* [PM] 3.9.1 get and put arbitrary sized integers (bignums) */
extern SPEXP(170) int SPCDECL SP_get_integer_bytes PROTOARGS((SP_term_ref tr, void *buf, size_t *pbuf_size, int native));
extern SPEXP(171) int SPCDECL SP_put_integer_bytes PROTOARGS((SP_term_ref tr, void *buf, size_t buf_size, int native));

/* [PM] WinCE sane interface to _environ. Also works on WinCE that
   does not have environment variables.

   SP run-time should use sp_setenv_internal and sp_getenv_internal
   instead.
*/
/* key and value are encoded string (UTF-8) */
extern SPEXP(172) int SPCDECL SP_setenv PROTOARGS((char const *key_ienc, char const *value_ienc));
/* key and returned value are encoded strings (UTF-8). Returned value is SP_malloc-ed and should be SP_free-ed. */
extern SPEXP(173) char* SPCDECL SP_getenv PROTOARGS((char const *key_ienc));

/* These (sp_spio_...) should not be used except as part of SP_ASSERT et al. */
extern SPEXP(176) int SPCDECL sp_spio_debug_break PROTOARGS((char const *file, int line));
extern SPEXP(177) char const * SPCDECL sp_spio_error_name PROTOARGS((spio_t_error_code code));
extern SPEXP(178) int SPCDECL sp_spio_trace_line PROTOARGS((char const *function, char const *file, int line, char const *msg, long l, char const *string, int level));
extern SPEXP(175) int SPCDECL sp_spio_assert_failure PROTOARGS((char const *function, char const *file, int line));


/* Character encodings (not yet public) */
extern SPEXP(179) SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_get_encoding PROTOARGS((char const *encoding_name, spio_t_encoding **pencoding, spio_t_bits options));
extern SPEXP(180) SPEXPFLAG_HIDDEN void SPCDECL SP_encoding_release PROTOARGS((spio_t_encoding *encoding));
extern SPEXP(181) SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_encoding_open PROTOARGS((spio_t_encoding *encoding,
                                                                        spio_t_encoding_state **pencoding_state,
                                                                        void *args,
                                                                        spio_t_bits options));

extern SPEXP(182) SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_encode_from_codes PROTOARGS((spio_t_encoding *encoding,
                                                                            spio_t_wchar const *src, size_t src_size,
                                                                            spio_t_encoding_state **pencoding_state,
                                                                            spio_t_byte *dst, size_t dst_size,
                                                                            size_t *psrc_size_read,
                                                                            size_t *pdst_size_wrote,
                                                                            spio_t_bits options));

extern SPEXP(183) SPEXPFLAG_HIDDEN spio_t_error_code SPCDECL SP_encode_to_codes PROTOARGS((spio_t_encoding *encoding,
                                                                          spio_t_byte const *src, size_t src_size,
                                                                          spio_t_encoding_state **pencoding_state,
                                                                          spio_t_wchar *dst, size_t dst_size,
                                                                          size_t *psrc_size_read,
                                                                          size_t *pdst_size_wrote,
                                                                          spio_t_bits options));



/* [PM] 4.0 Same as MEMORY_FAULT(msg, culprit). Internal for use by LAZY_MEMORY_FAULT and LAZY_NULL_CHECK */
extern SPEXP(185) void SPCDECL sp_memory_fault PROTOARGS((char const *msg, int culprit));

extern SPEXP(186) int SPCDECL SP_decode_reserved_exception PROTOARGS((SP_term_ref tr, long *pdata));

extern SPEXP(187) spio_t_error_code SPCDECL SP_install_idle_hook PROTOARGS((SP_idle_hook *hook, void *cookie, spio_t_bits options));



/* Largest used SPEXP number: 189 */
  
/* End of external function declarations */
#endif  /* ! SPDLL */

/* [PM] *NOTHING* below the PtrSPFuncs #if-#else-#endif */

#endif  /* (!SP_DISPATCH_API) 3.8 API */

SP_END_DECL

#endif /* INCLUDED_SICSTUS_H */
