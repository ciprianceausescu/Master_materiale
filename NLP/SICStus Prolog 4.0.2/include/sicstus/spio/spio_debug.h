#ifndef SPIO_DEBUG_H_INCLUDED
#define SPIO_DEBUG_H_INCLUDED
#include "spio_types.h"

enum spio_t_assertions_level {
  SPIO_ASSERTION_DISABLE = 0,
  SPIO_ASSERTION_REPORT,        /* report assertion violation but continue */
  SPIO_ASSERTION_DEBUG,         /* report and break into debugger. */
  SPIO_ASSERTION_ABORT          /* report and abort() (dump core) */
};

extern enum spio_t_assertions_level spio_assertions;

extern int spio_assert_failure(char const *function, char const *file, int line);


extern int spio_trace_line(char const *function, char const *file, int line, char const *msg, long l, char const *string, int level);
extern int spio_trace_line_p(char const *function, char const *file, int line, char const *msg, void *p, char const *string, int level);
extern int spio_trace_line_lls(char const *function, char const *file, int line, char const *msg, long l1, long l2, char const *string, int level);

#define SPIO_DEBUG_BREAK_FUNCTION_NEEDED 1
extern int spio_debug_break(char const *file, int line);

#if SPIO_DEBUG


#if 0
  #ifndef SPIO_HAVE___FUNCTION__
  #define SPIO_HAVE___FUNCTION__ 1
  #endif  /* SPIO_HAVE___FUNCTION__ */
#endif

#ifndef SPIO_DEBUG_BREAK_
#define SPIO_DEBUG_BREAK_ spio_debug_break
#endif  /* SPIO_DEBUG_BREAK_ */
#ifndef SPIO_ERROR_NAME_
#define SPIO_ERROR_NAME_ spio_error_name
#endif  /* SPIO_ERROR_NAME_ */
#ifndef SPIO_TRACE_LINE_
#define SPIO_TRACE_LINE_ spio_trace_line
#endif  /* SPIO_TRACE_LINE_ */

#define SPIO_COMPILETIME_ASSERT(TEST)  do { int spio_compiletime_assert[ (TEST) ? 1 : -1 ] = { 1 }; (void)spio_compiletime_assert; } while(0)

#define SPIO_COMPILETIME_ASSERT_DECLARATION(TEST) extern int spio_compiletime_assert[ (TEST) ? 1 : -1 ]


#if defined(_MSC_VER) && SPIO_INCLUDE_OS_TYPES
#define SPIO_DEBUG_BREAK_FL(F,L) do{(void)(L); (void)(F); DebugBreak();}while(0)
#else  /* !_MSC_VER */
#define SPIO_DEBUG_BREAK_FL(F,L) SPIO_DEBUG_BREAK_((F), (L))
#endif /* !_MSC_VER */

#ifndef SPIO_TRACE_LEVEL
#define SPIO_TRACE_LEVEL 1
#endif  /* SPIO_TRACE_LEVEL */

#if 1
#define SPIO_TRACE_LINE_ls(MSG, LONG, STRING) SPIO_TRACE_LINE_(SPIO__FUNCTION__, __FILE__,__LINE__, (MSG), (long)(LONG), (STRING), SPIO_TRACE_LEVEL)
#else
#if SPIO_HAVE___FUNCTION__
#define SPIO_TRACE_LINE_ls(MSG, LONG, STRING) SPIO_TRACE_LINE_(__FUNCTION__, __FILE__,__LINE__, (MSG), (long)(LONG), (STRING), SPIO_TRACE_LEVEL)
#else  /* !SPIO_HAVE___FUNCTION__ */
#define SPIO_TRACE_LINE_ls(MSG, LONG, STRING) SPIO_TRACE_LINE_(NULL, __FILE__,__LINE__, (MSG), (long)(LONG), (STRING), SPIO_TRACE_LEVEL)
#endif  /* !SPIO_HAVE___FUNCTION__ */
#endif

#if 1
#define SPIO_TRACE_LINE_lls(MSG, LONG1, LONG2, STRING) spio_trace_line_lls(SPIO__FUNCTION__, __FILE__,__LINE__, (MSG), (long)(LONG1), (long)(LONG2), (STRING), SPIO_TRACE_LEVEL)

#else
#if SPIO_HAVE___FUNCTION__
#define SPIO_TRACE_LINE_lls(MSG, LONG1, LONG2, STRING) spio_trace_line_lls(__FUNCTION__, __FILE__,__LINE__, (MSG), (long)(LONG1), (long)(LONG2), (STRING), SPIO_TRACE_LEVEL)
#else  /* !SPIO_HAVE___FUNCTION__ */
#define SPIO_TRACE_LINE_lls(MSG, LONG1, LONG2, STRING) spio_trace_line_lls(NULL,         __FILE__,__LINE__, (MSG), (long)(LONG1), (long)(LONG2), (STRING), SPIO_TRACE_LEVEL)
#endif  /* !SPIO_HAVE___FUNCTION__ */
#endif

#if 1
#define SPIO_TRACE_LINE_l(MSG, LONG) SPIO_TRACE_LINE_(SPIO__FUNCTION__, __FILE__,__LINE__, (MSG), (long)(LONG), NULL, SPIO_TRACE_LEVEL)

#else
#if 1
#define SPIO_TRACE_LINE_l(MSG, LONG) SPIO_TRACE_LINE_(SPIO__FUNCTION__, __FILE__,__LINE__, (MSG), (long)(LONG), NULL, SPIO_TRACE_LEVEL)
#else
#if SPIO_HAVE___FUNCTION__
#define SPIO_TRACE_LINE_l(MSG, LONG) SPIO_TRACE_LINE_(__FUNCTION__, __FILE__,__LINE__, (MSG), (long)(LONG), NULL, SPIO_TRACE_LEVEL)
#else  /* !SPIO_HAVE___FUNCTION__ */
#define SPIO_TRACE_LINE_l(MSG, LONG) SPIO_TRACE_LINE_(        NULL, __FILE__,__LINE__, (MSG), (long)(LONG), NULL, SPIO_TRACE_LEVEL)
#endif  /* !SPIO_HAVE___FUNCTION__ */
#endif
#endif

#if 1
#define SPIO_TRACE_LINE_p(MSG, PTR) spio_trace_line_p(SPIO__FUNCTION__, __FILE__,__LINE__, (MSG), (void*)(PTR), NULL, SPIO_TRACE_LEVEL)

#else
#if 1
#define SPIO_TRACE_LINE_p(MSG, PTR) spio_trace_line_p(SPIO__FUNCTION__, __FILE__,__LINE__, (MSG), (void*)(PTR), NULL, SPIO_TRACE_LEVEL)
#else
#if SPIO_HAVE___FUNCTION__
#define SPIO_TRACE_LINE_p(MSG, PTR) spio_trace_line_p(__FUNCTION__, __FILE__,__LINE__, (MSG), (void*)(PTR), NULL, SPIO_TRACE_LEVEL)
#else  /* !SPIO_HAVE___FUNCTION__ */
#define SPIO_TRACE_LINE_p(MSG, PTR) spio_trace_line_p(        NULL, __FILE__,__LINE__, (MSG), (void*)(PTR), NULL, SPIO_TRACE_LEVEL)
#endif  /* !SPIO_HAVE___FUNCTION__ */
#endif
#endif

#else  /* !SPIO_DEBUG */
#define SPIO_COMPILETIME_ASSERT(TEST) /* empty */
#define SPIO_COMPILETIME_ASSERT_DECLARATION(TEST) extern int spio_compiletime_assert[] /* not empty but harmless */


#define SPIO_TRACE_LINE_l(MSG, LONG) do {;}while(0)
#define SPIO_TRACE_LINE_p(MSG, PTR) do {;}while(0)
#define SPIO_TRACE_LINE_ls(MSG, LONG, STRING) do {;}while(0)
#define SPIO_TRACE_LINE_lls(MSG, LONG1, LONG2, STRING) do {;}while(0)

#define SPIO_DEBUG_BREAK_FL(F,L) do {;}while(0)


#endif  /* !SPIO_DEBUG */

#define SPIO_DEBUG_BREAK() SPIO_DEBUG_BREAK_FL(__FILE__, __LINE__)

#if SPIO_DEBUG
#ifndef SPIO_ASSERTIONS
#define SPIO_ASSERTIONS 1
#endif  /* !SPIO_ASSERTIONS */
#endif  /* SPIO_DEBUG */

/* MSVC complains about "C4555 ... expression has no effect" if this expands to empty when !SPIO_DEBUG*/
/* gcc complains about "warning: statement with no effect" if this expands to empty when SPIO_DEBUG */
#define SPIO_ASSERT_CAST_TO_VOID_ (void)

#if SPIO_ASSERTIONS

#ifndef SPIO_ASSERT_FAILURE_
#define SPIO_ASSERT_FAILURE_ spio_assert_failure
#endif  /* SPIO_ASSERT_FAILURE_ */

#define SPIO_ASSERT2(ENABLE,TEST)  do { (SPIO_ASSERT_CAST_TO_VOID_ ( (ENABLE) ? ( SPIO_UNLIKELY(!(TEST)) ? SPIO_ASSERT_FAILURE_(SPIO__FUNCTION__, __FILE__,__LINE__) : 1 ) : 1 )); SPIO_ASSUME((TEST)); } while(0)
#else  /* !SPIO_ASSERTIONS */
#define SPIO_ASSERT2(ENABLE,TEST)    (SPIO_ASSERT_CAST_TO_VOID_  1)

#endif  /* !SPIO_ASSERTIONS */

#ifndef SPIO_ASSUME
#if _MSC_VER >= 1400
#ifdef _PREFAST_                /* [PM] static analysis with cl.exe /analyze */
#include <CodeAnalysis/sourceannotations.h>
#define SPIO_ASSUME(CONDITION) __analysis_assume((CONDITION))
#else  /* !_PREFAST_ */
#if 0                           /* FIXME: consider enabling this. It could help speed things up. */
#define SPIO_ASSUME(CONDITION) __assume((CONDITION))
#endif  /* 0 */
#endif  /* !_PREFAST_ */
#endif /*_MSC_VER >= 1400 */

#endif  /* !SPIO_ASSUME */

#ifndef SPIO_ASSUME
#define SPIO_ASSUME(CONDITION)  /* empty */
#endif  /* !SPIO_ASSUME */

/* Both SPIO_ASSERT and SPIO_ASSERT1 (and thus SP_ASSERT) tells VC8 PreFast to assume the TEST is true. */
#define SPIO_ASSERT1(ENABLE,TEST) do { SPIO_ASSERT2((ENABLE), (TEST)); SPIO_ASSUME((TEST)); }while(0)

#ifndef SPIO_SOFT_ASSERTIONS
#if SPIO_DEBUG
#define SPIO_SOFT_ASSERTIONS 1
#endif  /* SPIO_DEBUG */
#endif  /* SPIO_SOFT_ASSERTIONS */

#if SPIO_SOFT_ASSERTIONS
/* An assertion for unexpected conditions that are not errors. Soft
   assertions should therefore always be disabled in non-debug code
   (whereas ordinary assertions could (and perhaps should) be enabled
   where it not for their speed impact)

   Also, soft assertions should NEVER let a static analyzer (like
   PreFast) assume that the condition holds
 */
#define SPIO_SOFT_ASSERT1(ENABLE,TEST) SPIO_ASSERT2((ENABLE), (TEST))
#else  /* !SPIO_SOFT_ASSERTIONS */
#define SPIO_SOFT_ASSERT1(ENABLE,TEST) /* empty */
#endif  /* !SPIO_SOFT_ASSERTIONS */

/* SPIO_ASSERT and SPIO_ASSERT1 become just SPIO_ASSUME(TEST) if
   !SPIO_ASSERTIONS

   Thus, eventually, we may use this to tell the compiler about what
   it can assume.
 */
#define SPIO_ASSERT(TEST)      SPIO_ASSERT1(spio_assertions!=SPIO_ASSERTION_DISABLE, (TEST))

/* SPIO_SOFT_ASSERT and SPIO_SOFT_ASSERT1 become no-ops if
   !SPIO_SOFT_ASSERTIONS (or if !SPIO_ASSERTIONS)
*/
#define SPIO_SOFT_ASSERT(TEST) SPIO_SOFT_ASSERT1(spio_assertions!=SPIO_ASSERTION_DISABLE, (TEST))

#if SPIO_DEBUG
#define SPIO_ALLOCATOR_DEBUG_ARGS_DECL , char const *file, int line
#define SPIO_ALLOCATOR_DEBUG_ARGS_PASS_ON , file, line
#define SPIO_ALLOCATOR_DEBUG_ARGS , __FILE__, __LINE__
#else  /* !SPIO_DEBUG */
#define SPIO_ALLOCATOR_DEBUG_ARGS_DECL /* empty */
#define SPIO_ALLOCATOR_DEBUG_ARGS_PASS_ON  /* empty */
#define SPIO_ALLOCATOR_DEBUG_ARGS /* empty */
#endif

/* Always executes (EXPR) and also asserts that it succeeds (not allowed to set 'code') */
#define SPIO_SUCCEEDS(EXPR) do {                        \
   spio_t_error_code spio_succeeds_tmp = (EXPR);        \
   SPIO_ASSERT(!SPIO_FAILED(spio_succeeds_tmp));        \
   (void)spio_succeeds_tmp;                             \
} while(0)

/* use this for testing for allowed cases of null s or *s */
#define SPIO_VALID(S) (((S) != NULL) && (*(S) != NULL))
/* use this more extensive test in assertions */
#define SPIO_VALIDATE(S) (SPIO_VALID(S) && ((*(S))->funcs != NULL))

#ifndef SPIO_BARF_HOOK
/* define SPIO_TRACKED_ERROR_CODE to, e.g. SPIO_E_INTERRUPTED to get a debug break when such an error happens anywhere */
#ifdef SPIO_TRACKED_ERROR_CODE
#define SPIO_TRACKED_CODE_ && code != SPIO_TRACKED_ERROR_CODE
#else  /* !SPIO_TRACKED_ERROR_CODE */
#define SPIO_TRACKED_CODE_      /* empty */
#endif /* !SPIO_TRACKED_ERROR_CODE */

#define SPIO_BARF_HOOK(CODE) if (code != SPIO_E_IMPOSSIBLE_ERROR && code != SPIO_E_INTERNAL_ERROR SPIO_TRACKED_CODE_) {} else { SPIO_DEBUG_BREAK(); }
#endif  /* SPIO_BARF_HOOK */

#define SPIO_BARF(CODE) do { code = (CODE); SPIO_TRACE_LINE_ls("barf", code, SPIO_ERROR_NAME_(code)); SPIO_BARF_HOOK(code); goto SPIO_BARF_LABEL; } while (0)
#define SPIO_CHECK(EXPR) if (SPIO_FAILED(code = (EXPR))) SPIO_BARF(code); else { /* empty */}

#define SPIO_NULL_CHECK(EXPR) if (SPIO_UNLIKELY((EXPR) == NULL)) { SPIO_BARF(SPIO_E_OUT_OF_MEMORY); } else { /* empty */}

#if SPIO_UNIX
/* Most POSIX functions return -1 on failure */
#define SPIO_POSIX_CHECK1(EXPR, ERRNO, DEFAULT_ERROR) if (SPIO_UNLIKELY((EXPR) == -1)) { int posix_check_err_no = (ERRNO); SPIO_TRACE_LINE_l("errno==", (long)posix_check_err_no); SPIO_BARF(spio_map_unix_error(posix_check_err_no, (DEFAULT_ERROR))); } else { /* empty */}
#define SPIO_POSIX_CHECK(EXPR, DEFAULT_ERROR) SPIO_POSIX_CHECK1((EXPR), errno, (DEFAULT_ERROR))
#endif  /* SPIO_UNIX */

#if SPIO_WIN32
/* Most Win32 functions return 0/NULL on failure */
#define SPIO_WIN32_CHECK1(EXPR, ERRNO, DEFAULT_ERROR) if (SPIO_UNLIKELY((EXPR) == 0)) { DWORD win32_check_err_no = (ERRNO); SPIO_TRACE_LINE_l("GetLastError==", (long)win32_check_err_no); SPIO_BARF(spio_map_win32_error(win32_check_err_no, (DEFAULT_ERROR))); } else { /* empty */}
#define SPIO_WIN32_CHECK(EXPR, DEFAULT_ERROR) SPIO_WIN32_CHECK1((EXPR), GetLastError(), (DEFAULT_ERROR))
#endif  /* SPIO_WIN32_CHECK1 */

#define SPIO_ASSERT_MAIN_THREAD() SPIO_ASSERT(spio_is_in_main_thread() == SPIO_S_TRUE)


#if SPIO_WIN32

#if SPIO_INCLUDE_OS_TYPES

#define SPIO_BARF_LAST_ERROR(DEFAULT_ERROR) do { SPIO_TRACE_LINE_l("BARF GetLastError()==", GetLastError()); SPIO_BARF(spio_map_win32_error(GetLastError(), (DEFAULT_ERROR))); } while(0)

#define SPIO_HANDLE_CHECK(EXPR, DEFAULT_ERROR) if ((EXPR) == INVALID_HANDLE_VALUE) { SPIO_BARF_LAST_ERROR(DEFAULT_ERROR); } else { /* empty */}

#if 0
#define SPIO_CHECK_LAST_ERROR(DEFAULT_ERROR) if (GetLastError() != NO_ERROR) { SPIO_BARF(spio_map_win32_error(GetLastError(), (DEFAULT_ERROR))); } else { /* empty */}
#endif  /* 0 */

#endif  /* SPIO_INCLUDE_OS_TYPES */

#endif  /* SPIO_WIN32 */

#define SPIO_DEBUG_DUMP_BACKTRACE_OPTION_STDOUT SPIO_BIT(0)
#define SPIO_DEBUG_DUMP_BACKTRACE_OPTION_STDERR SPIO_NEXT_BIT(SPIO_DEBUG_DUMP_BACKTRACE_OPTION_STDOUT)

extern void spio_debug_dump_backtrace(char const *title, spio_t_bits options);

extern spio_t_error_code spio_is_in_main_thread(void);

extern spio_t_error_code spio_init_debug(spio_t_bits options);



#endif  /* SPIO_DEBUG_H_INCLUDED */
