/* Copyright (C) 1995, Swedish Institute of Computer Science. */
#define SICSTUS_HIDDEN_API 1

#include <sicstus/config.h>
#include <sicstus/sicstus.h>


#if SP_WIN32
#include <winsock2.h>           /* instead of windows.h */
#endif  /* SP_WIN32 */

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#if HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <stdlib.h>
#include <string.h>

#if HAVE_TIME_H
#include <time.h>
#endif  /* HAVE_TIME_H */

#include <stdio.h>

#if HAVE_ERRNO_H
#include <errno.h>
#endif

#if WINCE
#define DEFINE_WINCE_REPLACEMENTS 1

/* sp_set_errno is used by wince.h when DEFINE_WINCE_REPLACEMENTS is true */
#define sp_set_errno(X)         /* not exported from sicstus.h */

#define WINCE_REPLACEMENT_LINKAGE static
#include "wince.h"

#endif /* WINCE */


#if SICSTUS_TODO
#error "[PM] 4.0 All of this should move into the kernel"
#endif  /* SICSTUS_TODO */


/*------------------------------------------------------------------*/
/* Exported functions */
#include "system_glue.h"

/*------------------------------------------------------------------*/
void SPCDECL sp_time(SPAPI_ARG_PROTO_DECL
                     SP_term_ref twhen)
{
#if SICSTUS_TODO
#error "use year 2038-safe version if available"
#endif  /* SICSTUS_TODO */

    time_t t = time((time_t *) 0);
    int res;

    /* time_t > long on future safe 32-bit platforms (e.g. Win32) */
    res = SP_put_integer_bytes(twhen, &t, sizeof t, 1 /* native */);
    SP_ASSERT(res != 0);        /* do not expect failure */
    (void)res;
}

/*------------------------------------------------------------------*/
long SPCDECL sp_datime(SPAPI_ARG_PROTO_DECL
                       SP_term_ref tclock, long *year, long *month, long *day, long *hour, long *min, long *sec)
{
  time_t clock;
  struct tm tm_storage;
  struct tm *tm;
  size_t clock_size = sizeof clock;
  SPAPI_ARG_IGNORE;

  if (!SP_get_integer_bytes(tclock, &clock, &clock_size, 1 /* native */))
    {
      return -1;
    }

#if SICSTUS_TODO
#error "use year 2038-safe version if available"
#endif  /* SICSTUS_TODO */

#if SP_WIN32
#if __STDC_SECURE_LIB__ 
  tm = &tm_storage;
  if (localtime_s(tm, &clock) != 0) /* Does not agree with ISO TR 24731 (duh!) */
    {
      tm = NULL;
    }
#else  /* __STDC_SECURE_LIB__ */
  (void)tm_storage;
  tm = localtime(&clock);       /* thread safe in Win32 */
#endif  /* __STDC_SECURE_LIB__ */
#else  /* !SP_WIN32 */
  /* [PM] 3.9.0 consider using localtime64[_r] on Tru64 (consider using localtime_s when the standard stabilizes */
  tm = localtime_r(&clock, &tm_storage);
#endif  /* !SP_WIN32 */

  if (tm == NULL)
    {
      return -2;
    }
  *year  = tm->tm_year + 1900;
  *month = tm->tm_mon  +    1;
  *day   = tm->tm_mday;
  *hour  = tm->tm_hour;
  *min   = tm->tm_min;
  *sec   = tm->tm_sec;
  return 0;
}

void SPCDECL sp_mktime(SPAPI_ARG_PROTO_DECL
                       long year, long month, long day, long hour, long min, long sec, SP_term_ref tclock)
{
  int res;
  time_t clock;
  struct tm tm;

  tm.tm_year = year - 1900;
  tm.tm_mon  = month - 1;
  tm.tm_mday = day;
  tm.tm_hour = hour;
  tm.tm_min  = min;
  tm.tm_sec  = sec;
  tm.tm_isdst = -1;             /* "attempt to determine whether Daylight Savings Time is in effect" */

  /* [PM] 3.9.0 consider using localtime64[_r] on Tru64 */
  clock = mktime(&tm);
  /* [PM] 4.0 time_t > long on future safe platforms */
  res = SP_put_integer_bytes(tclock, &clock, sizeof clock, 1 /* native */);
  SP_ASSERT(res != 0);          /* do not expect failure */
}

/*------------------------------------------------------------------*/
/* [PM] WinCE use the new generic SP_getenv API */
long SPCDECL sp_getenv1(SPAPI_ARG_PROTO_DECL
                        char SP_FLI_CONST *variable, SP_term_ref tvalue)
{
  char *value;
  int rc;

  value = SP_getenv(variable);  /* SP_malloced */
  if (value==NULL) return -2;         /* environment variable not found */

  rc = SP_put_string(tvalue, value);   /* zero on failure */
  SP_free(value);
  return (rc ? 0 : -1);         /* zero on success */
}

/* [PM] WinCE Use the new generic SP_getenv API */
long SPCDECL sp_setenv(SPAPI_ARG_PROTO_DECL
                       char SP_FLI_CONST *variable, char SP_FLI_CONST *value)
{
  return SP_setenv(variable, value);
}

/*------------------------------------------------------------------*/

long SPCDECL sp_environ(SPAPI_ARG_PROTO_DECL
                        SP_term_ref t)
{
  SP_term_ref tKey = SP_new_term_ref();
  SP_term_ref tVal = SP_new_term_ref();
  SP_term_ref tPair = SP_new_term_ref();
  char **env = SP_copy_environ(); /* [PM] 4.0 Now documened as ienc'd pairs */
  char **pp;
  long rc;
  size_t npairs;
  SPAPI_ARG_IGNORE;

  rc = 0;
  if (!env) goto cleanup;

  for (pp = env, npairs=0; *pp; pp++)
    {
      npairs++;                 /* npairs is number of non-NULL pairs (i.e., not the last entry) */
    }
  /* traverse backwards so the created list is in order */
  if (npairs > 0)
    {
      SP_atom atom_equal;
      size_t i = npairs-1;

      atom_equal = SP_atom_from_string("=");
      if (atom_equal == 0) goto cleanup; /* should not happen */

      do
        {
          char *pair;
          char *key;
          char *value;
          char *delim;


          pair = env[i];
          delim = strchr(pair, '=');
      
          *delim = '\0';
          key = pair;
          value = delim+1;
      
          if (!(SP_put_string(tKey, key)
                && SP_put_string(tVal, value)
                && SP_cons_functor(tPair, atom_equal, 2, tKey, tVal)
                && SP_cons_list(t, tPair, t)))
            {
              rc = -1;
              goto cleanup;
            }
        }
      while (i-- > 0);
    }
 cleanup:
  if (env) SP_free(env);
  return rc;
}

