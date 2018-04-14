#ifndef INCLUDED_DBAUX_H
#define INCLUDED_DBAUX_H
#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */

#include <db.h>
#include <time.h>
#include <sicstus/sicstus.h>
#include <sicstus/config.h>
#if SP_WIN32
#include <stdlib.h>

#if SICSTUS_TODO
#error "FIXME BDB: PATH_MAX et al are evil!"
#endif  /* SICSTUS_TODO */
#define PATH_MAX _MAX_PATH

struct tms {
  clock_t tms_utime;		/* user time */
  clock_t tms_stime;		/* system time */
  clock_t tms_cutime;		/* user time, children */
  clock_t tms_cstime;		/* system time, children */
};

#else /* SP_WIN32 */
#include <limits.h>             /* PATH_MAX */
#endif /* SP_WIN32 */


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#if 1				/* [PD] BDB version 3.2.9 */
extern u_int32_t identity(DB *dummy, const void *key, u_int32_t size);
#else
extern u_int32_t identity(const void *key, u_int32_t size);
#endif

extern int directory_exists(char const *path); /* form library/db/file.c */
extern int file_exists(char const *path);
extern int files_exist(char filename[][PATH_MAX+1], int n);
#if SP_WIN32
extern int times(struct tms *buffer);  
#endif /* SP_WIN32 */

/* [PM] 3.9 There is no documented mkdir on Windows */
extern int bdb_mkdir(char const *path, int mode);

#if SP_WIN32
#define BACKSLASH_PATHS 1
#endif

#if !BACKSLASH_PATHS 
#define Native_path_to_os(P) SP_to_os((P), WCX_FILE)
#define Native_path(P)       (P)
#else
#define Native_path_to_os(P) SP_to_os(db_native_path((P)), WCX_FILE)
#define Native_path(P)       db_native_path((P))
extern char *db_native_path(char const *);
#endif

extern int db_relative_file_name(char const *);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* INCLUDED_DBAUX_H */
