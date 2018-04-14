#include "dbaux.h"

#include <ctype.h>

#include <string.h>
#include <sys/types.h>          /* [PM] 3.11.2 needed for _stat on Win32 says docs */
#include <sys/stat.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if SP_WIN32
#include <direct.h>             /* _mkdir */
#undef stat
#define stat _stat              /* [PM] 3.11.2 Win32 uses _stat() and struct _stat */
#endif  /* SP_WIN32 */

#if SICSTUS_TODO
#error "[PM] 4.0 FIXME BDB: use SICStus/SPIO OS functions instead of home-brewed lossage"
#endif  /* SICSTUS_TODO */

/* [PM] (This used to mention SPRM 4917 but that PRM is not about BDB at all)

   Using a custom hash function is bad because it makes some of the
   standard tools (e.g., db_stat) unusable and makes it harder to
   access the data base tables directly through the various Berkeley
   DB APIs.

   Using the identity hash function below has the following additional problems:
   . Assumes size >= 4. There is no guarantees that this will be
     true. BDB may call the hash function for its own purposes (and
     does, although with a key longer than 4 bytes).
   . Assumes the key is suitably aligned so that reading a 4byte word
     will not trap. There is no reason to believe this will always be
     true.
   . The hash function depends on endianess which makes the data base
     platform dependant for no good reason.

   Unless there are compelling advantages with using a custom hash
   function we should instead rely on the built in
   default. (Unfortunately one compelling reason may be backward
   compatibility...)

*/
#if !SP_FORCE_BDB_BUILD
#if SP_USER_DEFINED_HASH	/* [PD] 4.0 */
#error "We should consider using the default hash functions. Custom hash functions make standard tools (like db_stat) unusable."
#endif /*  SP_USER_DEFINED_HASH */
#endif /* !SP_FORCE_BDB_BUILD */

/* [PD] 4.0 */
#if SP_USER_DEFINED_HASH
#if 1				/* [PD] BDB version 3.2.9 */
u_int32_t identity(DB *dummy, const void *key, u_int32_t size)
{
  (void)dummy;                  /* [PM] 3.9b5 avoid -Wunused */
  (void)size;                   /* [PM] 3.9b5 avoid -Wunused */

  return *(u_int32_t *)key;
}
#else
u_int32_t identity(const void *key, u_int32_t size)
{
  return *(u_int32_t *)key;
}
#endif
#endif /* SP_USER_DEFINED_HASH */

#if SICSTUS_TODO
#error "FIXME BDB: [PM] 4.0 never use stat() directly, use spio_stat or prolog"
#endif  /* SICSTUS_TODO */
int directory_exists(char const *path) /* form library/db/file.c */
{
  struct stat buf;
  return stat(path, &buf) ? 0 : (buf.st_mode & S_IFDIR) != 0;
}

#if SICSTUS_TODO
#error "FIXME BDB: never use stat() directly, use spio_stat or prolog"
#endif  /* SICSTUS_TODO */
int file_exists(char const *path)
{
  struct stat buf;
  return stat(path, &buf) == 0;
}

int files_exist(char filename[][PATH_MAX+1], int n)
{
  int r = 0;
  for (; n; --n) if (file_exists(Native_path(filename[n-1]))) ++r;
  return r;
}

/* [PM] 3.9 There is no documented mkdir on Windows. There is an
   undocumented alias mkdir for the documented procedure _mkdir but
   _mkdir does not take a MODE argument.
*/
#if SP_WIN32
int bdb_mkdir(char const *path, int mode)
{
  return _mkdir(path);
}
#else  /* !SP_WIN32 */
int bdb_mkdir(char const *path, int mode)
{
  return mkdir(path, mode);
}
#endif  /* !SP_WIN32 */

#ifdef STAT
#if SP_WIN32
#include <windows.h>

int times(struct tms *buffer)
{
    static int initialized = FALSE;
    static FARPROC proc;
    FILETIME Createtm, Exittm, Kerneltm, Usertm;

    buffer->tms_cutime = 0; /* child process user time (N/A) */
    buffer->tms_cstime = 0; /* child process system time (N/A) */

    if (!initialized) {
	/* TNT extender doesn't include GetProcessTimes */
	HINSTANCE hnd = GetModuleHandle("kernel32.dll");

	proc = hnd ? GetProcAddress(hnd, "GetProcessTimes") : NULL;
	initialized = TRUE;
    }

    /* Get 64 bit process time representing 100ns units */
    if (!proc ||
	!(*proc)(GetCurrentProcess(), &Createtm, &Exittm, &Kerneltm, &Usertm))
    {   /* GetProcessTimes() not supported */
	buffer->tms_utime = clock();	/* user time */
	buffer->tms_stime = 0;		/* system time */
	return 0;
    }

    /* convert process time to number of elasped milliseconds */
    buffer->tms_utime  = Usertm.dwHighDateTime * 429496.7296;
    buffer->tms_utime += Usertm.dwLowDateTime / 10000;
    buffer->tms_stime  = Kerneltm.dwHighDateTime * 429496.7296;
    buffer->tms_stime += Kerneltm.dwLowDateTime / 10000;

    return 0;
}
#endif
#endif /* STAT */

#include "bdb.h"
#include "bdb_glue.h"

#if BACKSLASH_PATHS

char *db_native_path(char const *path)
{
  static char native_path_buffer[PATH_MAX];
  size_t len;
  size_t i;

  if (path == NULL)
    {
      return NULL;
    }

  len = strlen(path);
  if ((len +1) > sizeof native_path_buffer) /* overflow */
    {
      SP_ASSERT(0);
      return NULL;
    }

  memcpy(native_path_buffer, path, len +1); /* +1 for NUL */
  for (i=0; i < len; i++)
    {
      if (native_path_buffer[i] == '/')
        {
          native_path_buffer[i] = '\\';
        }
    }
  return native_path_buffer;
}

#endif /* BACKSLASH_PATHS */


/* True if RelPath is a relative pathname.
 * Ripped off from '$relative_file_name'/2.
 */
int db_relative_file_name(char const *path)
{
#if SICSTUS_TODO
#error "FIXME BDB: Update whole library to use SICStus run-time routines for file system handling"
#endif  /* SICSTUS_TODO */
  switch (path[0])
    {
    case '\0':
      return 1;		/* empty string - catch error later */
    case '/':
    case '~':
    case '$':
      return 0;		/* RelPath is absolute */
#if SP_WIN32
    case '\\':
      return 0;		/* RelPath is absolute */
    default:
      if (isalpha(path[0]) && path[1] == ':') /* drive letter - absolute path */
	return 0;
#endif  /* SP_WIN32 */
    }
  return 1;
}

char SP_FLI_CONST *db_normal_path(char SP_FLI_CONST *path)
{
#if BACKSLASH_PATHS
  static char normal_path_buffer[PATH_MAX];
  int i;

  if (path == NULL)
    return NULL;
  strcpy(normal_path_buffer, path);
  for (i=strlen(normal_path_buffer); i--; )
    if (normal_path_buffer[i] == '\\')
      normal_path_buffer[i] = '/';
  return normal_path_buffer;
#else  /* BACKSLASH_PATHS */
  return path;
#endif
}
