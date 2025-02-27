/* config_template.h.  Generated by configure.  */
/* -*- Mode:C -*-
 * config.h.in:		Template file for config.h
 *
 * NOTE: Do *NOT* modify the contents of this file unless you know
 * exactly what you're doing. If you want to modify a flag or a
 * setting, use configure instead.
 * NOTE: This file must not include any other files. Ever!
 *       The reason is that it should be safe to include this file and
 *       then define feature macros (such as _XOPEN_SOURCE).
 */


/* [PM] 3.9 sicstus.h defines SP_NO_PRIVATE_CONFIG 1 to prevent
   non-public defines from being seen by user code. */

/* Things that should be seen by user code (and SP run-time) that
   includes sicstus.h */
#if !SP_PUBLIC_CONFIG_INCLUDED
#define SP_PUBLIC_CONFIG_INCLUDED 1

/***
 ***
 *** Beginning of public section ****
 ***
 ***/

#define SICSTUS_CONFIG_H 1
#define SICSTUS_PUBLIC_CONFIG_H 1

#define SICSTUS_MAJOR_VERSION 4
#define SICSTUS_MINOR_VERSION 0
#define SICSTUS_REVISION_VERSION 2
#define SICSTUS_BETA_VERSION 0

/* [PD] 3.11.1 */
#define SICSTUS_VERSION_STRING "4.0.2"

/*

  In 3.7 (3.8) SICSTUS_VERSION was 37 (38) and there was no way to
  find out the revision/patch level. Prior to 3.7 there was no
  SICSTUS_VERSION defined at all.

  From 3.9 format changed to enable more than 100 minor releases, each
  with 100 patch releases.
  (In fact SICStus minor release is what others might call major releases.)

  XXYYZZ, 3.9.1 would be 030901
*/
#define SICSTUS_VERSION (((((SICSTUS_MAJOR_VERSION)*100)+(SICSTUS_MINOR_VERSION))*100)+(SICSTUS_REVISION_VERSION))

/* [PM] 3.9.1 non-zero on platforms where loading multiple sicstuses into the same process does *not* work. */
#define SP_NO_MULTI_SP_FEATURE 0

/* [PM] 4.0 Use SPIO I/O package (always true in SP4) */
#define SICSTUS_SPIO 1

/* [PM] 4.0 True on OSes (like Win32) where user_main will be called
   with a argv containing UTF-8 strings. This also implies that the
   argv argument to SP_initialize, SU_initialize etc should use
   UTF-8. */
#define SP_USER_MAIN_ARGV_UTF8 1

/* [PM] 3.10.2 non-zero to make GetSICStusAPIProcInit the same as
   GetSICStusAPIProc. This requires the users to call
   SetupSICStusDISPATCH() explicitly before the first call to a
   SP_... routine. The only reason to have this non-zero is to work
   around a bug in the HPUX (ipf-hpux-B.11.22) C compiler (cc -V says:
   cc: HP aC++/ANSI C B3910B A.05.41 [Nov 1 2002]) */
#ifndef SP_INHIBIT_IMPLICIT_APIPROCINIT /* Allow users to redefine it from cc command line */
#define SP_INHIBIT_IMPLICIT_APIPROCINIT 0
#endif /* SP_INHIBIT_IMPLICIT_APIPROCINIT */

/* [PM] Calling convention for public API. Empty except on Windows where it is __cdecl */
#define SPCDECL __cdecl

/* [PM] 4.0 Are we using (the GCC 4.0 feature) -fvisibility=hidden? */
#define SP_USE_GCC_VISIBILITY 0

/* [PM] 3.9b4 The line following this marker is filled in with great
   effort by Emulator/Makefile */
/* @PUBLIC_CONFIGURE_THINGS_FOR_CONFIG_H@ */

#define SP_SIG_IGN ((SP_SigFun*)SIG_IGN)
#define SP_SIG_DFL ((SP_SigFun*)SIG_DFL)
#define SP_SIG_ERR ((SP_SigFun*)SIG_ERR)
#define SP_SIGINT SIGINT
#define SP_SIGQUIT SIGBREAK
#define SP_SIGBREAK SIGBREAK


/* [PM] 3.9.2 TERMINATIONDATE is now defined as zero if it should be ignored */
#define TERMINATIONDATE 0
#if 0
/* [PD] 3.9beta1 */
#if SICSTUS_BETA_VERSION
#define TERMINATIONDATE 0
#endif
#endif

/* used by spio_config.h */
#define SP_BIGENDIAN 0

/***
 ***
 *** End of public section ****
 ***
 ***/

#endif /* !SP_PUBLIC_CONFIG_INCLUDED */


#if !SP_NO_PRIVATE_CONFIG

#if  !SP_CONFIG_H_INCLUDED
#define SP_CONFIG_H_INCLUDED 1

#define SICSTUS_PRIVATE_CONFIG_H 1

/* [PM] 3.9.1 if a run-time system foo.exe is in <DIR>/foo.exe then
   <DIR>/<SP_RTSYS_DIRNAME> is the directory where all the sicstus
   files can be found (traditionally named "lib" on UNIX, "sp39" on
   Win32 although these should both be something like
   sicstus-<major>.<minor>.<revision> eventually).  Note that this is
   not the same as SP_EMULATOR_DIRNAME, the layout is something like
   <DIR>/<SP_RTSYS_DIRNAME>/<SP_EMULATOR_DIRNAME>
*/
#define SP_REGVER "4.0.2"
#define SP_RTSYS_DIRNAME "sp-4.0.2"
#define SP_EMULATOR_DIRNAME "sicstus-4.0.2"
/* [PM] 3.11.0 */
#define LICENSE_PRODUCT "sicstus4.0_x86-win32-nt-4"

/* [PM] 3.9 */
/* [PM] 3.9b5 note that all public headers (e.g., sicstus.h) assume this is 1 */
#define SP_DISPATCH_API 1

/* [PM] 3.9 Now (default) for all platforms */
#define PtrSPFuncs 1

/* [PM] 3.9b4 Put stash and API dispatch together in struct open_resource */
#define SP_FOREIGN_SPENV 1
/* [PM] 3.9b4 Pass an extra void** to all foreign glue funs */

#if !(SP_FOREIGN_SPENV)
#error "SP_FOREIGN_SPENV must be on in 3.9"
#endif /* !(SP_FOREIGN_SPENV) */

/* [PD] 3.12.7 Mac/64-bit (interim) */
#define SP_MACOSX_FROM_OS_WCX_FILE_IS_NFC 0

/* [PM] 3.9b4 Special things needed for supporting multiple sicstuses in the same process. Implies SP_DISPATCH_API */
#define ENABLE_MULTI_SP 1

/* [PM] 3.10 always 1 or else you need to uncomment get0/1 and get/1 in intrins2.pl */
#define ENABLE_C_GETX1 1

/* not used but bumped for 3.9 */
#define SPFliVersion 0x03020302

/* [PM] 3.10.2 xref bips_version.pl */
#define BIPS_VERSION 1040002

#if (ENABLE_MULTI_SP && !SP_DISPATCH_API)
#error "ENABLE_MULTI_SP requires SP_DISPATCH_API/PM"
#endif /* (ENABLE_MULTI_SP && !SP_DISPATCH_API) */

/* !! 3.9 Do not use pragma once for this file (since it is really two
   files, the public config and the sicstus run-time private config */

/* [PM] 3.9 URL_OBJECTS and restore from stream */
#define SP_URL_OBJECTS 1

/* System information */
#define SYSTEM_NAME "x86-win32-nt-4"
#define HARDWARE_NAME "i686"
#define NODE_NAME ""
#define HOST_TYPE "x86-win32-nt-4"

#define SHSFX "dll"
#define SHPRE ""
#define FLI_SHSFX "dll"

/* [PM] 4.0 for some platforms we need to know what calling convention is used. */

/* (e.g., x86_64-linux-glibc2.3) */
#define FLI_APPLY_ASM_SYSV_ABI_X86_64 0
/* MacOS X powerpc 32bit */
#define FLI_APPLY_ASM_ABI_PPC 0

/* [PM] 3.9.1 "libsprt39" used by fli_rhap to construct
   "libsprt39.dylib" which is then matched against loaded modules to
   find the SP_RT_DIR */
/* #define RTKERNEL RTKERNEL_undefined */
#define RTKERNEL_BASENAME_SANS_SUFFIX_STRING "sprt4-0-2"
#define RTKERNEL_BASENAME_WITH_DSO_SUFFIX_STRING "sprt4-0-2.dll"


/* Something like "libsprt-4-1-2-instance-XX.so". Must contain exactly
   two adjacent X that will be replaced by a counter. Must be a
   literal string) */
#define RTKERNEL_INSTANCE_TEMPLATE_STRING "sprt4-0-2-instance-XX"

/* Define as '0' to disable gauging. */
#define GAUGE 1

/* Define as '0' to disable BDD's. */
#define BDD 1

/* [PM] 3.9 Define as '0' to disable CLPFD. */
#define ENABLE_CLPFD 1

#if (ENABLE_CLPFD && ENABLE_MULTI_SP) /* if these are false then do not define at all */
#define ENABLE_CLPFD_MULTI_SP 1
#endif /* (ENABLE_CLPFD && ENABLE_MULTI_SP) */


/* [PM] 3.9b5 reinitialise/0 is gone. Still need deinitialize() for DS handling of SIGSEGV*/
#define ENABLE_DEINITIALIZE 1


/* Define as '0' to disable threaded code in the emulator. */
#define THREADED 0

/* 3.9.1 [PM] Update: Worker storage split into two decisions:
   . Define W_IS_SELF_STORAGE as 1 to enable statically allocated
     worker structure. This SHOULD ALWAYS BE 1, see mt.c.

   . Define WAM_USE_W_IS_SELF_STORAGE as 1 to make wam() access
     self_storage directly instead of using the w argument. This may
     or may not be a good idea depending on architecture. Off by
     default certainly.
   [PM] 4.0 Effect of WAM_USE_W_IS_SELF_STORAGE: moe much worse, zjuk slightly worse

*/
#define W_IS_SELF_STORAGE 1     /* [PM] 3.9.1 always 1, see above. */

#ifndef WAM_USE_W_IS_SELF_STORAGE /* allow override */
#define WAM_USE_W_IS_SELF_STORAGE 0
#endif  /* WAM_USE_W_IS_SELF_STORAGE */

/* [PM] 4.0.0 wam.c needs to know if assembly will be post-processed */
#define ENABLE_WAM_ASM_PEEP 1

#ifndef PREFETCHING             /* [PM] 4.0 allow manual override */
/* [PM] 3.10.1 This has been off for a long time. Should measure this. */
#define PREFETCHING 0
#endif  /* PREFETCHING */

#define WAM_SHADOW_REGS 1
#define WORKER_TERM_EARLY 0

#define USE_OLD_CTagToXXX_DEFS 1 /* [PM] 3.10 FIXME: workaround for bug in termdefs.h */

#if 0                           /* [PM] 4.0 These should only be passed on command line */
   /* [PM] 3.10.1 Define as '1' to use undocumented Core Process */
   #define USE_CPSEnableForegroundOperation 0
   /* [PM] 3.11.1 Define as '1' to use documented MacOS X 10.3 function TransformProcessType */
   #define USE_TransformProcessType 0
#endif  /* 0 */

/* [PM] 4.0.2 Use the new thread-device I/O */
#define SPIO_USE_LAYER_THREADDEVICE 0

/* [PM] 3.10 Development system can put up a license entry dialog */
#define DS_CAN_LICENSE_DIALOG 1

/* [PM] 3.10 Embed meta info into saved state (foreign resource names) */
#define SAV_META_INFO 1

#define SP_TRU64 0

#define SP_WIN32 1

/* [PM] 3.9.2 True if setitimer(ITIMER_VIRTUAL) sometimes signals too early. See library/timeout.c */
#define SETITIMER_EARLY 0

/* Define as 1 if system uses FIONBIO for non-blocking I/O */
/* #define USE_FIONBIO 0 */

/* Header files */
#define HAVE_ARPA_INET_H 0
#define HAVE_ALLOCA_H 0
#define HAVE_ASSERT_H 1
#define HAVE_CONIO_H 1
#define HAVE_CTYPE_H 1
#define HAVE_DIRECT_H 1
#define HAVE_ERRNO_H 1
#define HAVE_FCNTL_H 1
#define HAVE_FEATURES_H 0
#define HAVE_INTTYPES_H 0
#define HAVE_IO_H 1
#define HAVE_LANGINFO_H 0
#define HAVE_LOCALE_H 1
#define HAVE_MALLOC_H 1
#define HAVE_NETDB_H 0
#define HAVE_NETINET_IN_H 0
#define HAVE_SIGNAL_H 1
#define HAVE_STROPTS_H 0
#define HAVE_STRINGS_H 0
#define HAVE_SYS_FILE_H 0
#define HAVE_SYS_FILIO_H 0
#define HAVE_SYS_IOCTL_H 0
#define HAVE_SYS_PARAM_H 0
#define HAVE_SYS_SELECT_H 0
#define HAVE_SYS_SOCKET_H 0
#define HAVE_SYS_RESOURCE_H 0
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_WAIT_H 0
#define HAVE_SYS_TIMES_H 0
#define HAVE_SYS_TIME_H 0
#define HAVE_SYS_TYPES_H 1
#define HAVE_SYS_UN_H 0
#define HAVE_SYS_UTSNAME_H 0
#define HAVE_UCONTEXT_H 0
#define HAVE_UNISTD_H 0
#define HAVE_WINSOCK_H 1
#define HAVE_PROCINFO_H 0
#define HAVE_PROCESS_H 1
#define HAVE_SYS_LDR_H 0
#define HAVE_SYS_MMAN_H 0
#define HAVE_TIME_H 1
#define HAVE_THREAD_H 0
#define HAVE_PTHREAD_H 0
#define HAVE_DIRECT_H 1
#define HAVE_DLFCN_H 0
#define HAVE_DIRENT_H 0
#define HAVE_WCTYPE_H 1
#define HAVE_WCHAR_H 1
#define HAVE_LOADER_H 0
#define HAVE_MATH_H 1
#define HAVE_IEEEFP_H 0
#define HAVE_FLOAT_H 1
#define HAVE_LIMITS_H 1
#define HAVE_PWD_H 0
#define HAVE_GRP_H 0

/* [PM] 3.9.1 IRIX 6.5 */
#define HAVE_OBJLIST_H 0
#define HAVE_ELF_H 0



/* Libraries */
#define HAVE_LIBPOSIX4 0
#define HAVE_LIBPTHREAD 0

#define SOLARIS_THREADS 0
#define POSIX_THREADS (HAVE_LIBPTHREAD && ! SOLARIS_THREADS)
#define HAVE_NATIVE_THREADS (SOLARIS_THREADS || HAVE_LIBPTHREAD || SP_WIN32)

/* definition for btree to use SP alloc routines or C malloc() */
#define BTREE_USE_MEMMAN 1

/* Types */
#define HAVE_SIGINFO_T 0
#define HAVE_IN_ADDR_T 0
#define HAVE_SOCKLEN_T 0
#define HAVE_INTPTR_T 0

#define SP_SIZEOF_SHORT 2
#define SP_SIZEOF_INT 4
#define SP_SIZEOF_LONG 4
#define SP_SIZEOF_VOID_P 4
/* long long gcc and C99 */
#define SP_SIZEOF_LONG_LONG 8
/* __int64 Win32 */
#define SP_SIZEOF___INT64 8

/* Function availability. Keep sorted! */
#define HAVE_ABORT 1
#define HAVE_ACCESS 1

/* [PM] 3.9.1 this is getargs() from procinfo.h unrelated to the command line parsing getargs */
#define HAVE_AIX_GETARGS 0

/* #define HAVE_BCOPY 0 */
#define HAVE_CHDIR 1
#define HAVE_COMPLIANT_SNPRINTF 0
/* #define HAVE_CSETLEN 0 */
#define HAVE_CLOSEFROM 0
#define HAVE_DIRFD 0
#define HAVE__EXIT 0
#define HAVE__FILENO 1
#define HAVE_FCNTL 0
/* #define HAVE_FINITE 0 */
#define HAVE___FUNCTION__ 1

#if __STDC_VERSION__ < 199901L
# if __GNUC__ >= 2
#  define SP__FUNCTION__ __FUNCTION__
# elif _MSC_VER >= 1300         /* VS .NET 2003 or newer */
#  define SP__FUNCTION__ __FUNCTION__
# else
#  define SP__FUNCTION__ "<unknown>"
# endif
# else  /* c99 */
#  define SP__FUNCTION__ __func__
#endif

#define HAVE_GETADDRINFO 0
#define HAVE_GETNAMEINFO 0
#define HAVE_GAI_STRERROR 0
#define HAVE_FREEADDRINFO 0

#define HAVE_GET_CURRENT_DIR_NAME 0
#define HAVE_GETCWD 1
/* #define HAVE_GETHOSTID 0 */
#define HAVE_GETEUID 0
#define HAVE_GETEXECNAME 0
#define HAVE_GETGROUPS 0
#define HAVE_GETITIMER 0
#define HAVE_GETLONGPATHNAME 1
#define HAVE_LONG_AND_SHORT_PATHNAMES 1
#define HAVE_GETMODULEFILENAMEA 1
#define HAVE_GETPAGESIZE 0
#define HAVE_GETPROCESSTIMES 1
#define HAVE_GETTHREADTIMES 1
#define HAVE_GETRUSAGE 0
#define HAVE_GETRLIMIT 0
/* #define HAVE_GETWD 0 */
/* #define HAVE_NTP_GETTIME 0 */
#define HAVE_IOCTL 0
#define HAVE_ISATTY 1
#define HAVE__ISATTY 1
#define HAVE_ISWLOWER 1
#define HAVE_MEMMOVE 1
/* #define HAVE_MKTEMP 0 */
/* #define HAVE_MKSTEMP 0 */
#define HAVE_MMAP 0
#define HAVE_NL_LANGINFO 0
#define HAVE_PAUSE 0
#define HAVE_POPEN 0
#define HAVE_PTHREAD_MUTEXATTR_SETTYPE 0
#define HAVE_PUTENV 1
#define HAVE__PUTENV 1
#define HAVE_REGOPENKEYEXA 1
/* #define HAVE_SBRK 0 */
#define HAVE_SETITIMER 0
#define HAVE_SETENV 0
#define HAVE_SETENVIRONMENTVARIABLE 1
#define HAVE__STRDUP 1
#define HAVE_GETENV 1
#define HAVE_GETHOSTNAME 0
#define HAVE_PUTENV 1
#define HAVE_RAISE 1
#define HAVE_SETLOCALE 1
#define HAVE_SIGACTION 0
#define HAVE_SIGNAL 1
#define HAVE_SNPRINTF 0
#define HAVE__SNPRINTF 1
#define HAVE_STAT 1
#define HAVE_STRERROR 1
#define HAVE_THREAD_SAFE_STRERROR 0
#define HAVE_STRERROR_R 0
#define HAVE_SYSCONF 0
#define HAVE_UNAME 0
#define HAVE_UNSETENV 0
#define HAVE_VSNPRINTF 1
#define HAVE__VSNPRINTF 1
#define HAVE_WAITID 0
#define HAVE_TIMES 0
#define HAVE_TRUNC 0
#define HAVE_DEBUGBREAK 1
#define HAVE_DLADDR 0
#define HAVE_DLGETNAME 0
#define HAVE_DLMODINFO 0
#define HAVE_LDR_INQ_MODULE 0
#define HAVE_LOADQUERY 0
#define HAVE_LOADLIBRARYA 1
#define HAVE_LOCALTIME_R 0
#define HAVE_CANONICALIZE_FILE_NAME 0
#define HAVE_SETRLIMIT 0

/* [PM] 4.0  */
#define HAVE_ARITHMETIC_RIGHT_SHIFT 1

/* [PM] 3.9.1 IRIX 6.5 */
#define HAVE__RLD_OBJ_HEAD 0

/* [PM] 3.10 MSVC */
#define HAVE__HEAPMIN 1

#define HAVE_PROC_SELF_FD 0
#define HAVE_LINUX_PROC_SELF_EXE 0

/* Preprocessor symbols */
/* Define as 1 if I_SETSIG and S_INPUT are defined and needed for SIGPOLL signals to be generated. */
/* #define HAVE_SETSIG 0 */

/* [PM] 3.11.1 WinCE Set to 1 on platforms (Win32) where environment
   variable lookup is case insensitive
   (In fact, for $VAR in pathnames to work we need to have
   CASE_INSENSITIVE_GETENV true on any platform where pathnames are
   case forced lowercased (i.e., Win32).).
 */
#define CASE_INSENSITIVE_GETENV 1

/* [PM] 3.9.0 Should be 1 on everything except Win32 */
#define POSIX_SIGNALS 0

/* [PM] 3.11.2 Win32 structured exception handling (See Utils/Win32/console.c) */
#define HAVE_STRUCTURED_EXCEPTION_HANDLING 1

/* Misc */

/*     Define signal handlers as returning type `RETSIGTYPE':
         RETSIGTYPE hup_handler ()
*/
#define RETSIGTYPE void


#if 0 /* [PM] 4.0 No longer used */
   /* [PM] 3.9.1 Some platforms have statbuf.st_mtime as a long (64bit
     Solaris SunOS 5.7), some as an int (Tru64 5.1)
   */
   #define SP_STATBUF_ST_MTIME_IS_LONG 0
#endif  /* 0 */


/* [PM] detecting non-finite float */

/* [PM] 3.9 new way. */
#define SP_ISFINITE _finite
#define SP_ISNAN _isnan
#define SP_ISNAN_FINITE_IN_MATH_H 0
#define SP_ISNAN_FINITE_IN_FLOAT_H 1
#define SP_ISNAN_FINITE_IN_IEEEFP_H 0

/* [PM] 3.9.1 missing in 3.8 and 3.9.0! */
#define NAN_COMPARE_FAILS 1

/* [MC] 3.9.1 */
#define ACCURATE_QUOTIENT 1

/* [PD] 3.9 define as 1 to enable MT-Jasper to use PushLocalFrame/PopLocalFrame
   in make_arg_array() instead of crashing. */
#define JASPER_PUSH_LOCAL_REFS 0


/* [PM] Create all NaN equal */
#define CANONICAL_NAN 1

/* [PM] 3.10 xref memman.c, configure.in */
#define MEM_ALLOC MM_USE_WIN32

#define MSVC_STRING_POOLING_BUG 0

/* [PM] 3.9.1 set to 1 for platforms where SP_APP_DIR/SP_RT_DIR cannot
   be determined (and thus will require spld-generated wrapper)
 */

#define CANNOT_SET_APP_PATHS 0

/* [PM] 3.9.1b4 now set by configure.in */
#define LogSizeOfWord 2


/* [PM] WinCE configure.in */
#define SP_AIX 0

/* [PM] WinCE configure.in */
#define SP_DARWIN 0

#if (__alpha__ || __alpha) && !alpha
#define alpha 1
#endif

#define SP_HPUX 0

#if __sparc__ && !sparc
#define sparc 1
#endif

/* [PM] WinCE
   End of things that used to be output by config.c */

/* [PM] 3.9 The output of config.exe goes after the magic marker */
/* @MAGIC_MARKER@ */

/* [PM] 3.9.1 Define our sized types
 * These belong in support.h but are needed by some test-cases.
 */
#if HAVE_LIMITS_H
#include <limits.h>             /* [PM] 3.11.1 LLONG_MAX, LONG_LONG_MAX etc user below */
#endif /* HAVE_LIMITS_H */

#ifndef SP_int16
#if (SP_SIZEOF_SHORT==2)
typedef short SP_int16;
typedef unsigned short SP_uint16;
#define SP_int16 SP_int16
#endif /* (SP_SIZEOF_SHORT==2) */
#endif /* !SP_int16 */
#define SP_int16_max ((SP_int16)0x7fff)
#define SP_int16_min ((-SP_int16_max) - 1)
#ifndef SP_int16
#error "no 16bit int type"
#endif /* !SP_int16 */

#define SP_uint16 SP_uint16

#ifndef SP_int32
#if (SP_SIZEOF_INT==4)
typedef int SP_int32;
#define SP_int32 SP_int32
#endif /* (SP_SIZEOF_INT==4) */
#endif /* SP_int32 */
#define SP_int32_max_raw (0x7fffffff)
#define SP_int32_max ((SP_int32)SP_int32_max_raw)
#define SP_int32_min ((-SP_int32_max) - 1)
#ifndef SP_int32
#error "no 32bit int type"
#endif /* !SP_int32 */

#ifndef SP_uint32
#if (SP_SIZEOF_INT==4)
typedef unsigned int SP_uint32;
#define SP_uint32 SP_uint32
#endif /* (SP_SIZEOF_INT==4) */
#endif /* SP_uint32 */
#define SP_uint32_max_raw (0xffffffff)
#define SP_uint32_max ((SP_uint32)SP_uint32_max_raw)
#define SP_uint32_min ((SP_uint32)0)
#ifndef SP_uint32
#error "no 32bit unsigned int type"
#endif /* !SP_uint32 */

#if SP_SIZEOF_INT==4 && defined(INT_MAX)
#if SP_int32_max_raw != INT_MAX
#error "inconsistency SP_int32_max_raw != INT_MAX"
#endif
#endif /* (SP_SIZEOF_INT==4 && defined(INT_MAX)) */

/* SP_int64 is first of int,long,long long,__int64
   This type is optional
*/
#ifndef SP_int64
#if (SP_SIZEOF_INT==8)
typedef int SP_int64;
typedef unsigned int SP_uint64;
#define SP_int64 SP_int64
#endif /* (SP_SIZEOF_INT==8) */
#endif /* SP_int64 */
#ifndef SP_int64
#if (SP_SIZEOF_LONG==8)
typedef long SP_int64;
typedef unsigned long SP_uint64;
#define SP_int64 SP_int64
#endif /* (SP_SIZEOF_LONG==8) */
#endif /* SP_int64 */
#ifndef SP_int64
#if (SP_SIZEOF_LONG_LONG==8)
typedef long long SP_int64;
typedef unsigned long long SP_uint64;
#define SP_int64 SP_int64
#endif /* (SP_SIZEOF_LONG_LONG==8) */
#endif /* SP_int64 */
#ifndef SP_int64
#if (SP_SIZEOF___INT64==8)
typedef __int64 SP_int64;
typedef unsigned __int64 SP_uint64;
#define SP_int64 SP_int64
#endif /* (SP_SIZEOF___INT64==8) */
#endif /* SP_int64 */

#ifdef SP_int64
#define SP_uint64 SP_uint64
#endif

#ifdef SP_int64

#if (SP_SIZEOF_LONG_LONG==8)    /* int64 is long long */

/* C99 limit names */
#ifndef SP_int64_max
#ifdef LLONG_MAX
#define SP_int64_max LLONG_MAX
#endif /* LLONG_MAX */
#endif /* SP_int64_max */
#ifndef SP_int64_min
#ifdef LLONG_MIN
#define SP_int64_min LLONG_MIN
#endif /* LLONG_MIN */
#endif /* SP_int64_min */

/* GCC limit names */
#ifndef SP_int64_max
#ifdef LONG_LONG_MAX
#define SP_int64_max LONG_LONG_MAX
#endif /* LONG_LONG_MAX */
#endif /* SP_int64_max */
#ifndef SP_int64_min
#ifdef LONG_LONG_MIN
#define SP_int64_min LONG_LONG_MIN
#endif /* LONG_LONG_MIN */
#endif /* SP_int64_min */

/* AIX limit names */
#ifndef SP_int64_max
#ifdef LONGLONG_MAX
#define SP_int64_max LONGLONG_MAX
#endif /* LONGLONG_MAX */
#endif /* SP_int64_max */
#ifndef SP_int64_min
#ifdef LONGLONG_MIN
#define SP_int64_min LONGLONG_MIN
#endif /* LONGLONG_MIN */
#endif /* SP_int64_min */

#endif /* int64 is long long */

/* Microsoft Visual Studio VC98 */
#ifndef SP_int64_max
#ifdef _I64_MAX
#define SP_int64_max _I64_MAX
#endif /* _I64_MAX */
#endif /* SP_int64_max */
#ifndef SP_int64_min
#ifdef _I64_MIN
#define SP_int64_min _I64_MIN
#endif /* _I64_MIN */
#endif /* SP_int64_min */

/* Fallback. Problem is that we do not know what suffix to use for the constants (LL, i64) */
#ifndef SP_int64_max
#define SP_int64_max ((SP_int64)0x7fffffffffffffff)
#endif /* SP_int64_max */
#ifndef SP_int64_min
#define SP_int64_min ((-SP_int64_max) - 1)
#endif /* SP_int64_min */

#endif /* SP_int64 */

/* SP_int128 is first of long,long long, SSE-types
   This type is optional and not generally available (GCC on x64 has it)
*/
#ifndef SP_int128
#if (SP_SIZEOF_LONG==16)
typedef long SP_int128;
typedef unsigned long SP_int128;
#define SP_int128 SP_int128
#endif /* (SP_SIZEOF_LONG==16) */
#endif /* SP_int128 */
#ifndef SP_int128
#if (SP_SIZEOF_LONG_LONG==16)
typedef long long SP_int128;
typedef unsigned long long SP_uint128;
#define SP_int128 SP_int128
#endif /* (SP_SIZEOF_LONG_LONG==16) */
#endif /* SP_int128 */

#if 0                           /* [PM] 4.0beta3 disable, did not work on all GCC targets. Investigate GCC type __uint128_t later */

   /* [PM] 4.0 Experimental, use SSE type */
   #if defined(__SSE__) && defined(__GNUC__)
   typedef int __attribute__((__mode__(__TI__))) SP_int128;
   typedef unsigned int __attribute__((__mode__(__TI__))) SP_uint128;
   #define SP_int128 SP_int128
   #endif  /* SSE && GNUC */
#endif  /* 0 */

/* [PM] 4.0 Experimental, use GCC double word arithmetics (available at least since GCC 4.0) */
#if (__GNUC__+0) >= 4 && SIZEOF_VOID_P >= 8 /* 64bit machine (or larger :-) */
typedef __int128_t SP_int128;
typedef __uint128_t SP_uint128;
#define SP_int128 SP_int128
#endif  /* SSE && GNUC */

/* signed version implies presence of unsigned version */
#ifdef SP_int128
#define SP_uint128 SP_uint128
#endif



#ifdef SP_int128
#define SP_int128_max ((SP_int128)0x7fffffffffffffffffffffffffffffff )
#define SP_int128_min ((-SP_int128_max) - 1)

#endif /* SP_int128 */

/* SP_largest_int is first of SP_int128,SP_int64,SP_int32 */
#ifndef SP_largest_int
#ifdef SP_int128
typedef SP_int128 SP_largest_int;
#define SP_largest_int SP_largest_int
#endif /* SP_int128 */
#endif /* SP_largest_int */
#ifndef SP_largest_int
#ifdef SP_int64
typedef SP_int64 SP_largest_int;
#define SP_largest_int SP_largest_int
#endif /* SP_int64 */
#endif /* SP_largest_int */
#ifndef SP_largest_int
#ifdef SP_int32
typedef SP_int32 SP_largest_int;
#define SP_largest_int SP_largest_int
#endif /* SP_int32 */
#endif /* SP_largest_int */
#ifndef SP_largest_int
#error "no SP_largest_int defined"
#endif /* !SP_largest_int */

#define ENABLE_MEM_STATS 0
#define ENABLE_SPACE_OUT 0

#define SP_MALLOC_RETURNS_NULL 1

/* [PM] 3.10.2 cl 13.x bug. Intrinsic strcat is broken */
#define SP_STRCAT_INTRINSIC_BROKEN 0
#if _MSC_VER
#if SP_STRCAT_INTRINSIC_BROKEN
#pragma function(strcat)        /* force use of real function */
#endif  /* SP_STRCAT_INTRINSIC_BROKEN */
#endif  /* _MSC_VER */


#endif /* !SP_CONFIG_H_INCLUDED */
#endif /* !SP_NO_PRIVATE_CONFIG */
