#ifndef SPIO_TYPES_H_INCLUDED
#define SPIO_TYPES_H_INCLUDED 1

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) /* C99 or later */
#  define SPIO__FUNCTION__ __func__
#else
# if __GNUC__ >= 2
#  define SPIO__FUNCTION__ __FUNCTION__
# elif _MSC_VER >= 1300         /* VS .NET 2003 or newer */
#  define SPIO__FUNCTION__ __FUNCTION__
# endif
#endif
#ifndef SPIO__FUNCTION__
# define SPIO__FUNCTION__ "<unknown>"
#endif  /* SPIO__FUNCTION__ */

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) /* C99 or later */
#  define SPIO_INLINE inline
#else
# if __GNUC__
#  define SPIO_INLINE __inline__
# elif _MSC_VER
#  define SPIO_INLINE __inline
# endif
#endif
#ifndef SPIO_INLINE
# define SPIO_INLINE             /* nothing */
#endif

#include "spio_config.h"

#ifndef SPIO_EXPECT
#if defined(__GNUC__)
/* [PM] 4.0 Tells gcc that EXPR is likely to have value C. */
#define SPIO_EXPECT(EXPR, C) __builtin_expect((EXPR), (C))
#endif  /* __GNUC__ */
#endif  /* SPIO_EXPECT */

#ifndef SPIO_EXPECT
#define SPIO_EXPECT(EXPR, C) (EXPR)
#endif  /* SPIO_EXPECT */

/* [PM] 4.0 Tell compiler that EXPR is unlikely to be non-zero. Use in if conditions */
#define SPIO_UNLIKELY(EXPR) SPIO_EXPECT((EXPR), 0)


#if SPIO_INCLUDE_OS_TYPES
#if SPIO_WIN32
#ifndef STRICT
#define STRICT 1
#endif

#pragma warning( push )

#ifdef _PREFAST_
/*
  wspiapi.h(1001) : warning C6011: Dereferencing NULL pointer 'pfGetAddrInfo': Lines: 995, 996, 998, 999, 1001
 */
#pragma warning( disable : 6011 )
#endif  /* _PREFAST_ */

/* [PM] 4.0 Always include winsock2 instead of windows to prevent winsock.h from making subsequent inclusion of winsock2.h impossible */
#include <winsock2.h>

#pragma warning( pop ) 

#endif  /* SPIO_WIN32 */
#endif /* SPIO_INCLUDE_OS_TYPES */

#if SPIO_HAVE_UNISTD_H
#include <unistd.h>             /* ssize_t */
#endif  /* SPIO_HAVE_UNISTD_H */

#ifdef _MSC_VER
#define SPIO_CDECL __cdecl
#else  /* !_MSC_VER */
#define SPIO_CDECL              /* empty */
#endif  /* !_MSC_VER */


typedef unsigned char spio_t_uint8;
typedef spio_t_uint8 spio_t_byte;
typedef signed short spio_t_int16;
typedef unsigned short spio_t_uint16;
typedef signed int spio_t_int32;
typedef unsigned int spio_t_uint32;
#ifdef _MSC_VER                    /* MS CL.EXE */
typedef          __int64 spio_t_int64;
typedef unsigned __int64 spio_t_uint64;
#define SPIO_INT64_FORMAT "%I64d"
#define SPIO_UINT64_FORMAT "%I64u"
#else  /* !_MSC_VER */
typedef signed long long spio_t_int64;
typedef unsigned long long spio_t_uint64;
#define SPIO_INT64_FORMAT "%lld"
#define SPIO_UINT64_FORMAT "%llu"
#endif  /* !_MSC_VER */

#define SPIO_INT64_MAX 9223372036854775807LL /* 2*64-1 */
#define SPIO_INT64_MIN (-SPIO_INT64_MAX - 1LL)

#if _MSC_VER >= 1300            /* Visual Studio .NET */
#define SPIO__w64 __w64
#else
#define SPIO__w64               /* empty */
#endif

/* not all compilers have ssize_t yet */
#if SPIO_HAVE_SSIZE_T
typedef ssize_t spio_t_ssize;
#else  /* !SPIO_HAVE_SSIZE_T */

#if SPIO_WIN32

#if defined(_WIN64)
typedef __int64 spio_t_ssize;
#else  /* !_WIN64 */
typedef SPIO__w64 long spio_t_ssize;
#endif  /* !_WIN64 */

#else  /* !SPIO_WIN32 */

typedef long spio_t_ssize;      /* defacto OK on all sane (non-Win64) platforms*/
#if SPIO_DEBUG
#error "no ssize_t"             /* but we expect all sane platforms to have ssize_t  */
#endif  /* SPIO_DEBUG */

#endif  /* !SPIO_WIN32 */

#endif  /* !SPIO_HAVE_SSIZE_T */

typedef void *spio_t_thread_id;

typedef spio_t_int64 spio_t_offset; /* seek (file-) offset */
#define SPIO_OFFSET_MAX SPIO_INT64_MAX
#define SPIO_OFFSET_MIN SPIO_INT64_MIN

typedef spio_t_int32 spio_t_refcount;


#if SPIO_WIN32

#if SPIO_INCLUDE_OS_TYPES
typedef HANDLE spio_t_os_file_handle;
typedef HANDLE spio_t_os_process_handle;
typedef DWORD spio_t_pid;
#else  /* !SPIO_INCLUDE_OS_TYPES */
typedef void * spio_t_os_file_handle;
typedef void * spio_t_os_process_handle;
typedef unsigned long spio_t_pid;
#endif  /* !SPIO_INCLUDE_OS_TYPES */

#if SPIO_INCLUDE_OS_TYPES
#define SPIO_INVALID_OS_FILE_HANDLE ((spio_t_os_file_handle)INVALID_HANDLE_VALUE)
#define SPIO_INVALID_OS_PROCESS_HANDLE ((spio_t_os_process_handle)INVALID_HANDLE_VALUE)
#endif  /* SPIO_INCLUDE_OS_TYPES */

#elif SPIO_UNIX
typedef int spio_t_os_file_handle; /* file descriptor */
typedef pid_t spio_t_os_process_handle; /* pid_t always defined (in unistd.h) */
#define SPIO_INVALID_OS_FILE_HANDLE ((spio_t_os_file_handle) -1)
#define SPIO_INVALID_OS_PROCESS_HANDLE ((spio_t_os_process_handle) -1)

typedef pid_t spio_t_pid;

#endif  /* SPIO_UNIX */

/* flag bits for flags and options */
typedef spio_t_uint32 spio_t_bits;
typedef spio_t_uint16 spio_t_bits16;

typedef spio_t_uint32 spio_t_wchar; /* UNICODE char etc */
typedef spio_t_int32 spio_t_wcharint; /* Room for spio_t_wchar and negative (e.g., EOF) values.  */

typedef spio_t_uint8 spio_t_utf8;
typedef spio_t_uint16 spio_t_utf16;
typedef spio_t_uint32 spio_t_utf32;
typedef spio_t_int32 spio_t_utf32int;

typedef struct spio_t_timespec_ spio_t_timespec; /* for things like timeout */
struct spio_t_timespec_ {
  long sec;                     /* Seconds.  */
  long nsec;                    /* Nanoseconds.  */
};

typedef spio_t_int64 spio_t_time; /* seconds since the (POSIX-)Epoch UTC */

/* VARIANT */
enum spio_t_variant_type_ {
  SPIO_VARIANT_ILLEGAL=0,
  SPIO_VARIANT_END=1,           /* marks end of vector of properties (e.g., spio_t_arglist) */

  SPIO_VARIANT_STRING,
  SPIO_VARIANT_POINTER,
  SPIO_VARIANT_FLAGS,
  SPIO_VARIANT_INT,
  SPIO_VARIANT_INT64,

#if 0                           /* not for now */
  SPIO_VARIANT_BOOL,
#define SPIO_VARIANT_BOOL SPIO_VARIANT_BOOL
#endif
  SPIO_VARIANT_FIRST_UNUSED
};
#define SPIO_VARIANT_TYPE_MASK 0x07 /* at most 7 elements in spio_t_variant_type_ (must have all bits set!) */
extern int dummy1[( (SPIO_VARIANT_FIRST_UNUSED > (SPIO_VARIANT_TYPE_MASK+1)) ? -1 : 1 )]; /* boobytrap */

#define SPIO_VARIANT_TYPE(PVARIANT) ((spio_t_variant_type)((PVARIANT)->flags & SPIO_VARIANT_TYPE_MASK))
#if 0
/* requires SPIO_VARIANT_INIT already done */
#define SPIO_VARIANT_SET_TYPE(PVARIANT, TYPE) (((PVARIANT)->flags &= ~SPIO_VARIANT_TYPE_MASK), ((PVARIANT)->flags |= (TYPE)))
#endif  /* 0 */
#define SPIO_VARIANT_INIT_TYPE(VARIANT, TYPE) (((VARIANT).flags = (TYPE)))

#define SPIO_VARIANT_INIT(VARIANT) SPIO_VARIANT_INIT_TYPE((VARIANT), SPIO_VARIANT_ILLEGAL)
#define SPIO_VARIANT_INIT_STATIC_STRING(VARIANT, STRING) (SPIO_VARIANT_INIT_TYPE((VARIANT), SPIO_VARIANT_STRING), SPIO_SET_MASK((VARIANT).flags, SPIO_VARIANT_FLAG_STATIC_DATA), (VARIANT).u.string = (STRING))
#define SPIO_VARIANT_INIT_STRING(VARIANT, STRING) (SPIO_VARIANT_INIT_TYPE((VARIANT), SPIO_VARIANT_STRING), (VARIANT).u.string = (STRING))
#define SPIO_VARIANT_INIT_POINTER(VARIANT, PTR) (SPIO_VARIANT_INIT_TYPE((VARIANT), SPIO_VARIANT_POINTER), (VARIANT).u.p = (void*)(PTR))
#define SPIO_VARIANT_INIT_END(VARIANT) (SPIO_VARIANT_INIT_TYPE((VARIANT), SPIO_VARIANT_END), (VARIANT).u.p = NULL)

/* cannot forward declare enums (duh!) */
typedef enum spio_t_variant_type_ spio_t_variant_type;

typedef struct spio_t_variant_ spio_t_variant;
struct spio_t_variant_ {
#define SPIO_VARIANT_FLAG_DYNAMIC (SPIO_VARIANT_TYPE_MASK+1) /* variant struct is spio_alloc-ated and should be spio_free-d  */
#define SPIO_VARIANT_FLAG_STATIC_DATA SPIO_NEXT_BIT(SPIO_VARIANT_FLAG_DYNAMIC) /* variant u.string should NOT be spio_free-d  */

  spio_t_bits flags;
  union {
    char const *string;         /* SPIO_VARIANT_STRING */
    spio_t_int64 i64;           /* SPIO_VARIANT_INT64 */
    void const *p;              /* SPIO_VARIANT_POINTER (SPIO_VARIANT_END) */
    spio_t_bits flags;          /* SPIO_VARIANT_FLAGS */
    int i;                      /* SPIO_VARIANT_INT */
#ifdef SPIO_VARIANT_BOOL
    int b;                      /* SPIO_VARIANT_BOOL */
#endif
  } u;
};

/* KEYED PROPERTIES */
typedef struct spio_t_property_ spio_t_property;
struct spio_t_property_ {
  char const *key;
  spio_t_variant v;
};


#define SPIO_PROPERTY_INITIALIZER_END { NULL, { SPIO_VARIANT_END, { NULL } } }
#define SPIO_PROPERTY_INIT_END(PROPERY) do { (PROPERY).key = NULL; SPIO_VARIANT_INIT_END((PROPERY).v); }while(0)
#define SPIO_PROPERTY_INITIALIZER_STATIC_(KEY, TYPE, VALUE) { (KEY), { ((TYPE) | SPIO_VARIANT_FLAG_STATIC_DATA), { (VALUE) } } }
#if 1
#define SPIO_PROPERTY_INITIALIZER_STATIC_STRING(KEY, STRING) SPIO_PROPERTY_INITIALIZER_STATIC_((KEY), SPIO_VARIANT_STRING, (STRING))
#else
#define SPIO_PROPERTY_INITIALIZER_STATIC_STRING(KEY, STRING) { (KEY), { (SPIO_VARIANT_STRING | SPIO_VARIANT_FLAG_STATIC_DATA), { (STRING) } } }
#endif
#define SPIO_PROPERTY_INITIALIZER_STATIC_POINTER(KEY, PTR) SPIO_PROPERTY_INITIALIZER_STATIC_((KEY), SPIO_VARIANT_POINTER, (PTR))

typedef spio_t_property *spio_t_arglist;

/* Return values for spio_{os}_device_type() */
enum spio_t_device_type_ {
  SPIO_DEVICE_TYPE_ILLEGAL = 0,
  SPIO_DEVICE_TYPE_UNKNOWN,
  SPIO_DEVICE_TYPE_FILE,
  SPIO_DEVICE_TYPE_TTY,
#define SPIO_DEVICE_TYPE_CONSOLE SPIO_DEVICE_TYPE_TTY /* WIN32 NAME */
  SPIO_DEVICE_TYPE_SOCKET,
  SPIO_DEVICE_TYPE_FIFO,
#define SPIO_DEVICE_TYPE_PIPE SPIO_DEVICE_TYPE_FIFO /* WIN32 NAME */
  SPIO_DEVICE_TYPE_CHARACTER,
#define SPIO_DEVICE_TYPE_SERIAL SPIO_DEVICE_TYPE_CHARACTER /* WIN32 NAME */

  SPIO_DEVICE_TYPE_DIRECTORY,   /* UNIX only */
  SPIO_DEVICE_TYPE_BLOCK,       /* UNIX only */
  SPIO_DEVICE_TYPE_LINK,        /* UNIX only */

  SPIO_DEVICE_TYPE_FIRST_UNUSED_,
  SPIO_DEVICE_TYPE_LAST = SPIO_DEVICE_TYPE_FIRST_UNUSED_-1
};
typedef enum spio_t_device_type_ spio_t_device_type;

typedef struct spio_t_dirent_ spio_t_dirent;
typedef struct spio_t_dir_ spio_t_dir;


#include "spio_errors.h"

#endif  /* SPIO_TYPES_H_INCLUDED */
