#ifndef SPIO_OS_H_INCLUDED
#define SPIO_OS_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"

#if SPIO_WIN32
#include "spio_win32.h"
#endif  /* SPIO_WIN32 */
#if SPIO_UNIX
#include "spio_unix.h"
#endif  /* SPIO_UNIX */


#define SPIO_OS_FILE_HANDLE_CLOSE_OPTION_ABORTIVE SPIO_BIT(0)
#define SPIO_OS_FILE_HANDLE_CLOSE_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_OS_FILE_HANDLE_CLOSE_OPTION_ABORTIVE)

#if SPIO_WIN32
#define spio_os_file_handle_close spio_os_file_handle_close_win32
#define spio_os_thread_self_id spio_win32_thread_self_id
#define spio_os_default_charset spio_win32_default_charset
#endif  /* SPIO_WIN32 */

#if SPIO_UNIX
#define spio_os_file_handle_close spio_os_file_handle_close_unix
#define spio_os_thread_self_id spio_unix_thread_self_id
#define spio_os_default_charset spio_unix_default_charset
#endif  /* SPIO_UNIX */

#endif  /* SPIO_OS_H_INCLUDED */
