#ifndef SPIO_PROCESS_H_INCLUDED
#define SPIO_PROCESS_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"

#define SPIO_PROCESS_EXIT_STATUS_NORMAL SPIO_BIT(0) /* CLD_EXITED */
#define SPIO_PROCESS_EXIT_STATUS_SIGNALED SPIO_NEXT_BIT(SPIO_PROCESS_EXIT_STATUS_NORMAL) /* CLD_KILLED */
#define SPIO_PROCESS_EXIT_STATUS_COREDUMP SPIO_NEXT_BIT(SPIO_PROCESS_EXIT_STATUS_SIGNALED) /* CLD_DUMPED (SPIO_PROCESS_EXIT_STATUS_SIGNALED will be set too)  */
#define SPIO_PROCESS_EXIT_STATUS_STOPPED SPIO_NEXT_BIT(SPIO_PROCESS_EXIT_STATUS_COREDUMP) /* CLD_STOPPED */
#define SPIO_PROCESS_EXIT_STATUS_CONTINUED SPIO_NEXT_BIT(SPIO_PROCESS_EXIT_STATUS_STOPPED) /* CLD_CONTINUED */
#define SPIO_PROCESS_EXIT_STATUS_TRAPPED SPIO_NEXT_BIT(SPIO_PROCESS_EXIT_STATUS_CONTINUED) /* CLD_TRAPPED */

struct spio_t_process_exit_status_ {
  spio_t_bits flags;
  spio_t_int32 value;           /* value, interpreted according to flags */
};
typedef struct spio_t_process_exit_status_ spio_t_process_exit_status;

#define SPIO_PROCESS_CREATE_OPTION_STDIN_STD SPIO_BIT(0)
#define SPIO_PROCESS_CREATE_OPTION_STDIN_NUL SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDIN_STD)
#define SPIO_PROCESS_CREATE_OPTION_STDIN_PIPE SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDIN_NUL)

#define SPIO_PROCESS_CREATE_OPTION_STDOUT_STD SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDIN_PIPE)
#define SPIO_PROCESS_CREATE_OPTION_STDOUT_NUL SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDOUT_STD)
#define SPIO_PROCESS_CREATE_OPTION_STDOUT_PIPE SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDOUT_NUL)

#define SPIO_PROCESS_CREATE_OPTION_STDERR_STD SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDOUT_PIPE)
#define SPIO_PROCESS_CREATE_OPTION_STDERR_NUL SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDERR_STD)
#define SPIO_PROCESS_CREATE_OPTION_STDERR_PIPE SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDERR_NUL)

#define SPIO_PROCESS_CREATE_OPTION_NEED_HANDLE SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_STDERR_PIPE)

#define SPIO_PROCESS_CREATE_OPTION_DETACHED SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_NEED_HANDLE)
#define SPIO_PROCESS_CREATE_OPTION_DAEMON SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_DETACHED)
#define SPIO_PROCESS_CREATE_OPTION_USE_PATH_ SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_DAEMON) /* not supported */
#define SPIO_PROCESS_CREATE_OPTION_NEW_WINDOW SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_USE_PATH_)

#define SPIO_PROCESS_CREATE_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_NEW_WINDOW)
#define SPIO_PROCESS_CREATE_OPTION_PRIVATE_2 SPIO_NEXT_BIT(SPIO_PROCESS_CREATE_OPTION_PRIVATE_1)


extern spio_t_error_code spio_process_create(char const *cmd_utf8,
#if 1
                                             spio_t_property const **params_utf8,
#else
                                             char const *params_utf8[],
#endif
                                             char const *cwd_utf8,
                                             spio_t_os_file_handle *pstdin_parent_wr,
                                             spio_t_os_file_handle *pstdout_parent_rd,
                                             spio_t_os_file_handle *pstderr_parent_rd,
                                             spio_t_os_process_handle *process_handle,
                                             spio_t_bits options);

extern spio_t_error_code spio_process_kill(spio_t_os_process_handle process, int signal_number, spio_t_bits options);

#define SPIO_PROCESS_WAIT_OPTION_NONBLOCKING SPIO_BIT(0)
#define SPIO_PROCESS_WAIT_OPTION_RELEASE SPIO_NEXT_BIT(SPIO_PROCESS_WAIT_OPTION_NONBLOCKING) /* do not leave process waitable */
#define SPIO_PROCESS_WAIT_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_PROCESS_WAIT_OPTION_RELEASE)
#define SPIO_PROCESS_WAIT_OPTION_PRIVATE_2 SPIO_NEXT_BIT(SPIO_PROCESS_WAIT_OPTION_PRIVATE_1)
extern spio_t_error_code spio_process_wait(spio_t_os_process_handle process, spio_t_timespec *timeout, spio_t_process_exit_status *status, spio_t_bits options);

#define SPIO_PROCESS_RELEASE_OPTION_FORCE       SPIO_BIT(0)
#define SPIO_PROCESS_RELEASE_OPTION_HANDLE_ONLY SPIO_NEXT_BIT(SPIO_PROCESS_RELEASE_OPTION_FORCE)

/* Fails with SPIO_E_TRY_AGAIN if the process handle could not be
   released immediately (i.e. the (UNIX) process is still running)
   unless SPIO_PROCESS_RELEASE_OPTION_FORCE is set */
extern spio_t_error_code spio_process_release(spio_t_os_process_handle process, spio_t_bits options);

extern spio_t_error_code spio_process_id(spio_t_os_process_handle process, spio_t_pid *pid);

extern spio_t_error_code spio_process_self_id(spio_t_pid *pid);


extern spio_t_error_code spio_process_from_id(spio_t_pid pid, spio_t_os_process_handle *process);

extern spio_t_error_code spio_init_process(spio_t_bits options);

#endif  /* SPIO_PROCESS_H_INCLUDED */
