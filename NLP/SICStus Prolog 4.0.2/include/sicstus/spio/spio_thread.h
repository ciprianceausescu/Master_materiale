#ifndef SPIO_THREAD_H_INCLUDED
#define SPIO_THREAD_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"
#include "spio_event.h"

typedef struct spio_t_task_ spio_t_task_;
typedef spio_t_task_ volatile spio_t_task;

typedef void *spio_t_thread_funcion(spio_t_task *task, void *arg);

extern void spio_task_free(spio_t_task *task);

extern spio_t_error_code spio_task_result(spio_t_task *task, void **presult);
#define SPIO_TASK_STATUS_RUNNING SPIO_BIT(0)
extern spio_t_error_code spio_task_status(spio_t_task *task, spio_t_bits *pstatus);
extern spio_t_error_code spio_task_event(spio_t_task *task, spio_t_event **pevent);
extern int spio_task_is_running(spio_t_task *task);
#define SPIO_TASK_IS_RUNNING(TASK) spio_task_is_running((TASK))

/* SPIO_S_TRUE (SPIO_S_FALSE) if task has (not) been spio_task_cancel-ed */
extern spio_t_error_code spio_task_is_cancelled(spio_t_task *task);

/* only when SPIO_HAVE_TASK_CANCEL_EVENT (otherwise SPIO_E_NOT_SUPPORTED) */
extern spio_t_error_code spio_task_cancel_event(spio_t_task *task, spio_t_event **pevent);

#define SPIO_TASK_CANCEL_OPTION_ASYNC_ SPIO_BIT(0) /* do not wait for task to finish after cancel (currently not implemented) */
extern spio_t_error_code spio_task_cancel(spio_t_task *task, spio_t_bits options);



#define SPIO_THREAD_CREATE_OPTION_DETACHED SPIO_BIT(0)
/* #define SPIO_THREAD_CREATE_OPTION_JOINABLE SPIO_BIT(1) */
#define SPIO_THREAD_CREATE_OPTION_POOL SPIO_NEXT_BIT(SPIO_THREAD_CREATE_OPTION_DETACHED)

/* allow thread to be (pthread_/Win32 APC-)cancelled */
#define SPIO_THREAD_CREATE_OPTION_CANCELLABLE SPIO_NEXT_BIT(SPIO_THREAD_CREATE_OPTION_POOL)

/* used by spio_thread_pthread.h, spio_thread_win32.h */
#define SPIO_THREAD_CREATE_OPTION_PRIVATE_1 SPIO_NEXT_BIT(SPIO_THREAD_CREATE_OPTION_CANCELLABLE)
#define SPIO_THREAD_CREATE_OPTION_PRIVATE_2 SPIO_NEXT_BIT(SPIO_THREAD_CREATE_OPTION_PRIVATE_1)
#define SPIO_THREAD_CREATE_OPTION_PRIVATE_3 SPIO_NEXT_BIT(SPIO_THREAD_CREATE_OPTION_PRIVATE_2)

extern spio_t_error_code spio_task_create(spio_t_bits options, spio_t_thread_funcion *start_routine, void *arg, spio_t_task **ptask);

#if SPIO_TASK_PTHREAD_CANCEL_HANDLING
/* in spio_thread_pthread.c */
extern void spio_task_pthread_cancel_enable(void);
extern void spio_task_pthread_cancel_disable(void);
#define SPIO_TASK_PTHREAD_CANCEL_ENABLE() spio_task_pthread_cancel_enable()
#define SPIO_TASK_PTHREAD_CANCEL_DISABLE() spio_task_pthread_cancel_disable()
#else  /* !SPIO_TASK_PTHREAD_CANCEL_HANDLING */
#define SPIO_TASK_PTHREAD_CANCEL_ENABLE() /* empty */
#define SPIO_TASK_PTHREAD_CANCEL_DISABLE() /* empty */
#endif  /* !SPIO_TASK_PTHREAD_CANCEL_HANDLING */

extern spio_t_error_code spio_thread_self(spio_t_thread_id *pself);

extern spio_t_error_code spio_init_thread(spio_t_bits options);


#if SPIO_USE_PTHREADS
#include <pthread.h>

#define SPIO_TASK_CLEANUP_PUSH(TASK, FUN, COOKIE) pthread_cleanup_push((FUN), ((TASK)==0 ? (COOKIE) : (COOKIE))) /* also quiet compiler about unused TASK. */
#define SPIO_TASK_CLEANUP_POP(DOIT) pthread_cleanup_pop((DOIT))
#elif SPIO_WIN32
typedef void spio_t_task_cleanup_fun(void*);

#define SPIO_TASK_CLEANUP_PUSH(TASK, FUN, COOKIE) {                     \
    spio_t_task * const spio_task_cleanup_push_task_ = (TASK);          \
    spio_t_task_cleanup_fun * const spio_task_cleanup_push_fun_ = (FUN); \
    void * const spio_task_cleanup_push_cookie_ = (COOKIE);             \
    (void) 1;

#define SPIO_TASK_CLEANUP_POP(DOIT)                                     \
    if ((DOIT) != 0)                                                    \
      {                                                                 \
        (void)spio_task_cleanup_push_task_;                             \
        spio_task_cleanup_push_fun_(spio_task_cleanup_push_cookie_);    \
      }                                                                 \
    }
#endif  /* SPIO_WIN32 */

#endif  /* SPIO_THREAD_H_INCLUDED */
