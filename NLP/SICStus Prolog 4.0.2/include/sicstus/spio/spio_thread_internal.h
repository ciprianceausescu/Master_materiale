#ifndef SPIO_THREAD_INTERNAL_H_INCLUDED
#define SPIO_THREAD_INTERNAL_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"
#include "spio_event.h"

#include "spio_thread.h"

struct spio_t_task_ {
  spio_t_thread_funcion *fun;
  void *arg;
  void *result;
  spio_t_event *event;          /* signalled when thread terminates (fun has returned) */
#if SPIO_HAVE_TASK_CANCEL_EVENT
  spio_t_event *cancel_event;   /* signalled when someone wants to cancel the task (spio_task_cancel_pthread) */
#endif  /* SPIO_HAVE_TASK_CANCEL_EVENT */

#define SPIO_TASK_FLAG_CANCELLABLE_ SPIO_BIT(0)
#if 1
/* 4.0.2 Now support pseudo-cancellation also on Win32 */
#ifndef SPIO_TASK_FLAG_CANCELLABLE
#define SPIO_TASK_FLAG_CANCELLABLE SPIO_TASK_FLAG_CANCELLABLE_
#endif
#else  /* 0 */
#if SPIO_TASK_PTHREAD_CANCEL_HANDLING
#ifndef SPIO_TASK_FLAG_CANCELLABLE
#define SPIO_TASK_FLAG_CANCELLABLE SPIO_TASK_FLAG_CANCELLABLE_
#endif
#endif  /* SPIO_TASK_PTHREAD_CANCEL_HANDLING */
#endif  /* 0 */

#define SPIO_TASK_FLAG_CANCELLED SPIO_NEXT_BIT(SPIO_TASK_FLAG_CANCELLABLE_) /* has cancelled */
#define SPIO_TASK_FLAG_CANCELLING SPIO_NEXT_BIT(SPIO_TASK_FLAG_CANCELLED) /* has been asked to cancel by main thread */

#define SPIO_TASK_FLAG_PRIVATE_1 SPIO_NEXT_BIT(SPIO_TASK_FLAG_CANCELLING)
#define SPIO_TASK_FLAG_PRIVATE_2 SPIO_NEXT_BIT(SPIO_TASK_FLAG_PRIVATE_1)
#define SPIO_TASK_FLAG_PRIVATE_3 SPIO_NEXT_BIT(SPIO_TASK_FLAG_PRIVATE_2)
#define SPIO_TASK_FLAG_PRIVATE_4 SPIO_NEXT_BIT(SPIO_TASK_FLAG_PRIVATE_3)

  /* Only the main thread may modify the flags */
  spio_t_bits flags;

#define SPIO_TASK_THREAD_FLAG_EXITING SPIO_BIT(0) /* [PM] 4.0.1+ Thread is about to exit/has exited and will soon not access task structure anymore */
#define SPIO_TASK_THREAD_FLAG_IN_THREAD_FUNCTION SPIO_BIT(1) /* DBG */
  /* Only the thread may modify the thread_flags (unless the thread
     has exited, in which case the main thread owns thread_flags
     too). */
  spio_t_bits thread_flags;
};

extern spio_t_error_code spio_task_init(spio_t_thread_funcion *fun, void *arg, spio_t_bits options, spio_t_task *task);
extern void spio_task_deinit(spio_t_task *task);


#endif  /* SPIO_THREAD_INTERNAL_H_INCLUDED */
