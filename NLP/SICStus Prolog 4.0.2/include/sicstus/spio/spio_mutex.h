#ifndef SPIO_MUTEX_H_INCLUDED
#define SPIO_MUTEX_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"

struct spio_t_mutex_;           /* opaque */
typedef struct spio_t_mutex_ spio_t_mutex;

extern spio_t_error_code spio_mutex_new(spio_t_mutex **pthread, spio_t_bits options);
extern void spio_mutex_free(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_lock(spio_t_mutex *mutex);
extern spio_t_error_code spio_mutex_unlock(spio_t_mutex *mutex);

extern void spio_mutex_unlock_cleanup(void *arg); /* for use with SPIO_TASK_CLEANUP_PUSH */
#endif  /* SPIO_MUTEX_H_INCLUDED */
