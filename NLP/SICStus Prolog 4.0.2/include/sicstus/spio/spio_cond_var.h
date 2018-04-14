#ifndef SPIO_COND_VAR_H_INCLUDED
#define SPIO_COND_VAR_H_INCLUDED 1

#include "spio_types.h"
#include "spio_errors.h"
#include "spio_mutex.h"

struct spio_t_cond_var_;           /* opaque */
typedef struct spio_t_cond_var_ spio_t_cond_var;

extern spio_t_error_code spio_cond_var_new(spio_t_cond_var **pthread, spio_t_bits options);
extern void spio_cond_var_free(spio_t_cond_var *cond_var);
extern spio_t_error_code spio_cond_var_wait(spio_t_cond_var *cv, spio_t_mutex *mutex);

/* timeouts < 24h are taken to be relative, otherwise absolute */
extern spio_t_error_code spio_cond_var_timedwait(spio_t_cond_var *cond, spio_t_mutex *mutex, spio_t_timespec *timeout);

/* NOTE: the caller must lock mutex around the call to spio_cond_var_signal (unlike pthread_cond_signal) */
#define SPIO_COND_VAR_SIGNAL_OPTION_BROADCASE SPIO_BIT(0)
extern spio_t_error_code spio_cond_var_signal(spio_t_cond_var *cv, spio_t_mutex *mutex, spio_t_bits options);

extern spio_t_error_code spio_cond_var_now(spio_t_timespec *now);

extern spio_t_error_code spio_init_module_cond_var(spio_t_bits options);

#endif  /* SPIO_COND_VAR_H_INCLUDED */
