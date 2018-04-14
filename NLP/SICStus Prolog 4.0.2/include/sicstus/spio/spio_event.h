#ifndef SPIO_EVENT_H_INCLUDED
#define SPIO_EVENT_H_INCLUDED 1
#include "spio_types.h"
#include "spio_errors.h"

typedef struct spio_t_event_ spio_t_event;

typedef struct spio_t_event_funcs_ spio_t_event_funcs;
struct spio_t_event_funcs_ {
  /* All these must be thread safe (FIXME: check this) */
  spio_t_error_code (*set)(spio_t_event *event);
  spio_t_error_code (*reset)(spio_t_event *event);
  spio_t_error_code (*query)(spio_t_event *event);
  void (*release)(spio_t_event *event);
};

struct spio_t_event_ {
  spio_t_event_funcs *funcs;
#define SPIO_EVENT_FLAG_READONLY_ SPIO_BIT(0) /* cannot set or reset (not yet available) */
#define SPIO_EVENT_FLAG_PRIVATE1 SPIO_NEXT_BIT(SPIO_EVENT_FLAG_READONLY_)
#define SPIO_EVENT_FLAG_PRIVATE2 SPIO_NEXT_BIT(SPIO_EVENT_FLAG_PRIVATE1)
#define SPIO_EVENT_FLAG_PRIVATE3 SPIO_NEXT_BIT(SPIO_EVENT_FLAG_PRIVATE2)
#define SPIO_EVENT_FLAG_PRIVATE4 SPIO_NEXT_BIT(SPIO_EVENT_FLAG_PRIVATE3)
#define SPIO_EVENT_FLAG_PRIVATE5 SPIO_NEXT_BIT(SPIO_EVENT_FLAG_PRIVATE4)
  spio_t_bits flags;
  /* event-type specific data */
};

extern spio_t_error_code spio_event_set(spio_t_event *event);
extern spio_t_error_code spio_event_reset(spio_t_event *event);
extern spio_t_error_code spio_event_query(spio_t_event *event);
extern spio_t_error_code spio_event_wait(spio_t_event *events[], size_t nevents, spio_t_timespec *timeout);

extern spio_t_error_code spio_init_event(spio_t_bits options);

#define SPIO_EVENT_NEW_OPTION_SET SPIO_BIT(0) /* create the event set */
extern spio_t_error_code spio_event_new(spio_t_bits options, spio_t_event **pevent);
/* MUST BE THREAD SAFE */
extern void spio_event_free(spio_t_event *event);

#endif  /* SPIO_EVENT_H_INCLUDED */
