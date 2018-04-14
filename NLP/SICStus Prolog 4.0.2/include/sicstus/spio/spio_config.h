/* -*- Mode: C; -*- */
#ifndef SPIO_CONFIG_H_WIN32_INCLUDED
#define SPIO_CONFIG_H_WIN32_INCLUDED

#if !SICSTUS_CONFIG_H
#if HAVE_CONFIG_H
#include "config.h"
#endif  /* HAVE_CONFIG_H */
#endif  /* !SICSTUS_CONFIG_H */

/***************** Propagage from sicstus config.h to SPIO ****************************/
/* All things taken from SICSTus config.h must be in the PUBLIC section of config.h! */

#if SP_BIGENDIAN
#define SPIO_BIGENDIAN 1
#else  /* !SP_BIGENDIAN */
#define SPIO_BIGENDIAN 0
#endif  /* !SP_BIGENDIAN */

/* NOTE: The SPIO_DEBUG logic must be synced with config.h.unix etc */
#ifndef SPIO_DEBUG
#if DBG
#define SPIO_DEBUG 1
#endif  /* DBG */
#endif  /* SPIO_DEBUG */

#ifndef SPIO_DEBUG
#if SICSTUS_BETA_VERSION
#define SPIO_DEBUG 1
#endif  /* SICSTUS_BETA_VERSION */
#endif  /* SPIO_DEBUG */


/***************************************************************/

#include "spio_version.h"

#define SPIO_WIN32 1

/* *** FEATURES *** */

#define SPIO_USE_WIDTH_TABLES 0 /* width tables (used for seeking on text streams) does not yet work */

#define SPIO_USE_WIN32_THREADS 1

#define SPIO_USE_SPIO_EVENT_WIN32 1

#define SPIO_CDECL __cdecl

#ifndef SPIO_HAVE_TASK_CANCEL_EVENT
/* [PM] 4.0.2 */
#define SPIO_HAVE_TASK_CANCEL_EVENT 1

#endif  /* SPIO_HAVE_TASK_CANCEL_EVENT */



#endif  /* SPIO_CONFIG_H_WIN32_INCLUDED */
