
#ifndef CRAFTD_UTIL_H
#define CRAFTD_UTIL_H

/*
 * Copyright (c) 2010-2011 Kevin M. Bowling, <kevin.bowling@kev009.com>, USA
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <config.h>

#include <errno.h>
#include <stdint.h>
#include <syslog.h>
#include <stdarg.h>
#include <stdlib.h>
#include <pthread.h>
#include "craftd.h"
#include <sys/queue.h>
#include <event2/event.h>
#include <event2/buffer.h>
#include <event2/bufferevent.h>
#include <event2/bufferevent_struct.h>
#include <event2/thread.h>

#include "bstrlib.h"



/**
 * Define MC data types 
 */
typedef int8_t MCbyte;
typedef int16_t MCshort;
typedef int32_t MCint;
typedef int64_t MClong;
typedef float MCfloat;
typedef double MCdouble;
typedef struct MCitem
{
  MCshort itemid;
  MCbyte count;
  MCshort uses;
} MCitem;

/**
 * Global logging and error handling macros
 */
/* Function pointer to the current syslog() style logging routine */
void (*LOG)(int, const char *, ...);
int (*LOG_setlogmask)(int);

/* Console logging routines with syslog() style interface */
void log_console(int priority, const char *format, ...);
int log_console_setlogmask(int mask);

/* Trace logging wrapper */
#ifdef TRACE
#define LOGT(...) \
  do { LOG(__VA_ARGS__); } while (0)
#else
#define LOGT(...) \
  do { ; } while(0)
#endif

/* Fatal errors */
#define PERR(msg) \
  do { perror(msg); exit(EXIT_FAILURE); } while (0)
#define ERR(...) \
  do { LOG(LOG_CRIT, __VA_ARGS__); exit(EXIT_FAILURE); } while (0)

/* Override libevent error reporting to use our interface */
void ev_log_callback(int severity, const char *msg);

/* Public mcstring functiions */
int ismc_utf8(const char *str);
bstring getMCString(struct evbuffer *buf, int16_t len);

/* Public utility functions */
char *itoa(int value, char *result, int base);

/**
 * Utility function to create a WQ_*_output request
 */
void newOutputWq(struct evbuffer *tmpbuf, struct PL_entry *player, struct bufferevent *bev,
		 pthread_rwlock_t *lock);
void newProcessWq(struct PL_entry *player,struct bufferevent *bev, struct WQ_process_data *pdata);

/* Public memory management wrappers */
/**
 * Simple calloc wrapper w/error handling
 *
 * @param num number of objects to allocate
 * @param size size of each object
 * @return valid pointer to heap memory
 */
static inline void *
Calloc(size_t num, size_t size)
{
  void *ptr;
  if ( (ptr = calloc(num, size)) == NULL )
    ERR("calloc null ptr error!");
  return ptr;
}

/**
 * Simple malloc wrapper w/error handling
 *
 * @param size allocation size
 * @return valid pointer to heap memory
 */
static inline void *
Malloc(size_t size)
{
  void *ptr;
  if ( (ptr = malloc(size)) == NULL )
    ERR("malloc null ptr error!");
  return ptr;
}

/**
 * Simple realloc wrapper w/error handling.  Mimics glibc's implementation
 * 
 * @param ptr pointer to the heap address to reallocate
 * @param size reallocation size
 * @return resized valid pointer to heap memory
 */
static inline void *
Realloc(void *ptr, size_t size)
{
  void *newptr;
  
  if (ptr == NULL)
    return Malloc(size);
  else if (size == 0)
  {
    free(ptr);
    return NULL;
  }
  else
  {
    if( (newptr = realloc(ptr, size)) == NULL)
      ERR("realloc null ptr error!");
    return newptr;
  }
}

/* Daemonize implementation */
int CRAFTD_daemonize(int nochdir, int noclose);

/* Public evbuffer utility */
int CRAFTD_evbuffer_copyout_from(struct evbuffer *b, void *buf,
                             size_t len, struct evbuffer_ptr *ptr);

#endif
