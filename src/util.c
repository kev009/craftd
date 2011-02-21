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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <stdarg.h>
#include <signal.h>
#include <syslog.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <event2/buffer.h>

#include "craftd-config.h"
#include "util.h"
#include "craftd.h"

/** Private storage for the console log mask */
static int cd_logConsoleMask = 0;

/**
 * Provides a syslog style setlogmask() used by the console logger.  Usually
 * accessed through the LOG_setlogmask function pointer where setlogmask() and
 * CD_logConsole_setlogmask() can be selected at runtime.
 * 
 * @remarks See man setlogmask(3)
 * 
 * @param mask a bit-field mask of types to block
 * @return the old mask, before setting to the param
 */
int CD_logConsole_setlogmask (int mask)
{
  int oldmask = cd_logConsoleMask;
  
  if (mask != 0) { // POSIX specification
      cd_logConsoleMask = mask;
  }

  return oldmask;
}

/**
 * Provides a syslog() style interface but logs to stdout instead.
 * Usually used from the LOG() function pointer where syslog() or log_console()
 * can be selected at runtime.
 * 
 * @remarks See man syslog(3)
 * 
 * @param priority a syslog format priority designation
 * @param format a syslog format string
 * @param ... arguments for the format string
 */
void log_console(int priority, const char *format, ...)
{
    static const char* names[] = {
        "EMERG", "ALERT", "CRIT", "ERR", "WARNING", "NOTICE", "INFO", "DEBUG"
    };

    va_start(ap, format);
  
    /* Return on MASKed log priorities */
    if (LOG_MASK(priority) & log_consolemask) {
        return;
    }

    if (priority >= (sizeof(names) / sizeof(char*)) || priority < 0) {
        printf("UNKNOWN: ");
    }
    else {
        printf("%s", names[priority]);
    }

    vprintf(format, ap);
    printf("\n");
  
    va_end(ap);
}

/**
 * A callback used by event_set_log_callback() to make libevent use our
 * logging functionality since we do not have stderr after daemonizing
 *
 * @param severity _EVENT_LOG_DEBUG or _EVENT_LOG_ERR
 * @param msg An error string
 */
void
ev_log_callback(int severity, const char *msg)
{
  switch(severity)
  {
    case _EVENT_LOG_DEBUG:
      LOG(LOG_DEBUG, "libevent: %s", msg);
      break;
    case _EVENT_LOG_ERR:
      LOG(LOG_ERR, "libevent: %s", msg);
      break;
    default:
      LOG(LOG_WARNING, "libevent: UNKNOWN/new error type! Please report this");
      LOG(LOG_NOTICE, "libevent/unknown: %s", msg);
      break;
  }
}

/**
 * Check if str is valid MC UTF-8, nonzero otherwise
 * @param str string to check
 * @return 1 if valid, zero otherwise
 */
int
ismc_utf8(const char *str)
{ 
  const char *MC_UTF8 = 
  "\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_'abcdefghi"
  " jklmnopqrstuvwxyz{|}~.???????????????????????????????.?????Ѫ?????????";

  if( strspn(str, MC_UTF8) == strlen(str) )
    return 1; // Valid

  return 0; // Invalid
}

//TODO: static inline
bstring getMCString(struct evbuffer *buf, int16_t len)
{
  bstring str;

  str = bfromcstralloc(len+1, "");
  evbuffer_remove(buf, str->data, len);
  str->data[len] = '\0';
  str->slen = len;

  return str;
}
/**
 * Utility function to create a new output WQ 
 * 
 * @param tmpbuf Pointer an evbuffer containing the data to be written
 * @param player Pointer to the player struct this event descibes
 * @param bev The bufferevent this WQ should write to
 * @param lock Pointer to the lock that should be activated when handling the WQ
 */
void newOutputWq(struct evbuffer *tmpbuf, struct PL_entry *player,struct bufferevent *bev,
		 pthread_rwlock_t *lock)
{
  struct WQ_entry *workitem;
  workitem = Malloc(sizeof(struct WQ_entry));
  workitem->bev = bev;
  workitem->player = player;
  workitem->worktype = WQ_OUTPUT;
  workitem->workdata = tmpbuf;
  workitem->lock = lock;
  
  pthread_mutex_lock(&worker_cvmutex);
  STAILQ_INSERT_TAIL(&WQ_head, workitem, WQ_entries);
  
  pthread_cond_signal(&worker_cv);
  pthread_mutex_unlock(&worker_cvmutex);
}
void newProcessWq(struct PL_entry *player,struct bufferevent *bev,struct WQ_process_data *pdata)
{
  struct WQ_entry *workitem;
  workitem = Malloc(sizeof(struct WQ_entry));
  workitem->bev = bev;
  workitem->player = player;
  workitem->worktype = WQ_PROCESS;
  workitem->workdata = pdata;
  workitem->lock = NULL;
  pthread_mutex_lock(&worker_cvmutex);
  STAILQ_INSERT_TAIL(&WQ_head, workitem, WQ_entries);
  
  pthread_cond_signal(&worker_cv);
  pthread_mutex_unlock(&worker_cvmutex);
}
/**
 * Take an integer and convert it to a base 2 to 36 char representation.
 *
 * Ideas from (http://www.jb.man.ac.uk/~slowe/cpp/itoa.html) and K&R pg. 64
 *
 * @param value integer to convert
 * @param result string representation
 * @param base conversion base, 2 to 36
 * @return pointer to string representation
 */
char *itoa(int value, char *result, int base)
{
  int tmp_value;
  char *ptr = result;
  char *ptr1 = result, tmp_char;

  assert(base > 2 || base < 36);

  do
  {
    tmp_value = value;
    value /= base;
    *ptr++ = "zyxwvutsrqponmlkjihgfedcba9876543210123456789abcdefghijklmnopqr"
             "stuvwxyz" [35 + (tmp_value - value * base)];
  }
  while (value);

  /* Add a negative sign if needed */
  if(tmp_value < 0)
    *ptr++ = '-';
  *ptr-- = '\0';
  while (ptr1 < ptr)
  {
    tmp_char = *ptr;
    *ptr-- = *ptr1;
    *ptr1++ = tmp_char;
  }
  return result;
}

/**
 * Code to fork, close extra FDs, become session leader, etc.
 */
int
CRAFTD_daemonize(int nochdir, int noclose)
{
  int fd0, fd1, fd2;
  int status = 0;

  /* Switch to syslog */
  LOG = &syslog;
  LOG_setlogmask = &setlogmask;

  /* Set file creation mask */
  umask(117); // u+rw, g+rw

  /* Fork and kill the parent. */
  status = fork();
  if (status < 0)
    PERR("Daemonize: Cannot fork!");
  else if (status != 0) // Quit the parent process
    _exit(0);
  // Else we are the forked child
  
  /* Prevent future opens from attaching TTYs and fork again to be safe */
#ifdef HAVE_STRUCT_SIGACTION_SA_HANDLER
  struct sigaction sa;
  sa.sa_handler = SIG_IGN; 
  sigaction(SIGCHLD, &sa, NULL);
  sigaction(SIGPIPE, &sa, NULL);
  sigaction(SIGTSTP, &sa, NULL);
  sigaction(SIGTTOU, &sa, NULL);
  sigaction(SIGTTIN, &sa, NULL);
#else
  signal(SIGCHLD, SIG_IGN);
  signal(SIGPIPE, SIG_IGN);
  signal(SIGTSTP, SIG_IGN);
  signal(SIGTTOU, SIG_IGN);
  signal(SIGTTIN, SIG_IGN);
#endif

  // TODO: handle HUP, term

  status = fork();
  if (status < 0)
    ERR("Daemonize: Cannot fork!");
  else if (status != 0) // Quit the parent process
    _exit(0);

  /* Become a session leader */
  status = setsid();
  if (status < 0)
    ERR("Daemonize: Error becoming session leader");
  
  if (!nochdir)
  {
    //chdir(Config.rootdir); // TODO: switch to $prefix/datadir?
    status = chdir("/");
    if (status != 0)
      ERR("Daemonize: error changing directories");
  }

  if (!noclose)
  {
    /* Close standard streams */
    close(0); // stdin
    close(1); // stdout
    close(2); // stderr

    /* Redirect standard streams to /dev/null */
    fd0 = open("/dev/null", O_RDWR);
    fd1 = dup(0);
    fd2 = dup(0);
  }

  return 0;
}

/**
 * A temporary libevent copyout with positioning and size.
 * Handles iovec striding w/o complecated iovec ptrs at the cost of memcpys
 * 
 * @remarks Look for this in libevent 2.1
 * @param b input buffer
 * @param buf output buffer byte array
 * @param len size of data to copy out
 * @param ptr offset pointer
 */
int
CRAFTD_evbuffer_copyout_from(struct evbuffer *b, void *buf,
                             size_t len, struct evbuffer_ptr *ptr)
{
    struct evbuffer_iovec *v;
    int nvecs, i;
    char *cp = buf;
    int status;

    //evbuffer_enable_locking(b, NULL);
    //evbuffer_lock(b);
    /* TODO: check locking semantics wrt multiple threads */

    if (!ptr)
    {
        status = evbuffer_copyout(b, buf, len);
        //evbuffer_unlock(b);
        if (status < 0)
          return status;
        return 0;
    }

    if (evbuffer_get_length(b) < ptr->pos + len)
    {
      //evbuffer_unlock(b); 
      return EAGAIN;  /* not enough data */
    }

    if (ptr->pos + len < 128) {
        /* XXX Not sure if this optimization helps */
        char tmp[128];
        evbuffer_copyout(b, tmp, ptr->pos + len);
        memcpy(buf, tmp+ptr->pos, len);
        //evbuffer_unlock(b);
        return 0;
    }

    /* determine how many vecs we need */
    nvecs = evbuffer_peek(b, len, ptr, NULL, 0);
    if (nvecs < 1)
    {
        //evbuffer_unlock(b);
        return ENOBUFS;
    }
    v = calloc(sizeof(struct evbuffer_iovec), nvecs);
    if (v == NULL)
    {
        //evbuffer_unlock(b);
        return ENOBUFS;
    }

    if (evbuffer_peek(b, len, ptr, v, nvecs) < 0)
    {
        //evbuffer_unlock(b);
        free(v);
        return ENOBUFS; /* should be impossible, but let's take no chances */
    }

    for (i = 0; i < nvecs; ++i) {
        if (v[i].iov_len <= len) {
            memcpy(cp, v[i].iov_base, v[i].iov_len);
            cp += v[i].iov_len;
            len -= v[i].iov_len;
        } else {
            memcpy(cp, v[i].iov_base, len);
            break;
        }
    }

    //evbuffer_unlock(b);
    free(v);
    return 0;
}
