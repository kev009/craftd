/*
 * Copyright (c) 2010 Kevin M. Bowling, <kevin.bowling@kev009.com>, USA
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
#include <syslog.h>

#include <event2/buffer.h>

#include "craftd-config.h"
#include "util.h"

// Logging Routines
/** Private storage for the console log mask */
static int log_consolemask = 0;

/**
 * Provides a syslog style setlogmask() used by the console logger.  Usually
 * accessed through the LOG_setlogmask function pointer where setlogmask() and
 * log_console_setlogmask() can be selected at runtime.
 * 
 * @remarks See man setlogmask(3)
 * 
 * @param mask a bit-field mask of types to block
 * @return the old mask, before setting to the param
 */
int log_console_setlogmask(int mask)
{
  int oldmask = log_consolemask;
  if(mask == 0) // POSIX specification
    return oldmask;
  log_consolemask = mask;
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
  va_list arglist;
  const char *loglevel;
  
  /* Return on MASKed log priorities */
  if (LOG_MASK(priority) & log_consolemask)
    return;
  
  switch(priority)
  {
    case LOG_ALERT:
      loglevel = "ALERT: ";
      break;
    case LOG_CRIT:
      loglevel = "CRIT: ";
      break;
    case LOG_DEBUG:
      loglevel = "DEBUG: ";
      break;
    case LOG_EMERG:
      loglevel = "EMERG: ";
      break;
    case LOG_ERR:
      loglevel = "ERR: ";
      break;
    case LOG_INFO:
      loglevel = "INFO: ";
      break;
    case LOG_NOTICE:
      loglevel = "NOTICE: ";
      break;
    case LOG_WARNING:
      loglevel = "WARNING: ";
      break;
    default:
      loglevel = "UNKNOWN: ";
      break;
  }
  
  printf("%s", loglevel);
  vprintf(format, arglist);
  printf("\n");
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
  " jklmnopqrstuvwxyz{|}~.ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜø£Ø×.áíóúñÑªº¿®¬½¼¡«»";

  if( strspn(str, MC_UTF8) == strlen(str) )
    return 1; // Valid

  return 0; // Invalid
}

/*
 * MC Strings always have known lengths.  Take advantage of this and try to
 * create infallible strings.  Critique and review of this code needed.
 *
 * Possibly improvement: switch to strnlen and include autoconf check/gnulib compat
 */

/**
 * Create an mcstring from a slen and character array.  The character array does
 * not need to be NUL terminated, but slen MUST EQUAL the number of characters
 * not including a NUL terminator.  strlen provides this behavior for internal
 * C strings.
 *
 * @param slen string length, not including NUL terminator
 * @param strptr input string
 * @return a ptr to an mcstring structure or null
 */
mcstring_t *
mcstring_create(int16_t slen, const char *strp)
{
  char *strin = NULL;

  if (slen > MC_MAXSTRING)
    goto MCSTRING_CREATE_ERR; // LOG bad string

  strin = (char *)Malloc(slen+1);
  /* NUL terminate the end.  If strnlen doesn't match slen, something
   * strange is going on with the client.
   */
  strin[slen] = '\0';
  memcpy(strin, strp, slen);

  if (strlen(strin) != slen)
    goto MCSTRING_CREATE_ERR; // LOG bad string

  if (!ismc_utf8(strin))
    goto MCSTRING_CREATE_ERR; // LOG bad string

  mcstring_t *mcstring;
  mcstring = (mcstring_t *)Malloc(sizeof(mcstring_t));
  mcstring->slen = slen;
  mcstring->str = strin;
  mcstring->valid = 1;
  
  /* return a valid mcstring construction */
  return mcstring;

MCSTRING_CREATE_ERR:
  if(strin)
    free(strin);

  return NULL;
}

/**
 * Copy an allocated mcstring to another.  Resizes the string buffer according
 * to the source's size.
 * 
 * @param dest destination mcstring
 * @param src source mcstring
 * @return pointer to destination mcstring
 */
mcstring_t *
mcstring_copy(mcstring_t *dest, mcstring_t *src)
{
  /* The string must be allocated or on the stack.  Do a sanity check */
  assert(dest && src && src->str);
  
  if (dest->slen != src->slen)
  {
    dest->str = Realloc(dest->str, src->slen);
    dest->slen = src->slen;
  }
  
  memcpy(dest->str, src->str, dest->slen);
  
  dest->valid = src->valid;
  
  return dest;
}

/**
 * Allocate a string of known length useful for inbound packets.  This string is
 * invalid until it has been checked with mcstring_valid().
 *
 * @param slen length short
 * @return space for a slen mcstring
 */
mcstring_t *
mcstring_allocate(int16_t slen)
{
  if (slen > MC_MAXSTRING)
    return NULL;

  char *strin;
  strin = (char *)Malloc(slen+1);

  strin[slen] = '\0';

  mcstring_t *mcstring;
  mcstring = (mcstring_t *)Malloc(sizeof(mcstring_t));
  mcstring->slen = slen;
  mcstring->str = strin;
  mcstring->valid = 0;

  /* return a preallocated invalid construction -  fill and verify */
  return mcstring;
}

// TODO swap out for stdbool
/**
 * A pullup function to validate preallocated strings or manually constructed
 * strings that use the exposed struct interface.
 *
 * Undefined behavior if str is not NUL terminated.
 *
 * Side effect: set valid to 1 if the string is valid
 *
 * @param mcstring string to verify
 * @retrun 1 if valid, zero on error
 */
int
mcstring_valid(mcstring_t *mcstring)
{

  if(mcstring->slen > MC_MAXSTRING)
    return 0;
  if(!ismc_utf8(mcstring->str))
    return 0;
  if(mcstring->slen != strlen(mcstring->str))
    return 0;

  /* mcstring is valid */
  mcstring->valid = 1;
  return 1;
}

/**
 * Concat a C string onto an mcstring.
 *
 * src is a C string, use only for internal NUL terminated messages.
 *
 * @param dest mcstring
 * @param src C string
 * @param size src size
 * @return 0 if valid, nonzero on error
 */
mcstring_t *
mcstring_ncat(mcstring_t *dest, const char *src, size_t size)
{
  mcstring_t *mcnew;
  int16_t newlen = size + dest->slen;
  mcnew = mcstring_allocate(newlen);
  if(mcnew == NULL)
    return NULL;
  memcpy(dest->str, src, newlen);
  if(!mcstring_valid(mcnew))
    return NULL;

  mcstring_free(dest); /* should we free here? both, neither? */

  return mcnew;
}

/**
 * Concat two mcstrings.  Returns a pointer to the new string and frees the old
 * dest.
 *
 * @param dest mcstring to append to
 * @param src mcstring to append
 * @return new mcstring
 */
mcstring_t *
mcstring_mccat(mcstring_t *dest, mcstring_t *src)
{
  mcstring_t *mcnew;
  int16_t newlen = dest->slen + src->slen;
  mcnew = mcstring_allocate(newlen);
  if(mcnew == NULL)
    return NULL;
  memcpy(dest->str, src->str, newlen);
  if(!mcstring_valid(mcnew))
    return NULL;

  mcstring_free(dest); /*XXX should we free here?  both, neither? */

  return mcnew;
}
/**
 * Free an mcstring instance.
 *
 * @param mcstring string to free
 */
void
mcstring_free(mcstring_t *mcstring)
{
  if(!mcstring)
    return;
  
  if(mcstring->str)
    free(mcstring->str);
  
  free(mcstring);
}

/**
 * Simple calloc wrapper w/error handling
 *
 * @param num number of objects to allocate
 * @param size size of each object
 * @return valid pointer to heap memory
 */
void *
Calloc(size_t num, size_t size)
{
  void *ptr;
  if ( (ptr = calloc(num, size)) == NULL )
    perror("calloc null ptr error!");
  return ptr;
}

/**
 * Simple malloc wrapper w/error handling
 *
 * @param size allocation size
 * @return valid pointer to heap memory
 */
void *
Malloc(size_t size)
{
  void *ptr;
  if ( (ptr = malloc(size)) == NULL )
    perror("malloc null ptr error!");
  return ptr;
}

/**
 * Simple realloc wrapper w/error handling.  Mimics glibc's implementation
 * 
 * @param ptr pointer to the heap address to reallocate
 * @param size reallocation size
 * @return resized valid pointer to heap memory
 */
void *
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
      perror("realloc null ptr error!");
    return newptr;
  }
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

    evbuffer_enable_locking(b, NULL);
    //evbuffer_lock(b);
    /* TODO: check locking semantics wrt multiple threads */

    if (!ptr)
    {
        status = evbuffer_copyout(b, buf, len);
        evbuffer_unlock(b);
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
