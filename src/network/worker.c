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

#include <stdio.h>
#include <stdlib.h>
#include <sys/queue.h>
#include <pthread.h>
#include "util.h"
#include "network/network.h"
#include "network/network-private.h"

/*
 * The length and decoder state machines could possibly use callbacks/fuction
 * pointers?  Might have advantages for threading.
 */

/* TODO: use ev_addbuffer/removebuffer for efficiency!
 * Use more zero copy I/O and peeks if possible
 */

/* Forward declarations */
void processLogoutcb(evutil_socket_t fd, short what, void *arg);

/* Private instance variables */
//static pthread_barrier_t logoutbarrier;
static volatile int logouts = 0;

void
worker_init()
{
  /* TODO move the rest of the startup code out of craftd.c to here */
}

void
deferLogout(struct PL_entry *player)
{
  ++logouts;
  
  struct event *logoutev;
  struct timeval now = {0,0};
  
  logoutev = evtimer_new(base, processLogoutcb, player);
  event_add(logoutev, &now);
}

void
processLogoutcb(evutil_socket_t fd, short what, void *arg)
{
  struct PL_entry *player = (struct PL_entry *) arg;
  LOG(LOG_INFO, "processed logout cb");
  errorcb(player->bev, BEV_EVENT_EOF, player);
  --logouts;
}

void
waitLogout()
{
  if ((volatile int)logouts != 0)
    while((volatile int)logouts != 0)
      ; // Spin
}

/**
 * This is the worker thread's main function and performs packet length
 * detection, decoding, and response.  Work is passed in through the Work
 * Queue structure.  On errors, we disconnect the client.
 * 
 * @param arg void cast pointer to the thread's worker ID (int)
 * @return NULL
 */
void 
*run_worker(void *arg)
{
  int id = *(int *)arg;
  LOG(LOG_INFO, "Worker %d started!", id);
  
  struct bufferevent *bev;
  struct evbuffer *input;
  struct WQ_entry *workitem;
  struct PL_entry *player;
  size_t inlen;
  int pktlen;
  uint8_t pkttype;
  
  for(;;)
  {
    pthread_mutex_lock(&worker_cvmutex);
    
    /* Check our predicate again:  The WQ is not empty
     * Prevent a nasty race condition if the client disconnects
     * Works in tandem with errorcb FOREACH bev removal loop
     */
    while (STAILQ_EMPTY(&WQ_head))
    {
      LOGT(LOG_DEBUG, "Worker %d ready", id);
      pthread_cond_wait(&worker_cv, &worker_cvmutex);
    }
    
    
    LOGT(LOG_DEBUG, "in worker: %d", id);
    
    /* Pull work item */
    workitem = STAILQ_FIRST(&WQ_head);
    STAILQ_REMOVE_HEAD(&WQ_head, WQ_entries);
    pthread_mutex_unlock(&worker_cvmutex);

    bev = workitem->bev;
    player = workitem->player;
    if(workitem->lock!=NULL) //this will auto lock any type of WQ
      pthread_rwlock_wrlock(workitem->lock);
    
    if(workitem->worktype==WQ_GAME_INPUT||workitem->worktype==WQ_PROXY_INPUT)
    {
      if (bev == NULL || player == NULL)
      {
	LOG(LOG_CRIT, "Aaack, null bev or ctx in worker?");
	goto WORKER_ERR;
      }

      /* Do work */
      input = bufferevent_get_input(bev);
      
      inlen = evbuffer_get_length(input);
      
      /* Check another predicate: make sure there is work to do in case of
      * spurious wakeups
      */
      if (inlen == 0)
	goto WORKER_DONE;
      
      /* Drain the buffer in a loop to take care of compound packets.
      * We add pktlen for each iteration until inlen is reached or we jump
      * to the EAGAIN handler.
      */
      size_t processed = 0;
      do
      {
	evbuffer_copyout(input, &pkttype, 1);
	//LOG(LOG_DEBUG,"got packet %d",pkttype);
	pktlen = len_statemachine(pkttype, input);
	
	/* On exception conditions, negate the return value to get correct errno */
	if (pktlen < 0)
	{
	  pktlen = -pktlen;  /* NOTE: Inverted to get error states! */
	  if (pktlen == EAGAIN)
	  {
	    /* recvd a fragment, wait for another event */
	    LOGT(LOG_DEBUG,"EAGAIN");
	    goto WORKER_DONE;
	  }
	  else if (pktlen == EILSEQ)
	  {
	    /* recvd an packet that does not match known parameters
	    * Punt the client and perform cleanup
	    */
	    LOG(LOG_ERR, "EILSEQ in recv buffer!, pkttype: 0x%.2x", pkttype);
	    goto WORKER_ERR;
	  }
	  
	  perror("unhandled readcb error");
	}
	
	/* Invariant: else we received a full packet of pktlen */
	
	if(!worker_handler(pkttype,pktlen,workitem))
	  goto WORKER_ERR;
	
	/* On decoding errors, punt the client for now */
	
	/* Remove this temporarally until we have a sane way to handle decoder problems
	if (status != 0)
	{
	  LOG(LOG_ERR, "Decode error, punting client.  errno: %d", status);
	  goto WORKER_ERR;
	}*/
	
	processed += pktlen;
      }
      while(processed < inlen);
    }
    else if (workitem->worktype == WQ_OUTPUT)
    {
      //LOG(LOG_DEBUG,"Sent output");
      evbuffer_add_buffer(bufferevent_get_output(workitem->bev),workitem->workdata);
      evbuffer_free(workitem->workdata);
    }
    else if(!worker_handler((uint8_t)NULL,0,workitem))
      goto WORKER_ERR;

WORKER_DONE:
    /* On success or EAGAIN, free the work item and clear the worker */
    if(workitem->lock!=NULL)
      pthread_rwlock_unlock(workitem->lock);
    free(workitem);
    waitLogout();
    continue;

WORKER_ERR:
    /* On exception, remove all client allocations in correct order */

    if(workitem->lock!=NULL)
      pthread_rwlock_unlock(workitem->lock);
    free(workitem);

    bstring wmsg = bfromcstr("Error in work thread!");
    send_kick(player, wmsg);
    bstrFree(wmsg);
    
    waitLogout();
    
    continue;
  }

  return NULL;
}
