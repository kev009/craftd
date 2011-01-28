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

#include "network.h"
#include "network-private.h"

/*
 * The length and decoder state machines could possibly use callbacks/fuction
 * pointers?  Might have advantages for threading.
 */

/* TODO: use ev_addbuffer/removebuffer for efficiency!
 * Use more zero copy I/O and peeks if possible
 */

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
  struct evbuffer *input, *output;
  struct WQ_entry *workitem;
  struct PL_entry *player;
  size_t inlen;
  void * packet;
  int pktlen;
  uint8_t pkttype;
  
  for(;;)
  {
    pthread_mutex_lock(&worker_cvmutex);
    
    /* Check our predicate again:  The WQ is not empty
     * Prevent a nasty race condition if the client disconnects
     * Works in tandem with errorcb FOREACH bev removal loop
     */
    do
    {
      LOGT(LOG_DEBUG, "Worker %d ready", id);
      pthread_cond_wait(&worker_cv, &worker_cvmutex);
    }
    while (STAILQ_EMPTY(&WQ_head));
    
    LOGT(LOG_DEBUG, "in worker: %d", id);
    
    /* Pull work item */
    workitem = STAILQ_FIRST(&WQ_head);
    STAILQ_REMOVE_HEAD(&WQ_head, WQ_entries);
    pthread_mutex_unlock(&worker_cvmutex);

    bev = workitem->bev;
    player = workitem->player;
    
    if (bev == NULL || player == NULL)
    {
      LOG(LOG_CRIT, "Aaack, null bev or ctx in worker?");
      goto WORKER_ERR;
    }

    /* Do work */
    input = bufferevent_get_input(bev);
    output = bufferevent_get_output(bev);
    bufferevent_lock(bev);
    
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
      
      if(workitem->worktype == WQ_GAME)
      {
	
	if(Config.proxy_enabled)
	{
	  if(process_isproxypassthrough(pkttype) && player->sev)
	  {
	    bufferevent_lock(player->sev);
	    evbuffer_remove_buffer(bufferevent_get_input(bev),
				   bufferevent_get_output(player->sev),pktlen);
	    bufferevent_unlock(player->sev);
	  }else
	  {
	    //if(player->sev)
	      //bufferevent_lock(player->sev);
	    packet = packetdecoder(pkttype, pktlen, bev);
	    process_proxypacket(player,pkttype,packet);
	    packetfree(pkttype,packet);
	    //if(player->sev)
	      //bufferevent_unlock(player->sev);
	  }
	}
	else
	{
	  packet = packetdecoder(pkttype, pktlen, bev);
	  process_packet(player,pkttype,packet);
	  packetfree(pkttype,packet);
	}
	
      }
      else if(workitem->worktype ==WQ_PROXY)
      {
	//LOG(LOG_DEBUG,"Recieved packet %d from server",pkttype);
	bufferevent_lock(player->sev);
	if(process_isproxyserverpassthrough(pkttype))
	{
	  
	  evbuffer_remove_buffer(bufferevent_get_input(player->sev),
				   bufferevent_get_output(player->bev),pktlen);
	  
	}
	else
	{
	  packet = packetdecoder(pkttype, pktlen, bev);
	  process_proxyserverpacket(player,pkttype,packet);;
	  packetfree(pkttype,packet);
	  
	}
	bufferevent_unlock(player->sev);
      } else { goto WORKER_ERR; }
      /* On decoding errors, punt the client for now */
      
      /* Remove this temporarally untill we have a sane way to handle decoder problems
      if (status != 0)
      {
	LOG(LOG_ERR, "Decode error, punting client.  errno: %d", status);
        goto WORKER_ERR;
      }*/
      
      processed += pktlen;
    }
    while(processed < inlen);

WORKER_DONE:
    /* On success or EAGAIN, free the work item and clear the worker */
    bufferevent_unlock(bev);
    free(workitem);
    continue;

WORKER_ERR:
    /* On exception, remove all client allocations in correct order */
    bufferevent_unlock(bev);
    free(workitem);

    bstring wmsg = bfromcstr("Error in packet sequence.");
    send_kick(player, wmsg);
    bstrFree(wmsg);

    continue;
  }

  return NULL;
}
