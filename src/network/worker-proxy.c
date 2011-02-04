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

#include "network/network.h"
#include "network/network-private.h"

int workerproxy(uint8_t pkttype, size_t pktlen, struct WQ_entry *workitem)
{
      if(workitem->worktype == WQ_GAME)
      {
	  LOG(LOG_DEBUG,"Recieved packet type: %d from client",pkttype);
	  if(process_isproxypassthrough(pkttype) && workitem->player->sev)
	  {
	    bufferevent_lock(workitem->player->sev);
	    evbuffer_remove_buffer(bufferevent_get_input(workitem->bev),
				   bufferevent_get_output(workitem->player->sev),pktlen);
	    bufferevent_unlock(workitem->player->sev);
	  }
          else
	  {
	    //if(player->sev)
	      //bufferevent_lock(player->sev);
	    void *packet = packetdecoder(pkttype, pktlen, workitem->bev);
	    //evbuffer_lock(output);
	    process_proxypacket(workitem->player,pkttype,packet);
	    //evbuffer_unlock(output);
	    packetfree(pkttype,packet);
	    //if(player->sev)
	      //bufferevent_unlock(player->sev);
	  }
      }
      else if(workitem->worktype == WQ_PROXY)
      {
	LOG(LOG_DEBUG,"Recieved packet %d from server",pkttype);
	bufferevent_lock(workitem->player->sev);
	if(process_isproxyserverpassthrough(pkttype))
	{
	    evbuffer_remove_buffer(bufferevent_get_input(workitem->bev),bufferevent_get_output(workitem->player->bev),pktlen);
	}
	else
	{
	  void *packet = packetdecoder(pkttype, pktlen, workitem->bev);
	  //evbuffer_lock(output);
	  process_proxyserverpacket(workitem->player,pkttype,packet);
	  //evbuffer_unlock(output);
	  packetfree(pkttype,packet);
	}
	bufferevent_unlock(workitem->player->sev);
      }
      else {
        return -1;
      }
     return 1;
}
