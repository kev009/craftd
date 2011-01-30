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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>

// For sys info, maybe getrusage()
//#include <sys/resource.h>
#include "proxy.h"
#include <event2/event.h>
#include <event2/bufferevent.h>
#include <event2/buffer.h>
#include <event2/util.h>
#include "network/network-private.h"

#include "bstrlib.h"
#include "bstraux.h"

#include "craftd.h"
#include "craftd-config.h"
#include "util.h"


void proxy_readcb(struct bufferevent *bev, void *ctx)
{
  struct WQ_entry *workitem;
  struct PL_entry *player = ctx;
  
  workitem = Malloc(sizeof(struct WQ_entry));
  workitem->bev = bev;
  workitem->player = player;
  workitem->worktype = WQ_PROXY;
  
  pthread_mutex_lock(&worker_cvmutex);
  STAILQ_INSERT_TAIL(&WQ_head, workitem, WQ_entries);
  
  pthread_cond_signal(&worker_cv);
  pthread_mutex_unlock(&worker_cvmutex);
  
}
void proxy_errorcb(struct bufferevent *bev, short error, void *ctx)
{
      int finished = 0;
    
    // Get player context from linked list 
    struct PL_entry *player = ctx;
    if (error & BEV_EVENT_CONNECTED)
    {
      //bufferevent_lock(player->sev);
      send_proxyhandshake(player);
      //bufferevent_unlock(player->sev);
    }
    else if (error & BEV_EVENT_EOF)
    {
        /* Connection closed */
        finished = 1;
    }
    else if (error & BEV_EVENT_ERROR)
    {
        /* Some other kind of error, handle it here by checking errno */
        LOG(LOG_INFO, "libevent: ip %s - %s", player->ip,
            evutil_socket_error_to_string(EVUTIL_SOCKET_ERROR()));
        finished = 1;
    }
    else if (error & BEV_EVENT_TIMEOUT)
    {
        /* Handle timeout somehow */
        LOG(LOG_ERR, "A buf event timeout?");
	finished = 1;
    }

    if (finished)
    {
	/* If the client disconnects, remove any pending WP buffer events */
	struct WQ_entry *workitem, *workitemtmp;
	pthread_mutex_lock(&worker_cvmutex);
        STAILQ_FOREACH_SAFE(workitem, &WQ_head, WQ_entries, workitemtmp)
	{
	  if(workitem->bev == bev)
	  {
	    STAILQ_REMOVE(&WQ_head, workitem, WQ_entry, WQ_entries);
	    LOG(LOG_DEBUG, "bev removed from workerpool by errorcb");

            free(workitem);
	  }
	}
        pthread_rwlock_wrlock(&player->rwlock);
	player->sev = NULL;
	pthread_rwlock_unlock(&player->rwlock);
	if (bev)
	  bufferevent_free(bev);

        pthread_mutex_unlock(&worker_cvmutex);
    }
}

struct bufferevent *create_servercon(struct PL_entry *player, Server *server)
{
  struct bufferevent *bev;
  bev = bufferevent_socket_new(base,-1,BEV_OPT_CLOSE_ON_FREE|BEV_OPT_THREADSAFE);
  bufferevent_setcb(bev,proxy_readcb,NULL,proxy_errorcb,player);
  bufferevent_enable(bev, EV_READ|EV_WRITE);
  bufferevent_socket_connect_hostname(bev,dns_base,AF_INET,"192.168.1.5",25565);
  return bev;
}

