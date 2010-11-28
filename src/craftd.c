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

#include <config.h>
 
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/queue.h>
#include <pthread.h>

#include <event2/event.h>
#include <event2/buffer.h>
#include <event2/bufferevent.h>
#include <event2/thread.h>

#include "craftd-config.h"
#include "craftd.h"
#include "util.h"
#include "network.h"
#include "httpd.h"

void
readcb(struct bufferevent *bev, void *ctx)
{
    struct evbuffer *input, *output;
    input = bufferevent_get_input(bev);
    output = bufferevent_get_output(bev);
    struct PL_entry *player = ctx;

    /* TODO: use ev_addbuffer/removebuffer for efficiency!
     * Use more zero copy I/O and peeks if possible
     */

    size_t inlen;
    int status;
    int pktlen;
    uint8_t pkttype;

    inlen = evbuffer_get_length(input);

    evbuffer_copyout(input, &pkttype, 1);

    pktlen = len_statemachine(pkttype, input);
    
    /* On exception conditions, negate the return value to get correct errno */
    if (pktlen < 0)
    {
        pktlen = -pktlen;  /* NOTE: Inverted to get error states! */
        if (pktlen == EAGAIN)
        {
            /* recvd a fragment, wait for another event */
            puts("EAGAIN\n");
            return;
        }
        else if (pktlen == EILSEQ)
        {
            /* recvd an packet that does not match known parameters
             * Punt the client and perform cleanup
             */
            printf("EILSEQ in recv buffer!, pkttype: 0x%.2x\n", pkttype);
            goto readcberr;
        }
      perror("unhandled readcb error");
    }
    
    /* Invariant: else we recieved a full packet of pktlen */

    /* XXX add round-robin to packet worker pool here */
    /* Use a counting semaphore to keep only fewest required num threads 
     * cache hot */

    status = packetdecoder(pkttype, pktlen, bev, ctx);

    /* On decoding errors, punt the client for now */
    if(status != 0)
    {
      printf("Decode error, punting client.  errno: %d\n", status);
      goto readcberr;
    }

    return; // Good read, no exception handling

/* On exception, remove all client allocations in correct order */
readcberr:
    free(ctx);
    bufferevent_free(bev);
    return;

    /* DEBUG to print hex and ASCII
    uint8_t *fullpkt;
    fullpkt = malloc(pktlen);
    //evbuffer_remove(input, fullpkt, pktlen); // or evbuffer_copyout

    printf("input buf is: %d\n", pktlen);
    for(int i=0; i < pktlen; i++)
      printf("%.2x ", (uint8_t) fullpkt[i]);
    printf("\n");
    for(int i=0; i < pktlen; i++)
      printf("%c", fullpkt[i]);
    printf("\n");
    
    free(fullpkt);
    */
}

void
errorcb(struct bufferevent *bev, short error, void *ctx)
{
    int finished = 0;
    
    // Get player context from linked list 
    struct PL_entry *player = ctx;
    
    if (error & BEV_EVENT_EOF)
    {
        /* Connection closed, remove client from tables here */
        if( player->username.valid == 1)
          printf("Connection closed for: %s\n", player->username.str);
        else
          printf("Connection closed ip: %s\n", player->ip);
        finished = 1;
    }
    else if (error & BEV_EVENT_ERROR)
    {
        /* Some other kind of error, handle it here by checking errno */
        puts("Some kind of error to be handled in errorcb\n");
        //EVUTIL_EVENT_ERROR;
        finished = 1;
    }
    else if (error & BEV_EVENT_TIMEOUT)
    {
        /* Handle timeout somehow */
        puts("A buf event timeout?\n");
	finished = 1;
    }
    if (finished)
    {
        // Convert this to a SLIST_WHILE
        // Grab a rdlock until player is found, wrlock delete, free
        SLIST_REMOVE(&PL_head, ctx, PL_entry, PL_entries);
        --PL_count;
        free(ctx);
        bufferevent_free(bev);
    }
}

void
do_accept(evutil_socket_t listener, short event, void *arg)
{
    struct event_base *base = arg;

    struct PL_entry *player;

    struct sockaddr_storage ss;
    socklen_t slen = sizeof(ss);
    int fd = accept(listener, (struct sockaddr*)&ss, &slen);
    if (fd < 0)
    {
        perror("accept error");
    }
    else if (fd > FD_SETSIZE)
    {
        puts("too many clients"); // LOG  
        close(fd);
    }
    else
    {
        struct bufferevent *bev;

        /* Allocate space for a new player */
        player = Malloc(sizeof(struct PL_entry));
        player->fd = fd;

        /* Get the IPv4 or IPv6 address and store it */
        if (getpeername(fd, (struct sockaddr *)&ss, &slen))
        {
          puts("Couldn't get peer IP"); // LOG
          close(fd);
          free(player);
          return;
        }
        void *inaddr;
        if (ss.ss_family == AF_INET)
        {
          inaddr = &((struct sockaddr_in*)&ss)->sin_addr;
        }
        else if (ss.ss_family == AF_INET6)
        {
          inaddr = &((struct sockaddr_in6*)&ss)->sin6_addr;
        }
        else
        {
          puts("weird address family"); // LOG
          close(fd);
          free(player);
          return;
        }

        /* Initialize the player's internal rwlock */
        pthread_rwlock_init(&player->rwlock, NULL);
        
        /* Lock for the list ptr update and add them to the Player List */
        pthread_rwlock_wrlock(&PL_rwlock);
        SLIST_INSERT_HEAD(&PL_head, player, PL_entries);
        ++PL_count;
        pthread_rwlock_unlock(&PL_rwlock);

        evutil_inet_ntop(ss.ss_family, inaddr, player->ip, sizeof(player->ip));

        evutil_make_socket_nonblocking(fd);

        bev = bufferevent_socket_new(base, fd, 
				     BEV_OPT_CLOSE_ON_FREE|BEV_OPT_THREADSAFE);
        bufferevent_setcb(bev, readcb, NULL, errorcb, player);
        bufferevent_setwatermark(bev, EV_READ, 0, MAX_BUF);
        bufferevent_enable(bev, EV_READ|EV_WRITE);
    }
}

void
run_server(void)
{
    evutil_socket_t listener;
    struct sockaddr_in sin;
    struct event_base* base;
    struct event *listener_event;

    base = event_base_new();
    if (!base)
    {
        fprintf(stderr, "Could not create MC libevent base!\n");
        return;
    }

    bzero(&sin, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_addr.s_addr = INADDR_ANY;
    sin.sin_port = htons(SERVER_PORT);

    if ((listener = socket(PF_INET, SOCK_STREAM, 0)) < 0)
    {
        perror("cannot create socket");
        return;
    }

    evutil_make_socket_nonblocking(listener);

#ifndef WIN32
    {
        int one = 1;
        setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
    }
#endif

    if (bind(listener, (struct sockaddr*)&sin, sizeof(sin)) < 0)
    {
        perror("cannot bind");
        return;
    }

    if (listen(listener, MAX_LISTENBACKLOG) < 0)
    {
        perror("listen error");
        return;
    }

    listener_event = event_new(base, listener, EV_READ|EV_PERSIST, 
        do_accept, (void*)base);

    event_add(listener_event, NULL);

    event_base_dispatch(base);
}

int
main(int argc, char **argv)
{
  pthread_t httpd_thread_id;
  pthread_attr_t httpd_thread_attr;
  pthread_t timeloop_thread_id;
  pthread_attr_t timeloop_thread_attr;
  int status = 0;

  /* Player List singly linked-list setup */
  // hsearch w/direct ptr hashtable for name lookup if we need faster direct
  // access (keep two ADTs and entries for each player)
  pthread_rwlock_init(&PL_rwlock, NULL);
  SLIST_INIT(&PL_head);
  PL_count = 0;

  /* Print startup message */
  craftd_version(argv[0]); // LOG
  puts("Server starting!"); // LOG
  
  //setvbuf(stdout, NULL, _IONBF, 0); // set nonblocking stdout

#ifdef WIN32
  status = evthread_use_windows_threads();
#else
  status = evthread_use_pthreads();
#endif

  if(status != 0)
    perror("Cannot initialize libevent threading");

  /* Start timeloop */
  pthread_attr_init(&timeloop_thread_attr);
  pthread_attr_setdetachstate(&timeloop_thread_attr, PTHREAD_CREATE_DETACHED);
  //status = pthread_create(&timeloop_thread_id, &timeloop_thread_attr, 
  //    run_timeloop, NULL);
  if(status != 0)
    perror("Cannot start timeloop");
  
  /* Start httpd */
  pthread_attr_init(&httpd_thread_attr);
  pthread_attr_setdetachstate(&httpd_thread_attr, PTHREAD_CREATE_DETACHED);
  status = pthread_create(&httpd_thread_id, &httpd_thread_attr, 
      run_httpd, NULL);
  
  if(status != 0)
    perror("Cannot start httpd");

  /* Start inbound game server*/
  run_server();
  
  return 0;
}
