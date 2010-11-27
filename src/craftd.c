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
#include "packets.h"
#include "javaendian.h"
#include "httpd.h"

/*
 * The length and decoder state machines could possibly use callbacks/fuction
 * pointers?  Might have advantages for threading.
 */

/** 
 * Get and return length of full packet.
 * Otherwise EAGAIN EILSEQ on exception (need more data or bad input)
 * NOTE that we negate the error values since the positives could be valid len
 * e.g. return -EAGAIN;  it must be negated again if < 0 and used as an errno
 *
 * @remarks
 * A potential for optimization is to send in the bufferevent instead and use a
 * setwatermark() so we only run the machine when the current variable amount of
 * data has arrived.  The complexity with this is then resetting the watermark
 * in the packet router.
 *
 * @param pkttype packet type byte
 * @param input input evbuffer
 * @return int total length or -EAGAIN, -EILSEQ on exception
*/
int
len_statemachine(uint8_t pkttype, struct evbuffer* input)
{
    size_t inlen;
    inlen = evbuffer_get_length(input);

    switch (pkttype)
    {
    case PID_KEEPALIVE: // Keepalive packet 0x00
    {
        puts("recvd keepalive packet\n");
        return 1;
    }
    case PID_LOGIN: // Login packet 0x01
    {
        puts("recvd login packet\n"); // LOG
        uint16_t ulen;
        uint16_t plen;
        struct evbuffer_ptr ptr;
        const int basesize = sizeof(uint8_t)+sizeof(uint32_t)+sizeof(uint16_t)
                             +sizeof(uint16_t)+sizeof(uint64_t)+sizeof(uint8_t);
        const int stroffset1 = sizeof(uint8_t)+sizeof(uint32_t);
        int totalsize;
	int status;

        evbuffer_ptr_set(input, &ptr, stroffset1, EVBUFFER_PTR_SET);
        status = CRAFTD_evbuffer_copyout_from(input, &ulen, sizeof(uint16_t), &ptr);
	if (status != 0)
	  return -status;
        ulen = ntohs(ulen);

        evbuffer_ptr_set(input, &ptr, stroffset1+sizeof(uint16_t)
				  +ulen, EVBUFFER_PTR_SET);
	
        status = CRAFTD_evbuffer_copyout_from(input, &plen, sizeof(uint16_t), &ptr);
	if (status != 0)
	  return -status;
        plen = ntohs(plen);

        // Packet type + 2(strlen + varstring) + etc
        totalsize = basesize + ulen + plen;
        if (inlen == totalsize)
            return totalsize;
        else if (inlen < totalsize + ulen + plen)
            return -EAGAIN;
        else
            return -EILSEQ;

    }
    case PID_HANDSHAKE: // Handshake packet 0x02
    {
        uint16_t ulen;
        struct evbuffer_ptr ptr;
        const int basesize = (sizeof(uint8_t)+sizeof(uint16_t));
        int totalsize;
	int status;

        evbuffer_ptr_set(input, &ptr, sizeof(uint8_t), EVBUFFER_PTR_SET);
	
        status = CRAFTD_evbuffer_copyout_from(input, &ulen, sizeof(uint16_t), &ptr);
	if (status != 0)
	  return -status;

        ulen = ntohs(ulen);

        // Packet type + short string len + string
        totalsize = basesize + ulen;
        if (inlen == totalsize)
            return totalsize;
        else if (inlen < totalsize)
            return -EAGAIN;
        else
            return -EILSEQ;
    }
    case PID_CHAT: // Chat packet 0x03
    {
        puts("recvd chat packet\n");
        break;
    }
    case PID_PINVENTORY: // Update inventory packet 0x05
    {
        puts("recvd update inventory packet\n");
        break;
    }
    case PID_USEENTITY: // Use entity packet 0x07
    {
	puts("recvd use entity packet\n");
	break;
    }
    case PID_PLAYERFLY: // "Flying"/Player packet 0x0A
    {
        puts("recvd flying packet\n");
        break;
    }
    case PID_PLAYERPOS: // Player position packet 0x0B
    {
        puts("recvd player position packet\n");
        break;
    }
    case PID_PLAYERLOOK: // Player look packet 0x0C
    {
        puts("recvd player look packet\n");
        break;
    }
    case PID_PLAYERMOVELOOK: // Player move+look packet 0x0D
    {
        puts("recvd move+look packet\n");
        break;
    }
    case PID_PLAYERDIG: // Block dig packet 0x0E
    {
        puts("recvd block dig packet\n");
        break;
    }
    case PID_BLOCKPLACE: // Place packet 0x0F
    {
        puts("recvd place packet\n");
        break;
    }
    case PID_HOLDCHANGE: // Block/item switch packet 0x10
    {
        puts("recvd block/item switch packet\n");
        break;
    }
    case PID_ARMANIMATE: // Arm animate 0x12
    {
        puts("recvd arm animate packet\n");
	break;
    }
    case PID_PICKUPSPAWN:
    {
        puts("recvd pickup spawn packet\n");
	break;
    }
    case PID_DISCONNECT: // Disconnect packet 0xFF
    {
        puts("recvd disconnect packet\n");
        break;
    }
    default:
    {
        printf("Unknown packet type: %x, len: %d\n!", pkttype, (int)inlen);
        // Close connection
        return -EILSEQ;
    }
    }

    return -EILSEQ; // Temporary
}

/* Pass the packet off to a worker
 * Invariant: packet is of correct length from len state machine
 */
int
packetdecoder(uint8_t pkttype, int pktlen, struct bufferevent *bev, void *ctx)
{
  struct evbuffer *input, *output;
  input = bufferevent_get_input(bev);
  output = bufferevent_get_output(bev);

    switch (pkttype)
    {
    case PID_KEEPALIVE: // Keepalive packet 0x00
    {	
        /* recvd keepalive, only byte packet. Toss it out for now
         * We handle keepalives in timebound event loop.
         * Hopefully this useless packet goes away.
         */
        puts("decoded keepalive!\n");
        evbuffer_drain(input, 1);
	// evbuffer_add(output, 0x00, 1);

        return 0;
    }
    case PID_LOGIN: // Login packet 0x01
    {
        puts("decoded login packet\n");
	
	struct packet_login *u_login;
	u_login = Malloc(sizeof(struct packet_login));
	bzero(u_login, sizeof(struct packet_login));
	
	evbuffer_remove(input, &u_login->pid, sizeof(u_login->pid));
	evbuffer_remove(input, &u_login->version, sizeof(u_login->version));
	u_login->version = ntohl(u_login->version);
	
	evbuffer_remove(input, &u_login->ulen, sizeof(u_login->ulen));
	u_login->ulen = ntohs(u_login->ulen);
	
	u_login->username = (char *) Malloc((u_login->ulen)+1);
	bzero(u_login->username, (u_login->ulen) +1);
	evbuffer_remove(input, u_login->username, u_login->ulen);
	u_login->username[u_login->ulen] = '\0';
	
	evbuffer_remove(input, &u_login->plen, sizeof(u_login->plen));
	u_login->plen = ntohs(u_login->plen);
	
	u_login->password = (char *) Malloc((u_login->plen)+1);
	bzero(u_login->password, (u_login->plen) +1);
	evbuffer_remove(input, u_login->password, u_login->plen);
	u_login->password[u_login->plen] = '\0';
	
	evbuffer_remove(input, &u_login->mapseed, sizeof(u_login->mapseed));
	u_login->mapseed = CD_ntoh64(u_login->mapseed);
	
	evbuffer_remove(input, &u_login->dimension, sizeof(u_login->dimension));

	if(!ismc_utf8(u_login->username) || !ismc_utf8(u_login->password))
	  exit(-1); // Add a punt here
	
	
	printf("recvd login from: %s client ver: %d seed: %llu dim: %d\n", 
	       u_login->username, u_login->version, u_login->mapseed, 
	       u_login->dimension); // LOG
	
	/*struct packet_login slog;
	slog.pid = 0x01;
	slog.version = htonl(0);
	slog.ulen = htons(0);
	slog.username = NULL;
	slog.plen = htons(0);
	slog.password = NULL;
	slog.mapseed = CD_hton64(0);
	slog.dimension = 0;
	evbuffer_add(output, &slog, 18);
	
        bufferevent_flush(bev, EV_WRITE, BEV_FLUSH);*/
	
	struct packet_disconnect dcon;
	dcon.pid = 0xFF;
	dcon.slen = htons(11+u_login->ulen);
	dcon.message = (char *) Malloc(11+u_login->ulen);
	strncpy(dcon.message, "FFFFFUUUUU ", 11);
	memmove(dcon.message+11, u_login->username, u_login->ulen);
	evbuffer_add(output, &dcon.pid, sizeof(dcon.pid));
        evbuffer_add(output, &dcon.slen, sizeof(dcon.slen));
        evbuffer_add(output, dcon.message, 11+u_login->ulen);
	
	free(dcon.message);
	
	free(u_login->username);
	free(u_login->password);
	free(u_login);
	
	return 0;
    }
    case PID_HANDSHAKE: // Handshake packet 0x02
    {
        puts("decoded handshake packet\n");
	
        struct packet_handshake u_hs;
        int16_t ulen;

        evbuffer_remove(input, &u_hs.pid, sizeof(u_hs.pid));
	evbuffer_remove(input, &ulen, sizeof(ulen));
	ulen = ntohs(ulen);

        u_hs.username = mcstring_allocate(ulen);
        if(u_hs.username == NULL)
          exit(3); // LOG bad allocate

	evbuffer_remove(input, u_hs.username->str, u_hs.username->slen);
          
        if(!mcstring_valid(u_hs.username))
          exit(4); // LOG bad str, punt client

	printf("Handshake from: %s\n", u_hs.username->str);
  
	mcstring_free(u_hs.username);

	/* Use a non-authenticating handshake for now 
         * XXX: just a hack to get a login.
         * XXX  This needs to be added to the worker pool for computing hash
         * from minecraft.net
         */
       	struct packet_handshake s_hs;
	s_hs.pid = 0x02;
        int16_t nslen;
	const char *hashreply = "-";
	s_hs.username = mcstring_create(strlen(hashreply), hashreply);

        if (s_hs.username == NULL)
          exit(5); // DEBUG hash reply malformed

        nslen = ntohs(s_hs.username->slen);
	evbuffer_add(output, &s_hs.pid, sizeof(s_hs.pid));
        evbuffer_add(output, &nslen, sizeof(nslen));
        evbuffer_add(output, s_hs.username->str, s_hs.username->slen);

        bufferevent_flush(bev, EV_WRITE, BEV_FLUSH);

        mcstring_free(s_hs.username);

	return 0;
    }
    case PID_CHAT: // Chat packet 0x03
    {
        puts("recvd chat packet\n");
        break;
    }
    case PID_PINVENTORY: // Update inventory packet 0x05
    {
        puts("recvd update inventory packet\n");
        break;
    }
    case PID_USEENTITY: // Use entity packet 0x07
    {
	puts("recvd use entity packet\n");
	break;
    }
    case PID_PLAYERFLY: // "Flying"/Player packet 0x0A
    {
        puts("recvd flying packet\n");
        break;
    }
    case PID_PLAYERPOS: // Player position packet 0x0B
    {
        puts("recvd player position packet\n");
        break;
    }
    case PID_PLAYERLOOK: // Player look packet 0x0C
    {
        puts("recvd player look packet\n");
        break;
    }
    case PID_PLAYERMOVELOOK: // Player move+look packet 0x0D
    {
        puts("recvd move+look packet\n");
        break;
    }
    case PID_PLAYERDIG: // Block dig packet 0x0E
    {
        puts("recvd block dig packet\n");
        break;
    }
    case PID_BLOCKPLACE: // Place packet 0x0F
    {
        puts("recvd place packet\n");
        break;
    }
    case PID_HOLDCHANGE: // Block/item switch packet 0x10
    {
        puts("recvd block/item switch packet\n");
        break;
    }
    case PID_ARMANIMATE: // Arm animate 0x12
    {
        puts("recvd arm animate packet\n");
	break;
    }
    case PID_PICKUPSPAWN:
    {
        puts("recvd pickup spawn packet\n");
	break;
    }
    case PID_DISCONNECT: // Disconnect packet 0xFF
    {
        puts("recvd disconnect packet\n");
        break;
    }
    default:
    {
        printf("Unrouted packet type: %x\n!", pkttype);
        // Close connection
        return EILSEQ;
    }
    }

    return 0;
}

void
readcb(struct bufferevent *bev, void *ctx)
{
    struct evbuffer *input, *output;
    input = bufferevent_get_input(bev);
    output = bufferevent_get_output(bev);

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
        // SLIST_REMOVE(&PL_head, ctx, PL_entry, PL_entries);
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
