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
 
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <pthread.h>

#include <event2/event.h>
#include <event2/buffer.h>
#include <event2/bufferevent.h>

#include "craftd.h"
#include "packets.h"
#include "javaendian.h"

/*
 * The length and decoder state machines could possibly use callbacks/fuction
 * pointers?  Might have advantages for threading.
 */

/* TODO: use ev_addbuffer/removebuffer for efficiency!
 * Use more zero copy I/O and peeks if possible
 */

int len_statemachine(uint8_t pkttype, struct evbuffer* input);
int packetdecoder(uint8_t pkttype, int pktlen, struct bufferevent *bev);

void *run_worker(void *arg)
{
  int id = *(int *)arg;
  printf("Worker %d started!\n", id);
  
  struct bufferevent *bev;
  struct evbuffer *input, *output;
  struct WQ_entry *workitem;
  struct PL_entry *player;
  size_t inlen;
  int status;
  int pktlen;
  uint8_t pkttype;
  
  for(;;)
  {
    //pthread_mutex_lock(&worker_cvmutex);
    pthread_cond_wait(&worker_cv, &worker_cvmutex);
    //pthread_mutex_unlock(&worker_cvmutex);
    printf("in worker: %d\n", id);
    
    /* Pull work item */
    pthread_mutex_lock(&WQ_mutex);
    /* Check our predicate again:  The WQ is not empty
     * Prevent a nasty race condition if the client disconnects
     * Works in tandem with errorcb FOREACH bev removal loop
     */
    if(STAILQ_EMPTY(&WQ_head))
    {
      puts("Race+deadlock avoidance in workerpool");
      pthread_mutex_unlock(&WQ_mutex);
      //pthread_mutex_unlock(&worker_cvmutex);
      continue;
    }
    workitem = STAILQ_FIRST(&WQ_head);
    STAILQ_REMOVE_HEAD(&WQ_head, WQ_entries);
    pthread_mutex_unlock(&WQ_mutex);
    
    bev = workitem->bev;
    player = workitem->player;
    
    if (bev == NULL || player == NULL)
    {
      puts("Aaack, null bev or ctx in worker?");
      goto WORKER_ERR;
    }

    /* Do work */
    input = bufferevent_get_input(bev);
    output = bufferevent_get_output(bev);
    
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
	puts("EAGAIN");
	goto WORKER_DONE;
      }
      else if (pktlen == EILSEQ)
      {
        /* recvd an packet that does not match known parameters
         * Punt the client and perform cleanup
         */
        printf("EILSEQ in recv buffer!, pkttype: 0x%.2x\n", pkttype);
        goto WORKER_ERR;
      }
      perror("unhandled readcb error");
    }
    
    /* Invariant: else we received a full packet of pktlen */
    
    status = packetdecoder(pkttype, pktlen, bev);
    
    /* On decoding errors, punt the client for now */
    if(status != 0)
    {
      printf("Decode error, punting client.  errno: %d\n", status);
      goto WORKER_ERR;
    }

WORKER_DONE:
    /* On success or EAGAIN, free the work item and clear the worker */
    free(workitem);
    
    //sleep(2); // Test for WQ distribution
    
    //pthread_mutex_unlock(&worker_cvmutex);
    continue;

WORKER_ERR:
    /* On exception, remove all client allocations in correct order */

    /* TODO: Code a function to remove the player from playerlist with
     * correct locking semantics and share this code with errorcb() in craftd.c
     */
    
    free(workitem);
    if (player)
      free(player);
    if (bev)
      bufferevent_free(bev);
    
    //pthread_mutex_unlock(&worker_cvmutex);
    continue;
  }

  return NULL;
}


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
      struct evbuffer_ptr ptr;
      uint16_t ulen;  // Size of the username string
      uint16_t plen; // Size of the password string
      int totalsize;
      int status;

      evbuffer_ptr_set(input, &ptr, packet_loginsz.str1offset, 
		       EVBUFFER_PTR_SET);
      status = CRAFTD_evbuffer_copyout_from(input, &ulen, sizeof(ulen), &ptr);
      if (status != 0)
	return -status;
      ulen = ntohs(ulen);

      evbuffer_ptr_set(input, &ptr, packet_loginsz.str2offset
				  + ulen, EVBUFFER_PTR_SET);
	
      status = CRAFTD_evbuffer_copyout_from(input, &plen, sizeof(plen), &ptr);
      if (status != 0)
	return -status;
      plen = ntohs(plen);

      // Packet type + 2(strlen + varstring) + etc
      totalsize = packet_loginsz.base + ulen + plen;
      if (inlen == totalsize)
	return totalsize;
      else if (inlen < totalsize + ulen + plen)
        return -EAGAIN;
      else
        return -EILSEQ;
    }
    case PID_HANDSHAKE: // Handshake packet 0x02
    {
      struct evbuffer_ptr ptr;
      uint16_t ulen;
      int totalsize;
      int status;

      evbuffer_ptr_set(input, &ptr, packet_handshakesz.str1offset, 
		       EVBUFFER_PTR_SET);
	
      status = CRAFTD_evbuffer_copyout_from(input, &ulen, sizeof(ulen), &ptr);
      if (status != 0)
	return -status;

      ulen = ntohs(ulen);

      // Packet type + short string len + string
      totalsize = packet_handshakesz.base + ulen;
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
packetdecoder(uint8_t pkttype, int pktlen, struct bufferevent *bev)
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
        puts("decoded keepalive!");
        evbuffer_drain(input, 1);

        return 0;
    }
    case PID_LOGIN: // Login packet 0x01
    {
        puts("decoded login packet");
	
	struct packet_login u_login;
	int16_t ulen;
	int16_t plen;
	
	/* Get the version */
	evbuffer_remove(input, &u_login.pid, sizeof(u_login.pid));
	evbuffer_remove(input, &u_login.version, sizeof(u_login.version));
	u_login.version = ntohl(u_login.version);
	
	/* Get the username */
	evbuffer_remove(input, &ulen, sizeof(ulen));
	ulen = ntohs(ulen);
	
	u_login.username = mcstring_allocate(ulen);
	if (u_login.username == NULL)
          exit(3); // LOG bad allocate, punt
	
	evbuffer_remove(input, u_login.username->str, u_login.username->slen);
	if (!mcstring_valid(u_login.username))
          exit(4); // LOG bad str, punt client
	  
	/* Get the password */
	evbuffer_remove(input, &plen, sizeof(plen));
	plen = ntohs(plen);
	
	u_login.password = mcstring_allocate(plen);
	if (u_login.password == NULL)
	  exit(3); // LOG bad allocate, punt
	  
	evbuffer_remove(input, u_login.password->str, u_login.password->slen);
	if (!mcstring_valid(u_login.password))
	  exit(4);
	
	/* Get the mapseed */
	evbuffer_remove(input, &u_login.mapseed, sizeof(u_login.mapseed));
	u_login.mapseed = ntohll(u_login.mapseed);
	
	/* Get the dimension */
	evbuffer_remove(input, &u_login.dimension, sizeof(u_login.dimension));
	
	
	printf("recvd login from: %s client ver: %d seed: %lu dim: %d\n", 
	       u_login.username->str, u_login.version, u_login.mapseed, 
	       u_login.dimension); // LOG*/
	
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
	dcon.slen = htons(11+u_login.username->slen);
	dcon.message = (char *) Malloc(11+u_login.username->slen);
	strncpy(dcon.message, "FFFFFUUUUU ", 11);
	memmove(dcon.message+11, u_login.username->str, u_login.username->slen);
	evbuffer_add(output, &dcon.pid, sizeof(dcon.pid));
        evbuffer_add(output, &dcon.slen, sizeof(dcon.slen));
        evbuffer_add(output, dcon.message, 11+u_login.username->slen);
	
	free(dcon.message);
	
	mcstring_free(u_login.username);
	mcstring_free(u_login.password);
	
	return 0;
    }
    case PID_HANDSHAKE: // Handshake packet 0x02
    {
        puts("decoded handshake packet");
	
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
