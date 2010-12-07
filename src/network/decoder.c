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

#include <stdlib.h>
#include <stdio.h>

#include "network.h"
#include "network-private.h"
#include "packets.h"

/**
 * The decoder pulls data out of the buffer and populates a packet struct
 * with native data types.  Smaller packets are also handled here.
 * 
 * @remarks Invariant: packet is of correct length from len state machine
 * 
 * @param pkttype packet type header enum
 * @param pktlen length of entire packet
 * @param bev buffer event
 * @param player Player List entry (buffer event context)
 */
int
packetdecoder(uint8_t pkttype, int pktlen, struct bufferevent *bev, 
	     struct PL_entry *player)
{
  struct evbuffer *input, *output;
  input = bufferevent_get_input(bev);
  output = bufferevent_get_output(bev);

    switch (pkttype)
    {
    case PID_KEEPALIVE: // Keepalive packet 0x00
    {	
        /* recvd keepalive, only byte packet. Toss it out for now.
         * We handle keepalives in timebound event loop.
         * Hopefully this useless packet goes away.
         */
        evbuffer_drain(input, sizeof(int8_t));

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
	  exit(4); // LOG, punt
	
	/* Get the mapseed */
	evbuffer_remove(input, &u_login.mapseed, sizeof(u_login.mapseed));
	u_login.mapseed = ntohll(u_login.mapseed);
	
	/* Get the dimension */
	evbuffer_remove(input, &u_login.dimension, sizeof(u_login.dimension));

	printf("recvd login from: %s client ver: %d seed: %lu dim: %d\n", 
	       u_login.username->str, u_login.version, u_login.mapseed, 
	       u_login.dimension); // LOG
	
	/* Process the login */
	process_login(player, u_login.username, u_login.version);
	
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
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_PINVENTORY: // Update inventory packet 0x05
    {
        puts("recvd update inventory packet\n");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_USEENTITY: // Use entity packet 0x07
    {
	puts("recvd use entity packet\n");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
	break;
    }
    case PID_PLAYERFLY: // "Flying"/Player packet 0x0A
    {
        puts("recvd flying packet\n");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_PLAYERPOS: // Player position packet 0x0B
    {
        puts("recvd player position packet\n");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_PLAYERLOOK: // Player look packet 0x0C
    {
        puts("recvd player look packet");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_PLAYERMOVELOOK: // Player move+look packet 0x0D
    {
        puts("recvd move+look packet");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_PLAYERDIG: // Block dig packet 0x0E
    {
        puts("recvd block dig packet\n");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_BLOCKPLACE: // Place packet 0x0F
    {
        puts("recvd place packet\n");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_HOLDCHANGE: // Block/item switch packet 0x10
    {
        puts("recvd block/item switch packet\n");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_ARMANIMATE: // Arm animate 0x12
    {
        puts("recvd arm animate packet\n");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
	break;
    }
    case PID_PICKUPSPAWN:
    {
        puts("recvd pickup spawn packet\n");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
	break;
    }
    case PID_DISCONNECT: // Disconnect packet 0xFF
    {
        puts("recvd disconnect packet\n");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
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