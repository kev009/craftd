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

int sent = 0;
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
        LOG(LOG_DEBUG, "decoded login packet");
	
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
        u_login.username = getMCString(input, ulen); //TODO verify

	/* Get the password */
	evbuffer_remove(input, &plen, sizeof(plen));
	plen = ntohs(plen);
        u_login.password = getMCString(input, plen); //TODO verify
	
	/* Get the mapseed */
	evbuffer_remove(input, &u_login.mapseed, sizeof(u_login.mapseed));
	u_login.mapseed = ntohll(u_login.mapseed);
	
	/* Get the dimension */
	evbuffer_remove(input, &u_login.dimension, sizeof(u_login.dimension));

	LOG(LOG_INFO, "recvd login from: %s client ver: %d seed: %lu dim: %d", 
	       u_login.username->data, u_login.version, u_login.mapseed, 
	       u_login.dimension);
	
	/* Process the login */
	process_login(player, u_login.username, u_login.version);

        bstrFree(u_login.username);
        bstrFree(u_login.password);
	
	return 0;
    }
    case PID_HANDSHAKE: // Handshake packet 0x02
    {
        LOG(LOG_DEBUG, "decoded handshake packet");
	
        struct packet_handshake u_hs;
        u_hs.username = NULL;
        int16_t ulen;

        evbuffer_drain(input, sizeof(u_hs.pid));
	evbuffer_remove(input, &ulen, sizeof(ulen));
	ulen = ntohs(ulen);

        u_hs.username = getMCString(input, ulen);
        if(!u_hs.username)
          exit(4); // TODO punt
	
        LOG(LOG_DEBUG, "Handshake from: %s", u_hs.username->data);
	
	process_handshake(player, u_hs.username);

        bstrFree(u_hs.username);

	return 0;
    }
    case PID_CHAT: // Chat packet 0x03
    {
        LOG(LOG_DEBUG, "recvd chat packet");

        struct packet_chat chat;
        int16_t mlen;

        evbuffer_drain(input, sizeof(chat.pid));
        evbuffer_remove(input, &mlen, sizeof(mlen));
        mlen = ntohs(mlen);

        chat.message = getMCString(input, mlen); //TODO verify

        process_chat(player, chat.message);

        bstrFree(chat.message);

        return 0;
    }
    case PID_USEENTITY: // Use entity packet 0x07
    {
	LOG(LOG_DEBUG, "recvd use entity packet");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
	break;
    }
    case PID_PLAYERFLY: // "Flying"/Player packet 0x0A
    {
        LOG(LOG_DEBUG, "recvd flying packet");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_PLAYERPOS: // Player position packet 0x0B
    {
      LOG(LOG_DEBUG, "recvd player position packet");
      struct packet_playerpos pos;

      pthread_rwlock_wrlock(&player->position.rwlock);
      evbuffer_drain(input, sizeof(pos.pid));
      evbuffer_remove(input, &pos.x, sizeof(pos.x));
      evbuffer_remove(input, &pos.y, sizeof(pos.y));
      /* Throw away stance */
      evbuffer_drain(input, sizeof(pos.stance));
      evbuffer_remove(input, &pos.z, sizeof(pos.z));
      /* Throw away flying */
      evbuffer_drain(input, sizeof(int8_t));

      /* Flip bits */
      player->position.x = Cswapd(pos.x);
      player->position.y = Cswapd(pos.y);
      player->position.z = Cswapd(pos.z);

      pthread_rwlock_unlock(&player->position.rwlock);

      break;
    }
    case PID_PLAYERLOOK: // Player look packet 0x0C
    {
      LOG(LOG_DEBUG, "recvd player look packet");
      struct packet_look look;

      pthread_rwlock_wrlock(&player->position.rwlock);
      evbuffer_drain(input, sizeof(look.pid));
      evbuffer_remove(input, &look.yaw, sizeof(look.yaw));
      evbuffer_remove(input, &look.pitch, sizeof(look.pitch));
      /* Throw away flying */
      evbuffer_drain(input, sizeof(int8_t));

      /* Flip bits */
      player->position.yaw = Cswapf(look.yaw);
      player->position.pitch = Cswapf(look.pitch);

      pthread_rwlock_unlock(&player->position.rwlock);
	
      break;
    }
    case PID_PLAYERMOVELOOK: // Player move+look packet 0x0D
    {
        LOG(LOG_DEBUG, "recvd move+look packet");
        struct packet_movelook ml;

        pthread_rwlock_wrlock(&player->position.rwlock);
        evbuffer_drain(input, sizeof(ml.pid));
        evbuffer_remove(input, &ml.x, sizeof(ml.x));
        evbuffer_remove(input, &ml.y, sizeof(ml.y));
        /* Throw away stance */
        evbuffer_drain(input, sizeof(ml.stance));
        evbuffer_remove(input, &ml.z, sizeof(ml.z));
        evbuffer_remove(input, &ml.yaw, sizeof(ml.yaw));
        evbuffer_remove(input, &ml.pitch, sizeof(ml.pitch));
        evbuffer_remove(input, &ml.flying, sizeof(int8_t));

        /* Flip bits */
        player->position.x = Cswapd(ml.x);
        player->position.y = Cswapd(ml.y);
        player->position.z = Cswapd(ml.z);
        player->position.yaw = Cswapf(ml.yaw);
        player->position.pitch = Cswapf(ml.pitch);

        pthread_rwlock_unlock(&player->position.rwlock);

        break;
    }
    case PID_PLAYERDIG: // Block dig packet 0x0E
    {
        LOG(LOG_DEBUG, "recvd block dig packet");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_BLOCKPLACE: // Place packet 0x0F
    {
        LOG(LOG_DEBUG, "recvd place packet");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_HOLDCHANGE: // Block/item switch packet 0x10
    {
        LOG(LOG_DEBUG, "recvd block/item switch packet");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
        break;
    }
    case PID_ARMANIMATE: // Arm animate 0x12
    {
        LOG(LOG_DEBUG, "recvd arm animate packet");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
	break;
    }
    case PID_PICKUPSPAWN:
    {
        LOG(LOG_DEBUG, "recvd pickup spawn packet");
	
	evbuffer_drain(input, pktlen); // TODO: implement actual handler
	
	break;
    }
    case PID_CLOSEWINDOW: // Close window packet 0x65
    {
        LOG(LOG_DEBUG, "recvd close window packet");

        evbuffer_drain(input, pktlen);

        break;
    }
    case PID_WINDOWCLICK: // Window click 0x66
    {
        LOG(LOG_DEBUG, "recvd window click packet");

        evbuffer_drain(input, pktlen);

        break;
    }
    case PID_DISCONNECT: // Disconnect packet 0xFF
    {
        LOG(LOG_DEBUG, "recvd disconnect packet");

        evbuffer_drain(input, pktlen);
        
        errorcb(bev, BEV_EVENT_EOF, player);

        break;
    }
    default:
    {
        LOG(LOG_ERR, "Unrouted packet type: %x\n!", pkttype);
        // Close connection
        return EILSEQ;
    }
    }

    return 0;
}
