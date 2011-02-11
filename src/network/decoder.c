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

#include <stdlib.h>
#include <stdio.h>

#include "network/network.h"
#include "network/network-private.h"
#include "network/packets.h"

/**
 * The free packet memory method destroys a packet struct
 * 
 * @param pkttype packet type header enum
 * @param packet pointer to packet struct
 */
void packetfree(uint8_t pkttype, void * packet)
{
	switch(pkttype)
	{
		case PID_LOGIN:
		{
			bstrFree(((struct packet_login*) packet)->username);
			bstrFree(((struct packet_login*) packet)->password);
			break;
		}
		case PID_HANDSHAKE:
		{
			bstrFree(((struct packet_handshake*) packet)->username);
			break;
		}
		case PID_CHAT:
		{
			bstrFree(((struct packet_chat*) packet)->message);
			break;
		}
		case PID_DISCONNECT:
		{
			bstrFree(((struct packet_disconnect*) packet)->reason);
			break;
		}
	}
	free(packet);
}

/**
 * The decoder pulls data out of the buffer and populates a packet struct
 * with native data types.  Smaller packets are also handled here.
 * If the operation populated no structs then packetdecoder returns a NULL pointer
 * All returned structs must be at some point freed via packetfree()
 * 
 * @remarks Invariant: packet is of correct length from len state machine
 * 
 * @param pkttype packet type header enum
 * @param pktlen length of entire packet
 * @param bev buffer event
 */

int sent = 0;

// packet decoder should return a pointer to the packet struct decoded from arguments pkttype, pktlen and bufferevent (bev)
// If the operation populated no structs then packetdecoder returns a NULL pointer
// All returned structs must be at some point freed via packetfree()
void*
packetdecoder(uint8_t pkttype, int pktlen, struct bufferevent *bev)
{
  struct evbuffer *input;
  input = bufferevent_get_input(bev);

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
	
	struct packet_login *u_login = Malloc(sizeof(struct packet_login));
	memset(u_login,0,sizeof(struct packet_login));
	
	int16_t ulen;
	int16_t plen;
	
	/* Get the version */
	evbuffer_remove(input, &u_login->pid, sizeof(u_login->pid));
	evbuffer_remove(input, &u_login->version, sizeof(u_login->version));
	u_login->version = ntohl(u_login->version);
	
	/* Get the username */
	evbuffer_remove(input, &ulen, sizeof(ulen));
	ulen = ntohs(ulen);
        u_login->username = getMCString(input, ulen); //TODO verify

	/* Get the password */
	evbuffer_remove(input, &plen, sizeof(plen));
	plen = ntohs(plen);
        u_login->password = getMCString(input, plen); //TODO verify
	
	/* Get the mapseed */
	evbuffer_remove(input, &u_login->mapseed, sizeof(u_login->mapseed));
	u_login->mapseed = ntohll(u_login->mapseed);
	
	/* Get the dimension */
	evbuffer_remove(input, &u_login->dimension, sizeof(u_login->dimension));

	LOG(LOG_INFO, "recvd login packet from: %s client ver: %d seed: %lu dim: %d", 
	       u_login->username->data, u_login->version, u_login->mapseed, 
	       u_login->dimension);
	
	return u_login;
    }
    case PID_HANDSHAKE: // Handshake packet 0x02
    {
		LOG(LOG_DEBUG, "decoded handshake packet");

		struct packet_handshake *u_hs = Malloc(sizeof(struct packet_handshake));
		memset(u_hs,0,sizeof(struct packet_handshake));

		u_hs->username = NULL;
		int16_t ulen;

		evbuffer_drain(input, sizeof(u_hs->pid));
		evbuffer_remove(input, &ulen, sizeof(ulen));
		ulen = ntohs(ulen);

		u_hs->username = getMCString(input, ulen);
		if(!u_hs->username)
			exit(4); // TODO punt

		LOG(LOG_DEBUG, "Handshake from: %s", u_hs->username->data);

		return u_hs;
    }
    case PID_CHAT: // Chat packet 0x03
    {
        LOG(LOG_DEBUG, "recvd chat packet");

		struct packet_chat *chat = Malloc(sizeof(struct packet_chat));
		
        int16_t mlen;

        evbuffer_drain(input, sizeof(chat->pid));
        evbuffer_remove(input, &mlen, sizeof(mlen));
        mlen = ntohs(mlen);

        chat->message = getMCString(input, mlen); //TODO verify

        return chat;
    }
    case PID_USEENTITY: // Use entity packet 0x07
    {
		LOG(LOG_DEBUG, "recvd use entity packet");
		
		struct packet_useentity *ue = Malloc(sizeof(struct packet_useentity));
		
		evbuffer_drain(input, sizeof(ue->pid));
		evbuffer_remove(input, &ue->user, sizeof(MCint));
		evbuffer_remove(input, &ue->target, sizeof(MCint));
		evbuffer_remove(input, &ue->leftclick, sizeof(bool));
		
		return ue;
    }
    case PID_PLAYERFLY: // "Flying"/Player packet 0x0A
    {
        LOG(LOG_DEBUG, "recvd flying packet");
	
		struct packet_playerfly *fly = Malloc(sizeof(struct packet_playerfly));
	
		evbuffer_drain(input, sizeof(fly->pid));
		evbuffer_remove(input, &fly->onground, sizeof(bool));
		
		return fly;
    }
    case PID_PLAYERPOS: // Player position packet 0x0B
    {
		LOG(LOG_DEBUG, "recvd player position packet");

		struct packet_playerpos *pos = Malloc(sizeof(struct packet_playerpos));

		evbuffer_drain(input, sizeof(pos->pid));
		evbuffer_remove(input, &pos->x, sizeof(pos->x));
		evbuffer_remove(input, &pos->y, sizeof(pos->y));
		/* Throw away stance */
		evbuffer_drain(input, sizeof(pos->stance));
		evbuffer_remove(input, &pos->z, sizeof(pos->z));
		/* Throw away flying */
		evbuffer_drain(input, sizeof(MCbyte));
		
		return pos;
    }
    case PID_PLAYERLOOK: // Player look packet 0x0C
    {
		LOG(LOG_DEBUG, "recvd player look packet");

		struct packet_look *look = Malloc(sizeof(struct packet_look));

		evbuffer_drain(input, sizeof(look->pid));
		evbuffer_remove(input, &look->yaw, sizeof(look->yaw));
		evbuffer_remove(input, &look->pitch, sizeof(look->pitch));
		/* Throw away flying */
		evbuffer_drain(input, sizeof(MCbyte));
		  
		return look;
    }
    case PID_PLAYERMOVELOOK: // Player move+look packet 0x0D
    {
        LOG(LOG_DEBUG, "recvd move+look packet");
        
		struct packet_movelook *ml = Malloc(sizeof(struct packet_movelook));

        evbuffer_drain(input, sizeof(ml->pid));
        evbuffer_remove(input, &ml->x, sizeof(ml->x));
        evbuffer_remove(input, &ml->y, sizeof(ml->y));
        /* Throw away stance */
        evbuffer_drain(input, sizeof(ml->stance));
        evbuffer_remove(input, &ml->z, sizeof(ml->z));
        evbuffer_remove(input, &ml->yaw, sizeof(ml->yaw));
        evbuffer_remove(input, &ml->pitch, sizeof(ml->pitch));
        evbuffer_remove(input, &ml->flying, sizeof(MCbyte));

        return ml;
    }
    case PID_PLAYERDIG: // Block dig packet 0x0E
    {
        LOG(LOG_DEBUG, "recvd block dig packet");
        
		struct packet_dig *dig = Malloc(sizeof(struct packet_dig));
		
		evbuffer_drain(input, sizeof(dig->pid));
		evbuffer_remove(input, &dig->status, sizeof(MCbyte));
		evbuffer_remove(input, &dig->x, sizeof(MCint));
		evbuffer_remove(input, &dig->y, sizeof(MCbyte));
		evbuffer_remove(input, &dig->z, sizeof(MCint));
		evbuffer_remove(input, &dig->face, sizeof(MCbyte));
		
        return dig;
    }
    case PID_BLOCKPLACE: // Place packet 0x0F
    {
        LOG(LOG_DEBUG, "recvd place packet");
	
		struct packet_blockplace *bp = Malloc(sizeof(struct packet_blockplace));
		
		evbuffer_drain(input, sizeof(bp->pid));
		evbuffer_remove(input, &bp->x, sizeof(MCint));
		evbuffer_remove(input, &bp->y, sizeof(MCbyte));
		evbuffer_remove(input, &bp->z, sizeof(MCint));
		evbuffer_remove(input, &bp->direction, sizeof(MCbyte));
		evbuffer_remove(input, &bp->itemid, sizeof(MCshort));
		evbuffer_remove(input, &bp->amount, sizeof(MCbyte));
		evbuffer_remove(input, &bp->damage, sizeof(MCshort));
		
		return bp;
    }
    case PID_HOLDCHANGE: // Block/item switch packet 0x10
    {
        LOG(LOG_DEBUG, "recvd block/item switch packet");
        
        struct packet_holdchange *hc = Malloc(sizeof(struct packet_holdchange));
	
		evbuffer_drain(input, sizeof(hc->pid));
		evbuffer_remove(input, &hc->itemid, sizeof(MCshort));
		
		return hc;
    }
    case PID_ARMANIMATE: // Arm animate 0x12
    {
        LOG(LOG_DEBUG, "recvd arm animate packet");
	
		struct packet_armanimate *aa = Malloc(sizeof(struct packet_armanimate));
	
		evbuffer_drain(input, sizeof(aa->pid));
		evbuffer_remove(input, &aa->eid, sizeof(MCint));
		evbuffer_remove(input, &aa->animate, sizeof(MCbyte));
		
		return aa;
    }
    case PID_ENTITYACTION: // Entity action (crouch) 0x13
    {
		LOG(LOG_DEBUG, "recvd entity action packet");
	
		struct packet_entityaction *ea = Malloc(sizeof(struct packet_entityaction));
	
		evbuffer_drain(input, sizeof(ea->pid));
		evbuffer_remove(input, &ea->eid, sizeof(MCint));
		evbuffer_remove(input, &ea->action, sizeof(MCbyte));
		
		return ea;
    }
    case PID_PICKUPSPAWN:
    {
        LOG(LOG_DEBUG, "recvd pickup spawn packet");
	
		struct packet_pickupspawn *ps = Malloc(sizeof(struct packet_pickupspawn));
	
		evbuffer_drain(input, sizeof(ps->pid));
		evbuffer_remove(input, &ps->eid, sizeof(MCint));
		evbuffer_remove(input, &ps->item, sizeof(MCshort));
		evbuffer_remove(input, &ps->count, sizeof(MCbyte));
		evbuffer_remove(input, &ps->damage, sizeof(MCshort));
		evbuffer_remove(input, &ps->x, sizeof(MCint));
		evbuffer_remove(input, &ps->y, sizeof(MCint));
		evbuffer_remove(input, &ps->z, sizeof(MCint));
		evbuffer_remove(input, &ps->rotation, sizeof(MCbyte));
		evbuffer_remove(input, &ps->pitch, sizeof(MCbyte));
		evbuffer_remove(input, &ps->roll, sizeof(MCbyte));
		
		return ps;
    }
    case PID_CLOSEWINDOW: // Close window packet 0x65
    {
        LOG(LOG_DEBUG, "recvd close window packet");

        struct packet_closewindow *cw = Malloc(sizeof(struct packet_closewindow));
		
		evbuffer_drain(input, sizeof(cw->pid));
		evbuffer_remove(input, &cw->winid, sizeof(MCbyte));
		
		return cw;
    }
    case PID_WINDOWCLICK: // Window click 0x66
    {
        LOG(LOG_DEBUG, "recvd window click packet");

        evbuffer_drain(input, pktlen); //TODO: write handler

        break;
    }
    case PID_DISCONNECT: // Disconnect packet 0xFF
    {
        LOG(LOG_DEBUG, "recvd disconnect packet");
        
		struct packet_disconnect *dc = Malloc(sizeof(struct packet_disconnect));
		
		int16_t rlen;
		
		evbuffer_drain(input, sizeof(dc->pid));
		evbuffer_remove(input, &rlen, sizeof(rlen));
		rlen = ntohs(rlen);
		
        dc->reason = getMCString(input, rlen);
		
		return dc;
    }
    default:
    {
        LOG(LOG_ERR, "Unrouted packet type: %x\n!", pkttype);
        // Close connection
        return 0;
    }
    }

    return 0;
}
