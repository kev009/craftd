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

#include <assert.h>
#include <errno.h>

#include "network.h"
#include "network-private.h"
#include "packets.h"

/**
 * A utility function for the length state machine to return the correct length
 * or exception value.
 * 
 * @param inlen length of the packet received so far
 * @param totalsize size of the entire packet
 * @return totalsize on good read, -EAGAIN on incomplete data, else -EILSEQ
 */
static inline int
len_returncode(int inlen, int totalsize)
{
  if (inlen >= totalsize)
    return totalsize;
  else if (inlen < totalsize)
    return -EAGAIN;
  else
    return -EILSEQ;
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
 * @return total length or -EAGAIN, -EILSEQ on exception
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
        return len_returncode(inlen, packet_keepalivesz);
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
      return len_returncode(inlen, totalsize);
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
      return len_returncode(inlen, totalsize);
    }
    case PID_CHAT: // Chat packet 0x03
    {
        struct evbuffer_ptr ptr;
	uint16_t mlen;
	int totalsize;
	int status;
	
	evbuffer_ptr_set(input, &ptr, packet_chatsz.str1offset, 
			 EVBUFFER_PTR_SET);
	
	status = CRAFTD_evbuffer_copyout_from(input, &mlen, sizeof(mlen), &ptr);
	if (status != 0)
	  return -status;
	
	mlen = ntohs(mlen);
	
	totalsize = packet_chatsz.base + mlen;
	return len_returncode(inlen, totalsize);
    }
    case PID_USEENTITY: // Use entity packet 0x07
    {
	return len_returncode(inlen, packet_playerpossz);
    }
    case PID_RESPAWN: // Repawn packet 0x09
    {
	return len_returncode(inlen, packet_respawnsz);
    }
    case PID_PLAYERFLY: // "Flying"/Player packet 0x0A
    {
        return len_returncode(inlen, packet_playerflysz);
    }
    case PID_PLAYERPOS: // Player position packet 0x0B
    {
        return len_returncode(inlen, packet_playerpossz);
    }
    case PID_PLAYERLOOK: // Player look packet 0x0C
    {
        return len_returncode(inlen, packet_looksz);
    }
    case PID_PLAYERMOVELOOK: // Player move+look packet 0x0D
    {
	return len_returncode(inlen, packet_movelooksz);
    }
    case PID_PLAYERDIG: // Block dig packet 0x0E
    {
        return len_returncode(inlen, packet_digsz);
    }
    case PID_BLOCKPLACE: // Place packet 0x0F
    {
      struct evbuffer_ptr ptr;
      int status;
      int16_t itemid;

      evbuffer_ptr_set(input, &ptr, packet_blockplacesz.itemidoffset,
                       EVBUFFER_PTR_SET);
      
      status = CRAFTD_evbuffer_copyout_from(input, &itemid, sizeof(itemid), 
                                            &ptr);
      
      if ( status != 0)
        return -status;

      itemid = ntohs(itemid);
      if (itemid == -1)
        return len_returncode(inlen, packet_blockplacesz.emptyplace);
      else
        return len_returncode(inlen, packet_blockplacesz.place);
    }
    case PID_HOLDCHANGE: // Block/item switch packet 0x10
    {
        return len_returncode(inlen, packet_holdchangesz);
    }
    case PID_ARMANIMATE: // Arm animate 0x12
    {
        return len_returncode(inlen, packet_armanimatesz);
    }
    case PID_ENTITYACTION: // Entity action (crouch) 0x13
    {
	return len_returncode(inlen, packet_entityactionsz);
    }
    case PID_PICKUPSPAWN:
    {
        return len_returncode(inlen, packet_pickupspawnsz);
    }
    case PID_CLOSEWINDOW: // Close window 0x65
    {
        return len_returncode(inlen, packet_closewindowsz);
    }
    case PID_WINDOWCLICK: // Window click 0x66
    {
      struct evbuffer_ptr ptr;
      int status;
      int16_t itemid;

      evbuffer_ptr_set(input, &ptr, packet_windowclicksz.itemidoffset,
                       EVBUFFER_PTR_SET);

      status = CRAFTD_evbuffer_copyout_from(input, &itemid, sizeof(itemid), 
                                            &ptr);
      if ( status != 0)
        return -status;

      itemid = ntohs(itemid);
      if (itemid == -1)
        return len_returncode(inlen, packet_windowclicksz.clicknull);
      else
        return len_returncode(inlen, packet_windowclicksz.click);
    }
    case PID_DISCONNECT: // Disconnect packet 0xFF
    {
        struct evbuffer_ptr ptr;
	uint16_t mlen;
	int totalsize;
	int status;
	
	evbuffer_ptr_set(input, &ptr, packet_disconnectsz.str1offset, 
			 EVBUFFER_PTR_SET);
	
	status = CRAFTD_evbuffer_copyout_from(input, &mlen, sizeof(mlen), &ptr);
	if (status != 0)
	  return -status;
	
	mlen = ntohs(mlen);
	
	totalsize = packet_disconnectsz.base + mlen;
	return len_returncode(inlen, totalsize);
    }
    default:
    {
        LOG(LOG_ERR, "Unknown packet type: %x, len: %d\n!", pkttype, 
	    (int) inlen);
        // Close connection
        return -EILSEQ;
    }
    }
    
    /* We should never get here; a coding error occurred if so */ 
    bool unterminated_length_case = false;
    assert(unterminated_length_case);
}
