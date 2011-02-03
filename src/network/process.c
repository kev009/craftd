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

#include "network/network.h"
#include "network/network-private.h"
#include "network/packets.h"

// Temp for send radius
const int RADIUS = 10;

/**
 * This internal method process packets from a pointer to a struct and
 * a packet type
 * 
 * @remarks Scope: private
 * 
 * @param player Player this method affects
 * @param pkttype Type of packet that 'packet' points to
 * @param packet Pointer to struct of packet type 'packetid'
 */
void
process_packet(struct PL_entry *player, uint8_t pkttype, void * packet)
{
  switch(pkttype)
  {
    case PID_LOGIN:
    {
      process_login(player, ((struct packet_login*) packet)->username, ((struct packet_login*) packet)->version);
      return;
    }
    case PID_HANDSHAKE:
    {
      process_handshake(player,((struct packet_handshake*) packet)->username);
      return;
    }
    case PID_CHAT:
    {
      process_chat(player,((struct packet_chat*) packet)->message);
      return;
    }
    case PID_PLAYERPOS:
    {
      pthread_rwlock_wrlock(&player->position.rwlock);
      
      int oldx = player->position.x/16, oldz = player->position.z/16;
      
      /* Flip bits */
      player->position.x = Cswapd(((struct packet_playerpos*) packet)->x);
      player->position.y = Cswapd(((struct packet_playerpos*) packet)->y);
      player->position.z = Cswapd(((struct packet_playerpos*) packet)->z);
      
      int newx = player->position.x/16, newz = player->position.z/16;
      
      if (oldx != newx || oldz != newz)
	send_chunk_radius(player, player->position.x, player->position.z, 
			  RADIUS);
      
      pthread_rwlock_unlock(&player->position.rwlock);
      
      return;
    }
    case PID_PLAYERLOOK:
    {
      pthread_rwlock_wrlock(&player->position.rwlock);
      /* Flip bits */
      player->position.yaw = Cswapf(((struct packet_look*) packet)->yaw);
      player->position.pitch = Cswapf(((struct packet_look*) packet)->pitch);
      pthread_rwlock_unlock(&player->position.rwlock);
      return;
    }
    case PID_PLAYERMOVELOOK:
    {
      pthread_rwlock_wrlock(&player->position.rwlock);
      
      int oldx = player->position.x/16, oldz = player->position.z/16;
      
      /* Flip bits */
      player->position.x = Cswapd(((struct packet_movelook*) packet)->x);
      player->position.y = Cswapd(((struct packet_movelook*) packet)->y);
      player->position.z = Cswapd(((struct packet_movelook*) packet)->z);
      player->position.yaw = Cswapf(((struct packet_movelook*) packet)->yaw);
      player->position.pitch = Cswapf(((struct packet_movelook*) packet)->pitch);
      
      int newx = player->position.x/16, newz = player->position.z/16;
      
      if (oldx != newx || oldz != newz)
	send_chunk_radius(player, player->position.x, player->position.z, 
			  RADIUS);

      pthread_rwlock_unlock(&player->position.rwlock);
      
      return;
    }
    case PID_DISCONNECT:
    {
      deferLogout(player);
      return;
    }
    default:
    {
      return; //Do nothing
    }
  }
  return;
}