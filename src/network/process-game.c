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
#include "mapchunk.h"

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
      errorcb(player->bev, BEV_EVENT_EOF, player);
      return;
    }
    default:
    {
      return; //Do nothing
    }
  }
  return;
}
/**
 * This internal method checks login predicates, populates the rest of the
 * Player List entry, and sends the initial packet stream to spawn the player.
 * 
 * @remarks Scope: private
 * 
 * @param player Player List player pointer
 * @param username inbound username from client login packet
 * @param ver inbound version from client login packet
 */
void
process_login(struct PL_entry *player, bstring username, uint32_t ver)
{
  // TODO: Future, async check of minecraft.net for user validity
  // TODO: Future, check against local ACL
  
  /* Check if the client version is compatible with the craftd version */
  if (ver != PROTOCOL_VERSION)
  {
    bstring dconmsg;
    dconmsg = bfromcstr("Client version is incompatible with this server.");
    send_kick(player, dconmsg);
    bstrFree(dconmsg);
    return;
  }
  
  /* Otherwise, finish populating their Player List entry */
  pthread_rwlock_wrlock(&player->rwlock);
  player->username = bstrcpy(username);
  pthread_rwlock_unlock(&player->rwlock);
  
  send_loginresp(player);

  const int spawnradius = 5;
#ifdef USE_CDGAME
  send_chunk_radius(player, Config.spawn.x, Config.spawn.z, spawnradius);
  
  send_spawnpos(player, Config.spawn.x, Config.spawn.y, Config.spawn.z);
  //send inv
  send_movelook(player, Config.spawn.x, Config.spawn.y + 6.1, Config.spawn.y
		+ 6.2, Config.spawn.z, 0, 0, false);
#endif

  /* Login message */
  bstring loginmsg = bformat("Player %s has joined the game!", 
      player->username->data);
  send_syschat(loginmsg);
  bstrFree(loginmsg);

  /* Send player MOTD */
  for(int i=0; i < Config_motdsz; ++i)
  {
    send_directchat(player, Config_motd[i]);
  }
  
  return;
}
/**
 * Process a chat message or command
 *
 * @remarks Scope: private
 *
 * @param player Player List player pointer
 * @param message Chat message
 */
void
process_chat(struct PL_entry *player, bstring message)
{
  if (message->data[0] == '/')
  {
    LOG(LOG_INFO, "Command: %s", message->data);
    send_directchat(player, message);
    
    // TODO: temporary who cmd
    bstring whocmd = bfromcstr("/who");
    bstring lcmd = bfromcstr("/login");
    if (binstrr(message, (whocmd->slen), whocmd) != BSTR_ERR)
    {
      pthread_rwlock_rdlock(&PL_rwlock);
      bstring whomsg = bformat("There are %d players online", PL_count);
      send_directchat(player, whomsg);
      pthread_rwlock_unlock(&PL_rwlock);
      bstrFree(whomsg);
    } else if(binstrr(message, (lcmd->slen), lcmd) != BSTR_ERR)
    {
      send_loginresp(player);
    }
  }
  else
  {
    send_chat(player, message);
  }
}
/**
 * A wrapper to send and delete changed packets within a client's tracked
 * radius
 */
// Private utility functions for send_chunk_radius
static void chunkradiusunload(const void *T, void *ctx)
{
  struct PL_entry *player = (void *)ctx;
  chunk_coord coord = *(chunk_coord *)T;
  send_prechunk(player, coord.x, coord.z, false);
}
static void chunkradiusload(const void *T, void *ctx)
{
  // Send full chunk
  const int sizex = 16, sizey = 128, sizez = 16;
  
  struct PL_entry *player = (void *)ctx;
  chunk_coord coord = *(chunk_coord *)T;
  
  send_prechunk(player, coord.x, coord.z, true);
  send_chunk(player, coord.x, 0, coord.z, sizex, sizey, sizez);
}
static void chunkradiusfree(const void *T, void *ctx)
{
  chunk_coord *coord = (chunk_coord *)T;
  free(coord);
}
void
send_chunk_radius(struct PL_entry *player, int32_t xin, int32_t zin, int radius)
{
  // Get "chunk coords"
  xin /= 16;
  zin /= 16;
  
  LOG(LOG_DEBUG, "Sending new chunks center:(%d, %d)", xin, zin);
  
  Set_T oldchunkset = player->loadedchunks;
  
  // Make a new set containing coordinates of all chunks client should have
  Set_T newchunkset = Set_new(400, chunkcoordcmp, chunkcoordhash);
  for(int x = -radius; x < radius; x++)
  {
    for(int z = -radius; z < radius; z++)
    {
      // Use a circular send pattern based on Bravo/MostAwesomeDude's algo
      if ( x*x + z*z <= radius*radius )
      {
	chunk_coord *coord = Malloc(sizeof(chunk_coord));
	coord->x = x + xin;
	coord->z = z + zin;
	Set_put(newchunkset, coord);
      }
    }
  }
  
  Set_T toremove = Set_minus(oldchunkset, newchunkset);
  Set_T toadd = Set_minus(newchunkset, oldchunkset);
  
  Set_map(toremove, &chunkradiusunload, (void *)player);
  Set_map(toadd, &chunkradiusload, (void *)player);
  
  player->loadedchunks = newchunkset;
  Set_map(oldchunkset, &chunkradiusfree, NULL);
  Set_free(&oldchunkset);
}