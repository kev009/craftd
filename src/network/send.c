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

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "craftd-config.h"
#include "network.h"
#include "network-private.h"
#include "packets.h"
#include "mapchunk.h"

// Hack zlib in to test chunk sending
#include <stdio.h>
#include <zlib.h>
#include <fcntl.h>
#include <sys/stat.h>

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
      /* Flip bits */
      player->position.x = Cswapd(((struct packet_playerpos*) packet)->x);
      player->position.y = Cswapd(((struct packet_playerpos*) packet)->y);
      player->position.z = Cswapd(((struct packet_playerpos*) packet)->z);
      
      send_chunk_radius(player, player->position.x, player->position.z, RADIUS);
      
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
      /* Flip bits */
      player->position.x = Cswapd(((struct packet_movelook*) packet)->x);
      player->position.y = Cswapd(((struct packet_movelook*) packet)->y);
      player->position.z = Cswapd(((struct packet_movelook*) packet)->z);
      player->position.yaw = Cswapf(((struct packet_movelook*) packet)->yaw);
      player->position.pitch = Cswapf(((struct packet_movelook*) packet)->pitch);
      
      send_chunk_radius(player, player->position.x, player->position.z, RADIUS);

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
  
  // Temp
  const int spawnx = -264;
  const int spawnz = 261;
  
  const int spawnradius = 5;
  send_chunk_radius(player, spawnx, spawnz, spawnradius);
  
  send_spawnpos(player, spawnx, 65, spawnz); // TODO: pull spawn position from file
  //send inv
  send_movelook(player, spawnx, 70.1, 70.2, spawnz, 0, 0, false); //TODO: pull position from file

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
 * Internal method that sends a handshake response packet
 *
 * @remarks Scope: private
 *
 * @param player Player List player pointer
 * @param username mcstring of the handshake username
 */
void
process_handshake(struct PL_entry *player, bstring username)
{
  struct evbuffer *output = bufferevent_get_output(player->bev);
  struct evbuffer *tempbuf = evbuffer_new();

  /* Use a non-authenticating handshake for now 
   * XXX: just a hack to get a login.
   * XXX  This needs to be added to the worker pool for computing hash
   * from minecraft.net
   */
  
  uint8_t pid = PID_HANDSHAKE;
  bstring hashreply = bfromcstr("-");
  int16_t n_hlen = htons(hashreply->slen);
  
  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &n_hlen, sizeof(n_hlen));
  evbuffer_add(tempbuf, hashreply->data, hashreply->slen);
  
  evbuffer_add_buffer(output, tempbuf);
  evbuffer_free(tempbuf);
  bstrFree(hashreply);
  
  return;
}

/**
 * Process a chat message or command
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
    if (binstrr(message, (whocmd->slen), whocmd) != BSTR_ERR)
    {
      pthread_rwlock_rdlock(&PL_rwlock);
      bstring whomsg = bformat("There are %d players online", PL_count);
      send_directchat(player, whomsg);
      pthread_rwlock_unlock(&PL_rwlock);
      bstrFree(whomsg);
    }
  }
  else
  {
    send_chat(player, message);
  }
}

/**
 * Internal method that sends a login response packet
 *
 * @remarks Scope: private
 * 
 * @param player Player List player pointer
 */
void
send_loginresp(struct PL_entry *player)
{
  struct evbuffer *output = bufferevent_get_output(player->bev);
  struct evbuffer *tempbuf = evbuffer_new();
  
  uint8_t pid = PID_LOGIN;
  int32_t entityid = htonl(player->eid);
  int16_t unused1 = htons(0); // Future server name? mcstring.
  int16_t unused2 = htons(0); // Future MOTD? mcstring.
  int64_t mapseed = htonll(0);
  int8_t dimension = 0;
  
  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &entityid, sizeof(entityid));
  evbuffer_add(tempbuf, &unused1, sizeof(unused1));
  evbuffer_add(tempbuf, &unused2, sizeof(unused2));
  evbuffer_add(tempbuf, &mapseed, sizeof(mapseed));
  evbuffer_add(tempbuf, &dimension, sizeof(dimension));
  
  evbuffer_add_buffer(output, tempbuf);
  evbuffer_free(tempbuf);

  return;
}

void
send_directchat(struct PL_entry *player, bstring message)
{
  struct evbuffer *output = bufferevent_get_output(player->bev);
  struct evbuffer *tempbuf = evbuffer_new();

  uint8_t pid = PID_CHAT;
  int16_t mlen = htons(message->slen);

  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &mlen, sizeof(mlen));
  evbuffer_add(tempbuf, message->data, message->slen);

  evbuffer_add_buffer(output, tempbuf);
  evbuffer_free(tempbuf);

  return;
}

void
send_chat(struct PL_entry *player, bstring message)
{
  struct PL_entry *player_iter;

  bstring newmsg = bformat("<%s> %s", player->username->data, message->data);
  
  LOG(LOG_INFO, "Chat: %s", newmsg->data);

  pthread_rwlock_rdlock(&PL_rwlock);
  SLIST_FOREACH(player_iter, &PL_head, PL_entries)
  {
    send_directchat(player_iter, newmsg);
  }
  pthread_rwlock_unlock(&PL_rwlock);

  bstrFree(newmsg);
  
  return;
}

void
send_syschat(bstring message)
{
 struct PL_entry *player_iter;

  LOG(LOG_INFO, "Syschat: %s", message->data);

  pthread_rwlock_rdlock(&PL_rwlock);
  SLIST_FOREACH(player_iter, &PL_head, PL_entries)
  {
    send_directchat(player_iter, message);
  }
  pthread_rwlock_unlock(&PL_rwlock);

  return;
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

/**
 * Send a prechunk packet to the player
 * 
 * @remarks Scope: public API method
 *
 * @param player Player List player pointer
 * @param x chunk x coordinate
 * @param z chunk z coordinate
 * @param mode unload (false) or load (true) the specified chunk
 */
void
send_prechunk(struct PL_entry *player, int32_t x, int32_t z, bool mode)
{
  struct evbuffer *output = bufferevent_get_output(player->bev);
  struct evbuffer *tempbuf = evbuffer_new();
  
  int8_t pid = PID_PRECHUNK;
  int32_t n_x = htonl(x);
  int32_t n_z = htonl(z);
  uint8_t n_mode = mode;
  
  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &n_x, sizeof(n_x));
  evbuffer_add(tempbuf, &n_z, sizeof(n_z));
  evbuffer_add(tempbuf, &n_mode, sizeof(n_mode));
  
  evbuffer_add_buffer(output, tempbuf);
  evbuffer_free(tempbuf);
  
  return;
}

/**
 * Send the specified chunk to the player
 * 
 * @remarks Scope: public API method
 * @remarks Internally and over the network size{x,y,z} are -1 over the wire
 * 
 * @param player Player List player pointer
 * @param x global chunk x coordinate
 * @param y global chunk y coordinate
 * @param z global chunk z coordinate
 * @param sizex actual x size
 * @param sizey actual y size
 * @param sizez actual z size
 */
void
send_chunk(struct PL_entry *player, int32_t x, int16_t y, int32_t z,
	   uint8_t sizex, uint8_t sizey, uint8_t sizez)
{
  struct evbuffer *output = bufferevent_get_output(player->bev);
  struct evbuffer *tempbuf = evbuffer_new();
  int8_t pid = PID_MAPCHUNK;
  int32_t n_x = htonl(x * 16);
  int16_t n_y = htons(y);
  int32_t n_z = htonl(z * 16);
 
  /* Check that the chunk size is greater than zero since the protocol must
   * subtract one before sending.  If so, do it.
   */
  assert(sizex > 0 && sizey > 0 && sizez > 0);
  assert(sizex <= 128 && sizey <= 128 && sizez <= 128);
  --sizex;
  --sizey;
  --sizez;

  uint8_t mapdata[MAX_CHUNKARRAY];
  if (loadChunk(x, z, &mapdata[0]) != 0)
  {
    /* TODO: bad read.  add to mapgen queue if file DNE */
    return;
  }

  uLongf written = MAX_CHUNKARRAY;
  Bytef *buffer = (Bytef*)Malloc(MAX_CHUNKARRAY);
  if (compress(buffer, &written, &mapdata[0], MAX_CHUNKARRAY) != Z_OK)
    assert(false);
  int32_t n_written = htonl(written);
 
  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &n_x, sizeof(n_x));
  evbuffer_add(tempbuf, &n_y, sizeof(n_y));
  evbuffer_add(tempbuf, &n_z, sizeof(n_z));
  evbuffer_add(tempbuf, &sizex, sizeof(sizex));
  evbuffer_add(tempbuf, &sizey, sizeof(sizey));
  evbuffer_add(tempbuf, &sizez, sizeof(sizez));
  evbuffer_add(tempbuf, &n_written, sizeof(n_written));
  evbuffer_add(tempbuf, buffer, written);

  /* TODO: swap to this zero copy method */
  //evbuffer_add_reference(tempbuf, buffer, written, chunkfree_cb, buffer);
  
  evbuffer_add_buffer(output, tempbuf);
  evbuffer_free(tempbuf);
  
  free(buffer);
 
  return;
}

/**
 * Send the client their spawn position.  Can also be used to later update
 * their compass bearing.
 * 
 * @param player Player List player pointer
 * @param x global chunk x coordinate
 * @param y global chunk y coordinate
 * @param z global chunk z coordinate
 */
void
send_spawnpos(struct PL_entry *player, int32_t x, int32_t y, int32_t z)
{
  struct evbuffer *output = bufferevent_get_output(player->bev);
  struct evbuffer *tempbuf = evbuffer_new();
  
  int8_t pid = PID_SPAWNPOS;
  int32_t n_x = htonl(x);
  int32_t n_y = htonl(y);
  int32_t n_z = htonl(z);
  
  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &n_x, sizeof(n_x));
  evbuffer_add(tempbuf, &n_y, sizeof(n_y));
  evbuffer_add(tempbuf, &n_z, sizeof(n_z));
  
  evbuffer_add_buffer(output, tempbuf);
  evbuffer_free(tempbuf);
  
  return;
}

/**
 * Send a combined move+look packet to the player.
 * 
 * @remarks Scope: public API method
 * @remarks Note flip-flopped y and stance from client.  -_- Notch.
 * 
 * @param player Player List player pointer
 * @param x absolute x coordinate
 * @param stance modify player bounding box
 * @param y absolute y coordinate
 * @param z absolute z coordinate
 * @param yaw rotation on the x-axis 
 * @param pitch rotation on the y-axis 
 * @param flying on the ground or in the air (like 0x0A)
 */
void
send_movelook(struct PL_entry *player, double x, double stance, double y,
	      double z, float yaw, float pitch, bool flying)
{
  struct evbuffer *output = bufferevent_get_output(player->bev);
  struct evbuffer *tempbuf = evbuffer_new();
  
  int8_t pid = PID_PLAYERMOVELOOK;
  double n_x = Cswapd(x);
  double n_stance = Cswapd(stance);
  double n_y = Cswapd(y);
  double n_z = Cswapd(z);
  float n_yaw = Cswapf(yaw);
  float n_pitch = Cswapf(pitch);
  int8_t n_flying = flying; // Cast to int8 to ensure it is 1 byte
  
  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &n_x, sizeof(n_x));
  evbuffer_add(tempbuf, &n_y, sizeof(n_y));
  evbuffer_add(tempbuf, &n_stance, sizeof(n_stance));
  evbuffer_add(tempbuf, &n_z, sizeof(n_z));
  evbuffer_add(tempbuf, &n_yaw, sizeof(n_yaw));
  evbuffer_add(tempbuf, &n_pitch, sizeof(n_pitch));
  evbuffer_add(tempbuf, &n_flying, sizeof(n_flying));
  
  evbuffer_add_buffer(output, tempbuf);
  evbuffer_free(tempbuf);
  
  return;
}

void
send_namedentity(struct PL_entry *player, int32_t eid)
{
  struct evbuffer *output = bufferevent_get_output(player->bev);
  struct evbuffer *tempbuf = evbuffer_new();

  /*
  int8_t pid = PID_NAMEDENTITYSPAWN;
  int16_t slen = htons(0); //eid map to player
  */
  
  evbuffer_add_buffer(output, tempbuf);
  evbuffer_free(tempbuf);
}

void
send_entity(struct PL_entry *player, int32_t eid)
{
  struct evbuffer *output = bufferevent_get_output(player->bev);
  struct evbuffer *tempbuf = evbuffer_new();

  int8_t pid = PID_ENTITYINIT;
  eid = htonl(eid);

  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &eid, sizeof(eid));

  evbuffer_add_buffer(output, tempbuf);
  evbuffer_free(tempbuf);
}

void
send_entityrelmove(struct PL_entry *player, int32_t eid)
{
  // TODO
}

/**
 * Kick the specified player
 * 
 * @remarks Scope: public API method
 * 
 * @param player Player List player pointer
 * @param dconmsg Pointer to an mcstring with the kick message
 */
void
send_kick(struct PL_entry *player, bstring dconmsg)
{
  struct evbuffer *output = bufferevent_get_output(player->bev);
  struct evbuffer *tempbuf = evbuffer_new();
  
  uint8_t pid = PID_DISCONNECT;
  int16_t slen = htons(dconmsg->slen);

  LOG(LOG_NOTICE, "IP %s kicked: %s", player->ip, dconmsg->data);

  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &slen, sizeof(slen));
  evbuffer_add(tempbuf, dconmsg->data, dconmsg->slen);
  
  evbuffer_add_buffer(output, tempbuf);
  evbuffer_free(tempbuf);
  
  errorcb(player->bev, BEV_EVENT_EOF, player);
  
  return;
}
