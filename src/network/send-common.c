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
#include "network/network.h"
#include "network/network-private.h"
#include "network/packets.h"

// Hack zlib in to test chunk sending
#include <stdio.h>
#include <zlib.h>
#include <fcntl.h>
#include <sys/stat.h>

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
  
  newOutputWq(tempbuf,player,player->bev,&player->outlock);

  return;
}

/**
 * Send a chat packet to the player.
 * 
 * @remarks Scope: public API method
 *
 * @param player Player List player pointer
 * @param message Chat message
 */
void
send_directchat(struct PL_entry *player, bstring message)
{
  struct evbuffer *tempbuf = evbuffer_new();

  uint8_t pid = PID_CHAT;
  int16_t mlen = htons(message->slen);

  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &mlen, sizeof(mlen));
  evbuffer_add(tempbuf, message->data, message->slen);

  newOutputWq(tempbuf,player,player->bev,&player->outlock);

  return;
}

/**
 * Send chat message to all online players.
 * 
 * @remarks Scope: public API method
 *
 * @param player Player List player pointer
 * @param message Chat message
 */
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

/**
 * Send system message to all online players.
 * 
 * @remarks Scope: public API method
 *
 * @param message Chat message
 */
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
  struct evbuffer *tempbuf = evbuffer_new();
  
  int8_t pid = PID_PRECHUNK;
  int32_t n_x = htonl(x);
  int32_t n_z = htonl(z);
  uint8_t n_mode = mode;
  
  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &n_x, sizeof(n_x));
  evbuffer_add(tempbuf, &n_z, sizeof(n_z));
  evbuffer_add(tempbuf, &n_mode, sizeof(n_mode));
  
  newOutputWq(tempbuf,player,player->bev,&player->outlock);
  
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
  struct evbuffer *tempbuf = evbuffer_new();
  
  int8_t pid = PID_SPAWNPOS;
  int32_t n_x = htonl(x);
  int32_t n_y = htonl(y);
  int32_t n_z = htonl(z);
  
  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &n_x, sizeof(n_x));
  evbuffer_add(tempbuf, &n_y, sizeof(n_y));
  evbuffer_add(tempbuf, &n_z, sizeof(n_z));
  
  newOutputWq(tempbuf,player,player->bev,&player->outlock);
  
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
  
  newOutputWq(tempbuf,player,player->bev,&player->outlock);
  //evbuffer_free(tempbuf);
  
  return;
}

void
send_namedentity(struct PL_entry *player, int32_t eid)
{
  struct evbuffer *tempbuf = evbuffer_new();

  /*
  int8_t pid = PID_NAMEDENTITYSPAWN;
  int16_t slen = htons(0); //eid map to player
  */
  
  newOutputWq(tempbuf,player,player->bev,&player->outlock);
}

void
send_entity(struct PL_entry *player, int32_t eid)
{
  struct evbuffer *tempbuf = evbuffer_new();

  int8_t pid = PID_ENTITYINIT;
  eid = htonl(eid);

  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &eid, sizeof(eid));

  newOutputWq(tempbuf,player,player->bev,&player->outlock);
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
  struct evbuffer *tempbuf = evbuffer_new();
  
  uint8_t pid = PID_DISCONNECT;
  int16_t slen = htons(dconmsg->slen);

  LOG(LOG_NOTICE, "IP %s kicked: %s", player->ip, dconmsg->data);

  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &slen, sizeof(slen));
  evbuffer_add(tempbuf, dconmsg->data, dconmsg->slen);
  
  newOutputWq(tempbuf,player,player->bev,&player->outlock);
  
  deferLogout(player);
  return;
}

/**
 * Send time update packet to specified player
 * 
 * @remarks Scope: public API method
 * 
 * @param player Player List player pointer
 * @param time Time in raw game format.
 */
void
send_timeupdate(struct PL_entry *player, int time)
{
  struct evbuffer *tempbuf = evbuffer_new();
  
  uint8_t pid = PID_TIMEUPDATE;
  int64_t ntime = htonll(time);


  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &ntime, sizeof(ntime));
  
  newOutputWq(tempbuf,player,player->bev,&player->outlock);
  
  return;
}
