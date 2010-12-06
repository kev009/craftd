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

#include <string.h>

#include "craftd-config.h"
#include "network.h"
#include "network-private.h"
#include "packets.h"

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
process_login(struct PL_entry *player, mcstring_t *username, uint32_t ver)
{
  // TODO: Future, async check of minecraft.net for user validity
  // TODO: Future, check against local ACL
  
  /* Check if the client version is compatible with the craftd version */
  if (ver != PROTOCOL_VERSION)
  {
    const char *dconmsg = "Client version is incompatible with this server.";
    send_kick(player, mcstring_create(strlen(dconmsg), dconmsg) );
    return;
  }
  
  /* Otherwise, finish populating their Player List entry */
  pthread_rwlock_wrlock(&player->rwlock);
  mcstring_copy(&player->username, username);
  pthread_rwlock_unlock(&player->rwlock);
  
  send_loginresp(player);
  send_prechunk(player, 0, 0, true); // TODO: pull spwan position from file
  
  return;
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
  uint8_t pid = PID_LOGIN;
  int32_t entityid = htonl(1); // TODO generate player entity IDs
  int16_t unused1 = htons(0); // Future server name? mcstring.
  int16_t unused2 = htons(0); // Future MOTD? mcstring.
  int64_t mapseed = htonll(0);
  int8_t dimension = 0;
  
  evbuffer_add(output, &pid, sizeof(pid));
  evbuffer_add(output, &entityid, sizeof(entityid));
  evbuffer_add(output, &unused1, sizeof(unused1));
  evbuffer_add(output, &unused2, sizeof(unused2));
  evbuffer_add(output, &mapseed, sizeof(mapseed));
  evbuffer_add(output, &dimension, sizeof(dimension));

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
  struct evbuffer *output = bufferevent_get_output(player->bev);
  uint8_t pid = PID_PRECHUNK;
  int32_t n_x = htonl(x);
  int32_t n_z = htonl(z);
  uint8_t n_mode = mode;
  
  evbuffer_add(output, &pid, sizeof(pid));
  evbuffer_add(output, &n_x, sizeof(n_x));
  evbuffer_add(output, &n_z, sizeof(n_z));
  evbuffer_add(output, &n_mode, sizeof(n_mode));
  
  return;
}

/**
 * Send the specified chunk to the player
 * 
 * @remarks Scope: public API method
 * 
 * @param player Player List player pointer
 * @param x global chunk x coordinate
 * @param y global chunk y coordinate
 * @param z global chunk z coordinate
 */
void
send_chunk(struct PL_entry *player, int32_t x, int16_t y, int32_t z)
{
 struct evbuffer *output = bufferevent_get_output(player->bev);
 uint8_t pid = PID_MAPCHUNK;
 
 evbuffer_add(output, &pid, sizeof(pid));
 
 return;
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
send_kick(struct PL_entry *player, mcstring_t *dconmsg)
{
  struct evbuffer *output = bufferevent_get_output(player->bev);
  uint8_t pid = PID_DISCONNECT;
  int16_t slen = htons(dconmsg->slen);

  evbuffer_add(output, &pid, sizeof(pid));
  evbuffer_add(output, &slen, sizeof(slen));
  evbuffer_add(output, dconmsg->str, dconmsg->slen);
  
  mcstring_free(dconmsg);
  
  /* TODO forcefully close the socket and perform manual cleanup if the client
   * doesn't voluntarily disconnect
   */
}
