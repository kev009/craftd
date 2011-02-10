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

#include <event2/event.h>
#include <event2/buffer.h>
#include <event2/bufferevent.h>

#include "craftd.h"
#include "proxy.h"
#include "network/network.h"
#include "network/network-private.h"
#include "network/packets.h"

/**
 * This method sends a handshake to a connected server via proxy
 * The sever connection is handled with player->bev
 * 
 * @remarks Scope: private
 * 
 * @param player player struct to connect with
 */
void send_proxyhandshake(struct PL_entry *player)
{
  //struct evbuffer *output = bufferevent_get_output(player->sev);
  struct evbuffer *tempbuf = evbuffer_new();
  
  uint8_t pid = PID_HANDSHAKE;
  int16_t n_ulen = htons(player->username->slen);
  
  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &n_ulen, sizeof(n_ulen));
  evbuffer_add(tempbuf, player->username->data, player->username->slen);
  
  newOutputWq(tempbuf,player,player->sev,&player->sevoutlock);
}

/**
 * This method sends a chat message to a connected server via proxy
 * The sever connection is handled with player->bev
 * 
 * @remarks Scope: private
 * 
 * @param player player struct to connect with
 * @param message bstring message to conenct with
 */
void send_proxychat(struct PL_entry *player,bstring message)
{
  //struct evbuffer *output = bufferevent_get_output(player->sev);
  struct evbuffer *tempbuf = evbuffer_new();
  
  uint8_t pid = PID_CHAT;
  int16_t mlen = htons(message->slen);

  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &mlen, sizeof(mlen));
  evbuffer_add(tempbuf, message->data, message->slen);

  newOutputWq(tempbuf,player,player->sev,&player->sevoutlock);
  
}

/**
 * This method sends login packets to a connected server via proxy
 * The sever connection is handled with player->bev
 * 
 * @remarks Scope: private
 * 
 * @param player player struct to connect with
 */
void send_proxylogin(struct PL_entry *player)
{
  struct evbuffer *tempbuf = evbuffer_new();
  
  uint8_t pid = PID_LOGIN;
  int32_t entityid = htonl(8);
  int16_t l_su = htons(player->username->slen);
  int16_t unused2 = htons(0); // Future MOTD? mcstring.
  int64_t mapseed = 0;
  int8_t dimension = 0;
  
  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &entityid, sizeof(entityid));
  evbuffer_add(tempbuf, &l_su, sizeof(l_su));
  evbuffer_add(tempbuf, player->username->data, player->username->slen);
  evbuffer_add(tempbuf, &unused2, sizeof(unused2));
  evbuffer_add(tempbuf, &mapseed, sizeof(mapseed));
  evbuffer_add(tempbuf, &dimension, sizeof(dimension));
  
  newOutputWq(tempbuf,player,player->sev,&player->sevoutlock);
}


