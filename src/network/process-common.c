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
  //struct evbuffer *output = bufferevent_get_output(player->bev);
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
  
  newOutputWq(tempbuf, player,player->bev, &player->outlock);

  bstrFree(hashreply);
  
  return;
}
