#ifndef CRAFTD_NETWORK_PRIVATE_H
#define CRAFTD_NETWORK_PRIVATE_H

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

#endif

#include "network.h"

/* Private method forward declarations */
void process_packet(struct PL_entry *player, uint8_t pkttype, void * packet);

void process_proxypacket(struct PL_entry *player, uint8_t pkttype, void * packet);
void process_proxyserverpacket(struct PL_entry *player, uint8_t pkttype, void * packet);
bool process_isproxypassthrough(uint8_t pkttype);
bool process_isproxyserverpassthrough(uint8_t pkttype);
void send_proxyhandshake(struct PL_entry *player);

int len_statemachine(uint8_t pkttype, struct evbuffer *input);
void * packetdecoder(uint8_t pkttype, int pktlen, struct bufferevent *bev);
void packetfree(uint8_t pkttype, void * packet);
void process_handshake(struct PL_entry *player, bstring username);
void process_login(struct PL_entry *player, bstring username, 
			  uint32_t ver);
void process_chat(struct PL_entry *player, bstring message);
void send_loginresp(struct PL_entry *player);
