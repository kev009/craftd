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

#ifndef CRAFTD_SURVIVAL_PLAYER_H
#define CRAFTD_SURVIVAL_PLAYER_H

#include <craftd/Client.h>

#include <craftd/protocols/survival/common.h>
#include <craftd/protocols/survival/Packet.h>

struct _SVWorld;

/**
 * The Player class.
 */
typedef struct _SVPlayer {
    SVEntity entity;

    CDClient*        client;
    struct _SVWorld* world;

    SVFloat yaw;
    SVFloat pitch;

    CDString* username;

    CD_DEFINE_DYNAMIC;
    CD_DEFINE_ERROR;
} SVPlayer;

/**
 * Create a Player object on the given Server.
 *
 * @param server The Server the Player will play on
 *
 * @return The instantiated Player object
 */
SVPlayer* SV_CreatePlayer (CDClient* client);

/**
 * Destroy a Player object
 */
void SV_DestroyPlayer (SVPlayer* self);

/**
 * Send a chat message to a player
 *
 * @param message The message to send
 */
void SV_PlayerSendMessage (SVPlayer* self, CDString* message);

/**
 * Send a Packet to a Player
 *
 * @param packet The Packet object to send
 */
void SV_PlayerSendPacket (SVPlayer* self, SVPacket* packet);

/**
 * Send a Packet to a Player and destroy the packet
 *
 * @param packet The Packet object to send
 */
void SV_PlayerSendPacketAndClean (SVPlayer* self, SVPacket* packet);

/**
 * Send a Packet to a Player and destroy the packet data
 *
 * @param packet The Packet object to send
 */
void SV_PlayerSendPacketAndCleanData (SVPlayer* self, SVPacket* packet);

#endif
