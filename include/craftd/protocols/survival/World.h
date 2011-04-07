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

#ifndef CRAFTD_BETA_WORLD_H
#define CRAFTD_BETA_WORLD_H

#include <craftd/Server.h>

#include <craftd/protocols/survival/Player.h>

typedef enum _SVWorldError {
    SVWorldErrUnknown = 1,
    SVWorldErrUsernameTaken = 2
} SVWorldError;

typedef enum _SVWorldDimension {
    SVWorldHell   = -1,
    SVWorldNormal =  0
} SVWorldDimension;

typedef struct _SVWorld {
    CDServer* server;

    CDRawConfig config;

    CDString*        name;
    SVWorldDimension dimension;
    uint16_t         time;

    struct {
        pthread_spinlock_t time;
    } lock;

    /// The currently connected players
    CDHash* players;

    /// All world entities (including players)
    CDMap*  entities;

    SVBlockPosition spawnPosition;
    CDSet*          chunks;

    SVEntityId lastGeneratedEntityId;

    CD_DEFINE_DYNAMIC;
    CD_DEFINE_ERROR;
} SVWorld;

SVWorld* SV_CreateWorld (CDServer* server, const char* name);

bool SV_WorldSave (SVWorld* self);

void SV_DestroyWorld (SVWorld* self);

void SV_WorldLoad (SVWorld* self);

SVEntityId SV_WorldGenerateEntityId (SVWorld* self);

/**
 * Adds a player to the given world instance. If a player
 * with the same username exists, this may fail.
 *
 * @returns true if the player was successfully added; false otherwise.
 *          use ERROR(self) to get the error code.
 */
bool SV_WorldAddPlayer (SVWorld* self, SVPlayer* player);

/**
 * Removes the instance of the player from the world.
 */
void SV_WorldRemovePlayer (SVWorld* self, SVPlayer* player);

void SV_WorldBroadcastBuffer (SVWorld* self, CDBuffer* buffer);

void SV_WorldBroadcastPacket (SVWorld* self, SVPacket* packet);

void SV_WorldBroadcastMessage (SVWorld* self, CDString* message);

uint16_t SV_WorldGetTime (SVWorld* self);

uint16_t SV_WorldSetTime (SVWorld* self, uint16_t time);

SVChunk* SV_WorldGetChunk (SVWorld* self, int x, int z);

void SV_WorldSetChunk (SVWorld* self, SVChunk* chunk);

#endif
