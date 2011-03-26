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

#include <beta/Player.h>

typedef enum _CDWorldDimension {
    CDWorldHell   = -1,
    CDWorldNormal =  0
} CDWorldDimension;

typedef struct _CDWorld {
    CDServer* server;

    CDString*        name;
    CDWorldDimension dimension;
    uint16_t         time;

    struct {
        pthread_spinlock_t time;
    } lock;

    CDHash* players;
    CDMap*  clients;
    CDMap*  entities;

    MCBlockPosition spawnPosition;
    CDSet*          chunks;

    CD_DEFINE_DYNAMIC;
    CD_DEFINE_ERROR;
} CDWorld;

CDWorld* CD_CreateWorld (CDServer* server, const char* name);

bool CD_WorldSave (CDWorld* self);

void CD_DestroyWorld (CDWorld* self);

void CD_WorldLoad (CDWorld* self);

MCEntityId CD_WorldGenerateEntityId (CDWorld* self);

void CD_WorldAddPlayer (CDWorld* self, CDPlayer* player);

void CD_WorldBroadcast (CDWorld* self, CDString* message);

uint16_t CD_WorldGetTime (CDWorld* self);

uint16_t CD_WorldSetTime (CDWorld* self, uint16_t time);

MCChunk* CD_WorldGetChunk (CDWorld* self, int x, int z);

void CD_WorldSetChunk (CDWorld* self, MCChunk* chunk);

#endif
