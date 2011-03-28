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

#include <beta/World.h>

CDWorld*
CD_CreateWorld (CDServer* server, const char* name)
{
    CDWorld* self = CD_malloc(sizeof(CDWorld));

    assert(name);

    if (pthread_spin_init(&self->lock.time, 0) != 0) {
        CD_abort("pthread spinlock failed to initialize");
    }

    self->server = server;

    self->name      = CD_CreateStringFromCStringCopy(name);
    self->dimension = CDWorldNormal;
    self->time      = 0;

    self->players  = CD_CreateHash();
    self->clients  = CD_CreateMap();
    self->entities = CD_CreateMap();

    self->chunks = CD_CreateSetWith(2000, (CDSetCompare) MC_CompareChunkPosition, (CDSetHash) MC_HashChunkPosition);

    DYNAMIC(self) = CD_CreateDynamic();
    ERROR(self)   = CDNull;

    CD_EventDispatch(server, "World.create", self);

    return self;
}

bool
CD_WorldSave (CDWorld* self)
{
    bool status;

    CD_EventDispatchWithError(status, self->server, "World.save", self);

    return status == CDOk;
}

void
CD_DestroyWorld (CDWorld* self)
{
    assert(self);

    CD_EventDispatch(self->server, "World.destroy", self);

    CD_HASH_FOREACH(self->players, it) {
        CDPlayer* player = (CDPlayer*) CD_HashIteratorValue(it);

        if (player->client->status != CDClientDisconnect) {
            CD_ServerKick(self->server, player->client, NULL);
        }
    }

    CD_DestroyHash(self->players);
    CD_DestroyMap(self->clients);
    CD_DestroyMap(self->entities);

    CD_DestroySet(self->chunks);

    CD_DestroyString(self->name);

    CD_DestroyDynamic(DYNAMIC(self));

    pthread_spin_destroy(&self->lock.time);

    CD_free(self);
}

// FIXME: This is just a dummy function
MCEntityId
CD_WorldGenerateEntityId (CDWorld* self)
{
    MCEntityId result;

    assert(self);

    if (CD_MapLength(self->entities) != 0) {
        result = ((MCEntity*) CD_MapLast(self->entities))->id + 1;
    }
    else {
        result = 10;
    }

    return result;
}

void
CD_WorldAddPlayer (CDWorld* self, CDPlayer* player)
{
}

void
CD_WorldBroadcast (CDWorld* self, CDString* message)
{
    assert(self);

    CDPacketChat pkt = {
        .response = {
            .message = message
        }
    };

    CDPacket response = { CDResponse, CDChat, (CDPointer) &pkt };

    CDBuffer* buffer = CD_PacketToBuffer(&response);

    CD_HASH_FOREACH(self->players, it) {
        CDPlayer* player = (CDPlayer*) CD_HashIteratorValue(it);

        pthread_rwlock_rdlock(&player->client->lock.status);
        if (player->client->status != CDClientDisconnect) {
            CD_ClientSendBuffer(player->client, buffer);
        }
        pthread_rwlock_unlock(&player->client->lock.status);
    }

    CD_DestroyBuffer(buffer);
    CD_DestroyPacketData(&response);
}

uint16_t
CD_WorldGetTime (CDWorld* self)
{
    uint16_t result;

    assert(self);

    pthread_spin_lock(&self->lock.time);
    result = self->time;
    pthread_spin_unlock(&self->lock.time);

    return result;
}

uint16_t
CD_WorldSetTime (CDWorld* self, uint16_t time)
{
    assert(self);

    pthread_spin_lock(&self->lock.time);
    self->time = time;
    pthread_spin_unlock(&self->lock.time);

    return time;
}

MCChunk*
CD_WorldGetChunk (CDWorld* self, int x, int z)
{
    MCChunk* result = CD_alloc(sizeof(MCChunk));

    CD_EventDispatch(self->server, "World.chunk", self, x, z, result);

    return result;
}

void
CD_WorldSetChunk (CDWorld* self, MCChunk* chunk)
{
    CD_EventDispatch(self->server, "World.chunk=", self, chunk->position.x, chunk->position.z, chunk);
}
