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

#include <craftd/protocols/survival/World.h>

SVWorld*
SV_CreateWorld (CDServer* server, const char* name)
{
    SVWorld* self = CD_malloc(sizeof(SVWorld));

    assert(name);

    if (pthread_spin_init(&self->lock.time, 0) != 0) {
        CD_abort("pthread spinlock failed to initialize");
    }

    self->server = server;

    J_DO { self->config = NULL;
        J_IN(data, server->config->data, "server") {
            J_IN(game, data, "game") {
                J_IN(protocol, game, "protocol") {
                    J_FOREACH(world, protocol, "worlds") {
                        J_IF_STRING(world, "name") {
                            if (CD_CStringIsEqual(J_STRING_VALUE, name)) {
                                self->config = (CDRawConfig) world;
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    self->name      = CD_CreateStringFromCStringCopy(name);
    self->dimension = SVWorldNormal;
    self->time      = 0;

    self->players  = CD_CreateHash();
    self->entities = CD_CreateMap();

    self->chunks = CD_CreateSetWith(2000, (CDSetCompare) SV_CompareChunkPosition, (CDSetHash) SV_HashChunkPosition);

    DYNAMIC(self) = CD_CreateDynamic();
    ERROR(self)   = CDNull;

    CD_EventDispatch(server, "World.create", self);

    return self;
}

bool
SV_WorldSave (SVWorld* self)
{
    bool status;

    CD_EventDispatchWithError(status, self->server, "World.save", self);

    return status == CDOk;
}

void
SV_DestroyWorld (SVWorld* self)
{
    assert(self);

    CD_EventDispatch(self->server, "World.destroy", self);

    CD_HASH_FOREACH(self->players, it) {
        SVPlayer* player = (SVPlayer*) CD_HashIteratorValue(it);

        if (player->client->status != CDClientDisconnect) {
            CD_ServerKick(self->server, player->client, NULL);
        }
    }

    CD_DestroyHash(self->players);
    CD_DestroyMap(self->entities);

    CD_DestroySet(self->chunks);

    CD_DestroyString(self->name);

    CD_DestroyDynamic(DYNAMIC(self));

    pthread_spin_destroy(&self->lock.time);

    CD_free(self);
}

// FIXME: This is just a dummy function
SVEntityId
SV_WorldGenerateEntityId (SVWorld* self)
{
    SVEntityId result;

    assert(self);

    if (CD_MapLength(self->entities) != 0) {
        result = ((SVEntity*) CD_MapLast(self->entities))->id + 1;
    }
    else {
        result = 10;
    }

    return result;
}

void
SV_WorldAddPlayer (SVWorld* self, SVPlayer* player)
{
}

void
SV_WorldBroadcastBuffer (SVWorld* self, CDBuffer* buffer)
{
    assert(self);

    CD_HASH_FOREACH(self->players, it) {
        SVPlayer* player = (SVPlayer*) CD_HashIteratorValue(it);

        pthread_rwlock_rdlock(&player->client->lock.status);
        if (player->client->status != CDClientDisconnect) {
            CD_ClientSendBuffer(player->client, buffer);
        }
        pthread_rwlock_unlock(&player->client->lock.status);
    }
}

void
SV_WorldBroadcastPacket (SVWorld* self, SVPacket* packet)
{
    assert(self);

    CDBuffer* buffer = SV_PacketToBuffer(packet);

    SV_WorldBroadcastBuffer(self, buffer);

    CD_DestroyBuffer(buffer);
}

void
SV_WorldBroadcastMessage (SVWorld* self, CDString* message)
{
    assert(self);

    SVPacketChat pkt = {
        .response = {
            .message = message
        }
    };

    SVPacket response = { SVResponse, SVChat, (CDPointer) &pkt };

    SV_WorldBroadcastPacket(self, &response);

    SV_DestroyPacketData(&response);
}

uint16_t
SV_WorldGetTime (SVWorld* self)
{
    uint16_t result;

    assert(self);

    pthread_spin_lock(&self->lock.time);
    result = self->time;
    pthread_spin_unlock(&self->lock.time);

    return result;
}

uint16_t
SV_WorldSetTime (SVWorld* self, uint16_t time)
{
    assert(self);

    pthread_spin_lock(&self->lock.time);
    self->time = time;
    pthread_spin_unlock(&self->lock.time);

    return time;
}

SVChunk*
SV_WorldGetChunk (SVWorld* self, int x, int z)
{
    SVChunk* result = CD_alloc(sizeof(SVChunk));
    CDError  status;

    CD_EventDispatchWithError(status, self->server, "World.chunk", self, x, z, result);

    if (status == CDOk) {
        return result;
    }
    else {
        CD_free(result);

        errno = CD_ErrorToErrno(status);

        return NULL;
    }
}

void
SV_WorldSetChunk (SVWorld* self, SVChunk* chunk)
{
    CD_EventDispatch(self->server, "World.chunk=", self, chunk->position.x, chunk->position.z, chunk);
}
