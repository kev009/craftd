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

#include <craftd/Server.h>
#include <craftd/Plugin.h>
#include <craftd/Player.h>

// TODO Remove HAX
#include <zlib.h>

static struct {
    pthread_mutex_t login;
} lock;

static
bool
cdbase_CompareMCPosition (CDSet* self, MCPosition* a, MCPosition* b)
{
    return (a->x == b->x && a->y == b->y && a->z == b->z);
}

static
unsigned int
cdbase_HashMCPosition (CDSet* self, MCPosition* position)
{
    const int HASHMULTIPLIER = 31;

    // TODO: find a good 3D hash function
    return position->x * HASHMULTIPLIER * position->z + position->y;
}

static
bool
cdbase_SendChunk (CDServer* server, CDPlayer* player, MCPosition* coord)
{
    CD_PACKET_DO {
        CDPacketPreChunk pkt;
        pkt.response.x    = coord->x;
        pkt.response.z    = coord->z;
        pkt.response.mode = true;

        CDPacket response = { CDResponse, CDPreChunk, (CDPointer) &pkt };

        CD_PlayerSendPacketAndCleanData(player, &response);
    }

    CD_PACKET_DO {
        CDPacketMapChunk pkt;
        pkt.response.position.x = CD_WORLD_COORD(coord->x);
        pkt.response.position.y = 0;
        pkt.response.position.z = CD_WORLD_COORD(coord->z);
        pkt.response.size.x     = 16;
        pkt.response.size.y     = 128;
        pkt.response.size.z     = 16;

        SDEBUG(server, "sending chunk (%d, %d)", coord->x, coord->z);

        uint8_t mapdata[81920];

        CD_EventDispatch(server, "Chunk.load", coord->x, coord->z, mapdata);

        uLongf written = compressBound(81920);
        Bytef* buffer  = (Bytef*) CD_malloc(compressBound(81920));
        if (compress(buffer, &written, mapdata, 81920) != Z_OK) {
            SERR(server, "zlib compress failure");
            return false;
        }
        SDEBUG(server, "compressed %ld bytes", written);

        pkt.response.length = (MCInteger) written;
        pkt.response.item   = (MCByte*) buffer;

        CDPacket response = { CDResponse, CDMapChunk, (CDPointer) &pkt };

        CD_PlayerSendPacketAndCleanData(player, &response);
    }

    return true;
}

static
void
cdbase_ChunkRadiusUnload (CDSet* self, MCPosition* coord, CDPlayer* player)
{
    CD_PACKET_DO {
        CDPacketPreChunk pkt;
        pkt.response.x    = coord->x;
        pkt.response.z    = coord->z;
        pkt.response.mode = false;

        CDPacket response = { CDResponse, CDPreChunk, (CDPointer) &pkt };

        CD_PlayerSendPacketAndCleanData(player, &response);
    }

    CD_free(coord);
}

static
void
cdbase_ChunkMemberFree (CDSet* self, MCPosition* coord, CDPointer unused)
{
    CD_free(coord);
}

static
void
cdbase_ChunkRadiusLoad (CDSet* self, MCPosition* coord, CDPlayer* player)
{
    cdbase_SendChunk(player->server, player, coord);
}

static
void
cdbase_SendChunkRadius (CDPlayer* player, MCPosition* area, int radius)
{
    CDSet* loadedChunks = (CDSet*) CD_HashGet(PRIVATE(player), "Player.loadedChunks");
    CDSet* oldChunks    = loadedChunks;
    CDSet* newChunks    = (CDSet*) CD_CreateSet(400, (CDSetCompare) cdbase_CompareMCPosition,
        (CDSetHash) cdbase_HashMCPosition);

    for (int x = -radius; x < radius; x++) {
        for (int z = -radius; z < radius; z++) {
            if ((x * x + z * z) <= (radius * radius)) {
                MCPosition* coord = CD_malloc(sizeof(MCPosition));
                coord->x          = x + area->x;
                coord->y          = 0;
                coord->z          = z + area->z;

                CD_SetPut(newChunks, (CDPointer) coord);
            }
        }
    }

    CDSet* toRemove = CD_SetMinus(oldChunks, newChunks);
    CDSet* toAdd    = CD_SetMinus(newChunks, oldChunks);

    CD_SetMap(toRemove, (CDSetApply) cdbase_ChunkRadiusUnload, (CDPointer) player);
    CD_SetMap(toAdd, (CDSetApply) cdbase_ChunkRadiusLoad, (CDPointer) player);

    CD_DestroySet(toRemove);
    CD_DestroySet(toAdd);
    CD_DestroySet(oldChunks);

    CD_HashPut(PRIVATE(player), "Player.loadedChunks", (CDPointer) newChunks);
}

static
void
cdbase_TimeIncrease (void* _, void* __, CDServer* self)
{
    uint16_t current = CD_ServerGetTime(self);

    if (current >= 0 && current <= 11999) {
        CD_ServerSetTime(self, current += self->config->cache.rate.day);
    }
    else if (current >= 12000 && current <= 13799) {
        CD_ServerSetTime(self, current += self->config->cache.rate.sunset);
    }
    else if (current >= 13800 && current <= 22199) {
        CD_ServerSetTime(self, current += self->config->cache.rate.night);
    }
    else if (current >= 22200 && current <= 23999) {
        CD_ServerSetTime(self, current += self->config->cache.rate.sunrise);
    }

    if (current >= 24000) {
        CD_ServerSetTime(self, current - 24000);
    }
}

static
void
cdbase_TimeUpdate (void* _, void* __, CDServer* self)
{
    CD_PACKET_DO {
        CDPacketTimeUpdate pkt;
        pkt.response.time = CD_ServerGetTime(self);

        CDPacket packet = { CDResponse, CDTimeUpdate, (CDPointer) &pkt };

        CD_HASH_FOREACH(self->players, it) {
            CD_PlayerSendPacket((CDPlayer*) CD_HashIteratorValue(it), &packet);
        }
    }
}

static
void
cdbase_KeepAlive (void* _, void* __, CDServer* self)
{
    CD_PACKET_DO {
        CDPacket packet = { CDResponse, CDKeepAlive, };

        CD_HASH_FOREACH(self->players, it) {
            CD_PlayerSendPacketAndCleanData((CDPlayer*) CD_HashIteratorValue(it), &packet);
        }
    }
}

static
bool
cdbase_PlayerProcess (CDServer* server, CDPlayer* player, CDPacket* packet)
{
    switch (packet->type) {
        case CDKeepAlive: {
            SDEBUG(server, "%s is still alive", CD_StringContent(player->username));
        } break;

        case CDLogin: {
            CDPacketLogin* data = (CDPacketLogin*) packet->data;

            pthread_mutex_lock(&lock.login);

            SLOG(server, LOG_NOTICE, "%s tried login with client version %d", CD_StringContent(data->request.username), data->request.version);

            if (CD_HashGet(server->players, CD_StringContent(data->request.username))) {
                SLOG(server, LOG_NOTICE, "%s: nick exists on the server", CD_StringContent(data->request.username));

                if (server->config->cache.game.standard) {
                    CD_ServerKick(server, player, CD_CreateStringFromFormat("%s nick already exists",
                        CD_StringContent(data->request.username)));

                    CD_EventDispatch(server, "Player.login", player, false);

                    return false;
                }
            }

            player->username = CD_CloneString(data->request.username);

            CD_HashPut(server->players, CD_StringContent(player->username), (CDPointer) player);

            pthread_mutex_unlock(&lock.login);

            CD_PACKET_DO {
                CDPacketLogin pkt;
                pkt.response.id         = player->entity.id;
                pkt.response.serverName = CD_CreateStringFromCString("");
                pkt.response.motd       = CD_CreateStringFromCString("");
                pkt.response.mapSeed    = 0;
                pkt.response.dimension  = 0;

                CDPacket response = { CDResponse, CDLogin, (CDPointer) &pkt };

                CD_PlayerSendPacketAndCleanData(player, &response);
            }

            MCPosition* spawnPosition = (MCPosition*) CD_HashGet(PRIVATE(server), "World.spawnPosition");

            if (!spawnPosition) {
                SERR(server, "unknown spawn position, can't finish login procedure");

                return false;
            }

            // Hack in a square send for login
            for (int i = -7; i < 8; i++) {
                for ( int j = -7; j < 8; j++) {
                    MCPosition coords;
                    coords.x = CD_Div(spawnPosition->x, 16) + i;
                    coords.y = 0;
                    coords.z = CD_Div(spawnPosition->z, 16) + j;

                    if (!cdbase_SendChunk(server, player, &coords)) {
                        return false;
                    }
                }
            }

            /* Send Spawn Position to initialize compass */
            CD_PACKET_DO {
                CDPacketSpawnPosition pkt;
                pkt.response.position = *spawnPosition;

                CDPacket response = { CDResponse, CDSpawnPosition, (CDPointer) &pkt };

                CD_PlayerSendPacketAndCleanData(player, &response);
            }

            CD_PACKET_DO {
                CDPacketPlayerMoveLook pkt;
                pkt.response.position.x  = spawnPosition->x;
                pkt.response.position.y  = spawnPosition->y + 6;
                pkt.response.position.z  = spawnPosition->z;
                pkt.response.stance      = spawnPosition->y + 6.1; // TODO: ??
                pkt.response.yaw         = 0;
                pkt.response.pitch       = 0;
                pkt.response.is.onGround = false;

                CDPacket response = { CDResponse, CDPlayerMoveLook, (CDPointer) &pkt };

                CD_PlayerSendPacketAndCleanData(player, &response);
            }

            CD_EventDispatch(server, "Player.login", player, true);
        } break;

        case CDHandshake: {
            CDPacketHandshake* data = (CDPacketHandshake*) packet->data;

            SLOG(server, LOG_NOTICE, "%s tried handshake", CD_StringContent(data->request.username));

            CDPacketHandshake pkt;
            pkt.response.hash = CD_CreateStringFromCString("-");

            CDPacket response = { CDResponse, CDHandshake, (CDPointer) &pkt };

            CD_PlayerSendPacketAndCleanData(player, &response);
        } break;

        case CDChat: {
            CDPacketChat* data = (CDPacketChat*) packet->data;

            if (CD_StringEmpty(player->username)) {
                break;
            }

            if (CD_StringStartWith(data->request.message, "/")) {
                CD_EventDispatch(server, "Player.command", player, data->request.message);
            }
            else {
                CD_EventDispatch(server, "Player.chat", player, data->request.message);
            }
        } break;

        case CDOnGround: {
            // Stub.  Probably not needed
        } break;

        case CDPlayerPosition: {
            // Stub.  Do dead reckoning or some other sanity check for data
            // and send CD_SetDifference of chunks on boundary change.
        } break;

        case CDPlayerLook: {
            // Stub
        } break;

        case CDPlayerMoveLook: {
            // Stub.  Do dead reckoning or some other sanity check for data
            // and send CD_SetDifference of chunks on boundary change.

            CDPacketPlayerMoveLook* data = (CDPacketPlayerMoveLook*) packet->data;

            int newx = CD_Div(data->request.position.x, 16);
            int newz = CD_Div(data->request.position.z, 16);
            MCPosition area = { newx, 0, newz };

            if ( newx != CD_Div(player->entity.position.x, 16) ||
                 newz != CD_Div(player->entity.position.z, 16))
            {
                cdbase_SendChunkRadius(player, &area, 10);
            }

            player->entity.position = data->request.position;

        } break;

        case CDDisconnect: {
            CDPacketDisconnect* data = (CDPacketDisconnect*) packet->data;

            CD_ServerKick(server, player, CD_CloneString(data->request.reason));
        } break;

        default: {
            SERR(server, "unimplemented packet 0x%.2X from %s (%s)", packet->type, 
                    CD_StringContent(player->username), player->ip);
        }
    }

    return true;
}

static
bool
cdbase_PlayerDestroy (CDServer* server, CDPlayer* player)
{
    if (player->username) {
        CDSet* chunks = (CDSet*) CD_HashDelete(PRIVATE(player), "Player.loadedChunks");

        if (chunks) {
            CD_SetMap(chunks, (CDSetApply) cdbase_ChunkMemberFree, CDNull);
            CD_DestroySet(chunks);
        }
    }

    return true;
}

static
bool
cdbase_HandleLogin (CDServer* server, CDPlayer* player, int status)
{
    if (!status) {
        return true;
    }

    cdbase_TimeUpdate(NULL, NULL, server);

    CD_ServerBroadcast(server, CD_StringColor(CD_CreateStringFromFormat("%s has joined the game",
                CD_StringContent(player->username)), CDColorYellow));

    CD_HashPut(PRIVATE(player), "Player.loadedChunks", (CDPointer) CD_CreateSet (400,
                (CDSetCompare) cdbase_CompareMCPosition, (CDSetHash) cdbase_HashMCPosition));

    return true;
}

static
bool
cdbase_HandleLogout (CDServer* server, CDPlayer* player)
{
    CD_ServerBroadcast(server, CD_CreateStringFromFormat("%s has left the game",
        CD_StringContent(player->username)));

    return true;
}

static
bool
cdbase_HandleCommand (CDServer* server, CDPlayer* player, CDString* command, CDString* parameters)
{
    return true;
}

static
bool
cdbase_HandleChat (CDServer* server, CDPlayer* player, CDString* message)
{
    SLOG(server, LOG_NOTICE, "<%s> %s", CD_StringContent(player->username),
            CD_StringContent(message));

    CD_ServerBroadcast(server, CD_CreateStringFromFormat("<%s> %s",
        CD_StringContent(player->username),
        CD_StringContent(message)));

    return true;
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->name = CD_CreateStringFromCString("Base");

    pthread_mutex_init(&lock.login, NULL);

    CD_HashPut(PRIVATE(self), "Event.timeIncrease", CD_SetInterval(self->server->timeloop, 1,  (event_callback_fn) cdbase_TimeIncrease));
    CD_HashPut(PRIVATE(self), "Event.timeUpdate",   CD_SetInterval(self->server->timeloop, 30, (event_callback_fn) cdbase_TimeUpdate));
    CD_HashPut(PRIVATE(self), "Event.keepAlive",    CD_SetInterval(self->server->timeloop, 10, (event_callback_fn) cdbase_KeepAlive));

    CD_EventRegister(self->server, "Player.process", cdbase_PlayerProcess);

    CD_EventRegister(self->server, "Player.login", cdbase_HandleLogin);
    CD_EventRegister(self->server, "Player.logout", cdbase_HandleLogout);

    CD_EventRegister(self->server, "Player.command", cdbase_HandleCommand);
    CD_EventRegister(self->server, "Player.chat", cdbase_HandleChat);

    CD_EventRegister(self->server, "Player.destroy", cdbase_PlayerDestroy);

    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_ClearInterval(self->server->timeloop, (int) CD_HashGet(PRIVATE(self), "Event.timeIncrease"));
    CD_ClearInterval(self->server->timeloop, (int) CD_HashGet(PRIVATE(self), "Event.timeUpdate"));
    CD_ClearInterval(self->server->timeloop, (int) CD_HashGet(PRIVATE(self), "Event.keepAlive"));

    CD_EventUnregister(self->server, "Player.process", cdbase_PlayerProcess);

    CD_EventUnregister(self->server, "Player.login", cdbase_HandleLogin);
    CD_EventUnregister(self->server, "Player.logout", cdbase_HandleLogout);

    CD_EventUnregister(self->server, "Player.command", cdbase_HandleCommand);
    CD_EventUnregister(self->server, "Player.chat", cdbase_HandleChat);

    pthread_mutex_destroy(&lock.login);

    return true;
}
