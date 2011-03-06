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

struct {
    pthread_mutex_t login;
} cdbase_lock;

static
void
cdbase_TimeIncrease (evutil_socket_t fd, short event, CDServer* self)
{
    short current = CD_ServerGetTime(self);

    if (current >= 0 && current <= 11999) {
        CD_ServerSetTime(self, current + self->config->cache.rate.day);
    }
    else if (current >= 12000 && current <= 13799) {
        CD_ServerSetTime(self, current + self->config->cache.rate.sunset);
    }
    else if (current >= 13800 && current <= 22199) {
        CD_ServerSetTime(self, current + self->config->cache.rate.night);
    }
    else if (current >= 22200 && current <= 23999) {
        CD_ServerSetTime(self, current + self->config->cache.rate.sunrise);
    }

    current = CD_ServerGetTime(self);

    if (current >= 24000) {
        CD_ServerSetTime(self, current - 24000);
    }
}

static
void
cdbase_TimeUpdate (evutil_socket_t fd, short event, CDServer* self)
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
cdbase_KeepAlive (evutil_socket_t fd, short event, CDServer* self)
{
    CD_PACKET_DO {
        CDPacket packet = { CDResponse, CDKeepAlive, };

        CD_HASH_FOREACH(self->players, it) {
            CD_PlayerSendPacket((CDPlayer*) CD_HashIteratorValue(it), &packet);
        }
    }
}

static
bool
cdbase_PlayerProcess (CDServer* server, CDPlayer* player)
{
    CDPacket* packet = (CDPacket*) CD_HashGet(PRIVATE(player), "packet");

    switch (packet->type) {
        case CDKeepAlive: {
            SDEBUG(server, "%s is still alive", CD_StringContent(player->username));
        } break;

        case CDLogin: {
            CDPacketLogin* data = (CDPacketLogin*) packet->data;

            pthread_mutex_lock(&cdbase_lock.login);

            SLOG(server, LOG_NOTICE, "%s tried login with client version %d", CD_StringContent(data->request.username), data->request.version);

            if (CD_HashGet(server->players, CD_StringContent(data->request.username))) {
                SLOG(server, LOG_NOTICE, "%s exists on the server", CD_StringContent(data->request.username));

                // TODO: kill it with fire or give it another name IRC-like.
            }

            player->username = CD_CloneString(data->request.username);

            CD_HashSet(server->players, CD_StringContent(player->username), (CDPointer) player);

            pthread_mutex_unlock(&cdbase_lock.login);

            CD_PACKET_DO {
                CDPacketLogin pkt;
                pkt.response.id         = player->entity.id;
                pkt.response.serverName = CD_CreateStringFromCString("");
                pkt.response.motd       = CD_CreateStringFromCString("");
                pkt.response.mapSeed    = 0;
                pkt.response.dimension  = 0;

                CDPacket response = { CDResponse, CDLogin, (CDPointer) &pkt };

                CD_PlayerSendPacket(player, &response);

                CD_DestroyString(pkt.response.serverName);
                CD_DestroyString(pkt.response.motd);
            }

            MCPosition* spawnPosition = (MCPosition*) CD_HashGet(PRIVATE(server), "World.spawnPosition");

            if (!spawnPosition) {
                SERR(server, "unknown spawn position, can't finish login procedure");

                return false;
            }

            int x = spawnPosition->x / 16;
            int z = spawnPosition->z / 16;

            // Hack in a square send for now
            for ( int i = -7; i < 8; i++)
            {
            for ( int j = -7; j < 8; j++)
            {
                CD_PACKET_DO {
                    CDPacketPreChunk pkt;
                    pkt.response.x    = x + i;
                    pkt.response.z    = z + j;
                    pkt.response.mode = true;

                    CDPacket response = { CDResponse, CDPreChunk, (CDPointer) &pkt };

                    CD_PlayerSendPacket(player, &response);
                }

                CD_PACKET_DO {
                    CDPacketMapChunk pkt;
                    pkt.response.position.x = CD_WORLD_COORD(x + i);
                    pkt.response.position.y = CD_WORLD_COORD(0);
                    pkt.response.position.z = CD_WORLD_COORD(z + j);
                    pkt.response.size.x     = 16;
                    pkt.response.size.y     = 128;
                    pkt.response.size.z     = 16;

                    SDEBUG(server, "sending chunk (%d, %d)", x + i, z + j);

                    uint8_t mapdata[81920];

                    CD_EventDispatch(server, "Chunk.load", x + i, z + j, mapdata);

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

                    CD_PlayerSendPacket(player, &response);
                }
            }
            }

            /* Send Spawn Position to initialize compass */
            CD_PACKET_DO {
                CDPacketSpawnPosition pkt;
                pkt.response.position = *spawnPosition;

                CDPacket response = { CDResponse, CDSpawnPosition, (CDPointer) &pkt };

                CD_PlayerSendPacket(player, &response);
            }

            CD_PACKET_DO {
                CDPacketPlayerMoveLook pkt;
                MCPrecisePosition spawnPrecise = { spawnPosition->x, spawnPosition->y + 6, spawnPosition->z };
                pkt.response.position          = spawnPrecise;
                pkt.response.stance            = spawnPosition->y + 6.1; // TODO: ??
                pkt.response.yaw               = 0;
                pkt.response.pitch             = 0;
                pkt.response.is.onGround       = false;

                CDPacket response = { CDResponse, CDPlayerMoveLook, (CDPointer) &pkt };

                CD_PlayerSendPacket(player, &response);
            }

            CD_PACKET_DO {
                MCString inband = CD_CreateStringFromFormat("%s has joined the game",
                        CD_StringContent(player->username));

                CDPacketChat pkt;
                
                pkt.response.message = inband;
                
                CDPacket packet = { CDResponse, CDChat, (CDPointer) &pkt };

                CD_HASH_FOREACH(server->players, it) {
                    CD_PlayerSendPacket((CDPlayer*) CD_HashIteratorValue(it), &packet);
                }

                MC_DestroyString(inband);
            }

        } break;

        case CDHandshake: {
            CDPacketHandshake* data = (CDPacketHandshake*) packet->data;

            SLOG(server, LOG_NOTICE, "%s tried handshake", CD_StringContent(data->request.username));

            CDPacketHandshake pkt;
            pkt.response.hash = CD_CreateStringFromCString("-");

            CDPacket response = { CDResponse, CDHandshake, (CDPointer) &pkt };

            CD_PlayerSendPacket(player, &response);

            CD_DestroyString(pkt.response.hash);
        } break;

        case CDChat: {
            CDPacketChat* data = (CDPacketChat*) packet->data;

            if (CD_StringEmpty(player->username)) {
                break;
            }

            if (CD_StringStartWith(data->request.message, "/")) {

            }
            else {
                SLOG(server, LOG_NOTICE, "<%s> %s", CD_StringContent(player->username),
                        CD_StringContent(data->request.message));

                CD_PACKET_DO {
                    MCString inband = CD_CreateStringFromFormat("<%s> %s", 
                            CD_StringContent(player->username),
                            CD_StringContent(data->request.message));

                    CDPacketChat pkt;
                    pkt.response.message = inband;

                    CDPacket packet = { CDResponse, CDChat, (CDPointer) &pkt };

                    CD_HASH_FOREACH(server->players, it) {
                        CD_PlayerSendPacket((CDPlayer*) CD_HashIteratorValue(it), &packet);
                    }

                    MC_DestroyString(inband);
                }
            }
        } break;

        case CDDisconnect: {
            CDPacketDisconnect* data = (CDPacketDisconnect*) packet->data;

            CD_PACKET_DO {
                MCString inband = CD_CreateStringFromFormat("%s has left the game",
                        CD_StringContent(player->username));

                CDPacketChat pkt;
                pkt.response.message = inband;

                CDPacket packet = { CDResponse, CDChat, (CDPointer) &pkt };

                CD_HASH_FOREACH(server->players, it) {
                    CD_PlayerSendPacket((CDPlayer*) CD_HashIteratorValue(it), &packet);
                }

                MC_DestroyString(inband);
            }
 
            CD_ServerKick(server, player, CD_StringContent(data->request.reason));
        } break;

        default: {
            SERR(server, "unimplemented packet 0x%.2X from %s (%s)", packet->type, CD_StringContent(player->username), player->ip);
        }
    }

    return true;
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->name = CD_CreateStringFromCString("Base");

    pthread_mutex_init(&cdbase_lock.login, NULL);

    CD_HashSet(PRIVATE(self), "Event.timeIncrease", CD_SetInterval(self->server->timeloop, 1,  (event_callback_fn) cdbase_TimeIncrease));
    CD_HashSet(PRIVATE(self), "Event.timeUpdate",   CD_SetInterval(self->server->timeloop, 30, (event_callback_fn) cdbase_TimeUpdate));
    CD_HashSet(PRIVATE(self), "Event.keepAlive",    CD_SetInterval(self->server->timeloop, 10, (event_callback_fn) cdbase_KeepAlive));

    CD_EventRegister(self->server, "Player.process", cdbase_PlayerProcess);

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

    pthread_mutex_destroy(&cdbase_lock.login);

    return true;
}
