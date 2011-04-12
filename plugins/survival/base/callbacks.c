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
/**
 * All of the event callbacks for the survival plugin are implemented
 * here.
 * @inmodule Survival
 */
#include <zlib.h>

#include <craftd/Logger.h>

#include <craftd/protocols/survival/World.h>
#include <craftd/protocols/survival/Region.h>
#include <craftd/protocols/survival/Player.h>

static
bool
cdsurvival_SendChunk (CDServer* server, SVPlayer* player, SVChunkPosition* coord)
{
    DO {
        SVPacketPreChunk pkt = {
            .response = {
                .position = *coord,
                .mode     = true
            }
        };

        SVPacket response = { SVResponse, SVPreChunk, (CDPointer) &pkt };

        SV_PlayerSendPacketAndCleanData(player, &response);
    }

    DO {
        SDEBUG(server, "sending chunk (%d, %d)", coord->x, coord->z);

        SVChunk* chunk = CD_malloc(sizeof(SVChunk));
        CDError  status;

        CD_EventDispatchWithError(status, server, "World.chunk", player->world, coord->x, coord->z, chunk);

        if (status != CDOk) {
            CD_free(chunk);

            return false;
        }

        uLongf written = compressBound(81920);
        Bytef* buffer  = CD_malloc(written);
        Bytef* data    = CD_malloc(81920);
            
        SV_ChunkToByteArray(chunk, data);

        if (compress(buffer, &written, (Bytef*) data, 81920) != Z_OK) {
            SERR(server, "zlib compress failure");

            CD_free(chunk);
            CD_free(data);

            return false;
        }

        SDEBUG(server, "compressed to %ld bytes", written);

        CD_free(chunk);
        CD_free(data);

        SVPacketMapChunk pkt = {
            .response = {
                .position = SV_ChunkPositionToBlockPosition(*coord),

                .size = {
                    .x = 16,
                    .y = 128,
                    .z = 16
                },

                .length = written,
                .item   = (SVByte*) buffer
            }
        };

        SVPacket response = { SVResponse, SVMapChunk, (CDPointer) &pkt };

        SV_PlayerSendPacketAndCleanData(player, &response);
    }

    return true;
}

static
void
cdsurvival_ChunkRadiusUnload (CDSet* self, SVChunkPosition* coord, SVPlayer* player)
{
    assert(self);
    assert(coord);
    assert(player);

    DO {
        SVPacketPreChunk pkt = {
            .response = {
                .position = *coord,
                .mode = false
            }
        };

        SVPacket response = { SVResponse, SVPreChunk, (CDPointer) &pkt };

        SV_PlayerSendPacketAndCleanData(player, &response);
    }

    CD_free(coord);
}

static
void
cdsurvival_ChunkMemberFree (CDSet* self, SVChunkPosition* coord, void* _)
{
    assert(self);
    assert(coord);

    CD_free(coord);
}

static
void
cdsurvival_ChunkRadiusLoad (CDSet* self, SVChunkPosition* coord, SVPlayer* player)
{
    assert(self);
    assert(coord);
    assert(player);

    cdsurvival_SendChunk(player->client->server, player, coord);
}

static
void
cdsurvival_SendChunkRadius (SVPlayer* player, SVChunkPosition* area, int radius)
{
    CDSet* loadedChunks = (CDSet*) CD_DynamicGet(player, "Player.loadedChunks");
    CDSet* oldChunks    = loadedChunks;
    CDSet* newChunks    = CD_CreateSetWith(400, (CDSetCompare) SV_CompareChunkPosition, (CDSetHash) SV_HashChunkPosition);

    for (int x = -radius; x < radius; x++) {
        for (int z = -radius; z < radius; z++) {
            if ((x * x + z * z) <= (radius * radius)) {
                SVChunkPosition* coord = CD_malloc(sizeof(SVChunkPosition));
                coord->x          = x + area->x;
                coord->z          = z + area->z;

                CD_SetPut(newChunks, (CDPointer) coord);
            }
        }
    }

    CDSet* toRemove = CD_SetMinus(oldChunks, newChunks);
    CDSet* toAdd    = CD_SetMinus(newChunks, oldChunks);

    CD_SetMap(toRemove, (CDSetApply) cdsurvival_ChunkRadiusUnload, (CDPointer) player);
    CD_SetMap(toAdd, (CDSetApply) cdsurvival_ChunkRadiusLoad, (CDPointer) player);

    CD_DestroySet(toRemove);
    CD_DestroySet(toAdd);

    if (oldChunks) {
        CD_DestroySet(oldChunks);
    }

    CD_DynamicPut(player, "Player.loadedChunks", (CDPointer) newChunks);
}

static
bool
cdsurvival_CoordInRadius(SVChunkPosition *coord, SVChunkPosition *centerCoord, int radius)
{
    if (coord->x >= centerCoord->x - radius &&
        coord->x <= centerCoord->x + radius &&
        coord->z >= centerCoord->z - radius &&
        coord->z <= centerCoord->z + radius)
    {
        return true;
    }

    return false;
}

static
bool
cdsurvival_DistanceGreater (SVPrecisePosition a, SVPrecisePosition b, int maxDistance)
{
    return (abs( a.x - b.x ) > maxDistance ||
            abs( a.y - b.y ) > maxDistance ||
            abs( a.z - b.z ) > maxDistance);
}

static
SVRelativePosition
cdsurvival_RelativeMove (SVPrecisePosition* a, SVPrecisePosition* b)
{
    SVAbsolutePosition absoluteA = SV_PrecisePositionToAbsolutePosition(*a);
    SVAbsolutePosition absoluteB = SV_PrecisePositionToAbsolutePosition(*b);

    return (SVRelativePosition) {
        .x = absoluteA.x - absoluteB.x,
        .y = absoluteA.y - absoluteB.y,
        .z = absoluteA.z - absoluteB.z
    };
}

static
void
cdsurvival_SendPacketToAllInRegion(SVPlayer *player, SVPacket *pkt)
{
  CDList *seenPlayers = (CDList *) CD_DynamicGet(player, "Player.seenPlayers");

  CD_LIST_FOREACH(seenPlayers, it)
  {
    if ( player != (SVPlayer *) CD_ListIteratorValue(it) )
      SV_PlayerSendPacket( (SVPlayer *) CD_ListIteratorValue(it), pkt );
    else
      CERR("We have a player with himself in the List????");
  }
}

static
void
cdsurvival_SendUpdatePos(SVPlayer* player, SVPrecisePosition* newPosition, bool andLook, SVFloat pitch, SVFloat yaw)
{
    if (cdsurvival_DistanceGreater(player->entity.position, *newPosition, 2) || true) {
        DO {
            SVPacketEntityTeleport pkt;

            pkt.response.entity   = player->entity;
            pkt.response.position = SV_PrecisePositionToAbsolutePosition(*newPosition);

            if (andLook) {
                pkt.response.pitch = pitch;
                pkt.response.rotation = yaw;
            }
            else {
                pkt.response.pitch = player->pitch;
                pkt.response.rotation = player->yaw;
            }
            
            SVPacket response = { SVResponse, SVEntityTeleport, (CDPointer)  &pkt };
            
            cdsurvival_SendPacketToAllInRegion(player, &response);
        }
    }
    else {
        if (andLook) {
            SVPacketEntityLookMove pkt = {
                .response = {
                    .entity   = player->entity,
                    .position = SV_RelativeMove(&player->entity.position, newPosition),
                    .yaw      = yaw,
                    .pitch    = pitch
                }
            };


            SVPacket response = { SVResponse, SVEntityLookMove, (CDPointer) &pkt };

            SV_RegionBroadcastPacket(player, &response);
        }
        else {
            SVPacketEntityRelativeMove pkt = {
                .response = {
                    .entity   = player->entity,
                    .position = SV_RelativeMove(&player->entity.position, newPosition)
                }
            };

            SVPacket response = { SVResponse, SVEntityRelativeMove, (CDPointer) &pkt };

            SV_RegionBroadcastPacket(player, &response);
        }
    }
}

static
void
cdsurvival_SendNamedPlayerSpawn(SVPlayer *player, SVPlayer *other)
{
    DO {
        SVPacketNamedEntitySpawn pkt = {
            .response = {
                .entity   = other->entity,
                .name     = other->username,
                .pitch    = other->pitch,
                .rotation = other->yaw,

                .item = {
                    .id = 0
                },

                .position = SV_PrecisePositionToAbsolutePosition(other->entity.position)
            }
        };

        SVPacket response = { SVResponse, SVNamedEntitySpawn, (CDPointer) &pkt };

        SV_PlayerSendPacket(player, &response);
    }

    for (int i = 0; i < 5; i++) {
        DO {
            SVPacketEntityEquipment pkt = {
                .response = {
                    .entity = other->entity,

                    .slot   = i,
                    .item   = -1,
                    .damage = 0
                }
            };

            SVPacket response = { SVResponse, SVEntityEquipment, (CDPointer) &pkt };

            SV_PlayerSendPacket(player, &response);
        }
    }

    DO {
        SVPacketEntityTeleport pkt = {
            .response = {
                .entity   = other->entity,
                .pitch    = other->pitch,
                .rotation = 0,
                .position = SV_PrecisePositionToAbsolutePosition(other->entity.position)
            }
        };

        SVPacket response = { SVResponse, SVEntityTeleport, (CDPointer) &pkt };

        SV_PlayerSendPacket(player, &response);
    }

    SV_PlayerSendMessage(player, CD_CreateStringFromFormat("You should now see %s", CD_StringContent(other->username)));
}

static
void
cdsurvival_SendDestroyEntity (SVPlayer* player, SVEntity* entity)
{
    DO {
        SVPacketEntityDestroy pkt = {
            .response = {
                .entity = *entity
            }
        };

        SVPacket response = { SVResponse, SVEntityDestroy, (CDPointer) &pkt };

        SV_PlayerSendPacket(player, &response);
    }
}

static
void
cdsurvival_CheckPlayersInRegion (CDServer* server, SVPlayer* player, SVChunkPosition *coord, int radius)
{
    CDList* seenPlayers = (CDList*) CD_DynamicGet(player, "Player.seenPlayers");;

    CD_HASH_FOREACH(player->world->players, it) {
        SVPlayer* otherPlayer = (SVPlayer *) CD_HashIteratorValue(it);

        // If we are the player to check just skip
        if (otherPlayer == player) {
            continue;
        }

        SVChunkPosition chunkPos = SV_PrecisePositionToChunkPosition(otherPlayer->entity.position);

        if (cdsurvival_CoordInRadius(&chunkPos, coord, radius)) {
            /* If the player is in range, but not in the list. */
            if (!CD_ListContains(seenPlayers, (CDPointer) otherPlayer)) {
                CD_ListPush(seenPlayers, (CDPointer) otherPlayer);
                cdsurvival_SendNamedPlayerSpawn(player, otherPlayer);

                CDList *otherSeenPlayers = (CDList *) CD_DynamicGet(otherPlayer, "Player.seenPlayers");
                CD_ListPush(otherSeenPlayers, (CDPointer) player);
                cdsurvival_SendNamedPlayerSpawn(otherPlayer, player);
            }
        }
        else {
            /* If the player is out of range but in the list */
            if (CD_ListContains(seenPlayers, (CDPointer) otherPlayer)) {
                CDList *otherSeenPlayers = (CDList *) CD_DynamicGet(otherPlayer, "Player.seenPlayers");

                CD_ListDeleteAll(seenPlayers, (CDPointer) otherPlayer);
                CD_ListDeleteAll(otherSeenPlayers, (CDPointer) player);

                /* Should send both players an update. */
                cdsurvival_SendDestroyEntity(player, &otherPlayer->entity);
                cdsurvival_SendDestroyEntity(otherPlayer, &player->entity);
            }
        }
    }
}

static
bool
cdsurvival_ClientProcess (CDServer* server, CDClient* client, SVPacket* packet)
{
    SVWorld*  world;
    SVPlayer* player = (SVPlayer*) CD_DynamicGet(client, "Client.player");

    if (player && player->world) {
        world = player->world;
    }
    else {
        world = (SVWorld*) CD_DynamicGet(server, "World.default");
    }

    switch (packet->type) {
        case SVKeepAlive: {
            SDEBUG(server, "%s is still alive", player ? CD_StringContent(player->username) : client->ip);
        } break;

        case SVLogin: {
            SVPacketLogin* data = (SVPacketLogin*) packet->data;


            SLOG(server, LOG_NOTICE, "%s tried login with client version %d", CD_StringContent(data->request.username), data->request.version);

            if (data->request.version != CRAFTD_PROTOCOL_VERSION) {
                CD_ServerKick(server, client, CD_CreateStringFromFormat(
                    "Protocol mismatch, we support %d, you're using %d.",
                    CRAFTD_PROTOCOL_VERSION, data->request.version));

                return false;
            }

            if (data->request.username->length < 1) {
                CD_ServerKick(server, client, CD_CreateStringFromCString(
                    "Invalid username"));
                return false;
            }

            player->username = CD_CloneString(data->request.username);


            if (!SV_WorldAddPlayer(world, player)) {
                CD_ServerKick(server, client, CD_CreateStringFromFormat(
                    "Login failed: %d", ERROR(world)));
                CD_EventDispatch(server, "Player.login", player, false);
                return false;
            }

            // The player is now added to the world and logged-in.

            DO {
                SVPacketLogin pkt = {
                    .response = {
                        .id         = player->entity.id,
                        .serverName = CD_CreateStringFromCString(""),
                        .motd       = CD_CreateStringFromCString(""),
                        .mapSeed    = 0,
                        .dimension  = 0
                    }
                };

                SVPacket response = { SVResponse, SVLogin, (CDPointer) &pkt };

                SLOG(server, LOG_INFO, "%s responding with entity id %d", CD_StringContent(player->username), player->entity.id);

               SV_PlayerSendPacketAndCleanData(player, &response);
            }

            SVChunkPosition spawnChunk = SV_BlockPositionToChunkPosition(world->spawnPosition);

            // Hack in a square send for login
            for (int i = -7; i < 8; i++) {
                for ( int j = -7; j < 8; j++) {
                    SVChunkPosition coords = {
                        .x = spawnChunk.x + i,
                        .z = spawnChunk.z + j
                    };

                    if (!cdsurvival_SendChunk(server, player, &coords)) {
                        return false;
                    }
                }
            }

            /* Send Spawn Position to initialize compass */
            DO {
                SVPacketSpawnPosition pkt = {
                    .response = {
                        .position = world->spawnPosition
                    }
                };

                SVPacket response = { SVResponse, SVSpawnPosition, (CDPointer) &pkt };

                SV_PlayerSendPacketAndCleanData(player, &response);
            }

            DO {
                SVPrecisePosition pos = SV_BlockPositionToPrecisePosition(world->spawnPosition);
                SVPacketPlayerMoveLook pkt = {
                    .response = {
                        .position = {
                            .x = pos.x,
                            .y = pos.y + 6,
                            .z = pos.z
                        },

                        .stance = pos.y + 6.1,
                        .yaw    = 0,
                        .pitch  = 0,

                        .is = {
                            .onGround = false
                        }
                    }
                };

                SVPacket response = { SVResponse, SVPlayerMoveLook, (CDPointer) &pkt };

                SV_PlayerSendPacketAndCleanData(player, &response);
            }

            CD_EventDispatch(server, "Player.login", player, true);
        } break;

        case SVHandshake: {
            SVPacketHandshake* data = (SVPacketHandshake*) packet->data;

            SLOG(server, LOG_NOTICE, "%s tried handshake", CD_StringContent(data->request.username));

            SVPacketHandshake pkt = {
                .response = {
                    .hash = CD_CreateStringFromCString("-")
                }
            };

            player = SV_CreatePlayer(client);

            CD_DynamicPut(client, "Client.player", (CDPointer) player);

            SVPacket response = { SVResponse, SVHandshake, (CDPointer) &pkt };

            SV_PlayerSendPacketAndCleanData(player, &response);
        } break;

        case SVChat: {
            SVPacketChat* data = (SVPacketChat*) packet->data;

            if (CD_StringEmpty(player->username)) {
                break;
            }

            if (CD_StringStartWith(data->request.message, _config.command)) {
                CDString* commandString = CD_CreateStringFromOffset(data->request.message, 1, 0);
                CD_EventDispatch(server, "Player.command", player, commandString);
                SV_DestroyString(commandString);
            }
            else {
                CD_EventDispatch(server, "Player.chat", player, data->request.message);
            }
        } break;

        case SVOnGround: {
            // Stub.  Probably not needed
        } break;

        case SVPlayerPosition: {
            // Stub.  Do dead reckoning or some other sanity check for data
            // and send CD_SetDifference of chunks on boundary change.

            SVPacketPlayerPosition* data = (SVPacketPlayerPosition*) packet->data;

            SVChunkPosition newChunk = SV_PrecisePositionToChunkPosition(data->request.position);
            SVChunkPosition curChunk = SV_PrecisePositionToChunkPosition(player->entity.position);

            if (!SV_ChunkPositionEqual(newChunk, curChunk)) {
                cdsurvival_SendChunkRadius(player, &newChunk, 10);

                cdsurvival_CheckPlayersInRegion(server, player, &newChunk, 5);
            }

            cdsurvival_SendUpdatePos(player, &data->request.position, false, 0, 0);

            player->entity.position = data->request.position;
        } break;

        case SVPlayerLook: {
            // Stub.  Add input validation and sanity checks.

            SVPacketPlayerLook* data = (SVPacketPlayerLook*) packet->data;

            player->yaw   = data->request.yaw;
            player->pitch = data->request.pitch;

            DO {
              SVPacketEntityLook pkt;

              pkt.response.entity = player->entity;
              pkt.response.pitch = player->pitch;
              pkt.response.yaw = player->yaw;

              SVPacket response = { SVResponse, SVEntityLook, (CDPointer) &pkt };

              cdsurvival_SendPacketToAllInRegion(player, &response);
            }
        } break;

        case SVPlayerMoveLook: {
            // Stub.  Do dead reckoning or some other sanity check for data
            // and send CD_SetDifference of chunks on boundary change.

            SVPacketPlayerMoveLook* data = (SVPacketPlayerMoveLook*) packet->data;

            SVChunkPosition oldChunk = SV_PrecisePositionToChunkPosition(player->entity.position);
            SVChunkPosition newChunk = SV_PrecisePositionToChunkPosition(data->request.position);

            if (!SV_ChunkPositionEqual(oldChunk, newChunk)) {
                cdsurvival_SendChunkRadius(player, &newChunk, 10);

                cdsurvival_CheckPlayersInRegion(server, player, &newChunk, 5);
            }

            cdsurvival_SendUpdatePos(player, &data->request.position, true, data->request.pitch, data->request.yaw);

            player->entity.position = data->request.position;
            player->yaw             = data->request.yaw;
            player->pitch           = data->request.pitch;
        } break;

        case SVDisconnect: {
            SVPacketDisconnect* data = (SVPacketDisconnect*) packet->data;

            CD_ServerKick(server, client, CD_CloneString(data->request.reason));
        } break;

        default: {
            if (player) {
                SERR(server, "unimplemented packet 0x%.2X from %s (%s)", packet->type, 
                    player->client->ip, CD_StringContent(player->username));
            }
            else {
                SERR(server, "unimplemented packet 0x%.2X from %s", packet->type, player->client->ip);
            }
        }
    }

    return true;
}

static
bool
cdsurvival_ClientProcessed (CDServer* server, CDClient* client, SVPacket* packet)
{
    return true;
}

static
bool
cdsurvival_ClientConnect (CDServer* server, CDClient* client)
{
    return true;
}

static
bool
cdsurvival_PlayerLogin (CDServer* server, SVPlayer* player, int status)
{
    if (!status) {
        return true;
    }

    DO {
        SVPacketTimeUpdate pkt = {
            .response = {
                .time = SV_WorldGetTime(player->world)
            }
        };

        SVPacket packet = { SVResponse, SVTimeUpdate, (CDPointer) &pkt };

        SV_PlayerSendPacket(player, &packet);
    }

    SV_WorldBroadcastMessage(player->world, SV_StringColor(CD_CreateStringFromFormat("%s has joined the game",
                CD_StringContent(player->username)), SVColorYellow));


    CD_DynamicPut(player, "Player.loadedChunks", (CDPointer) CD_CreateSetWith(
        400, (CDSetCompare) SV_CompareChunkPosition, (CDSetHash) SV_HashChunkPosition));

    CD_DynamicPut(player, "Player.seenPlayers", (CDPointer) CD_CreateList());

    SVChunkPosition playerChunk = SV_PrecisePositionToChunkPosition(player->entity.position);

    cdsurvival_CheckPlayersInRegion(server, player, &playerChunk, 5);

    return true;
}

static
bool
cdsurvival_PlayerLogout (CDServer* server, SVPlayer* player)
{
    assert(player);

    SV_WorldBroadcastMessage(player->world, SV_StringColor(CD_CreateStringFromFormat("%s has left the game",
        CD_StringContent(player->username)), SVColorYellow));

    CDList* seenPlayers = (CDList*) CD_DynamicDelete(player, "Player.seenPlayers");

    if (seenPlayers) {
        CD_LIST_FOREACH(seenPlayers, it) {
            SVPlayer* other            = (SVPlayer*) CD_ListIteratorValue(it);
            CDList*   otherSeenPlayers = (CDList*) CD_DynamicGet(other, "Player.seenPlayers");

            cdsurvival_SendDestroyEntity(other, &player->entity);
            CD_ListDeleteAll(otherSeenPlayers, (CDPointer) player);
        }

        CD_HashDelete(player->world->players, CD_StringContent(player->username));
        CD_MapDelete(player->world->entities, player->entity.id);

        CD_DestroyList(seenPlayers);
    }

    CDSet* chunks = (CDSet*) CD_DynamicDelete(player, "Player.loadedChunks");

    if (chunks) {
        CD_SetMap(chunks, (CDSetApply) cdsurvival_ChunkMemberFree, CDNull);
        CD_DestroySet(chunks);
    }

    SV_WorldRemovePlayer(player->world, player);

    return true;
}

static
bool
cdsurvival_ClientDisconnect (CDServer* server, CDClient* client, bool status)
{
    assert(server);
    assert(client);

    SVPlayer* player = (SVPlayer*) CD_DynamicGet(client, "Client.player");

    if (player->world) {
        CD_EventDispatch(server, "Player.logout", player, status);
    }

    return true;
}

static
bool
cdsurvival_ClientKick (CDServer* server, CDClient* client, CDString* reason)
{
    DO {
        SVPacketDisconnect pkt = {
            .response = {
                .reason = reason
            }
        };

        SVPacket  packet = { SVResponse, SVDisconnect, (CDPointer) &pkt };
        CDBuffer* buffer = SV_PacketToBuffer(&packet);

        CD_ClientSendBuffer(client, buffer);

        CD_DestroyBuffer(buffer);
    }

    return true;
}

static
bool
cdsurvival_PlayerCommand (CDServer* server, SVPlayer* player, CDString* command)
{
    CDRegexpMatches* matches = CD_RegexpMatchString("^(\\w+)(?:\\s+(.*?))?$", CDRegexpNone, command);

    if (matches) {
        SV_PlayerSendMessage(player, SV_StringColor(CD_CreateStringFromFormat("%s: unknown command",
            CD_StringContent(matches->item[1])), SVColorRed));
    }

    return false;
}

static
bool
cdsurvival_PlayerChat (CDServer* server, SVPlayer* player, CDString* message)
{
    SLOG(server, LOG_NOTICE, "<%s> %s", CD_StringContent(player->username),
            CD_StringContent(message));

    CD_ServerBroadcast(server, CD_CreateStringFromFormat("<%s> %s",
        CD_StringContent(player->username),
        CD_StringContent(message)));

    return true;
}

static
bool
cdsurvival_PlayerDestroy (CDServer* server, SVPlayer* player)
{
    return true;
}
