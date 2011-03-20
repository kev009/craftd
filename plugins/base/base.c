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
#include <craftd/minecraft.h>

// TODO Remove HAX
#include <zlib.h>

// FIXME: This is just a dummy function
MCEntityId
CD_ServerGenerateEntityId (CDServer* self)
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

kick {
    CD_DO {
        CDPacketDisconnect pkt = {
            .response = {
                .reason = reason
            }
        };

        CDPacket response = { CDResponse, CDDisconnect, (CDPointer) &pkt };

        CD_ClientSendPacketAndCleanData(client, &response);
    }
}


void
CD_ServerBroadcast (CDServer* self, CDString* message)
{
    CD_DO {
        CDPacketChat pkt = {
            .response = {
                .message = message
            }
        };

        CDPacket response = { CDResponse, CDChat, (CDPointer) &pkt };

        CDBuffer* buffer = CD_PacketToBuffer(&response);

        CD_LIST_FOREACH(self->clients, it) {
            CDClient* client = (CDClient*) CD_ListIteratorValue(it);

            pthread_rwlock_rdlock(&client->lock.status);
            if (client->status != CDClientDisconnect) {
                CD_ClientSendBuffer(client, buffer);
            }
            pthread_rwlock_unlock(&client->lock.status);
        }

        CD_DestroyBuffer(buffer);
        CD_DestroyPacketData(&response);
    }
}

uint16_t
CD_ServerGetTime (CDServer* self)
{
    uint16_t result;

    assert(self);

    pthread_spin_lock(&self->lock.time);
    result = self->time;
    pthread_spin_unlock(&self->lock.time);

    return result;
}

uint16_t
CD_ServerSetTime (CDServer* self, uint16_t time)
{
    assert(self);

    pthread_spin_lock(&self->lock.time);
    self->time = time;
    pthread_spin_unlock(&self->lock.time);

    return time;
}

static struct {
    pthread_mutex_t login;
} _lock;

static struct {
    const char* commandChar;
} _config;

static
bool
cdbase_CompareMCChunkPosition (CDSet* self, MCChunkPosition* a, MCChunkPosition* b)
{
    return (a->x == b->x && a->z == b->z);
}

static
unsigned int
cdbase_HashMCPosition (CDSet* self, MCChunkPosition* position)
{
    const int HASHMULTIPLIER = 31;
    const int CHUNKBUCKETS = 401; // Max chunks to the nearest prime

    return ((((position->x * HASHMULTIPLIER)) * HASHMULTIPLIER + position->z) * HASHMULTIPLIER) % CHUNKBUCKETS;
}

static
bool
cdbase_SendChunk (CDServer* server, CDPlayer* player, MCChunkPosition* coord)
{
    CD_DO {
        CDPacketPreChunk pkt = {
            .response = {
                .position = *coord,
                .mode = true
            }
        };

        CDPacket response = { CDResponse, CDPreChunk, (CDPointer) &pkt };

        CD_PlayerSendPacketAndCleanData(player, &response);
    }

    CD_DO {
        SDEBUG(server, "sending chunk (%d, %d)", coord->x, coord->z);

        MCChunkData* data = CD_malloc(sizeof(MCChunkData));
        bool         interrupted;

        CD_EventDispatchWithResult(interrupted, server, "Chunk.load", coord->x, coord->z, data);

        if (interrupted) {
            return false;
        }

        uLongf written = compressBound(81920);
        Bytef* buffer  = (Bytef*) CD_malloc(compressBound(81920));
        if (compress(buffer, &written, (Bytef*) data, 81920) != Z_OK) {
            SERR(server, "zlib compress failure");
            CD_free(data);
            return false;
        }
        SDEBUG(server, "compressed %ld bytes", written);
        CD_free(data);

        CDPacketMapChunk pkt = {
            .response = {
                .position = MC_ChunkPositionToBlockPosition(*coord),

                .size = {
                    .x = 16,
                    .y = 128,
                    .z = 16
                },

                .length = written,
                .item   = (MCByte*) buffer
            }

        };

        CDPacket response = { CDResponse, CDMapChunk, (CDPointer) &pkt };

        CD_PlayerSendPacketAndCleanData(player, &response);
    }

    return true;
}

static
void
cdbase_ChunkRadiusUnload (CDSet* self, MCChunkPosition* coord, CDPlayer* player)
{
    CD_DO {
        CDPacketPreChunk pkt = {
            .response = {
                .position = *coord,
                .mode = false
            }
        };

        CDPacket response = { CDResponse, CDPreChunk, (CDPointer) &pkt };

        CD_PlayerSendPacketAndCleanData(player, &response);
    }

    CD_free(coord);
}

static
void
cdbase_ChunkMemberFree (CDSet* self, MCChunkPosition* coord, CDPointer unused)
{
    CD_free(coord);
}

static
void
cdbase_ChunkRadiusLoad (CDSet* self, MCChunkPosition* coord, CDPlayer* player)
{
    cdbase_SendChunk(player->server, player, coord);
}

static
void
cdbase_SendChunkRadius (CDPlayer* player, MCChunkPosition* area, int radius)
{
    CDSet* loadedChunks = (CDSet*) CD_HashGet(PRIVATE(player), "Player.loadedChunks");
    CDSet* oldChunks    = loadedChunks;
    CDSet* newChunks    = (CDSet*) CD_CreateSet(400, (CDSetCompare) cdbase_CompareMCChunkPosition,
        (CDSetHash) cdbase_HashMCPosition);

    for (int x = -radius; x < radius; x++) {
        for (int z = -radius; z < radius; z++) {
            if ((x * x + z * z) <= (radius * radius)) {
                MCChunkPosition* coord = CD_malloc(sizeof(MCChunkPosition));
                coord->x          = x + area->x;
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
bool
cdbase_CoordInRadius(MCChunkPosition *coord, MCChunkPosition *centerCoord, int radius)
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
cdbase_posDistanceGreater( MCPrecisePosition a, MCPrecisePosition b, int maxdist )
{
  return (abs( a.x - b.x ) > maxdist ||
          abs( a.y - b.y ) > maxdist ||
          abs( a.z - b.z ) > maxdist);
}

static
MCRelativePosition
cdbase_relativeMove(MCPrecisePosition *pos1, MCPrecisePosition *pos2)
{
  MCAbsolutePosition absPos1 = MC_PrecisePositionToAbsolutePosition(*pos1);
  MCAbsolutePosition absPos2 = MC_PrecisePositionToAbsolutePosition(*pos2);
  MCRelativePosition relPos;

  relPos.x = absPos2.x - absPos1.x;
  relPos.y = absPos2.y - absPos1.y;
  relPos.z = absPos2.z - absPos1.z;

  return relPos;
}

static
void
cdbase_SendPacketToAllInRegion(CDPlayer *player, CDPacket *pkt)
{
  CDList *seenPlayerList = (CDList *) CD_HashGet(PRIVATE(player), "Player.seenPlayers");

  CD_LIST_FOREACH(seenPlayerList, it)
  {
    if ( player != (CDPlayer *) CD_ListIteratorValue(it) )
      CD_PlayerSendPacket( (CDPlayer *) CD_ListIteratorValue(it), pkt );
    else
      CERR("We have a player with himself in the List????");
  }
}

static
void
cdbase_SendUpdatePos(CDPlayer *player, MCPrecisePosition *newpos, bool andLook, MCFloat pitch, MCFloat yaw )
{
  if ( cdbase_posDistanceGreater(player->entity.position, *newpos, 2) || true )
  {
    CD_DO {
      CDPacketEntityTeleport pkt;

      pkt.response.entity   = player->entity;
      pkt.response.position = MC_PrecisePositionToAbsolutePosition(*newpos);

      if (andLook)
      {
        pkt.response.pitch = pitch;
        pkt.response.rotation = yaw;
      }
      else
      {
        pkt.response.pitch = player->pitch;
        pkt.response.rotation = player->yaw;
      }

      CDPacket response = { CDResponse, CDEntityTeleport, (CDPointer)  &pkt };

      cdbase_SendPacketToAllInRegion(player, &response);
    }
  }
  else
  {
    if (andLook)
    {
      CDPacketEntityLookMove pkt;

      pkt.response.entity = player->entity;
      pkt.response.position = cdbase_relativeMove(&player->entity.position, newpos);
      pkt.response.yaw = yaw;
      pkt.response.pitch = pitch;
    }
    else
    {
      CDPacketEntityRelativeMove pkt;

      pkt.response.entity = player->entity;
      pkt.response.position = cdbase_relativeMove(&player->entity.position, newpos);

      CDPacket response = { CDResponse, CDEntityRelativeMove,(CDPointer)  &pkt };

      cdbase_SendPacketToAllInRegion(player, &response );
    }
  }
}

static
void
cdbase_SendNamedPlayerSpawn(CDPlayer *player, CDPlayer *other)
{
  CD_DO {
    CDPacketNamedEntitySpawn pkt;

    pkt.response.entity = other->entity;
    pkt.response.name = other->username;
    pkt.response.pitch = other->pitch;
    pkt.response.rotation = other->yaw;
    pkt.response.item.id = 0;
    pkt.response.position = MC_PrecisePositionToAbsolutePosition(other->entity.position);

    CDPacket response = { CDResponse, CDNamedEntitySpawn, (CDPointer) &pkt };

    CD_PlayerSendPacket(player, &response);
  }

  for (int i = 0; i < 5; i++)
  {
     CD_DO {
       CDPacketEntityEquipment pkt;
       pkt.response.entity = other->entity;
       pkt.response.slot = i;
       pkt.response.item = -1;
       pkt.response.damage = 0;

       CDPacket response = { CDResponse, CDEntityEquipment, (CDPointer) &pkt };

       CD_PlayerSendPacket(player, &response);
     }
  }

  CD_DO {
    CDPacketEntityTeleport pkt;

    pkt.response.entity = other->entity;
    pkt.response.pitch = other->pitch;
    pkt.response.rotation = 0;
    pkt.response.position = MC_PrecisePositionToAbsolutePosition(other->entity.position);

    CDPacket response = { CDResponse, CDEntityTeleport, (CDPointer) &pkt };

    CD_PlayerSendPacket(player, &response);
  }

  CDString *s = CD_CreateStringFromFormat("You should now see %s", CD_StringContent(other->username));
  CD_PlayerSendMessage(player, s);
}

static
void
cdbase_SendDestroyEntity(CDPlayer *player, MCEntity *entity)
{
  CD_DO {
    CDPacketEntityDestroy pkt;
    pkt.response.entity = *entity;

    CDPacket response = { CDResponse, CDEntityDestroy, (CDPointer) &pkt };

    CD_PlayerSendPacket(player, &response);
  }
}

static
void
cdbase_CheckPlayersInRegion(CDServer* server, CDPlayer* player, MCChunkPosition *coord, int radius )
{
    CDList *seenPlayerList = (CDList *) CD_HashGet(PRIVATE(player), "Player.seenPlayers");

    CD_HASH_FOREACH(server->players, it)
    {
        CDPlayer *otherPlayer = (CDPlayer *) CD_HashIteratorValue(it);

        // If we are the player to check just skip
        if (otherPlayer == player)
            continue;

        MCChunkPosition chunkPos = MC_PrecisePositionToChunkPosition(otherPlayer->entity.position);

        if (cdbase_CoordInRadius(&chunkPos, coord, radius))
        {
            /* If the player is in range, but not in the list. */
            if (!CD_ListContains(seenPlayerList, (CDPointer) otherPlayer))
            {
                CD_ListPush(seenPlayerList, (CDPointer) otherPlayer);
                cdbase_SendNamedPlayerSpawn(player, otherPlayer);

                CDList *otherSeenPlayerList = (CDList *) CD_HashGet(PRIVATE(otherPlayer), "Player.seenPlayers");
                CD_ListPush(otherSeenPlayerList, (CDPointer) player);
                cdbase_SendNamedPlayerSpawn(otherPlayer, player);
            }
        }
        else
        {
            /* If the player is out of range but int he list */
            if (CD_ListContains(seenPlayerList, (CDPointer) otherPlayer))
            {
                CDList *otherSeenPlayerList = (CDList *) CD_HashGet(PRIVATE(otherPlayer), "Player.seenPlayers");

                CD_ListDeleteAll(seenPlayerList, (CDPointer) otherPlayer);
                CD_ListDeleteAll(otherSeenPlayerList, (CDPointer) player);

                /* Should send both players an update. */
                cdbase_SendDestroyEntity(player, &otherPlayer->entity);
                cdbase_SendDestroyEntity(otherPlayer, &player->entity);
            }
        }
    }
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
    CD_DO {
        CDPacketTimeUpdate pkt = {
            .response = {
                .time = CD_ServerGetTime(self)
            }
        };

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
    CD_DO {
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

            pthread_mutex_lock(&_lock.login);

            SLOG(server, LOG_NOTICE, "%s tried login with client version %d", CD_StringContent(data->request.username), data->request.version);

            if (CD_HashGet(server->players, CD_StringContent(data->request.username))) {
                SLOG(server, LOG_NOTICE, "%s: nick exists on the server", CD_StringContent(data->request.username));

//                if (server->config->cache.game.standard) {
//                    CD_ServerKick(server, player, CD_CreateStringFromFormat("%s nick already exists",
//                        CD_StringContent(data->request.username)));

//                    CD_EventDispatch(server, "Player.login", player, false);

//                    return false;
//                }

              player->username = CD_CreateStringFromFormat("unknown_%d", player->entity.id);
            }
            else
            {
              player->username = CD_CloneString(data->request.username);
            }

            CD_HashPut(server->players, CD_StringContent(player->username), (CDPointer) player);

            pthread_mutex_unlock(&_lock.login);

            CD_DO {
                CDPacketLogin pkt = {
                    .response = {
                        .id         = player->entity.id,
                        .serverName = CD_CreateStringFromCString(""),
                        .motd       = CD_CreateStringFromCString(""),
                        .mapSeed    = 0,
                        .dimension  = 0
                    }
                };

                CDPacket response = { CDResponse, CDLogin, (CDPointer) &pkt };

                CD_PlayerSendPacketAndCleanData(player, &response);
            }

            MCBlockPosition* spawnPosition = (MCBlockPosition*) CD_HashGet(PRIVATE(server), "World.spawnPosition");

            if (!spawnPosition) {
                SERR(server, "unknown spawn position, can't finish login procedure");

                return false;
            }

            MCChunkPosition spawnChunk = MC_BlockPositionToChunkPosition(*spawnPosition);
            // Hack in a square send for login
            for (int i = -7; i < 8; i++) {
                for ( int j = -7; j < 8; j++) {
                    MCChunkPosition coords = {
                        .x = spawnChunk.x + i,
                        .z = spawnChunk.z + j
                    };

                    if (!cdbase_SendChunk(server, player, &coords)) {
                        return false;
                    }
                }
            }

            /* Send Spawn Position to initialize compass */
            CD_DO {
                CDPacketSpawnPosition pkt = {
                    .response = {
                        .position = *spawnPosition
                    }
                };

                CDPacket response = { CDResponse, CDSpawnPosition, (CDPointer) &pkt };

                CD_PlayerSendPacketAndCleanData(player, &response);
            }

            CD_DO {
                MCPrecisePosition pos = MC_BlockPositionToPrecisePosition(*spawnPosition);
                CDPacketPlayerMoveLook pkt = {
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

                CDPacket response = { CDResponse, CDPlayerMoveLook, (CDPointer) &pkt };

                CD_PlayerSendPacketAndCleanData(player, &response);
            }

            CD_EventDispatch(server, "Player.login", player, true);
        } break;

        case CDHandshake: {
            CDPacketHandshake* data = (CDPacketHandshake*) packet->data;

            SLOG(server, LOG_NOTICE, "%s tried handshake", CD_StringContent(data->request.username));

            CDPacketHandshake pkt = {
                .response = {
                    .hash = CD_CreateStringFromCString("-")
                }
            };

            CDPacket response = { CDResponse, CDHandshake, (CDPointer) &pkt };

            CD_PlayerSendPacketAndCleanData(player, &response);
        } break;

        case CDChat: {
            CDPacketChat* data = (CDPacketChat*) packet->data;

            if (CD_StringEmpty(player->username)) {
                break;
            }

            if (CD_StringStartWith(data->request.message, _config.commandChar)) {
                CDString* commandString = CD_CreateStringFromOffset(data->request.message, 1, 0);
                CD_EventDispatch(server, "Player.command", player, commandString);
                CD_DestroyString(commandString);
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

            CDPacketPlayerPosition* data = (CDPacketPlayerPosition*) packet->data;

            MCChunkPosition newChunk = MC_PrecisePositionToChunkPosition(data->request.position);
            MCChunkPosition curChunk = MC_PrecisePositionToChunkPosition(player->entity.position);

            if (!MC_ChunkPositionEqual(newChunk, curChunk)) {
                cdbase_SendChunkRadius(player, &newChunk, 10);

                cdbase_CheckPlayersInRegion(server, player, &newChunk, 5);
            }

            cdbase_SendUpdatePos(player, &data->request.position, false, 0, 0);

            player->entity.position = data->request.position;
        } break;

        case CDPlayerLook: {
            // Stub.  Add input validation and sanity checks.

            CDPacketPlayerLook* data = (CDPacketPlayerLook*) packet->data;

            player->yaw   = data->request.yaw;
            player->pitch = data->request.pitch;

            CD_DO {
              CDPacketEntityLook pkt;

              pkt.response.entity = player->entity;
              pkt.response.pitch = player->pitch;
              pkt.response.yaw = player->yaw;

              CDPacket response = { CDResponse, CDEntityLook, (CDPointer) &pkt };

              cdbase_SendPacketToAllInRegion(player, &response);
            }
        } break;

        case CDPlayerMoveLook: {
            // Stub.  Do dead reckoning or some other sanity check for data
            // and send CD_SetDifference of chunks on boundary change.

            CDPacketPlayerMoveLook* data = (CDPacketPlayerMoveLook*) packet->data;

            MCChunkPosition oldChunk = MC_PrecisePositionToChunkPosition(player->entity.position);
            MCChunkPosition newChunk = MC_PrecisePositionToChunkPosition(data->request.position);

            if (!MC_ChunkPositionEqual(oldChunk, newChunk)) {
                cdbase_SendChunkRadius(player, &newChunk, 10);

                cdbase_CheckPlayersInRegion(server, player, &newChunk, 5);
            }

            cdbase_SendUpdatePos(player, &data->request.position, true, data->request.pitch, data->request.yaw);

            player->entity.position = data->request.position;
            player->yaw             = data->request.yaw;
            player->pitch           = data->request.pitch;
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

    CD_ServerBroadcast(server, MC_StringColor(CD_CreateStringFromFormat("%s has joined the game",
                CD_StringContent(player->username)), MCColorYellow));

    CD_HashPut(PRIVATE(player), "Player.loadedChunks", (CDPointer) CD_CreateSet (400,
                (CDSetCompare) cdbase_CompareMCChunkPosition, (CDSetHash) cdbase_HashMCPosition));
    CD_HashPut(PRIVATE(player), "Player.seenPlayers", (CDPointer) CD_CreateList());

    MCChunkPosition playerChunk = MC_PrecisePositionToChunkPosition( player->entity.position );
    cdbase_CheckPlayersInRegion(server, player, &playerChunk, 5);

    return true;
}

static
bool
cdbase_HandleLogout (CDServer* server, CDPlayer* player)
{
    CD_ServerBroadcast(server, MC_StringColor(CD_CreateStringFromFormat("%s has left the game",
        CD_StringContent(player->username)), MCColorYellow));

    /* TODO: Send and clean the others seenplayers. */

    CDList *seenPlayer = (CDList *) CD_HashGet(PRIVATE(player), "Player.seenPlayers");
    CD_LIST_FOREACH(seenPlayer, it)
    {
      CDPlayer *other = (CDPlayer *) CD_ListIteratorValue(it);
      CDList *otherSeenPlayer = (CDList *) CD_HashGet(PRIVATE(other), "Player.seenPlayers");

      cdbase_SendDestroyEntity(other, &player->entity);
      CD_ListDeleteAll(otherSeenPlayer, (CDPointer) player);
    }

    CD_DestroyList((CDList *) CD_HashDelete(PRIVATE(player), "Player.seenPlayers"));

    return true;
}

static
bool
cdbase_HandleCommand (CDServer* server, CDPlayer* player, CDString* command)
{
    CDRegexpMatches* matches = CD_RegexpMatchString("^(\\w+)(?:\\s+(.*?))?$", CDRegexpNone, command);

    if (matches) {
        CD_PlayerSendMessage(player, MC_StringColor(CD_CreateStringFromFormat("%s: unknown command",
            CD_StringContent(matches->item[1])), MCColorRed));
    }

    return false;
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

    pthread_mutex_init(&_lock.login, NULL);


    CD_DO { // Initiailize config cache
        _config.commandChar = "/";

        J_DO {
            J_IN(server, self->server->config->data, "server") {
                J_IN(plugin, server, "plugin") {
                    J_FOREACH(plugin, plugin, "plugins") {
                        J_IF_STRING(plugin, "name") {
                            if (CD_CStringIsEqual(J_STRING_VALUE, "base")) {
                                J_STRING(plugin, "commandChar", _config.commandChar);

                                break;
                            }
                        }
                    }
                }
            }
        }
    }


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

    pthread_mutex_destroy(&_lock.login);

    return true;
}
