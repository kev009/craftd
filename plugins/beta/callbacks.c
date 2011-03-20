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

#include <zlib.h>

#include <beta/Player.h>

static
bool
cdbeta_CompareMCChunkPosition (CDSet* self, MCChunkPosition* a, MCChunkPosition* b)
{
    return (a->x == b->x && a->z == b->z);
}

static
unsigned int
cdbeta_HashMCPosition (CDSet* self, MCChunkPosition* position)
{
    const int HASHMULTIPLIER = 31;
    const int CHUNKBUCKETS   = 401; // Max chunks to the nearest prime

    return ((((position->x * HASHMULTIPLIER)) * HASHMULTIPLIER + position->z) * HASHMULTIPLIER) % CHUNKBUCKETS;
}

static
bool
cdbeta_SendChunk (CDServer* server, CDPlayer* player, MCChunkPosition* coord)
{
    CD_DO {
        CDPacketPreChunk pkt = {
            .response = {
                .position = *coord,
                .mode     = true
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
cdbeta_ChunkRadiusUnload (CDSet* self, MCChunkPosition* coord, CDPlayer* player)
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
cdbeta_ChunkMemberFree (CDSet* self, MCChunkPosition* coord, CDPointer unused)
{
    CD_free(coord);
}

static
void
cdbeta_ChunkRadiusLoad (CDSet* self, MCChunkPosition* coord, CDPlayer* player)
{
    cdbeta_SendChunk(player->client->server, player, coord);
}

static
void
cdbeta_SendChunkRadius (CDPlayer* player, MCChunkPosition* area, int radius)
{
    CDSet* loadedChunks = ((CDBetaPlayerCache*) CACHE(player)->slot[0])->loadedChunks;
    CDSet* oldChunks    = loadedChunks;
    CDSet* newChunks    = CD_CreateSet(400, (CDSetCompare) cdbeta_CompareMCChunkPosition, (CDSetHash) cdbeta_HashMCPosition);

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

    CD_SetMap(toRemove, (CDSetApply) cdbeta_ChunkRadiusUnload, (CDPointer) player);
    CD_SetMap(toAdd, (CDSetApply) cdbeta_ChunkRadiusLoad, (CDPointer) player);

    CD_DestroySet(toRemove);
    CD_DestroySet(toAdd);
    CD_DestroySet(oldChunks);

    CD_HashPut(PRIVATE(player), "Player.loadedChunks", (CDPointer)
        (((CDBetaPlayerCache*) CACHE(player)->slot[0])->loadedChunks = newChunks));
}

static
bool
cdbeta_CoordInRadius(MCChunkPosition *coord, MCChunkPosition *centerCoord, int radius)
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
cdbeta_posDistanceGreater( MCPrecisePosition a, MCPrecisePosition b, int maxdist )
{
  return (abs( a.x - b.x ) > maxdist ||
          abs( a.y - b.y ) > maxdist ||
          abs( a.z - b.z ) > maxdist);
}

static
MCRelativePosition
cdbeta_relativeMove(MCPrecisePosition *pos1, MCPrecisePosition *pos2)
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
cdbeta_SendPacketToAllInRegion(CDPlayer *player, CDPacket *pkt)
{
  CDList *seenPlayers = (CDList *) CD_HashGet(PRIVATE(player), "Player.seenPlayers");

  CD_LIST_FOREACH(seenPlayers, it)
  {
    if ( player != (CDPlayer *) CD_ListIteratorValue(it) )
      CD_PlayerSendPacket( (CDPlayer *) CD_ListIteratorValue(it), pkt );
    else
      CERR("We have a player with himself in the List????");
  }
}

static
void
cdbeta_SendUpdatePos(CDPlayer *player, MCPrecisePosition *newpos, bool andLook, MCFloat pitch, MCFloat yaw )
{
  if ( cdbeta_posDistanceGreater(player->entity.position, *newpos, 2) || true )
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

      cdbeta_SendPacketToAllInRegion(player, &response);
    }
  }
  else
  {
    if (andLook)
    {
      CDPacketEntityLookMove pkt;

      pkt.response.entity = player->entity;
      pkt.response.position = cdbeta_relativeMove(&player->entity.position, newpos);
      pkt.response.yaw = yaw;
      pkt.response.pitch = pitch;
    }
    else
    {
      CDPacketEntityRelativeMove pkt;

      pkt.response.entity = player->entity;
      pkt.response.position = cdbeta_relativeMove(&player->entity.position, newpos);

      CDPacket response = { CDResponse, CDEntityRelativeMove,(CDPointer)  &pkt };

      cdbeta_SendPacketToAllInRegion(player, &response );
    }
  }
}

static
void
cdbeta_SendNamedPlayerSpawn(CDPlayer *player, CDPlayer *other)
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
cdbeta_SendDestroyEntity (CDPlayer* player, MCEntity* entity)
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
cdbeta_CheckPlayersInRegion (CDServer* server, CDPlayer* player, MCChunkPosition *coord, int radius)
{
    CDList* seenPlayers = (CDList*) ((CDBetaPlayerCache*) CACHE(player)->slot[0])->seenPlayers;

    CD_HASH_FOREACH(player->world->players, it) {
        CDPlayer* otherPlayer = (CDPlayer *) CD_HashIteratorValue(it);

        // If we are the player to check just skip
        if (otherPlayer == player) {
            continue;
        }

        MCChunkPosition chunkPos = MC_PrecisePositionToChunkPosition(otherPlayer->entity.position);

        if (cdbeta_CoordInRadius(&chunkPos, coord, radius)) {
            /* If the player is in range, but not in the list. */
            if (!CD_ListContains(seenPlayers, (CDPointer) otherPlayer)) {
                CD_ListPush(seenPlayers, (CDPointer) otherPlayer);
                cdbeta_SendNamedPlayerSpawn(player, otherPlayer);

                CDList *otherSeenPlayers = (CDList *) CD_HashGet(PRIVATE(otherPlayer), "Player.seenPlayers");
                CD_ListPush(otherSeenPlayers, (CDPointer) player);
                cdbeta_SendNamedPlayerSpawn(otherPlayer, player);
            }
        }
        else {
            /* If the player is out of range but in the list */
            if (CD_ListContains(seenPlayers, (CDPointer) otherPlayer)) {
                CDList *otherSeenPlayers = (CDList *) CD_HashGet(PRIVATE(otherPlayer), "Player.seenPlayers");

                CD_ListDeleteAll(seenPlayers, (CDPointer) otherPlayer);
                CD_ListDeleteAll(otherSeenPlayers, (CDPointer) player);

                /* Should send both players an update. */
                cdbeta_SendDestroyEntity(player, &otherPlayer->entity);
                cdbeta_SendDestroyEntity(otherPlayer, &player->entity);
            }
        }
    }
}

static
bool
cdbeta_ClientProcess (CDServer* server, CDClient* client, CDPacket* packet)
{
    CDWorld*           world;
    CDBetaClientCache* cache  = (CDBetaClientCache*) CACHE(client)->slot[0];
    CDPlayer*          player = cache->player;

    if (player) {
        world = player->world;
    }
    else {
        world = _world;
    }

    switch (packet->type) {
        case CDKeepAlive: {
            SDEBUG(server, "%s is still alive", player ? CD_StringContent(player->username) : client->ip);
        } break;

        case CDLogin: {
            CDPacketLogin* data = (CDPacketLogin*) packet->data;

            pthread_mutex_lock(&_lock.login);

            SLOG(server, LOG_NOTICE, "%s tried login with client version %d", CD_StringContent(data->request.username), data->request.version);

            if (data->request.version != CRAFTD_PROTOCOL_VERSION) {
                CD_ServerKick(server, client, CD_CreateStringFromFormat(
                    "Protocol mismatch, we support %d, you're using %d.",
                    CRAFTD_PROTOCOL_VERSION, data->request.version));

                return false;
            }

            CD_HashPut(PRIVATE(client), "Client.player", (CDPointer) (player = cache->player = CD_CreatePlayer(client)));

            player->world = world;

            if (CD_HashHasKey(world->players, CD_StringContent(data->request.username))) {
                SLOG(server, LOG_NOTICE, "%s: nick exists on the server", CD_StringContent(data->request.username));

                if (server->config->cache.game.standard) {
                    CD_ServerKick(server, client, CD_CreateStringFromFormat("%s nick already exists",
                        CD_StringContent(data->request.username)));

                    CD_EventDispatch(server, "Player.login", player, false);

                    return false;
                }
                else {
                    player->username = CD_CreateStringFromFormat("Player_%d", player->entity.id);
                }
            }
            else {
                player->username = CD_CloneString(data->request.username);
            }

            CD_HashPut(world->players, CD_StringContent(player->username), (CDPointer) player);

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

                    if (!cdbeta_SendChunk(server, player, &coords)) {
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
                cdbeta_SendChunkRadius(player, &newChunk, 10);

                cdbeta_CheckPlayersInRegion(server, player, &newChunk, 5);
            }

            cdbeta_SendUpdatePos(player, &data->request.position, false, 0, 0);

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

              cdbeta_SendPacketToAllInRegion(player, &response);
            }
        } break;

        case CDPlayerMoveLook: {
            // Stub.  Do dead reckoning or some other sanity check for data
            // and send CD_SetDifference of chunks on boundary change.

            CDPacketPlayerMoveLook* data = (CDPacketPlayerMoveLook*) packet->data;

            MCChunkPosition oldChunk = MC_PrecisePositionToChunkPosition(player->entity.position);
            MCChunkPosition newChunk = MC_PrecisePositionToChunkPosition(data->request.position);

            if (!MC_ChunkPositionEqual(oldChunk, newChunk)) {
                cdbeta_SendChunkRadius(player, &newChunk, 10);

                cdbeta_CheckPlayersInRegion(server, player, &newChunk, 5);
            }

            cdbeta_SendUpdatePos(player, &data->request.position, true, data->request.pitch, data->request.yaw);

            player->entity.position = data->request.position;
            player->yaw             = data->request.yaw;
            player->pitch           = data->request.pitch;
        } break;

        case CDDisconnect: {
            CDPacketDisconnect* data = (CDPacketDisconnect*) packet->data;

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
cdbeta_ClientProcessed (CDServer* server, CDClient* client, CDPacket* packet)
{
    return true;
}

static
bool
cdbeta_ClientConnect (CDServer* server, CDClient* client)
{
    CD_CacheAvailable(CACHE(client), 0);

    CACHE(client)->slot[0] = (CDPointer) CD_alloc(sizeof(CDBetaClientCache));

    return true;
}

static
bool
cdbeta_PlayerLogin (CDServer* server, CDPlayer* player, int status)
{
    if (!status) {
        return true;
    }

    CD_DO {
        CDPacketTimeUpdate pkt = {
            .response = {
                .time = CD_WorldGetTime(player->world)
            }
        };

        CDPacket packet = { CDResponse, CDTimeUpdate, (CDPointer) &pkt };

        CD_PlayerSendPacket(player, &packet);
    }

    CD_WorldBroadcast(player->world, MC_StringColor(CD_CreateStringFromFormat("%s has joined the game",
                CD_StringContent(player->username)), MCColorYellow));


    CD_CacheAvailable(CACHE(player), 0);

    CDBetaPlayerCache* cache = CD_alloc(sizeof(CDBetaPlayerCache));
    CACHE(player)->slot[0]   = (CDPointer) cache;

    cache->loadedChunks = CD_CreateSet(400, (CDSetCompare) cdbeta_CompareMCChunkPosition, (CDSetHash) cdbeta_HashMCPosition);
    cache->seenPlayers  = CD_CreateList();

    CD_HashPut(PRIVATE(player), "Player.loadedChunks", (CDPointer) cache->loadedChunks);
    CD_HashPut(PRIVATE(player), "Player.seenPlayers", (CDPointer) cache->seenPlayers);

    MCChunkPosition playerChunk = MC_PrecisePositionToChunkPosition(player->entity.position);

    cdbeta_CheckPlayersInRegion(server, player, &playerChunk, 5);

    return true;
}

static
bool
cdbeta_PlayerLogout (CDServer* server, CDPlayer* player)
{
    if (player->world) {
        CD_WorldBroadcast(player->world, MC_StringColor(CD_CreateStringFromFormat("%s has left the game",
            CD_StringContent(player->username)), MCColorYellow));

        CDList* seenPlayers = (CDList*) ((CDBetaPlayerCache*) CACHE(player)->slot[0])->seenPlayers;

        CD_LIST_FOREACH(seenPlayers, it) {
            CDPlayer* other            = (CDPlayer*) CD_ListIteratorValue(it);
            CDList*   otherSeenPlayers = (CDList*) ((CDBetaPlayerCache*) CACHE(other)->slot[0])->seenPlayers;

            cdbeta_SendDestroyEntity(other, &player->entity);
            CD_ListDeleteAll(otherSeenPlayers, (CDPointer) player);
        }
    }

    return true;
}

static
bool
cdbeta_ClientDisconnect (CDServer* server, CDClient* client, bool status)
{
    CDList* worlds = ((CDBetaServerCache*) CACHE(server)->slot[0])->worlds;

    CD_LIST_FOREACH(worlds, it) {
        CDWorld* world = (CDWorld*) CD_ListIteratorValue(it);

        if (CD_MapHasKey(world->clients, (CDPointer) client)) {
            CD_EventDispatch(server, "Player.logout", (CDPlayer*) CD_MapGet(world->clients, (CDPointer) client), status);

            CD_LIST_BREAK(worlds);
        }
    }

    return true;
}

static
bool
cdbeta_ClientKick (CDClient* client, CDString* reason)
{
    CD_DO {
        CDPacketDisconnect pkt = {
            .response = {
                .reason = reason
            }
        };

        CDPacket  packet = { CDResponse, CDDisconnect, (CDPointer) &pkt };
        CDBuffer* buffer = CD_PacketToBuffer(&packet);

        CD_ClientSendBuffer(client, buffer);

        CD_DestroyBuffer(buffer);
    }

    return true;
}

static
bool
cdbeta_PlayerCommand (CDServer* server, CDPlayer* player, CDString* command)
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
cdbeta_PlayerChat (CDServer* server, CDPlayer* player, CDString* message)
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
cdbeta_PlayerDestroy (CDServer* server, CDPlayer* player)
{
    CD_DestroyList((CDList*) CD_HashDelete(PRIVATE(player), "Player.seenPlayers"));

    if (player->username) {
        CDSet* chunks = (CDSet*) CD_HashDelete(PRIVATE(player), "Player.loadedChunks");

        if (chunks) {
            CD_SetMap(chunks, (CDSetApply) cdbeta_ChunkMemberFree, CDNull);
            CD_DestroySet(chunks);
        }
    }

    return true;
}
