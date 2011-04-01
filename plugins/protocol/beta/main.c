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

#include <craftd/Plugin.h>
#include <craftd/Server.h>

#include <beta/common.h>
#include <beta/Packet.h>
#include <beta/PacketLength.h>
#include <beta/World.h>

static struct {
    pthread_mutex_t login;
} _lock;

static struct {
    const char* commandChar;
} _config;

#include "callbacks.c"

static
void
cdbeta_TimeIncrease (void* _, void* __, CDServer* server)
{
    CDList* worlds = (CDList*) CD_DynamicGet(server, "World.list");

    CD_LIST_FOREACH(worlds, it) {
        CDWorld* world = (CDWorld*) CD_ListIteratorValue(it);

        uint16_t current = CD_WorldGetTime(world);

        if (current >= 0 && current <= 11999) {
            CD_WorldSetTime(world, current += server->config->cache.rate.day);
        }
        else if (current >= 12000 && current <= 13799) {
            CD_WorldSetTime(world, current += server->config->cache.rate.sunset);
        }
        else if (current >= 13800 && current <= 22199) {
            CD_WorldSetTime(world, current += server->config->cache.rate.night);
        }
        else if (current >= 22200 && current <= 23999) {
            CD_WorldSetTime(world, current += server->config->cache.rate.sunrise);
        }

        if (current >= 24000) {
            CD_WorldSetTime(world, current - 24000);
        }
    }
}

static
void
cdbeta_TimeUpdate (void* _, void* __, CDServer* server)
{
    CDList* worlds = (CDList*) CD_DynamicGet(server, "World.list");

    CD_LIST_FOREACH(worlds, it) {
        CDWorld* world = (CDWorld*) CD_ListIteratorValue(it);

        CDPacketTimeUpdate pkt = {
            .response = {
                .time = CD_WorldGetTime(world)
            }
        };

        CDPacket packet = { CDResponse, CDTimeUpdate, (CDPointer) &pkt };

        CD_WorldBroadcastPacket(world, &packet);
    }
}

static
void
cdbeta_KeepAlive (void* _, void* __, CDServer* server)
{
    CDPacket  packet = { CDResponse, CDKeepAlive, CDNull };
    CDBuffer* buffer = CD_PacketToBuffer(&packet);

    CD_LIST_FOREACH(server->clients, it) {
        CD_ClientSendBuffer((CDClient*) CD_ListIteratorValue(it), buffer);
    }

    CD_DestroyBuffer(buffer);
}

static
bool
cdbeta_ServerStart (CDServer* server)
{
    CDPlugin* self = CD_GetPlugin(server->plugins, "protocol.beta");

    CDList*  worlds       = CD_CreateList();
    CDWorld* defaultWorld = NULL;

    J_DO {
        J_FOREACH(world, self->config, "worlds") {
            J_IF_BOOL(world, "default") {
                if (J_BOOL_VALUE) {
                    J_IF_STRING(world, "name") {
                        defaultWorld = CD_CreateWorld(self->server, J_STRING_VALUE);
                    }

                    break;
                }
            }
        }
    }

    if (!defaultWorld) {
        defaultWorld = CD_CreateWorld(self->server, "default");
    }

    CD_ListPush(worlds, (CDPointer) defaultWorld);

    J_DO {
        J_FOREACH(world, self->config, "worlds") {
            J_IF_BOOL(world, "default") {
                if (!J_BOOL_VALUE) {
                    J_IF_STRING(world, "name") {
                        CD_ListPush(worlds, (CDPointer) CD_CreateWorld(self->server, J_STRING_VALUE));
                    }
                }
            }
        }
    }

    CD_DynamicPut(self->server, "World.list", (CDPointer) worlds);
    CD_DynamicPut(self->server, "World.default", (CDPointer) defaultWorld);

    return true;
}

static
bool
cdbeta_ServerStop (CDServer* server)
{
    CD_DynamicDelete(server, "World.default");

    CDList* worlds = (CDList*) CD_DynamicDelete(server, "World.list");

    CD_LIST_FOREACH(worlds, it) {
        CD_DestroyWorld((CDWorld*) CD_ListIteratorValue(it));
    }

    return true;
}

bool
cdbeta_JSON (CDServer* server, json_t* input, json_t* output)
{
    char* text = json_dumps(input, JSON_INDENT(2));

    SLOG(server, LOG_NOTICE, "%s", text);

    free(text);

    return true;
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->description = CD_CreateStringFromCString("Beta 1.3");

    DO { // Initiailize config cache
        _config.commandChar = "/";

        J_DO {
            J_STRING(self->config, "commandChar", _config.commandChar);
        }
    }

    self->server->packet.parsable = CD_PacketParsable;
    self->server->packet.parse    = (void* (*)(CDBuffers *)) CD_PacketFromBuffers;

    pthread_mutex_init(&_lock.login, NULL);

    CD_DynamicPut(self, "Event.timeIncrease", CD_SetInterval(self->server->timeloop, 1,  (event_callback_fn) cdbeta_TimeIncrease, CDNull));
    CD_DynamicPut(self, "Event.timeUpdate",   CD_SetInterval(self->server->timeloop, 30, (event_callback_fn) cdbeta_TimeUpdate, CDNull));
    CD_DynamicPut(self, "Event.keepAlive",    CD_SetInterval(self->server->timeloop, 10, (event_callback_fn) cdbeta_KeepAlive, CDNull));

    CD_EventRegister(self->server, "RPC.JSON", cdbeta_JSON);

    CD_EventRegister(self->server, "Server.start!", cdbeta_ServerStart);
    CD_EventRegister(self->server, "Server.stop!", cdbeta_ServerStop);

    CD_EventRegister(self->server, "Client.process", cdbeta_ClientProcess);
    CD_EventRegister(self->server, "Client.processed", cdbeta_ClientProcessed);

    CD_EventRegister(self->server, "Client.connect", cdbeta_ClientConnect);
    CD_EventRegister(self->server, "Player.login", cdbeta_PlayerLogin);
    CD_EventRegister(self->server, "Player.logout", cdbeta_PlayerLogout);
    CD_EventRegister(self->server, "Client.disconnect", (CDEventCallbackFunction) cdbeta_ClientDisconnect);

    CD_EventRegister(self->server, "Client.kick", cdbeta_ClientKick);

    CD_EventRegister(self->server, "Player.command", cdbeta_PlayerCommand);
    CD_EventRegister(self->server, "Player.chat", cdbeta_PlayerChat);

    CD_EventRegister(self->server, "Player.destroy", cdbeta_PlayerDestroy);

    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_ClearInterval(self->server->timeloop, (int) CD_DynamicDelete(self, "Event.timeIncrease"));
    CD_ClearInterval(self->server->timeloop, (int) CD_DynamicDelete(self, "Event.timeUpdate"));
    CD_ClearInterval(self->server->timeloop, (int) CD_DynamicDelete(self, "Event.keepAlive"));

    CD_EventUnregister(self->server, "RPC.JSON", cdbeta_JSON);

    CD_EventUnregister(self->server, "Server.start!", cdbeta_ServerStart);
    CD_EventUnregister(self->server, "Server.stop!", cdbeta_ServerStop);

    CD_EventUnregister(self->server, "Client.process", cdbeta_ClientProcess);
    CD_EventUnregister(self->server, "Client.processed", cdbeta_ClientProcessed);

    CD_EventUnregister(self->server, "Client.connect", cdbeta_ClientConnect);
    CD_EventUnregister(self->server, "Player.login", cdbeta_PlayerLogin);
    CD_EventUnregister(self->server, "Player.logout", cdbeta_PlayerLogout);
    CD_EventUnregister(self->server, "Client.disconnect", (CDEventCallbackFunction) cdbeta_ClientDisconnect);

    CD_EventUnregister(self->server, "Client.kick", cdbeta_ClientKick);

    CD_EventUnregister(self->server, "Player.command", cdbeta_PlayerCommand);
    CD_EventUnregister(self->server, "Player.chat", cdbeta_PlayerChat);

    CD_EventUnregister(self->server, "Player.destroy", cdbeta_PlayerDestroy);

    pthread_mutex_destroy(&_lock.login);

    return true;
}
