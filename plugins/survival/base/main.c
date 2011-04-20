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

#include <craftd/protocols/survival.h>

#ifdef HAVE_JSON
#include <jansson.h>
#endif

static struct {
    pthread_mutex_t login;
} _lock;

static struct {
    const char* command;
} _config;

#include "callbacks.c"

static
void
cdsurvival_TimeIncrease (void* _, void* __, CDServer* server)
{
    CDList* worlds = (CDList*) CD_DynamicGet(server, "World.list");

    CD_LIST_FOREACH(worlds, it) {
        SVWorld* world = (SVWorld*) CD_ListIteratorValue(it);

        uint16_t current = SV_WorldGetTime(world);

        if (current >= 0 && current <= 11999) {
            SV_WorldSetTime(world, current += world->config.cache.rate.day);
        }
        else if (current >= 12000 && current <= 13799) {
            SV_WorldSetTime(world, current += world->config.cache.rate.sunset);
        }
        else if (current >= 13800 && current <= 22199) {
            SV_WorldSetTime(world, current += world->config.cache.rate.night);
        }
        else if (current >= 22200 && current <= 23999) {
            SV_WorldSetTime(world, current += world->config.cache.rate.sunrise);
        }

        if (current >= 24000) {
            SV_WorldSetTime(world, current - 24000);
        }
    }
}

static
void
cdsurvival_TimeUpdate (void* _, void* __, CDServer* server)
{
    CDList* worlds = (CDList*) CD_DynamicGet(server, "World.list");

    CD_LIST_FOREACH(worlds, it) {
        SVWorld* world = (SVWorld*) CD_ListIteratorValue(it);

        SVPacketTimeUpdate pkt = {
            .response = {
                .time = SV_WorldGetTime(world)
            }
        };

        SVPacket packet = { SVResponse, SVTimeUpdate, (CDPointer) &pkt };

        SV_WorldBroadcastPacket(world, &packet);
    }
}

static
void
cdsurvival_KeepAlive (void* _, void* __, CDServer* server)
{
    SVPacket  packet = { SVResponse, SVKeepAlive, CDNull };
    CDBuffer* buffer = SV_PacketToBuffer(&packet);

    CD_LIST_FOREACH(server->clients, it) {
        CD_ClientSendBuffer((CDClient*) CD_ListIteratorValue(it), buffer);
    }

    CD_DestroyBuffer(buffer);
}

static
bool
cdsurvival_ServerStart (CDServer* server)
{
    CDPlugin* self = CD_GetPlugin(server->plugins, "survival.base");

    CDList*  worlds       = CD_CreateList();
    SVWorld* defaultWorld = NULL;

    C_FOREACH(world, C_PATH(server->config, "server.game.protocol.worlds")) {
         if (C_TO_BOOL(C_GET(world, "default"))) {
            defaultWorld = SV_CreateWorld(self->server, C_TO_STRING(C_GET(world, "name")));
            break;
        }
    }

    if (!defaultWorld) {
        defaultWorld = SV_CreateWorld(self->server, "default");
    }

    CD_ListPush(worlds, (CDPointer) defaultWorld);

    C_FOREACH(world, C_PATH(self->config, "server.game.protocol.worlds")) {
         if (!C_TO_BOOL(C_GET(world, "default"))) {
            CD_ListPush(worlds, (CDPointer) SV_CreateWorld(self->server, C_TO_STRING(C_GET(world, "name"))));
        }
    }

    CD_DynamicPut(self->server, "World.list", (CDPointer) worlds);
    CD_DynamicPut(self->server, "World.default", (CDPointer) defaultWorld);

    return true;
}

static
bool
cdsurvival_ServerStop (CDServer* server)
{
    CD_DynamicDelete(server, "World.default");

    CDList* worlds = (CDList*) CD_DynamicDelete(server, "World.list");

    CD_LIST_FOREACH(worlds, it) {
        SV_DestroyWorld((SVWorld*) CD_ListIteratorValue(it));
    }

    return true;
}

#ifdef HAVE_JSON
static
bool
cdsurvival_JSON (CDServer* server, json_t* input, json_t* output)
{
    char* text = json_dumps(input, JSON_INDENT(2));

    SLOG(server, LOG_NOTICE, "%s", text);

    free(text);

    return true;
}
#endif

static
bool
cdsurvival_PersistenceInitialized (CDServer* server, CDPlugin* persistence)
{
    return true;
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->description = CD_CreateStringFromCString("Minecraft Beta 1.4");

    DO { // Initiailize config cache
        _config.command = "/";

        C_SAVE(C_PATH(self->config, "command"), C_STRING, _config.command);
    }

    CD_InitializeSurvivalProtocol(self->server);

    pthread_mutex_init(&_lock.login, NULL);

    CD_DynamicPut(self, "Event.timeIncrease", CD_SetInterval(self->server->timeloop, 1,  (event_callback_fn) cdsurvival_TimeIncrease, CDNull));
    CD_DynamicPut(self, "Event.timeUpdate",   CD_SetInterval(self->server->timeloop, 30, (event_callback_fn) cdsurvival_TimeUpdate, CDNull));
    CD_DynamicPut(self, "Event.keepAlive",    CD_SetInterval(self->server->timeloop, 10, (event_callback_fn) cdsurvival_KeepAlive, CDNull));

    #ifdef HAVE_JSON
    CD_EventRegister(self->server, "RPC.JSON", cdsurvival_JSON);
    #endif

    CD_EventRegister(self->server, "Server.start!", cdsurvival_ServerStart);
    CD_EventRegister(self->server, "Server.stop!", cdsurvival_ServerStop);

    CD_EventRegister(self->server, "Persistence.initialized", cdsurvival_PersistenceInitialized);

    CD_EventRegister(self->server, "Client.connect", cdsurvival_ClientConnect);
    CD_EventRegister(self->server, "Client.process", cdsurvival_ClientProcess);
    CD_EventRegister(self->server, "Client.processed", cdsurvival_ClientProcessed);
    CD_EventRegister(self->server, "Player.command", cdsurvival_PlayerCommand);
    CD_EventRegister(self->server, "Player.chat", cdsurvival_PlayerChat);
    CD_EventRegister(self->server, "Player.logout", cdsurvival_PlayerLogout);
    CD_EventRegister(self->server, "Player.destroy", cdsurvival_PlayerDestroy);
    CD_EventRegister(self->server, "Client.kick", cdsurvival_ClientKick);
    CD_EventRegister(self->server, "Client.disconnect", (CDEventCallbackFunction) cdsurvival_ClientDisconnect);

    CD_EventProvides(self->server, "Player.login", CD_CreateEventParameters("SVPlayer", "bool", NULL));
    CD_EventProvides(self->server, "Player.logout", CD_CreateEventParameters("SVPlayer", "bool", NULL));

    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_ClearInterval(self->server->timeloop, (int) CD_DynamicDelete(self, "Event.timeIncrease"));
    CD_ClearInterval(self->server->timeloop, (int) CD_DynamicDelete(self, "Event.timeUpdate"));
    CD_ClearInterval(self->server->timeloop, (int) CD_DynamicDelete(self, "Event.keepAlive"));

    #ifdef HAVE_JSON
    CD_EventUnregister(self->server, "RPC.JSON", cdsurvival_JSON);
    #endif

    CD_EventUnregister(self->server, "Server.start!", cdsurvival_ServerStart);
    CD_EventUnregister(self->server, "Server.stop!", cdsurvival_ServerStop);

    CD_EventUnregister(self->server, "Persistence.initialized", cdsurvival_PersistenceInitialized);

    CD_EventUnregister(self->server, "Client.connect", cdsurvival_ClientConnect);
    CD_EventUnregister(self->server, "Client.process", cdsurvival_ClientProcess);
    CD_EventUnregister(self->server, "Client.processed", cdsurvival_ClientProcessed);
    CD_EventUnregister(self->server, "Player.command", cdsurvival_PlayerCommand);
    CD_EventUnregister(self->server, "Player.chat", cdsurvival_PlayerChat);
    CD_EventUnregister(self->server, "Player.logout", cdsurvival_PlayerLogout);
    CD_EventUnregister(self->server, "Player.destroy", cdsurvival_PlayerDestroy);
    CD_EventUnregister(self->server, "Client.kick", cdsurvival_ClientKick);
    CD_EventUnregister(self->server, "Client.disconnect", (CDEventCallbackFunction) cdsurvival_ClientDisconnect);

    pthread_mutex_destroy(&_lock.login);

    return true;
}
