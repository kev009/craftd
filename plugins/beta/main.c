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
#include <beta/Cache.h>
#include <beta/Packet.h>
#include <beta/PacketLength.h>

static struct {
    pthread_mutex_t login;
} _lock;

static struct {
    const char* commandChar;
} _config;

#include <beta/callbacks.h>

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->name = CD_CreateStringFromCString("Beta 1.3");

    CD_DO { // Initiailize config cache
        _config.commandChar = "/";

        J_DO {
            J_IN(server, self->server->config->data, "server") {
                J_IN(plugin, server, "plugin") {
                    J_FOREACH(plugin, plugin, "plugins") {
                        J_IF_STRING(plugin, "name") {
                            if (CD_CStringIsEqual(J_STRING_VALUE, "beta")) {
                                J_STRING(plugin, "commandChar", _config.commandChar);

                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    CD_CacheAvailable(CACHE(self->server), 0);

    CD_DO { // Initialize server's cache base slot
        CDBetaServerCache* cache = CACHE(self->server)->slot[0] = CD_malloc(sizeof(CDBetaServerCache));

        cache->worlds = CD_CreateList();
    }

    self->server->packet.parsable = CD_PacketParsable;
    self->server->packet.parse    = CD_PacketFromBuffers;

    pthread_mutex_init(&_lock.login, NULL);

    CD_HashPut(PRIVATE(self), "Event.timeIncrease", CD_SetInterval(self->server->timeloop, 1,  (event_callback_fn) cdbeta_TimeIncrease));
    CD_HashPut(PRIVATE(self), "Event.timeUpdate",   CD_SetInterval(self->server->timeloop, 30, (event_callback_fn) cdbeta_TimeUpdate));
    CD_HashPut(PRIVATE(self), "Event.keepAlive",    CD_SetInterval(self->server->timeloop, 10, (event_callback_fn) cdbeta_KeepAlive));

    CD_EventRegister(self->server, "Client.process", cdbeta_ClientProcess);
    CD_EventRegister(self->server, "Client.processed", cdbeta_ClientProcessed);

    CD_EventRegister(self->server, "Client.connect", cdbeta_ClientConnect);
    CD_EventRegister(self->server, "Player.login", cdbeta_PlayerLogin);
    CD_EventRegister(self->server, "Player.logout", cdbeta_PlayerLogout);
    CD_EventRegister(self->server, "Client.disconnect", cdbeta_ClientDisconnect);

    CD_EventRegister(self->server, "Client.kick", cdbeta_ClientKick);

    CD_EventRegister(self->server, "Player.command", cdbeta_HandleCommand);
    CD_EventRegister(self->server, "Player.chat", cdbeta_HandleChat);

    CD_EventRegister(self->server, "Player.destroy", cdbeta_PlayerDestroy);

    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_ClearInterval(self->server->timeloop, (int) CD_HashDelete(PRIVATE(self), "Event.timeIncrease"));
    CD_ClearInterval(self->server->timeloop, (int) CD_HashDelete(PRIVATE(self), "Event.timeUpdate"));
    CD_ClearInterval(self->server->timeloop, (int) CD_HashDelete(PRIVATE(self), "Event.keepAlive"));

    CD_EventUnregister(self->server, "Client.process", cdbeta_ClientProcess);
    CD_EventUnregister(self->server, "Client.processed", cdbeta_ClientProcessed);

    CD_EventUnregister(self->server, "Client.connect", cdbeta_ClientConnect);
    CD_EventUnregister(self->server, "Player.login", cdbeta_PlayerLogin);
    CD_EventUnregister(self->server, "Player.logout", cdbeta_PlayerLogout);
    CD_EventUnregister(self->server, "Client.disconnect", cdbeta_ClientDisconnect);

    CD_EventUnregister(self->server, "Client.kick", cdbeta_ClientKick);

    CD_EventUnregister(self->server, "Player.command", cdbeta_PlayerCommand);
    CD_EventUnregister(self->server, "Player.chat", cdbeta_PlayerChat);

    CD_EventUnregister(self->server, "Player.destroy", cdbeta_PlayerDestroy);

    pthread_mutex_destroy(&_lock.login);

    return true;
}
