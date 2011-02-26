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

#include "common.h"
#include "Server.h"
#include "Plugin.h"
#include "Player.h"

static
void
cd_TimeIncrease (evutil_socket_t fd, short event, CDServer* self)
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
cd_TimeUpdate (evutil_socket_t fd, short event, CDServer* self)
{
    CDPacketTimeUpdate data   = { CD_ServerGetTime(self) };
    CDPacket           packet = { CDResponse, CDTimeUpdate, &data };

    CD_HASH_FOREACH(self->players, it) {
        CD_PlayerSendPacket(CD_HashIteratorValue(self->players, it), &packet);
    }
}

static
void
cd_KeepAlive (evutil_socket_t fd, short event, CDServer* self)
{
    CDPacket packet = { CDResponse, CDKeepAlive, NULL };

    CD_HASH_FOREACH(self->players, it) {
        CD_PlayerSendPacket(CD_HashIteratorValue(self->players, it), &packet);
    }
}

static
bool
cd_PlayerProcess (CDServer* server, CDPlayer* player)
{
    CDPacket* packet = CD_HashGet(PRIVATE(player), "packet");

    switch (packet->type) {
        case CDKeepAlive: {
            SDEBUG(server, "This was a triumph, I'm making a note here, huge success.");
        } break;

        case CDLogin: {
            CDPacketLogin* data = packet->data;

            SLOG(server, LOG_NOTICE, "%s:%s tried login", CD_StringContent(data->request.username), CD_StringContent(data->request.password));
        } break;

        case CDHandshake: {
            CDPacketHandshake* data = packet->data;

            SLOG(server, LOG_NOTICE, "%s tried handshake", CD_StringContent(data->request.username));

            CDPacketHandshake pkt;
            pkt.response.hash = CD_CreateStringFromCString("-");

            CDPacket response = { CDResponse, CDHandshake, &pkt };

            CD_PlayerSendPacket(player, &response);

            CD_DestroyString(pkt.response.hash);
        } break;
    }

    return true;
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    CD_HashSet(PRIVATE(self), "Event.timeIncrease", (void*) CD_SetInterval(self->server->timeloop, 1,  cd_TimeIncrease));
    CD_HashSet(PRIVATE(self), "Event.timeUpdate",   (void*) CD_SetInterval(self->server->timeloop, 30, cd_TimeUpdate));
    CD_HashSet(PRIVATE(self), "Event.keepAlive",    (void*) CD_SetInterval(self->server->timeloop, 10, cd_KeepAlive));

    CD_EventRegister(self->server, "Player.process", cd_PlayerProcess);
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_ClearInterval(self->server->timeloop, (int) CD_HashGet(PRIVATE(self), "Event.timeIncrease"));
    CD_ClearInterval(self->server->timeloop, (int) CD_HashGet(PRIVATE(self), "Event.timeUpdate"));
    CD_ClearInterval(self->server->timeloop, (int) CD_HashGet(PRIVATE(self), "Event.keepAlive"));

    CD_EventUnregister(self->server, "Player.process", cd_PlayerProcess);
}
