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

struct {
    pthread_mutex_t login;
} cd_lock;

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
    CDPacketTimeUpdate pkt;
    pkt.response.time = CD_ServerGetTime(self);

    CDPacket packet = { CDResponse, CDTimeUpdate, (CDPointer) &pkt };

    CD_HASH_FOREACH(self->players, it) {
        CD_PlayerSendPacket((CDPlayer*) CD_HashIteratorValue(self->players, it), &packet);
    }
}

static
void
cd_KeepAlive (evutil_socket_t fd, short event, CDServer* self)
{
    CDPacket packet = { CDResponse, CDKeepAlive, (CDPointer) NULL };

    CD_HASH_FOREACH(self->players, it) {
        CD_PlayerSendPacket((CDPlayer*) CD_HashIteratorValue(self->players, it), &packet);
    }
}

static
bool
cd_PlayerProcess (CDServer* server, CDPlayer* player)
{
    CDPacket* packet = (CDPacket*) CD_HashGet(PRIVATE(player), "packet");

    switch (packet->type) {
        case CDKeepAlive: {
            SDEBUG(server, "This was a triumph, I'm making a note here, huge success.");
        } break;

        case CDLogin: {
            CDPacketLogin* data = (CDPacketLogin*) packet->data;

            pthread_mutex_lock(&cd_lock.login);

            SLOG(server, LOG_NOTICE, "%s with client version %d tried login", CD_StringContent(data->request.username), data->request.version);

            if (CD_HashGet(server->players, CD_StringContent(data->request.username))) {
                SLOG(server, LOG_NOTICE, "%s exists on the server", CD_StringContent(data->request.username));

                // TODO: kill it with fire or give it another name IRC-like.
            }

            player->username = CD_CloneString(data->request.username);

            CD_HashSet(server->players, CD_StringContent(player->username), (CDPointer) player);

            pthread_mutex_unlock(&cd_lock.login);
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
                SLOG(server, LOG_NOTICE, "<%s> %s");
            }
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

    pthread_mutex_init(&cd_lock.login, NULL);

    CD_HashSet(PRIVATE(self), "Event.timeIncrease", CD_SetInterval(self->server->timeloop, 1,  (event_callback_fn) cd_TimeIncrease));
    CD_HashSet(PRIVATE(self), "Event.timeUpdate",   CD_SetInterval(self->server->timeloop, 30, (event_callback_fn) cd_TimeUpdate));
    CD_HashSet(PRIVATE(self), "Event.keepAlive",    CD_SetInterval(self->server->timeloop, 10, (event_callback_fn) cd_KeepAlive));

    CD_EventRegister(self->server, "Player.process", cd_PlayerProcess);

    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_ClearInterval(self->server->timeloop, (int) CD_HashGet(PRIVATE(self), "Event.timeIncrease"));
    CD_ClearInterval(self->server->timeloop, (int) CD_HashGet(PRIVATE(self), "Event.timeUpdate"));
    CD_ClearInterval(self->server->timeloop, (int) CD_HashGet(PRIVATE(self), "Event.keepAlive"));

    CD_EventUnregister(self->server, "Player.process", cd_PlayerProcess);

    pthread_mutex_destroy(&cd_lock.login);

    return true;
}
