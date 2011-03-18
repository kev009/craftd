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

#define CRAFTD_SERVER_IGNORE_EXTERN
#include <craftd/Server.h>
#undef CRAFTD_SERVER_IGNORE_EXTERN

#include <craftd/common.h>
#include <craftd/Player.h>
#include <craftd/PacketLength.h>
#include <signal.h>

CDServer* CDMainServer = NULL;

static
void
cd_HandleSignal (evutil_socket_t fd, short what, CDServer* self)
{
    CD_StopServer(self);
}

CDServer*
CD_CreateServer (const char* path)
{
    CDServer* self = CD_malloc(sizeof(CDServer));

    assert(self);

    assert(pthread_spin_init(&self->lock.time, PTHREAD_PROCESS_PRIVATE) == 0);

    self->name     = NULL;
    self->logger   = CDConsoleLogger;
    self->config   = CD_ParseConfig(path);

    if (!self->config) {
        return NULL;
    }

    self->timeloop = CD_CreateTimeLoop(self);
    self->workers  = CD_CreateWorkers(self);
    self->plugins  = CD_CreatePlugins(self);

    self->entities      = CD_CreateMap();
    self->players       = CD_CreateHash();
    self->disconnecting = CD_CreateList();

    assert(self->config && self->workers && self->timeloop && self->plugins
        && self->entities && self->players);

    self->event.callbacks = CD_CreateHash();

    self->running = false;

    CD_ServerSetTime(self, 0);

    PRIVATE(self) = CD_CreateHash();
    ERROR(self)   = CDNull;

    return self;
}

void
CD_DestroyServer (CDServer* self)
{
    assert(self);

    CD_StopTimeLoop(self->timeloop);

    CD_HASH_FOREACH(self->players, it) {
        CD_ServerKick(self, (CDPlayer*) CD_HashIteratorValue(it), CD_CreateStringFromCString("shutting down"));
    }

    CD_DestroyMap(self->entities);

    if (self->workers) {
        CD_DestroyWorkers(self->workers);
    }

    CD_StopServer(self);

    if (self->plugins) {
        CD_DestroyPlugins(self->plugins);
    }

    CD_DestroyTimeLoop(self->timeloop);

    if (self->event.listener) {
        event_free(self->event.listener);
        self->event.listener = NULL;
    }

    if (self->event.base) {
        event_base_free(self->event.base);
        self->event.base = NULL;
    }

    if (self->config) {
        CD_DestroyConfig(self->config);
    }

    if (self->event.callbacks) {
        CD_DestroyHash(self->event.callbacks);
    }

    if (PRIVATE(self)) {
        CD_DestroyHash(PRIVATE(self));
    }

    CD_free(self->name);

    pthread_spin_destroy(&self->lock.time);

    CD_free(self);
}

const char*
CD_ServerToString (CDServer* self)
{
    assert(self);

    if (!self->name) {
        return "craftd";
    }
    else {
        return self->name;
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

static
void
cd_ReadCallback (struct bufferevent* event, CDPlayer* player)
{
    assert(player);

    pthread_rwlock_wrlock(&player->lock.status);

    SDEBUG(player->server, "read data from %s (%s), %d byte/s available",
        CD_StringContent(player->username), player->ip, CD_BufferLength(player->buffers->input));

    if (player->status == CDPlayerIdle) {
        CDPacket* packet;

        if (CD_PacketParsable(player->buffers) && (packet = CD_PacketFromBuffer(player->buffers->input))) {
            CD_BufferReadIn(player->buffers, CDNull, CDNull);

            SDEBUG(player->server, "received packet 0x%.2X from %s", (uint8_t) packet->type, player->ip);

            player->status = CDPlayerProcess;
            player->jobs++;

            CD_AddJob(player->server->workers, CD_CreateJob(CDPlayerProcessJob,
                (CDPointer) CD_CreatePlayerProcessJob(player, packet)));
        }
        else {
            if (errno == EILSEQ) {
                CD_ServerKick(player->server, player, CD_CreateStringFromCString("bad packet"));
            }
        }
    }

    pthread_rwlock_unlock(&player->lock.status);
}

static
void
cd_ErrorCallback (struct bufferevent* event, short error, CDPlayer* player)
{
    assert(player);

    if (!((error & BEV_EVENT_EOF) || (error & BEV_EVENT_ERROR) || (error & BEV_EVENT_TIMEOUT))) {
        return;
    }

    pthread_rwlock_wrlock(&player->lock.status);

    player->status = CDPlayerDisconnect;

    CDServer* self = player->server;

    if (error & BEV_EVENT_ERROR) {
        SLOG(self, LOG_INFO, "libevent: ip %s - %s", player->ip, evutil_socket_error_to_string(EVUTIL_SOCKET_ERROR()));
    }
    else if (error & BEV_EVENT_TIMEOUT) {
        SERR(self, "A bufferevent timeout?");
    }

    ERROR(player) = error;

    SLOG(self, LOG_NOTICE, "%s (%s) disconnected", CD_StringContent(player->username), player->ip);

    CD_AddJob(player->server->workers, CD_CreateJob(CDPlayerDisconnectJob, (CDPointer) player));

    pthread_rwlock_unlock(&player->lock.status);
}

static
void
cd_LogCallback (int priority, const char* message)
{
    static char priorities[] = { LOG_DEBUG, LOG_NOTICE, LOG_WARNING, LOG_ERR };

    if (CDMainServer) {
        CDString* output = CD_CreateStringFromFormat("%s> %s", CD_ServerToString(CDMainServer), message);

        CDMainServer->logger.log(priorities[priority], CD_StringContent(output));

        CD_DestroyString(output);
    }
    else {
        CDDefaultLogger.log(priorities[priority], message);
    }
}

static
void
cd_Accept (evutil_socket_t listener, short event, CDServer* self)
{
    CDPlayer*               player;
    struct sockaddr_storage storage;
    socklen_t               length = sizeof(storage);
    int                     fd     = accept(listener, (struct sockaddr*) &storage, &length);

    if (fd < 0) {
        SERR(self, "accept error: %s", strerror(-fd));
        return;
    }

    if (getpeername(fd, (struct sockaddr*) &storage, &length) < 0) {
        SERR(self, "could not get peer IP");
        close(fd);
        return;
    }

    player = CD_CreatePlayer(self);

    if (storage.ss_family == AF_INET) {
        evutil_inet_ntop(storage.ss_family, &((struct sockaddr_in*) &storage)->sin_addr, player->ip, sizeof(player->ip));
    }
    else if (storage.ss_family == AF_INET6) {
        evutil_inet_ntop(storage.ss_family, &((struct sockaddr_in6*) &storage)->sin6_addr, player->ip, sizeof(player->ip));
    }
    else {
        SERR(self, "weird address family");
        close(fd);
        CD_DestroyPlayer(player);
    }

    if (self->config->cache.game.players.max > 0) {
        if (CD_HashLength(self->players) >= self->config->cache.game.players.max) {
            SERR(self, "too many clients");
            close(fd);
            CD_DestroyPlayer(player);
            return;
        }
    }

    if (self->config->cache.connection.simultaneous > 0) {
        size_t same = 0;

        CD_MAP_FOREACH(self->entities, it) {
            MCEntity* entity = (MCEntity*) CD_MapIteratorValue(it);

            if (entity->type != MCEntityPlayer) {
                continue;
            }

            CDPlayer* tmp = (CDPlayer*) entity;

            if (CD_CStringIsEqual(tmp->ip, player->ip)) {
                same++;
            }

            if (same >= self->config->cache.connection.simultaneous) {
                CD_MAP_BREAK(self->entities);
            }
        }

        if (same >= self->config->cache.connection.simultaneous) {
            SERR(self, "too many connections from %s", player->ip);
            close(fd);
            CD_DestroyPlayer(player);
            return;
        }
    }

    player->socket = fd;
    evutil_make_socket_nonblocking(player->socket);

    player->buffers = CD_WrapBuffers(bufferevent_socket_new(self->event.base, player->socket, BEV_OPT_CLOSE_ON_FREE | BEV_OPT_THREADSAFE));

    bufferevent_setcb(player->buffers->raw, (bufferevent_data_cb) cd_ReadCallback, NULL, (bufferevent_event_cb) cd_ErrorCallback, player);
    bufferevent_enable(player->buffers->raw, EV_READ | EV_WRITE);

    CD_MapPut(self->entities, player->entity.id, (CDPointer) player);

    CD_AddJob(self->workers, CD_CreateJob(CDPlayerConnectJob, (CDPointer) player));
}

bool
CD_RunServer (CDServer* self)
{
    event_set_mem_functions(CD_malloc, CD_realloc, CD_free);
    event_set_log_callback(cd_LogCallback);

    if ((self->event.base = event_base_new()) == NULL) {
        SERR(self, "could not create MC libevent base!");

        return false;
    }

    event_add(evsignal_new(self->event.base, SIGINT, (event_callback_fn) cd_HandleSignal, self), NULL);

    if ((self->socket = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        SERR(self, "could not create socket: %s", strerror(-self->socket));

        return false;
    }

    evutil_make_socket_nonblocking(self->socket);

    #ifndef WIN32
    CD_DO {
        int one = 1;
        setsockopt(self->socket, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
    }
    #endif

    if ((ERROR(self) = bind(self->socket, (struct sockaddr*) &self->config->cache.connection.bind.ipv4, sizeof(self->config->cache.connection.bind.ipv4))) < 0) {
        SERR(self, "cannot bind: %s", strerror(ABS(ERROR(self))));
        return false;
    }

    if ((ERROR(self) = listen(self->socket, self->config->cache.connection.backlog)) < 0) {
        SERR(self, "listen error: %s", strerror(ABS(ERROR(self))));
        return false;
    }

    SLOG(self, LOG_INFO, "server listening on port %d (%s gameplay)", self->config->cache.connection.port,
        self->config->cache.game.standard ? "standard" : "custom");

    if (self->config->cache.game.players.max > 0) {
        SLOG(self, LOG_INFO, "server can host max %d players", self->config->cache.game.players.max);
    }

    CD_free(CD_SpawnWorkers(self->workers, self->config->cache.workers));

    // Start the TimeLoop for timed events
    pthread_create(&self->timeloop->thread, &self->timeloop->attributes, (void *(*)(void *)) CD_RunTimeLoop, self->timeloop);

    self->event.listener = event_new(self->event.base, self->socket, EV_READ | EV_PERSIST, (event_callback_fn) cd_Accept, self);

    event_add(self->event.listener, NULL);

    CD_LoadPlugins(self->plugins);

    self->running = true;

    while (self->running) {
        event_base_loop(self->event.base, 0);

        CD_ServerCleanDisconnects(self);
    }

    return true;
}

bool
CD_StopServer (CDServer* self)
{
    self->running = false;

    CD_ServerFlush(self, true);

    return true;
}

void
CD_ServerFlush (CDServer* self, bool now)
{
    if (now) {
        event_base_loopbreak(self->event.base);
    }
    else {
        struct timeval interval = { 0, 0 };

        event_base_loopexit(self->event.base, &interval);
    }
}

void
CD_ServerCleanDisconnects (CDServer* self)
{
    if (CD_ListLength(self->disconnecting) > 0) {
        CD_LIST_FOREACH(self->disconnecting, it) {
            CDPlayer* player = (CDPlayer*) CD_ListIteratorValue(it);

            CD_MapDelete(self->entities, player->entity.id);

            if (player->username) {
                CD_HashDelete(self->players, CD_StringContent(player->username));
            }

            CD_DestroyPlayer(player);
        }

        CD_free(CD_ListClear(self->disconnecting));
    }
}

void
CD_ReadFromPlayer (CDPlayer* player)
{
    bufferevent_lock(player->buffers->raw);
    cd_ReadCallback(player->buffers->raw, player);
    bufferevent_unlock(player->buffers->raw);
}

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

void
CD_ServerKick (CDServer* self, CDPlayer* player, CDString* reason)
{
    assert(self);
    assert(player);

    pthread_rwlock_wrlock(&player->lock.status);

    if (reason == NULL) {
        reason = CD_CreateStringFromCString("No reason");
    }

    SLOG(self, LOG_NOTICE, "%s (%s) kicked: %s", CD_StringContent(player->username),
        player->ip, CD_StringContent(reason));

    player->status = CDPlayerDisconnect;

    CD_DO {
        CDPacketDisconnect pkt = {
            .response = {
                .reason = reason
            }
        };

        CDPacket response = { CDResponse, CDDisconnect, (CDPointer) &pkt };

        CD_PlayerSendPacketAndCleanData(player, &response);
    }

    CD_AddJob(player->server->workers, CD_CreateJob(CDPlayerDisconnectJob, (CDPointer) player));

    pthread_rwlock_unlock(&player->lock.status);
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

        CD_HASH_FOREACH(self->players, it) {
            CDPlayer* player = (CDPlayer*) CD_HashIteratorValue(it);

            pthread_rwlock_rdlock(&player->lock.status);
            if (player->status != CDPlayerDisconnect) {
                CD_PlayerSendBuffer(player, buffer);
            }
            pthread_rwlock_unlock(&player->lock.status);
        }

        CD_DestroyBuffer(buffer);
        CD_DestroyPacketData(&response);
    }
}
