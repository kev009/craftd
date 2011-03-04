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

CDServer* CDMainServer = NULL;

CDServer*
CD_CreateServer (const char* path)
{
    CDServer* self = CD_malloc(sizeof(CDServer));

    if (!self) {
        return NULL;
    }

    pthread_spin_init(&self->lock.time, PTHREAD_PROCESS_PRIVATE);

    self->name     = NULL;
    self->logger   = CDConsoleLogger;
    self->timeloop = CD_CreateTimeLoop(self);
    self->config   = CD_ParseConfig(path);
    self->workers  = CD_CreateWorkers(self);
    self->plugins  = CD_CreatePlugins(self);

    if (!self->config || !self->workers || !self->timeloop || !self->plugins) {
        if (!self->config) {
            SERR(self, "Config couldn't be initialized");
        }

        if (!self->workers) {
            SERR(self, "Workers couldn't be initialized");
        }

        if (!self->timeloop) {
            SERR(self, "TimeLoop couldn't be initialized");
        }

        if (!self->plugins) {
            SERR(self, "Plugins couldn't be initialized");
        }

        CD_DestroyServer(self);

        return NULL;
    }

    self->entities = CD_CreateMap();
    self->players  = CD_CreateHash();

    self->event.callbacks = CD_CreateHash();

    CD_ServerSetTime(self, 0);

    PRIVATE(self) = CD_CreateHash();

    return self;
}

void
CD_DestroyServer (CDServer* self)
{
    if (self->config) {
        CD_DestroyConfig(self->config);
    }

    if (self->workers) {
        CD_DestroyWorkers(self->workers);
    }

    if (self->plugins) {
        CD_DestroyPlugins(self->plugins);
    }

    CD_HASH_FOREACH(self->players, it) {
        CD_DestroyPlayer((CDPlayer*) CD_HashIteratorValue(self->players, it));
    }

    if (self->event.base) {
        event_base_free(self->event.base);
        self->event.base = NULL;
    }

    if (self->event.listener) {
        event_free(self->event.listener);
        self->event.listener = NULL;
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
    if (!self->name) {
        return "craftd";
    }
    else {
        return self->name;
    }
}

short
CD_ServerGetTime (CDServer* self)
{
    short result;

    pthread_spin_lock(&self->lock.time);
    result = self->time;
    pthread_spin_unlock(&self->lock.time);

    return result;
}

short
CD_ServerSetTime (CDServer* self, short time)
{
    pthread_spin_lock(&self->lock.time);
    self->time = time;
    pthread_spin_unlock(&self->lock.time);

    return time;
}

static
void
cd_ReadCallback (struct bufferevent* event, CDPlayer* player)
{
    pthread_mutex_lock(&player->lock.status);
    pthread_rwlock_wrlock(&player->lock.jobs);

    SDEBUG(player->server, "read data from %s (%s), %d byte/s available", CD_StringContent(player->username), player->ip, evbuffer_get_length(bufferevent_get_input(event)));

    if (player->status == CDPlayerIdle) {
        if (CD_PacketParsable(player->buffers)) {
            CD_BufferReadIn(player->buffers, CDNull, CDNull);

            CDPacket* packet = CD_PacketFromBuffer(player->buffers->input);

            SDEBUG(player->server, "received packet 0x%.2X from %s", packet->type, player->ip);

            packet = (CDPacket*) CD_HashSet(PRIVATE(player), "packet", (CDPointer) packet);

            if (packet) {
                CD_DestroyPacket(packet);
            }

            player->status = CDPlayerProcess;
            player->jobs++;

            CD_AddJob(player->server->workers, CD_CreateJob(CDPlayerProcessJob, (CDPointer) player));
        }
        else {
            if (errno == EILSEQ) {
                CD_ServerKick(player->server, player, "bad packet");
            }
        }
    }

    pthread_rwlock_unlock(&player->lock.jobs);
    pthread_mutex_unlock(&player->lock.status);
}

static
void
cd_ErrorCallback (struct bufferevent* event, short error, CDPlayer* player)
{
    if (!((error & BEV_EVENT_EOF) || (error & BEV_EVENT_ERROR) || (error & BEV_EVENT_TIMEOUT))) {
        return;
    }

    pthread_mutex_lock(&player->lock.status);

    player->status = CDPlayerDisconnect;

    CDServer* self = player->server;

    if (error & BEV_EVENT_ERROR) {
        SLOG(self, LOG_INFO, "libevent: ip %s - %s", player->ip, evutil_socket_error_to_string(EVUTIL_SOCKET_ERROR()));
    }
    else if (error & BEV_EVENT_TIMEOUT) {
        SERR(self, "A bufferevent timeout?");
    }

    SLOG(self, LOG_NOTICE, "%s (%s) disconnected", CD_StringContent(player->username), player->ip);

    CD_AddJob(player->server->workers, CD_CreateJob(CDPlayerDisconnectJob, (CDPointer) player));

    pthread_mutex_unlock(&player->lock.status);
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

    if (self->config->cache.maxPlayers > 0 && CD_HashLength(self->players) >= self->config->cache.maxPlayers) {
        close(fd);
        SERR(self, "too many clients");
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

    player->socket = fd;
    evutil_make_socket_nonblocking(player->socket);

    player->buffers = CD_WrapBuffers(bufferevent_socket_new(self->event.base, player->socket, BEV_OPT_CLOSE_ON_FREE | BEV_OPT_THREADSAFE));

    bufferevent_setcb(player->buffers->raw, (bufferevent_data_cb) cd_ReadCallback, NULL, (bufferevent_event_cb) cd_ErrorCallback, player);
    bufferevent_enable(player->buffers->raw, EV_READ | EV_WRITE);

    CD_MapSet(self->entities, player->entity.id, (CDPointer) player);
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

    if ((self->socket = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        SERR(self, "could not create socket: %s", strerror(-self->socket));

        return false;
    }

    evutil_make_socket_nonblocking(self->socket);

    #ifndef WIN32
    {
        int one = 1;
        setsockopt(self->socket, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
    }
    #endif

    if ((self->error = bind(self->socket, (struct sockaddr*) &self->config->cache.connection.bind.ipv4, sizeof(self->config->cache.connection.bind.ipv4))) < 0) {
        SERR(self, "cannot bind: %s", strerror(ERROR(self)));
        return false;
    }

    if ((self->error = listen(self->socket, self->config->cache.connection.backlog)) < 0) {
        SERR(self, "listen error: %s", strerror(ERROR(self)));
        return false;
    }

    SLOG(self, LOG_INFO, "server listening on port %d", self->config->cache.connection.port);

    if (self->config->cache.maxPlayers > 0) {
        SLOG(self, LOG_INFO, "server can host max %d players", self->config->cache.maxPlayers);
    }

    CD_SpawnWorkers(self->workers, self->config->cache.workers);

    // Start the TimeLoop for timed events
    pthread_create(&self->timeloop->thread, &self->timeloop->attributes, (void *(*)(void *)) CD_RunTimeLoop, self->timeloop);

    self->event.listener = event_new(self->event.base, self->socket, EV_READ | EV_PERSIST, (event_callback_fn) cd_Accept, self);

    event_add(self->event.listener, NULL);

    // TODO: replace this with the plugin load cycle
    CD_LoadPlugin(self->plugins, "libcdbase");
    CD_LoadPlugin(self->plugins, "libcdnbt");
//    CD_LoadPlugin(self->plugins, "libcdtests");

    return event_base_dispatch(self->event.base) != 0;
}

void
CD_ReadFromPlayer (CDServer* self, CDPlayer* player)
{
    cd_ReadCallback(player->buffers->raw, player);
}

void
CD_ServerKick (CDServer* self, CDPlayer* player, const char* reason)
{
    pthread_mutex_lock(&player->lock.status);

    player->status = CDPlayerDisconnect;

    SLOG(self, LOG_NOTICE, "%s (%s) kicked: %s", CD_StringContent(player->username), player->ip, reason);

    CD_PACKET_DO {
        CDPacketDisconnect pkt;
        pkt.response.reason = CD_CreateStringFromCString(reason);

        CDPacket response = { CDResponse, CDDisconnect, (CDPointer) &pkt };

        CD_PlayerSendPacket(player, &response);

        CD_DestroyString(pkt.response.reason);
    }

    CD_AddJob(player->server->workers, CD_CreateJob(CDPlayerDisconnectJob, (CDPointer) player));

    pthread_mutex_unlock(&player->lock.status);
}

// FIXME: This is just a dummy function
MCEntityId
CD_ServerGenerateEntityId (CDServer* self)
{
    MCEntityId result;

    if (CD_MapLength(self->entities) != 0) {
        result = ((MCEntity*) CD_MapLast(self->entities))->id + 1;
    }
    else {
        result = INT_MIN;
    }

    return result;
}

bool
cd_EventBeforeDispatch (CDServer* self, const char* eventName, ...)
{
    CDList* callbacks = (CDList*) CD_HashGet(self->event.callbacks, "Event.dispatch:before");
    va_list ap;

    va_start(ap, eventName);

    CD_LIST_FOREACH(callbacks, it) {
        if (!CD_ListIteratorValue(it)) {
            continue;
        }

        if (!((CDEventCallback) CD_ListIteratorValue(it))(self, eventName, ap)) {
            return false;
        }
    }

    return true;
}

bool
cd_EventAfterDispatch (CDServer* self, const char* eventName, ...)
{
    CDList* callbacks = (CDList*) CD_HashGet(self->event.callbacks, "Event.dispatch:after");
    va_list ap;

    va_start(ap, eventName);

    CD_LIST_FOREACH(callbacks, it) {
        if (!CD_ListIteratorValue(it)) {
            continue;
        }

        if (!((CDEventCallback) CD_ListIteratorValue(it))(self, eventName, ap)) {
            return false;
        }
    }

    return true;
}

void
CD_EventRegister (CDServer* self, const char* eventName, CDEventCallback callback)
{
    CDList* callbacks = (CDList*) CD_HashGet(self->event.callbacks, eventName);

    if (!callbacks) {
        callbacks = CD_CreateList();
    }

    CD_ListPush(callbacks, (CDPointer) callback);

    CD_HashSet(self->event.callbacks, eventName, (CDPointer) callbacks);
}

CDEventCallback*
CD_EventUnregister (CDServer* self, const char* eventName, CDEventCallback callback)
{
    CDList*          callbacks = (CDList*) CD_HashGet(self->event.callbacks, eventName);
    CDEventCallback* result    = NULL;

    if (!callbacks) {
        return NULL;
    }

    if (callback) {
        result    = CD_calloc(2, sizeof(CDEventCallback));
        result[0] = (CDEventCallback) CD_ListDeleteAll(callbacks, (CDPointer) callback);
    }
    else {
        result = (CDEventCallback*) CD_ListClear(callbacks);
    }

    if (CD_ListLength(callbacks) == 0) {
        CD_DestroyList(callbacks);
    }

    return result;
}
