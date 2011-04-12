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

    self->name = NULL;

    self->logger = CDConsoleLogger;
    self->config = CD_ParseConfig(path);

    if (!self->config) {
        CD_free(self);

        return NULL;
    }

    self->event.callbacks = CD_CreateHash();
    self->event.provided  = CD_CreateHash();

    self->protocol = NULL;

    self->timeloop         = CD_CreateTimeLoop(self);
    self->workers          = CD_CreateWorkers(self);
    self->plugins          = CD_CreatePlugins(self);
    self->scriptingEngines = CD_CreateScriptingEngines(self);

    self->clients       = CD_CreateList();
    self->disconnecting = CD_CreateList();

    self->running = false;

    DYNAMIC(self) = CD_CreateDynamic();
    ERROR(self)   = CDNull;

    CD_EventProvides(self, "Server.create",     CD_CreateEventParameters(NULL));
    CD_EventProvides(self, "Server.start!",     CD_CreateEventParameters(NULL));
    CD_EventProvides(self, "Client.connect",    CD_CreateEventParameters("CDClient", NULL));
    CD_EventProvides(self, "Client.kick",       CD_CreateEventParameters("CDClient", "CDString", NULL));
    CD_EventProvides(self, "Client.disconnect", CD_CreateEventParameters("CDClient", "bool", NULL));
    CD_EventProvides(self, "Client.destroy",    CD_CreateEventParameters("CDClient", NULL));
    CD_EventProvides(self, "Server.stop!",      CD_CreateEventParameters(NULL));
    CD_EventProvides(self, "Server.destroy",    CD_CreateEventParameters(NULL));

    CD_EventDispatch(self, "Server.create");

    return self;
}

void
CD_DestroyServer (CDServer* self)
{
    assert(self);

    CD_EventDispatch(self, "Server.destroy");

    CD_StopTimeLoop(self->timeloop);

    CD_LIST_FOREACH(self->clients, it) {
        CD_ServerKick(self, (CDClient*) CD_ListIteratorValue(it), CD_CreateStringFromCString("shutting down"));
    }

    if (self->plugins) {
        CD_DestroyPlugins(self->plugins);
    }

    if (self->scriptingEngines) {
        CD_DestroyScriptingEngines(self->scriptingEngines);
    }

    CD_DestroyTimeLoop(self->timeloop);

    CD_StopServer(self);

    if (self->workers) {
        CD_DestroyWorkers(self->workers);
    }

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

    CD_DestroyHash(self->event.callbacks);

    CD_HASH_FOREACH(self->event.provided, it) {
        CD_DestroyEventParameters((CDList*) CD_HashIteratorValue(it));
    }

    CD_DestroyHash(self->event.provided);

    if (DYNAMIC(self)) {
        CD_DestroyDynamic(DYNAMIC(self));
    }

    if (self->name) {
        CD_free(self->name);
    }

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

static
void
cd_ReadCallback (struct bufferevent* event, CDClient* client)
{
    assert(client);

    CDServer* self = client->server;

    if (!self->protocol) {
      return;
    }

    pthread_rwlock_wrlock(&client->lock.status);

    SDEBUG(self, "read data from %s, %d byte/s available", client->ip, CD_BufferLength(client->buffers->input));

    if (client->status == CDClientIdle) {
        void* packet;

        if (self->protocol->parsable(client->buffers)) {
            if ((packet = self->protocol->parse(client->buffers))) {
                CD_BufferReadIn(client->buffers, CDNull, CDNull);

                client->status = CDClientProcess;
                client->jobs++;

                CD_AddJob(self->workers, CD_CreateJob(CDClientProcessJob,
                    (CDPointer) CD_CreateClientProcessJob(client, packet)));
            }
        }
        else {
            if (errno == EILSEQ) {
                CD_ServerKick(self, client, CD_CreateStringFromCString("bad packet"));
            }
        }
    }

    pthread_rwlock_unlock(&client->lock.status);
}

static
void
cd_ErrorCallback (struct bufferevent* event, short error, CDClient* client)
{
    assert(client);

    if (!((error & BEV_EVENT_EOF) || (error & BEV_EVENT_ERROR) || (error & BEV_EVENT_TIMEOUT))) {
        return;
    }

    pthread_rwlock_wrlock(&client->lock.status);

    client->status = CDClientDisconnect;

    CDServer* self = client->server;

    if (error & BEV_EVENT_ERROR) {
        SLOG(self, LOG_INFO, "libevent: ip %s - %s", client->ip, evutil_socket_error_to_string(EVUTIL_SOCKET_ERROR()));
    }
    else if (error & BEV_EVENT_TIMEOUT) {
        SERR(self, "A bufferevent timeout?");
    }

    ERROR(client) = error;

    SLOG(self, LOG_INFO, "%s[%p] errored/disconnected", client->ip, client);

    CD_AddJob(client->server->workers, CD_CreateExternalJob(CDClientDisconnectJob, (CDPointer) client));

    pthread_rwlock_unlock(&client->lock.status);
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
    CDClient*               client;
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

    client = CD_CreateClient(self);

    if (storage.ss_family == AF_INET) {
        evutil_inet_ntop(storage.ss_family, &((struct sockaddr_in*) &storage)->sin_addr, client->ip, sizeof(client->ip));
    }
    else if (storage.ss_family == AF_INET6) {
        evutil_inet_ntop(storage.ss_family, &((struct sockaddr_in6*) &storage)->sin6_addr, client->ip, sizeof(client->ip));
    }
    else {
        SERR(self, "weird address family");
        close(fd);
        CD_DestroyClient(client);
    }

    if (self->config->cache.game.clients.max > 0) {
        if (CD_ListLength(self->clients) >= self->config->cache.game.clients.max) {
            SERR(self, "too many clients");
            close(fd);
            CD_DestroyClient(client);
            return;
        }
    }

    if (self->config->cache.game.clients.simultaneous > 0) {
        size_t same = 0;

        CD_LIST_FOREACH(self->clients, it) {
            CDClient* tmp = (CDClient*) CD_ListIteratorValue(it);

            if (CD_CStringIsEqual(tmp->ip, client->ip)) {
                same++;
            }

            if (same >= self->config->cache.game.clients.simultaneous) {
                CD_LIST_BREAK(self->clients);
            }
        }

        if (same >= self->config->cache.game.clients.simultaneous) {
            SERR(self, "too many connections from %s", client->ip);
            close(fd);
            CD_DestroyClient(client);
            return;
        }
    }

    client->socket = fd;
    evutil_make_socket_nonblocking(client->socket);

    client->buffers = CD_WrapBuffers(bufferevent_socket_new(self->event.base, client->socket, BEV_OPT_CLOSE_ON_FREE | BEV_OPT_THREADSAFE));

    bufferevent_setcb(client->buffers->raw, (bufferevent_data_cb) cd_ReadCallback, NULL, (bufferevent_event_cb) cd_ErrorCallback, client);
    bufferevent_enable(client->buffers->raw, EV_READ | EV_WRITE);

    CD_ListPush(self->clients, (CDPointer) client);

    CD_AddJob(self->workers, CD_CreateExternalJob(CDClientConnectJob, (CDPointer) client));
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
    DO {
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
        self->config->cache.game.protocol.standard ? "standard" : "custom");

    if (self->config->cache.game.clients.max > 0) {
        SLOG(self, LOG_INFO, "server can host max %d clients", self->config->cache.game.clients.max);
    }

    CD_free(CD_SpawnWorkers(self->workers, self->config->cache.workers));

    // Start the TimeLoop for timed events
    pthread_create(&self->timeloop->thread, &self->timeloop->attributes, (void *(*)(void *)) CD_RunTimeLoop, self->timeloop);

    self->event.listener = event_new(self->event.base, self->socket, EV_READ | EV_PERSIST, (event_callback_fn) cd_Accept, self);

    event_add(self->event.listener, NULL);

    CD_LoadPlugins(self->plugins);
    CD_LoadScriptingEngines(self->scriptingEngines);

    CD_EventDispatch(self, "Server.start!");

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
    if (!self->running) {
        return true;
    }

    self->running = false;

    CD_ServerFlush(self, true);

    CD_EventDispatch(self, "Server.stop!");

    CD_StopWorkers(self->workers);

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
            CDClient* client = (CDClient*) CD_ListDelete(self->clients, CD_ListIteratorValue(it));

            if (client) {
                CD_DestroyClient(client);
            }
        }

        CD_free(CD_ListClear(self->disconnecting));
    }
}

void
CD_ReadFromClient (CDClient* client)
{
    bufferevent_lock(client->buffers->raw);
    cd_ReadCallback(client->buffers->raw, client);
    bufferevent_unlock(client->buffers->raw);
}

void
CD_ServerKick (CDServer* self, CDClient* client, CDString* reason)
{
    assert(self);
    assert(client);

    pthread_rwlock_wrlock(&client->lock.status);

    if (client->status == CDClientDisconnect) {
        pthread_rwlock_unlock(&client->lock.status);

        return;
    }

    if (reason == NULL) {
        reason = CD_CreateStringFromCString("No reason");
    }

    SLOG(self, LOG_INFO, "%s kicked: %s", client->ip, CD_StringContent(reason));

    CD_EventDispatch(self, "Client.kick", client, reason);

    client->status = CDClientDisconnect;

    CD_AddJob(client->server->workers, CD_CreateExternalJob(CDClientDisconnectJob, (CDPointer) client));

    pthread_rwlock_unlock(&client->lock.status);
}
