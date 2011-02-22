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
#include "Server.h"
#undef CRAFTD_SERVER_IGNORE_EXTERN

CDServer* CDMainServer = NULL;

CDServer*
CD_CreateServer (const char* path)
{
    CDServer* self = CD_malloc(sizeof(CDServer));

    if (!self) {
        return NULL;
    }

    self->logger = CDConsoleLogger;

    self->config  = CD_ParseConfig(path);
    self->workers = CD_CreateWorkers(self);
    self->plugins = CD_CreatePlugins(self)

    if (!self->config || !self->workers) {
        CD_DestroyServer(self)
    }

/*
    size_t i;

    for (i = 0; i < self->config->plugins->length) {
        CD_LoadPlugin(self->plugins, self->config->plugins->item[i]);
    }
*/

    self->_private = CD_CreatePrivateData();

    return self;
}

void
CD_DestroyServer (CDServer* self)
{
    CD_DestroyConfig(self->config)
    CD_DestroyWorkers(self->workers)
    CD_DestroyPlugins(self->plugins);

    CD_DestroyPrivateData(self->_private);

    CD_free(self->name);
}

char*
CD_ServerToString (CDServer* self)
{
    return self->name;
}

static
void
cd_Accept (evutil_socket_t listener, short event, void* arg)
{
    struct sockaddr_storage storage;
    CDPlayer*               player;
    CDServer*               self   = arg;

    if (self->players->length >= self->config->max_players) {
        SERR(self, "too many clients");
        return;
    }

    socklen_t length = sizeof(storage);
    int       fd     = accept(listener, (struct sockaddr*) &storage, &length);

    if (fd < 0) {
        SERR(self, "accept error");
        return;
    }

  else if (fd > FD_SETSIZE)
  {
    LOG(LOG_CRIT, "too many clients");
    close(fd);
  }
}

bool
CD_RunServer (CDServer* self)
{
    if ((self->event.base = event_base_new()) == NULL) {
        ERR("could not create MC libevent base!");

        return false;
    }

    if ((self->socket = socket(PF_INET, SOCK_STREAM, 0)) < 0) {
        ERR("could not create socket!");

        return false;
    }

    evutil_make_socket_nonblocking(listener);

    #ifndef WIN32
    {
        int one = 1;
        setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
    }
    #endif

    self->event.listener = event_new(self->event.base, self->socket, EV_READ | EV_PERSIST, cd_Accept, self);

    event_add(self->event.listener, NULL);
    
    return event_base_dispatch(self->event.base) != 0;
}
