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

#include <craftd/Config.h>
#include <craftd/Logger.h>

// TODO: Implement real parsing of the json file
CDConfig*
CD_ParseConfig (const char* path)
{
    CDConfig* self = CD_malloc(sizeof(CDConfig));

    if (!self) {
        return NULL;
    }

    self->data = json_load_file(path, 0, &self->error);

    if (!self->data) {
        ERR("[Config.parse] error on line %d: %s", self->error.line, self->error.text);

        CD_DestroyConfig(self);
        return NULL;
    }

    self->cache.daemonize = true;

    self->cache.connection.port    = 25565;
    self->cache.connection.backlog = 42;

    self->cache.connection.bind.ipv4.sin_family      = AF_INET;
    self->cache.connection.bind.ipv4.sin_addr.s_addr = INADDR_ANY;
    self->cache.connection.bind.ipv4.sin_port        = htons(self->cache.connection.port);

    self->cache.connection.bind.ipv6.sin6_family = AF_INET6;
    self->cache.connection.bind.ipv6.sin6_addr   = in6addr_any;
    self->cache.connection.bind.ipv6.sin6_port   = htons(self->cache.connection.port);

    self->cache.httpd.enabled         = true;
    self->cache.httpd.connection.port = 25566;

    self->cache.httpd.connection.bind.ipv4.sin_family      = AF_INET;
    self->cache.httpd.connection.bind.ipv4.sin_addr.s_addr = INADDR_ANY;
    self->cache.httpd.connection.bind.ipv4.sin_port        = htons(self->cache.httpd.connection.port);

    self->cache.httpd.connection.bind.ipv6.sin6_family = AF_INET6;
    self->cache.httpd.connection.bind.ipv6.sin6_addr   = in6addr_any;
    self->cache.httpd.connection.bind.ipv6.sin6_port   = htons(self->cache.httpd.connection.port);

    self->cache.rate.sunrise = 20;
    self->cache.rate.day     = 20;
    self->cache.rate.sunset  = 20;
    self->cache.rate.night   = 20;

    self->cache.spawn.x = 0;
    self->cache.spawn.y = 0;
    self->cache.spawn.z = 0;

    self->cache.workers = 2;

    self->cache.maxPlayers = 0;

    return self;
}

void
CD_DestroyConfig (CDConfig* self)
{
    if (self->data) {
        json_decref(self->data);
    }

    CD_free(self);
}

bool
CD_ConfigParseBool (const json_t* json, const char* key)
{
    json_t* obj = json_object_get(json, key);

    return (obj && json_is_true(obj));
}
