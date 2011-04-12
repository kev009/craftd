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

CDConfig*
CD_ParseConfig (const char* path)
{
    CDConfig* self = CD_malloc(sizeof(CDConfig));

    config_init(&self->data);

    if (config_read_file(&self->data, path) != CONFIG_TRUE) {
        ERR("error on line %d while parsing %s: %s", config_error_line(&self->data), path, config_error_text(&self->data));

        CD_DestroyConfig(self);

        return NULL;
    }

    self->cache.daemonize = true;

    self->cache.connection.port    = 25565;
    self->cache.connection.backlog = 16;

    self->cache.connection.bind.ipv4.sin_family      = AF_INET;
    self->cache.connection.bind.ipv4.sin_addr.s_addr = INADDR_ANY;
    self->cache.connection.bind.ipv4.sin_port        = htons(self->cache.connection.port);

    self->cache.connection.bind.ipv6.sin6_family = AF_INET6;
    self->cache.connection.bind.ipv6.sin6_addr   = in6addr_any;
    self->cache.connection.bind.ipv6.sin6_port   = htons(self->cache.connection.port);

    self->cache.files.motd = "/etc/craftd/motd.conf";

    self->cache.workers = 2;

    self->cache.game.protocol.standard    = true;
    self->cache.game.clients.max          = 0;
    self->cache.game.clients.simultaneous = 3;

    C_IN(server, C_ROOT(self), "server") {
        C_SAVE(C_GET(server, "daemonize"), C_BOOL, self->cache.daemonize);

        C_SAVE(C_GET(server, "workers"), C_INT, self->cache.workers);

        C_IN(connection, server, "connection") {
            C_SAVE(C_GET(connection, "port"),    C_INT, self->cache.connection.port);
            C_SAVE(C_GET(connection, "backlog"), C_INT, self->cache.connection.backlog);

            self->cache.connection.bind.ipv4.sin_port  = htons(self->cache.connection.port);
            self->cache.connection.bind.ipv6.sin6_port = htons(self->cache.connection.port);

            C_IN(bind, connection, "bind") {
                if (C_GET(bind, "ipv4")) {
                    if (evutil_inet_pton(AF_INET, C_TO_STRING(C_GET(bind, "ipv4")), &self->cache.connection.bind.ipv4.sin_addr) != 1) {
                        self->cache.connection.bind.ipv4.sin_addr.s_addr = INADDR_ANY;
                    }
                }

                if (C_GET(bind, "ipv6")) {
                    if (evutil_inet_pton(AF_INET6, C_TO_STRING(C_GET(bind, "ipv6")), &self->cache.connection.bind.ipv6.sin6_addr) != 1) {
                        self->cache.connection.bind.ipv6.sin6_addr = in6addr_any;
                    }
                }
            }
        }

        C_IN(game, server, "game") {
            C_IN(clients, game, "clients") {
                C_SAVE(C_GET(clients, "max"),          C_INT, self->cache.game.clients.max);
                C_SAVE(C_GET(clients, "simultaneous"), C_INT, self->cache.game.clients.simultaneous);
            }

            C_SAVE(C_GET(game, "standard"), C_BOOL, self->cache.game.protocol.standard);
        }

        C_IN(files, server, "files") {
            C_SAVE(C_GET(files, "motd"), C_STRING, self->cache.files.motd);
        }
    }

    return self;
}

void
CD_DestroyConfig (CDConfig* self)
{
    config_destroy(&self->data);

    CD_free(self);
}
