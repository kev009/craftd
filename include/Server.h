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

#ifndef CRAFTD_SERVER_H
#define CRAFTD_SERVER_H

#include "Config.h"
#include "Workers.h"
#include "Plugins.h"
#include "Logger.h"
#include "Players.h"

#include "List.h"

typedef struct _CDServer {
    char* name;

    CDWorkers* workers;
    CDConfig*  config;
    CDPlugins* plugins;
    CDPlayers* players;
    CDLogger   logger;

    struct {
        struct event_base* base;
        struct event*      listener;


        CDHash* callbacks;
    } event;

    struct {
        pthread
    } lock;

    evutil_socket_t socket;

    CDHash* _private;
} CDServer;

CDServer* CD_CreateServer (const char* path);

void CD_DestroyServer (CDServer* self);

char* CD_ServerToString (CDServer* self);

void CD_RunServer (CDServer* self);

typedef void (*CDEventCallback)();

bool cd_EventBeforeDispatch (CDServer* self, const char* eventName, ...);

bool cd_EventAfterDispatch (CDServer* self, const char* eventName, ...);

#define CD_EventDispatch (server, eventName, ...)                                           \
    do {                                                                                    \
        if (!cd_EventBeforeDispatch(server, eventName, __VA_ARGS__)) {                      \
            break;                                                                          \
        }                                                                                   \
                                                                                            \
        CDList*        callbacks = CD_HashGet(server->events, eventName);                   \
        CDListIterator it;                                                                  \
                                                                                            \
        if (callbacks) {                                                                    \
            for (it = CD_ListBegin(self); it != CD_ListEnd(self); it = CD_ListNext(it)) {   \
                if (!((CDEventCallback*) CD_ListIteratorValue(it))(self, __VA_ARGS__)) {    \
                    break;                                                                  \
                }                                                                           \
            }                                                                               \
        }                                                                                   \
                                                                                            \
        cd_EventAfterDispatch(server, eventName, __VA_ARGS__);                              \
    } while (0)

void CD_EventRegister (CDServer* server, const char* eventName, CDEventCallback callback);

void CD_EventUnregister (CDServer* server, const char* eventName, CDEventCallback callback);

#ifndef CRAFTD_SERVER_IGNORE_EXTERN
extern CDServer* CDMainServer;
#endif

#endif
