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

#ifndef CRAFTD_TIMELOOP_H
#define CRAFTD_TIMELOOP_H

#include "Map.h"

struct _CDServer;

typedef struct _CDTimeLoop {
    struct _CDServer* server;

    int    last;
    CDMap* callbacks;

    struct {
        struct event_base* base;
    } event;

    struct {
        pthread_spinlock_t last;
    } lock;
} CDTimeLoop;

CDTimeLoop* CD_CreateTimeLoop (struct _CDServer* server);

void CD_DestroyTimeLoop (CDTimeLoop* self);

void* CD_RunTimeLoop (void* arg);

int CD_SetTimeout (CDTimeLoop* self, float seconds, event_callback_fn callback);

void CD_ClearTimeout (CDTimeLoop* self, float seconds, int id);

int CD_SetInterval (CDTimeLoop* self, float seconds, event_callback_fn callback);

void CD_ClearInterval (CDTimeLoop* self, float seconds, int id);

#endif
