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

#ifndef CRAFTD_WORKERS_H
#define CRAFTD_WORKERS_H

#include <craftd/common.h>
#include <craftd/Worker.h>

#define CD_THREAD_STACK 8388608

struct _CDServer;

typedef struct _CDWorkers {
    struct _CDServer* server;

    int64_t last;

    size_t     length;
    CDWorker** item;

    CDList* jobs;

    pthread_attr_t attributes;

    struct {
        pthread_cond_t  condition;
        pthread_mutex_t mutex;
    } lock;
} CDWorkers;

CDWorkers* CD_CreateWorkers (struct _CDServer* server);

void CD_DestroyWorkers (CDWorkers* self);

CDWorker** CD_SpawnWorkers (CDWorkers* self, size_t number);

CDWorkers* CD_ConcatWorkers (CDWorkers* self, CDWorker** workers, size_t number);

CDWorkers* CD_AppendWorker (CDWorkers* self, CDWorker* worker);

bool CD_HasJobs (CDWorkers* self);

void CD_AddJob (CDWorkers* self, CDJob* job);

CDJob* CD_NextJob (CDWorkers* self);

#endif
