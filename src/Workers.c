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

#include "Workers.h"
#include "Server.h"
#include "common.h"

CDWorkers*
CD_CreateWorkers (CDServer* server)
{
    CDWorkers* object = CD_malloc(sizeof(CDWorkers));

    if (!object) {
        return NULL;
    }

    object->server = server;
    object->length = 0;
    object->item   = NULL;

    pthread_attr_init(&object->attributes);
    pthread_attr_setdetachstate(&object->attributes, PTHREAD_CREATE_DETACHED);

    pthread_mutex_init(&object->mutex, NULL);
    
    if (pthread_cond_init(&object->condition, NULL) != 0) {
        return CD_DestroyWorkers(object);
    }

    return object;
}

void
CD_DestroyWorkers (CDWorkers* object)
{
    size_t i;

    for (i = 0; i < object->length; i++) {
        CD_DestroyWorker(object->item[i]);
    }

    CD_free(object->item);
    CD_free(object);
}

CDWorker**
CD_SpawnWorkers (CDWorkers* workers, size_t number)
{
    CDWorker** result = CD_malloc(sizeof(CDWorker*) * number);
    size_t i;

    for (i = 0; i < number; i++) {
        result[i]     = CD_CreateWorker();
        result[i]->id = workers->length + i;
        
        if (pthread_create(&result[i]->thread, &workers->attributes, CD_RunWorker, result[i]) != 0) {
            ERR("Worker pool startup failed!");
        }
    }

    CD_ConcatWorkers(workers, result, number)

    return result;
}

CDWorkers*
CD_ConcatWorkers (CDWorkers* workers, CDWorker** items, size_t length)
{
    size_t i;

    for (i = 0; i < length; i++) {
        CD_AppendWorker(workers, items[i]);
    }

    return workers;
}

CDWorkers*
CD_AppendWorker (CDWorkers* workers, CDWorker* worker)
{
    workers->item = CD_realloc(workers->item, sizeof(CDWorker*) * ++workers->length);

    workers->item[workers->length - 1] = worker;

    return workers;
}
