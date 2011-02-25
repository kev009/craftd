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

#include "common.h"
#include "TimeLoop.h"

CDTimeLoop*
CD_CreateTimeLoop (struct _CDServer* server)
{
    CDTimeLoop* self = CD_malloc(sizeof(CDTimeLoop));

    if (!self) {
        return NULL;
    }

    pthread_spin_init(&self->lock.last, PTHREAD_PROCESS_PRIVATE);

    self->server     = server;
    self->running    = false;
    self->event.base = event_base_new();
    self->callbacks  = CD_CreateMap();
    self->last       = INT_MIN;
}

void
CD_DestroyTimeLoop (CDTimeLoop* self)
{
    event_base_free(self->event.base);

    CD_DestroyMap(self->callbacks);

    pthread_spin_destroy(&self->lock.last);

    CD_free(self);
}

void*
CD_RunTimeLoop (void* arg)
{
    CDTimeLoop* self = arg;

    return event_base_dispatch(self->event.base);
}

int
CD_SetTimeout (CDTimeLoop* self, float seconds, event_callback_fn callback)
{
    struct timeval timeout      = { (int) seconds, (int) seconds * 1000000 };
    struct event*  timeoutEvent = event_new(self->event.base, -1, 0, callback, self->server);
    int            result;

    pthread_spin_lock(&self->lock.last);
    CD_MapSet(self->callbacks, (result = self->last++), timeoutEvent);
    pthread_spin_unlock(&self->lock.last);

    evtimer_add(timeoutEvent, &timeout);

    return result;
}

void
CD_ClearTimeout (CDTimeLoop* self, int id)
{
    struct event* clear = CD_MapDelete(self->callbacks, id);

    if (clear) {
        evtimer_del(clear);
        event_free(clear);
    }
}

int
CD_SetInterval (CDTimeLoop* self, float seconds, event_callback_fn callback)
{
    struct timeval interval      = { (int) seconds, (int) seconds * 1000000 };
    struct event*  intervalEvent = event_new(self->event.base, -1, EV_PERSIST, callback, self->server);
    int            result;

    pthread_spin_lock(&self->lock.last);
    CD_MapSet(self->callbacks, (result = self->last++), intervalEvent);
    pthread_spin_unlock(&self->lock.last);

    evtimer_add(intervalEvent, &interval);

    return result;
}

void
CD_ClearInterval (CDTimeLoop* self, int id)
{
    struct event* clear = CD_MapDelete(self->callbacks, id);

    if (clear) {
        evtimer_del(clear);
        event_free(clear);
    }
}
