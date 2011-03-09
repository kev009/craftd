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

#include <craftd/TimeLoop.h>
#include <craftd/Logger.h>

static
void
cd_KeepTimeLoopAlive (void)
{
    return;
}

CDTimeLoop*
CD_CreateTimeLoop (struct _CDServer* server)
{
    CDTimeLoop* self = CD_malloc(sizeof(CDTimeLoop));

    if (!self) {
        return NULL;
    }

    pthread_spin_init(&self->lock.last, PTHREAD_PROCESS_PRIVATE);

    pthread_attr_init(&self->attributes);
    pthread_attr_setdetachstate(&self->attributes, PTHREAD_CREATE_DETACHED);

    self->server     = server;
    self->running    = false;
    self->event.base = event_base_new();
    self->callbacks  = CD_CreateMap();
    self->last       = INT_MIN;

    CD_SetInterval(self, 10000000, (event_callback_fn) cd_KeepTimeLoopAlive);

    return self;
}

void
CD_DestroyTimeLoop (CDTimeLoop* self)
{
    CD_StopTimeLoop(self);

    event_base_free(self->event.base);

    CD_DestroyMap(self->callbacks);

    pthread_spin_destroy(&self->lock.last);

    CD_free(self);
}

bool
CD_RunTimeLoop (CDTimeLoop* self)
{
    return event_base_loop(self->event.base, 0);
}

bool
CD_StopTimeLoop (CDTimeLoop* self)
{
    struct timeval interval = { 0, 0 };

    return event_base_loopexit(self->event.base, &interval);
}

int
CD_SetTimeout (CDTimeLoop* self, float seconds, event_callback_fn callback)
{
    struct timeval timeout      = { (int) seconds, (seconds - (int) seconds) * 1000000 };
    struct event*  timeoutEvent = event_new(self->event.base, -1, 0, callback, self->server);
    int            result;

    if (evtimer_add(timeoutEvent, &timeout) < 0) {
        SERR(self->server, "could not add a timeout");
        event_free(timeoutEvent);
        return 0;
    }

    pthread_spin_lock(&self->lock.last);
    if ((self->last + 1) == 0) {
        self->last++;
    }

    CD_MapPut(self->callbacks, (result = self->last++), (CDPointer) timeoutEvent);
    pthread_spin_unlock(&self->lock.last);

    return result;
}

void
CD_ClearTimeout (CDTimeLoop* self, int id)
{
    struct event* clear = (struct event*) CD_MapDelete(self->callbacks, id);

    if (clear) {
        evtimer_del(clear);
        event_free(clear);
    }
}

int
CD_SetInterval (CDTimeLoop* self, float seconds, event_callback_fn callback)
{
    struct timeval interval      = { (int) seconds, (seconds - (int) seconds) * 1000000 };
    struct event*  intervalEvent = event_new(self->event.base, -1, EV_PERSIST, callback, self->server);
    int            result;

    if (evtimer_add(intervalEvent, &interval) < 0) {
        SERR(self->server, "could not add an interval");
        event_free(intervalEvent);
        return 0;
    }

    pthread_spin_lock(&self->lock.last);
    if ((self->last + 1) == 0) {
        self->last++;
    }

    CD_MapPut(self->callbacks, (result = self->last++), (CDPointer) intervalEvent);
    pthread_spin_unlock(&self->lock.last);

    return result;
}

void
CD_ClearInterval (CDTimeLoop* self, int id)
{
    struct event* clear = (struct event*) CD_MapDelete(self->callbacks, id);

    if (clear) {
        evtimer_del(clear);
        event_free(clear);
    }
}
