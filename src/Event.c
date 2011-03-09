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

#include <craftd/Event.h>

static
int8_t
cd_EventIsEqual (CDEventCallbackFunction a, CDEventCallback* b)
{
    if (a == b->function) {
        return 0;
    }

    return 1;
}

int8_t
cd_EventCompare (CDEventCallback* a, CDEventCallback* b)
{
    if (a->priority > b->priority) {
        return 1;
    }
    else if (a->priority < b->priority) {
        return -1;
    }
    else {
        return 0;
    }
}

CDEventCallback*
CD_CreateEventCallback (CDEventCallbackFunction function, int priority)
{
    CDEventCallback* self = CD_malloc(sizeof(CDEventCallback));

    assert(self);

    self->function = function;
    self->priority = priority;

    return self;
}

void
CD_DestroyEventCallback (CDEventCallback* self)
{
    CD_free(self);
}

bool
cd_EventBeforeDispatch (CDServer* self, const char* eventName, ...)
{
    CDList* callbacks = (CDList*) CD_HashGet(self->event.callbacks, "Event.dispatch:before");
    bool    result    = true;
    va_list ap;

    va_start(ap, eventName);

    CD_LIST_FOREACH(callbacks, it) {
        if (!CD_ListIteratorValue(it)) {
            continue;
        }

        if (!((CDEventCallback*) CD_ListIteratorValue(it))->function(self, eventName, ap)) {
            result = CD_ListStopIterating(callbacks, false);
            break;
        }
    }

    va_end(ap);

    return result;
}

bool
cd_EventAfterDispatch (CDServer* self, const char* eventName, bool interrupted, ...)
{
    CDList* callbacks = (CDList*) CD_HashGet(self->event.callbacks, "Event.dispatch:after");
    bool    result    = true;
    va_list ap;

    va_start(ap, interrupted);

    CD_LIST_FOREACH(callbacks, it) {
        if (!CD_ListIteratorValue(it)) {
            continue;
        }

        if (!((CDEventCallback*) CD_ListIteratorValue(it))->function(self, eventName, interrupted, ap)) {
            result = CD_ListStopIterating(callbacks, false);
            break;
        }
    }

    va_end(ap);

    return result;
}

void
CD_EventRegister (CDServer* self, const char* eventName, CDEventCallbackFunction callback)
{
    assert(self);

    CDList* callbacks = (CDList*) CD_HashGet(self->event.callbacks, eventName);

    if (!callbacks) {
        callbacks = CD_CreateList();
        CD_HashPut(self->event.callbacks, eventName, (CDPointer) callbacks);
    }

    CD_ListSortedPush(callbacks, (CDPointer) CD_CreateEventCallback(callback, 0), (CDListCompareCallback) cd_EventCompare);
}

void
CD_EventRegisterWithPriority (CDServer* self, const char* eventName, int priority, CDEventCallbackFunction callback)
{
    assert(self);

    CDList* callbacks = (CDList*) CD_HashGet(self->event.callbacks, eventName);

    if (!callbacks) {
        callbacks = CD_CreateList();
        CD_HashPut(self->event.callbacks, eventName, (CDPointer) callbacks);
    }

    CD_ListSortedPush(callbacks, (CDPointer) CD_CreateEventCallback(callback, priority), (CDListCompareCallback) cd_EventCompare);
}

CDEventCallback**
CD_EventUnregister (CDServer* self, const char* eventName, CDEventCallbackFunction callback)
{
    CDList*           callbacks = (CDList*) CD_HashGet(self->event.callbacks, eventName);
    CDEventCallback** result    = NULL;

    if (!callbacks) {
        return NULL;
    }

    if (callback) {
        result    = CD_calloc(2, sizeof(CDEventCallback));
        result[0] = (CDEventCallback*) CD_ListDeleteAllIf(callbacks, (CDPointer) callback,
            (CDListCompareCallback) cd_EventIsEqual);
    }
    else {
        result = (CDEventCallback**) CD_ListClear(callbacks);
    }

    if (CD_ListLength(callbacks) == 0) {
        CD_DestroyList(callbacks);
    }

    return result;
}
