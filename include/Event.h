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

#ifndef CRAFTD_EVENT_H
#define CRAFTD_EVENT_H

#include <craftd/Server.h>

typedef bool (*CDEventCallbackFunction)();

typedef struct _CDEventCallback {
    CDEventCallbackFunction function;
    int                     priority;
} CDEventCallback;

CDEventCallback* CD_CreateEventCallback (CDEventCallbackFunction function, int priority);

void CD_DestroyEventCallback (CDEventCallback* self);

bool cd_EventBeforeDispatch (CDServer* self, const char* eventName, ...);

bool cd_EventAfterDispatch (CDServer* self, const char* eventName, bool interrupted, ...);

/**
 * Dispatch an event with the given name and the given parameters.
 *
 * Pay attention to the parameters you pass, those go on the stack and passing float/double
 * could get them borked. Pointers are always safe to pass.
 *
 * @param eventName The name of the event to dispatch
 */
#define CD_EventDispatch(self, eventName, ...)                                                      \
    DO {                                                                                         \
        assert(self);                                                                               \
        assert(eventName);                                                                          \
                                                                                                    \
        bool __interrupted__ = false;                                                               \
                                                                                                    \
        if (!cd_EventBeforeDispatch(self, eventName, ##__VA_ARGS__)) {                              \
            break;                                                                                  \
        }                                                                                           \
                                                                                                    \
        CDList* __callbacks__ = (CDList*) CD_HashGet(self->event.callbacks, eventName);             \
                                                                                                    \
        CD_LIST_FOREACH(__callbacks__, it) {                                                        \
            if (!CD_ListIteratorValue(it)) {                                                        \
                continue;                                                                           \
            }                                                                                       \
                                                                                                    \
            if (!((CDEventCallback*) CD_ListIteratorValue(it))->function(self, ##__VA_ARGS__)) {    \
                __interrupted__ = !CD_ListStopIterating(__callbacks__, false);                      \
                break;                                                                              \
            }                                                                                       \
        }                                                                                           \
                                                                                                    \
        cd_EventAfterDispatch(self, eventName, __interrupted__, ##__VA_ARGS__);                     \
    }

#define CD_EventDispatchWithResult(interrupted, self, eventName, ...)                               \
    DO {                                                                                         \
        assert(self);                                                                               \
        assert(eventName);                                                                          \
                                                                                                    \
        interrupted = false;                                                                        \
                                                                                                    \
        if (!cd_EventBeforeDispatch(self, eventName, ##__VA_ARGS__)) {                              \
            break;                                                                                  \
        }                                                                                           \
                                                                                                    \
        CDList* __callbacks__ = (CDList*) CD_HashGet(self->event.callbacks, eventName);             \
                                                                                                    \
        CD_LIST_FOREACH(__callbacks__, it) {                                                        \
            if (!CD_ListIteratorValue(it)) {                                                        \
                continue;                                                                           \
            }                                                                                       \
                                                                                                    \
            if (!((CDEventCallback*) CD_ListIteratorValue(it))->function(self, ##__VA_ARGS__)) {    \
                interrupted = !CD_ListStopIterating(__callbacks__, false);                          \
                break;                                                                              \
            }                                                                                       \
        }                                                                                           \
                                                                                                    \
        cd_EventAfterDispatch(self, eventName, interrupted, ##__VA_ARGS__);                         \
    }

#define CD_EventDispatchWithError(error, self, eventName, ...)                                              \
    DO {                                                                                                 \
        assert(self);                                                                                       \
        assert(eventName);                                                                                  \
                                                                                                            \
        bool __interrupted__ = false;                                                                       \
             error           = CDOk;                                                                        \
                                                                                                            \
        if (!cd_EventBeforeDispatch(self, eventName, ##__VA_ARGS__, &error)) {                              \
            break;                                                                                          \
        }                                                                                                   \
                                                                                                            \
        CDList* __callbacks__ = (CDList*) CD_HashGet(self->event.callbacks, eventName);                     \
                                                                                                            \
        CD_LIST_FOREACH(__callbacks__, it) {                                                                \
            if (!CD_ListIteratorValue(it)) {                                                                \
                continue;                                                                                   \
            }                                                                                               \
                                                                                                            \
            if (!((CDEventCallback*) CD_ListIteratorValue(it))->function(self, ##__VA_ARGS__, &error)) {    \
                __interrupted__ = !CD_ListStopIterating(__callbacks__, false);                              \
                break;                                                                                      \
            }                                                                                               \
        }                                                                                                   \
                                                                                                            \
        cd_EventAfterDispatch(self, eventName, __interrupted__, ##__VA_ARGS__, &error);                     \
    }


/**
 * Register a callback for an event.
 *
 * @param eventName The name of the event
 * @param callback The callback to be added
 */
void CD_EventRegister (CDServer* server, const char* eventName, CDEventCallbackFunction callback);


/**
 * Register a callback with the given priority.
 *
 * The default priority is 0, bigger means less important, smaller means more important.
 *
 * @param eventName The name of the event
 * @param priority The callback priority
 * @param callback THe callback to be added
 */
void CD_EventRegisterWithPriority (CDServer* server, const char* eventName, int priority, CDEventCallbackFunction callback);

/**
 * Unregister the event with the passed name, unregisters only the passed callback or every callback if NULL.
 *
 * @param callback The callback to unregister or NULL to unregister every callback
 *
 * @return The unregistered callbacks
 */
CDEventCallback** CD_EventUnregister (CDServer* server, const char* eventName, CDEventCallbackFunction callback);

#endif
