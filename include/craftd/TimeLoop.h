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

#include <craftd/common.h>
#include <craftd/Map.h>

struct _CDServer;

/**
 * The TimeLoop class.
 */
typedef struct _CDTimeLoop {
    struct _CDServer* server;

    pthread_t      thread;
    pthread_attr_t attributes;

    bool running;

    int    last;
    CDMap* callbacks;

    struct {
        struct event_base* base;
    } event;

    struct {
        pthread_spinlock_t last;
    } lock;
} CDTimeLoop;

/**
 * Create a TimeLoop object for the given server.
 *
 * @param server The Server the TimeLoop will run on.
 *
 * @return The instantiated TimeLoop object
 */
CDTimeLoop* CD_CreateTimeLoop (struct _CDServer* server);

/**
 * Destroy a TimeLoop object.
 */
void CD_DestroyTimeLoop (CDTimeLoop* self);

/**
 * Start the TimeLoop operations.
 */
bool CD_RunTimeLoop (CDTimeLoop* self);

bool CD_StopTimeLoop (CDTimeLoop* self);

/**
 * Create an event that will run after the given seconds and delete itself after that
 *
 * @param seconds The seconds after which the event will be fired
 * @param callback The callback to call after the given time
 *
 * @return An ID referring to the timeout with which you can stop it
 */
int CD_SetTimeout (CDTimeLoop* self, float seconds, event_callback_fn callback);

/**
 * Stop the timeout from happening
 *
 * @param id The timeout ID as returned by CD_SetTimeout
 */
void CD_ClearTimeout (CDTimeLoop* self, int id);

/**
 * Create an event that will run after the given seconds and keep repeating
 *
 * @param seconds The seconds after which the event will be fired each time
 * @param callback The callback to call after the given time
 *
 * @return An ID referring to the interval with which you can stop it
 */
int CD_SetInterval (CDTimeLoop* self, float seconds, event_callback_fn callback);

/**
 * Stop the interval from happening
 *
 * @param id The timeout ID as returned by CD_SetInterval
 */
void CD_ClearInterval (CDTimeLoop* self, int id);

#endif
