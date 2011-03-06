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

#include <craftd/Config.h>
#include <craftd/Logger.h>
#include <craftd/TimeLoop.h>
#include <craftd/Workers.h>
#include <craftd/Plugins.h>
#include <craftd/Player.h>

/**
 * Server class.
 */
typedef struct _CDServer {
    char* name;

    int error;

    CDTimeLoop* timeloop;
    CDWorkers*  workers;
    CDConfig*   config;
    CDPlugins*  plugins;
    CDLogger    logger;

    CDHash* players;
    CDMap*  entities;
    CDList* disconnecting;

    bool running;

    uint16_t time;

    struct {
        struct event_base* base;
        struct event*      listener;

        CDHash* callbacks;
    } event;

    struct {
        pthread_spinlock_t time;
    } lock;

    evutil_socket_t socket;

    CDHash* _private;
    CDHash* _persistent;
} CDServer;

/**
 * Create a Server instance with the given Config path.
 *
 * @param path Path to the config file
 *
 * @return The instantiated object
 */
CDServer* CD_CreateServer (const char* path);

/**
 * Destroy a Server instance.
 */
void CD_DestroyServer (CDServer* self);

/**
 * Get the name of the Server.
 *
 * @return The name of the Server or "craftd"
 */
const char* CD_ServerToString (CDServer* self);

/**
 * Get the current Server time in ticks.
 *
 * @return The current time in ticks
 */
uint16_t CD_ServerGetTime (CDServer* self);

/**
 * Set the current Server time in ticks.
 *
 * @param time The new time
 *
 * @return The set time
 */
uint16_t CD_ServerSetTime (CDServer* self, uint16_t time);

/**
 * Run a Server instance.
 *
 * @return true if everything went as expected or false otherwise
 */
bool CD_RunServer (CDServer* self);

/**
 * Get a new unique entity ID
 *
 * @return The generated entity ID
 */
MCEntityId CD_ServerGenerateEntityId (CDServer* self);

typedef bool (*CDEventCallback)();

bool cd_EventBeforeDispatch (CDServer* self, const char* eventName, ...);

bool cd_EventAfterDispatch (CDServer* self, const char* eventName, ...);

/**
 * Dispatch an event with the given name and the given parameters.
 *
 * Pay attention to the parameters you pass, those go on the stack and passing float/double
 * could get them borked. Pointers are always safe to pass.
 *
 * @param eventName The name of the event to dispatch
 */
#define CD_EventDispatch(self, eventName, ...)                                          \
    do {                                                                                \
        if (!cd_EventBeforeDispatch(self, eventName, ##__VA_ARGS__)) {                  \
            break;                                                                      \
        }                                                                               \
                                                                                        \
        CDList* __callbacks__ = (CDList*) CD_HashGet(self->event.callbacks, eventName); \
                                                                                        \
        CD_LIST_FOREACH(__callbacks__, it) {                                            \
            if (!CD_ListIteratorValue(it)) {                                            \
                continue;                                                               \
            }                                                                           \
                                                                                        \
            if (!((CDEventCallback) CD_ListIteratorValue(it))(self, ##__VA_ARGS__)) {   \
                break;                                                                  \
            }                                                                           \
        }                                                                               \
                                                                                        \
        cd_EventAfterDispatch(self, eventName, ##__VA_ARGS__);                          \
    } while (0)

/**
 * Register a callback for an event.
 *
 * @param eventName The name of the event
 * @param callback The callback to be added
 */
void CD_EventRegister (CDServer* server, const char* eventName, CDEventCallback callback);

/**
 * Unregister the event with the passed name, unregisters only the passed callback or every callback if NULL.
 *
 * @param callback The callback to unregister or NULL to unregister every callback
 *
 * @return The unregistered callbacks
 */
CDEventCallback* CD_EventUnregister (CDServer* server, const char* eventName, CDEventCallback callback);

#ifndef CRAFTD_SERVER_IGNORE_EXTERN
extern CDServer* CDMainServer;
#endif

/**
 * Various server utils
 */

/**
 * Kick a Player from the server.
 *
 * The passed string is destroyed after being used, so clone it if you want to keep it
 *
 * @param reason The reason for the kicking
 */
void CD_ServerKick (CDServer* self, CDPlayer* player, CDString* reason);

/**
 * Broadcast a chat message to every connected player.
 *
 * The passed string is destroyed after being used, so clone it if you want to keep it
 *
 * @param message The message to send
 */
void CD_ServerBroadcast (CDServer* self, CDString* message);

#endif
