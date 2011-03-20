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
#include <craftd/Client.h>

/**
 * Server class.
 */
typedef struct _CDServer {
    char* name;

    struct {
        bool  (*parsable) (CDBuffers* buffers);
        void* (*parse)    (CDBuffers* buffers);
    } packet;

    CDTimeLoop* timeloop;
    CDWorkers*  workers;
    CDConfig*   config;
    CDPlugins*  plugins;
    CDLogger    logger;

    CDList* clients;
    CDList* disconnecting;

    bool running;

    uint16_t time;

    struct {
        struct event_base* base;
        struct event*      listener;

        CDHash* callbacks;
    } event;

    evutil_socket_t socket;

    CD_DEFINE_PRIVATE;
    CD_DEFINE_CACHE;
    CD_DEFINE_ERROR;
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

bool CD_StopServer (CDServer* self);

void CD_ServerFlush (CDServer* self, bool now);

void CD_ServerCleanDisconnects (CDServer* self);

void CD_ReadFromClient (CDClient* client);

#ifndef CRAFTD_SERVER_IGNORE_EXTERN
extern CDServer* CDMainServer;
#endif

/**
 * Various server utils
 */

/**
 * Kick a Client from the server.
 *
 * The passed string is destroyed after being used, so clone it if you want to keep it
 *
 * @param reason The reason for the kicking
 */
void CD_ServerKick (CDServer* self, CDClient* client, CDString* reason);

/**
 * Broadcast a chat message to every connected client.
 *
 * The passed string is destroyed after being used, so clone it if you want to keep it
 *
 * @param message The message to send
 */
void CD_ServerBroadcast (CDServer* self, CDString* message);

#include <craftd/Event.h>

#endif
