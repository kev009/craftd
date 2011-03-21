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

#ifndef CRAFTD_CLIENT_H
#define CRAFTD_CLIENT_H

#include <craftd/common.h>

struct _CDServer;

typedef enum _CDClientStatus {
    CDClientConnect,
    CDClientIdle,
    CDClientProcess,
    CDClientDisconnect
} CDClientStatus;

typedef struct _CDClient {
    struct _CDServer* server;

    char            ip[128];
    evutil_socket_t socket;
    CDBuffers*      buffers;

    CDClientStatus status;
    uint8_t        jobs;

    struct {
        pthread_rwlock_t status;
    } lock;

    CD_DEFINE_PRIVATE;
    CD_DEFINE_CACHE;
    CD_DEFINE_ERROR;
} CDClient;

/**
 * Create a Client object on the given Server.
 *
 * @param server The Server the Client will play on
 *
 * @return The instantiated Client object
 */
CDClient* CD_CreateClient (struct _CDServer* server);

/**
 * Destroy a Client object
 */
void CD_DestroyClient (CDClient* self);

/**
 * Send a raw String to a Client
 *
 * @param data The raw String to send
 */
void CD_ClientSendBuffer (CDClient* self, CDBuffer* data);

#endif
