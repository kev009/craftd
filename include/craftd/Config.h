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

#ifndef CRAFTD_CONFIG_H
#define CRAFTD_CONFIG_H

#include <craftd/common.h>
#include <craftd/jansson/jansson.h>

typedef struct _CDConfig {
    json_t*      data;
    json_error_t error;

    struct {
        bool daemonize;

        struct {
            struct {
                struct sockaddr_in  ipv4;
                struct sockaddr_in6 ipv6;
            } bind;

            uint16_t port;
            int backlog;
        } connection;

        struct {
            bool enabled;

            struct {
                struct {
                    struct sockaddr_in  ipv4;
                    struct sockaddr_in6 ipv6;
                } bind;

                uint16_t port;
            } connection;

            const char* root;
        } httpd;

        struct {
            short sunrise;
            short day;
            short sunset;
            short night;
        } rate;

        struct {
            int x;
            int y;
            int z;
        } spawn;

        struct {
            const char* motd;
            const char* world;
        } files;

        int workers;

        int maxPlayers;
    } cache;
} CDConfig;

/**
 * Parse a Config file given a path.
 *
 * @param path The path to the Config file
 *
 * @return The instantiated Config object
 */
CDConfig* CD_ParseConfig (const char* path);

/**
 * Destroy a Config object
 */
void CD_DestroyConfig (CDConfig* self);

#define J_DO for (const json_t* __tmp__ = NULL, *__check__ = NULL; __check__ == NULL; __check__++)

#define J_BOOL_VALUE \
    json_is_true(__tmp__)

#define J_STRING_VALUE \
    json_string_value(__tmp__)

#define J_INT_VALUE \
    json_integer_value(__tmp__)

#define J_OBJ(var, parent, key) \
    const json_t* var = json_object_get(parent, key); \
    if (var && !json_is_null(var))

#define J_IF_BOOL(parent, key) \
    if ((__tmp__ = json_object_get(parent, key)) && !json_is_null(__tmp__))

#define J_IF_STRING(parent, key) \
    if ((__tmp__ = json_object_get(parent, key)) && json_is_string(__tmp__))

#define J_IF_INT(parent, key) \
    if ((__tmp__ = json_object_get(parent, key)) && json_is_integer(__tmp__))

#define J_BOOL(parent, key, into) \
    J_IF_BOOL(parent, key) { into = J_BOOL_VALUE; }

#define J_STRING(parent, key, into) \
    J_IF_STRING(parent, key) { into = J_STRING_VALUE; }

#define J_INT(parent, key, into) \
    J_IF_INT(parent, key) { into = J_INT_VALUE; }

#endif
