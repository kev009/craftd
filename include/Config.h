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
#include <jansson.h>

typedef json_t*      CDRawConfig;
typedef json_error_t CDRawConfigError;

typedef struct _CDConfig {
    CDRawConfig      data;
    CDRawConfigError error;

    struct {
        bool daemonize;

        struct {
            struct {
                struct sockaddr_in  ipv4;
                struct sockaddr_in6 ipv6;
            } bind;

            uint16_t port;
            int      backlog;
            uint8_t  simultaneous;
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
        } files;

        int workers;

        struct {
            bool standard;

            struct {
                int max;
            } players;
        } game;
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

#define J_DO for (const json_t* __tmp__ = NULL; __tmp__ == NULL; __tmp__++)

#define J_IN(var, parent, key)                          \
    const json_t* var = json_object_get(parent, key);   \
    if (var && !json_is_null(var))

#define J_FOREACH(var, parent, key)                                     \
    for (const json_t* __array__ = json_object_get(parent, key),        \
         *var = json_array_get(__array__, 0),                           \
         *__i__ = 0, *__ie__ = (json_t*) json_array_size(__array__);    \
                                                                        \
         ((size_t) __i__) < ((size_t) __ie__);                          \
                                                                        \
         __i__ = (json_t*) (((size_t) __i__) + 1),                      \
         var = json_array_get(__array__, (size_t) __i__))

#define J_BOOL_CAST(var) \
    json_is_true(var)

#define J_STRING_CAST(var) \
    json_string_value(var)

#define J_INT_CAST(var) \
    json_integer_value(var)

#define J_BOOL_VALUE \
    J_BOOL_CAST(__tmp__)

#define J_STRING_VALUE \
    J_STRING_CAST(__tmp__)

#define J_INT_VALUE \
    J_INT_CAST(__tmp__)

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
