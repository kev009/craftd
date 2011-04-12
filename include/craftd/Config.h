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
#include <libconfig.h>

typedef struct _CDConfig {
    config_t data;

    struct {
        bool daemonize;

        struct {
            struct {
                struct sockaddr_in  ipv4;
                struct sockaddr_in6 ipv6;
            } bind;

            uint16_t port;
            int      backlog;
        } connection;

        struct {
            const char* motd;
        } files;

        int workers;

        struct {
            struct {
                bool        standard;
                const char* name;
            } protocol;

            struct {
                int     max;
                uint8_t simultaneous;
            } clients;
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

#define C_DO \
    for (config_setting_t* __tmp__ = NULL; __tmp__ != NULL; __tmp__ = 1)

#define C_ROOT(config) \
    config_root_setting(config)

#define C_IN(var, config, name) \
    config_setting_t* var = config_setting_get_member(config, name); \
    if (var)

#define C_INT_CAST    config_setting_get_int
#define C_LONG_CAST   config_setting_get_int64
#define C_FLOAT_CAST  config_setting_get_float
#define C_BOOL_CAST   config_setting_get_bool
#define C_STRING_CAST config_setting_get_string

#define C_INT(x)    (C_INT_CAST(x))
#define C_LONG(x)   (C_LONG_CAST(x))
#define C_FLOAT(x)  (C_FLOAT_CAST(x))
#define C_BOOL(x)   ((bool) C_BOOL_CAST(x))
#define C_STRING(x) (C_STRING_CAST(x))

#define C_GET(config, name) \
    config_setting_get_member(config, name)

#define C_PATH(config, path) (config_lookup((config_t*) config, path))
#define C_PATH_OR(config, path, x, cast) \
    (C_PATH((config_t*) config, path) ?  \
        cast(C_PATH((config_t*) config, path)) : x)

#define C_SAVE(conf, cast, into)    \
    if (conf) {                     \
        into = cast(conf);          \
    }

#define C_FOREACH(name, config)                                                                 \
    for (config_setting_t* name = NULL, *__i__ = (config_setting_t*) 0,                         \
            *__end__ = (config_setting_t*) (long) config_setting_length(config);                \
                                                                                                \
         (long) __i__ < (long) __end__;                                                         \
                                                                                                \
         __i__ = (config_setting_t*) ((long) __i__) + 1,                                        \
            name = config_setting_get_elem(config, (long) __i__))

#endif
