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

#ifndef CRAFTD_PLUGIN_H
#define CRAFTD_PLUGIN_H

#include <ltdl.h>

#include <craftd/common.h>
#include <craftd/Config.h>

struct _CDPlugin;
struct _CDServer;

/**
 * Callback type to initialize the Plugin
 */
typedef bool (*CDPluginInitializer)(struct _CDPlugin*);

/**
 * Callback type to finalize the Plugin
 */
typedef bool (*CDPluginFinalizer)(struct _CDPlugin*);

/**
 * The Plugin class.
 */
typedef struct _CDPlugin {
    struct _CDServer* server;

    CDString* name;
    CDString* description;

    lt_dlhandle handle;

    CDPluginInitializer initialize;
    CDPluginFinalizer   finalize;

    const json_t* config;

    CD_DEFINE_PRIVATE;
    CD_DEFINE_CACHE;
    CD_DEFINE_ERROR;
} CDPlugin;

/**
 * Create a Plugin from a given path.
 *
 * @param server The Server the plugin will run on
 * @param name The name of the plugin
 *
 * @return The instantiated Plugin object
 */
CDPlugin* CD_CreatePlugin (struct _CDServer* server, const char* name);

/**
 * Destroy a Plugin object
 */
void CD_DestroyPlugin (CDPlugin* self);

#endif
