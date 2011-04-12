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

#include <craftd/Logger.h>

#include <craftd/Plugins.h>

CDPlugins*
CD_CreatePlugins (struct _CDServer* server)
{
    CDPlugins* self = CD_malloc(sizeof(CDPlugins));

    self->server = server;
    self->items  = CD_CreateHash();

    lt_dlinit();
    
    lt_dladvise_init(&self->advise);
    lt_dladvise_ext(&self->advise);
    lt_dladvise_global(&self->advise);

    C_FOREACH(path, C_PATH(self->server->config, "server.plugins.paths")) {
        lt_dladdsearchdir(C_STRING(path));
    }

    return self;
}

void
CD_DestroyPlugins (CDPlugins* self)
{
    CD_HASH_FOREACH(self->items, it) {
        CD_DestroyPlugin((CDPlugin*) CD_HashIteratorValue(it));
    }

    CD_DestroyHash(self->items);

    CD_free(self);

    lt_dladvise_destroy(&self->advise);
    lt_dlexit();
}

bool
CD_LoadPlugins (CDPlugins* self)
{
    C_FOREACH(plugin, C_PATH(self->server->config, "server.plugins.load")) {
        CD_LoadPlugin(self, C_STRING(C_GET(plugin, "name")));
    }

    return true;
}

CDPlugin*
CD_LoadPlugin (CDPlugins* self, const char* name)
{
    CDPlugin* plugin = CD_CreatePlugin(self->server, name);

    if (!plugin) {
        return NULL;
    }

    CD_HashPut(self->items, name, (CDPointer) plugin);

    return plugin;
}

CDPlugin*
CD_GetPlugin (CDPlugins* self, const char* name)
{
    return (CDPlugin*) CD_HashGet(self->items, name);
}

void
CD_UnloadPlugin (CDPlugins* self, const char* name)
{
    CD_DestroyPlugin((CDPlugin*) CD_HashDelete(self->items, name));
}
