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

#include <craftd/ScriptingEngines.h>

CDScriptingEngines*
CD_CreateScriptingEngines (struct _CDServer* server)
{
    CDScriptingEngines* self = CD_malloc(sizeof(CDScriptingEngines));

    self->server = server;
    self->items  = CD_CreateHash();

    lt_dlinit();
    
    lt_dladvise_init(&self->advise);
    lt_dladvise_ext(&self->advise);
    lt_dladvise_local(&self->advise);

    C_FOREACH(path, C_PATH(self->server->config, "server.scripting.paths")) {
        lt_dladdsearchdir(C_STRING(path));
    }

    return self;
}

void
CD_DestroyScriptingEngines (CDScriptingEngines* self)
{
    CD_HASH_FOREACH(self->items, it) {
        CD_DestroyScriptingEngine((CDScriptingEngine*) CD_HashIteratorValue(it));
    }

    CD_DestroyHash(self->items);

    CD_free(self);

    lt_dladvise_destroy(&self->advise);
    lt_dlexit();
}

bool
CD_LoadScriptingEngines (CDScriptingEngines* self)
{
    C_FOREACH(engine, C_PATH(self->server->config, "server.scripting.engines")) {
       CD_LoadScriptingEngine(self, C_STRING(C_GET(engine, "name")));
    }

    return true;
}

CDScriptingEngine*
CD_LoadScriptingEngine (CDScriptingEngines* self, const char* name)
{
    CDScriptingEngine* engine = CD_CreateScriptingEngine(self->server, name);

    if (!engine) {
        return NULL;
    }

    CD_HashPut(self->items, name, (CDPointer) engine);

    return engine;
}

CDScriptingEngine*
CD_GetScriptingEngine (CDScriptingEngines* self, const char* name)
{
    return (CDScriptingEngine*) CD_HashGet(self->items, name);
}

void
CD_UnloadScriptingEngine (CDScriptingEngines* self, const char* name)
{
    CD_DestroyScriptingEngine((CDScriptingEngine*) CD_HashDelete(self->items, name));
}
