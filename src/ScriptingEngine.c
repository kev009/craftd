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

#include <craftd/ScriptingEngine.h>
#include <craftd/Server.h>

CDScriptingEngine*
CD_CreateScriptingEngine (CDServer* server, const char* name)
{
    CDScriptingEngine* self = CD_alloc(sizeof(CDScriptingEngine));

    self->server = server;

    self->name        = CD_CreateStringFromCString(name);
    self->description = NULL;

    self->handle = lt_dlopenadvise(name, server->scriptingEngines->advise);

    if (!self->handle) {
        CDString* tmp = CD_CreateStringFromFormat("lib%s", name);
        self->handle = lt_dlopenext(CD_StringContent(tmp));
        CD_DestroyString(tmp);
    }

    if (!self->handle) {
        CDString* tmp = CD_CreateStringFromFormat("libcd%s", name);
        self->handle = lt_dlopenext(CD_StringContent(tmp));
        CD_DestroyString(tmp);
    }

    if (!self->handle) {
        CD_DestroyScriptingEngine(self);

        SERR(server, "Couldn't load scripting engine %s", name);

        errno = ENOENT;

        return NULL;
    }

    self->initialize = lt_dlsym(self->handle, "CD_ScriptingEngineInitialize");
    self->finalize   = lt_dlsym(self->handle, "CD_ScriptingEngineFinalize");

    DYNAMIC(self) = CD_CreateDynamic();
    ERROR(self)   = CDNull;

    C_FOREACH(engine, C_PATH(server->config, "server.scripting.engines")) {
        if (CD_CStringIsEqual(name, C_TO_STRING(C_GET(engine, "name")))) {
            self->config = CD_malloc(sizeof(config_t));
            config_export(engine, self->config);
            break;
        }
    }

    if (self->initialize) {
        self->initialize(self);
    }

    if (self->description) {
        SLOG(server, LOG_NOTICE, "Loaded scripting engine %s - %s", name, CD_StringContent(self->description));
    }
    else {
        SLOG(server, LOG_NOTICE, "Loaded scripting engine %s", name);
    }

    return self;
}

void
CD_DestroyScriptingEngine (CDScriptingEngine* self)
{
    if (self->finalize) {
        self->finalize(self);
    }

    if (self->handle) {
        lt_dlclose(self->handle);
    }

    CD_DestroyString(self->name);

    if (self->description) {
        CD_DestroyString(self->description);
    }

    if (DYNAMIC(self)) {
        CD_DestroyDynamic(DYNAMIC(self));
    }

    if (self->config) {
        config_unexport(self->config);

        CD_free(self->config);
    }

    CD_free(self);
}
