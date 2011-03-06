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

#include <craftd/Plugin.h>

CDPlugin*
CD_CreatePlugin (struct _CDServer* server, const char* path)
{
    CDPlugin* self = CD_malloc(sizeof(CDPlugin));

    assert(self);

    self->server = server;
    self->name   = NULL;
    self->path   = CD_CreateStringFromCString(path);
    self->handle = lt_dlopenext(path);

    if (!self->handle) {
        CDString* tmp = CD_CreateStringFromFormat("libcd%s", path);
        self->handle = lt_dlopenext(CD_StringContent(tmp));
        CD_DestroyString(tmp);
    }

    if (!self->handle) {
        CDString* tmp = CD_CreateStringFromFormat("lib%s", path);
        self->handle = lt_dlopenext(CD_StringContent(tmp));
        CD_DestroyString(tmp);
    }

    if (!self->handle) {
        CD_DestroyPlugin(self);

        errno = ENOENT;

        return NULL;
    }

    self->initialize = lt_dlsym(self->handle, "CD_PluginInitialize");
    self->finalize   = lt_dlsym(self->handle, "CD_PluginFinalize");

    PRIVATE(self)    = CD_CreateHash();
    PERSISTENT(self) = CD_CreateHash();

    if (self->initialize) {
        self->initialize(self);
    }

    if (!self->name) {
        self->name = CD_CloneString(self->path);
    }

    return self;
}

void
CD_DestroyPlugin (CDPlugin* self)
{
    if (self->finalize) {
        self->finalize(self);
    }

    if (self->handle) {
        lt_dlclose(self->handle);
    }

    CD_DestroyString(self->path);

    if (self->name) {
        CD_DestroyString(self->name);
    }

    if (PRIVATE(self)) {
        CD_DestroyHash(PRIVATE(self));
    }

    CD_free(self);
}
