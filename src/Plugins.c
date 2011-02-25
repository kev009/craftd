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

#include "common.h"
#include <ltdl.h>

#include "Plugins.h"

CDPlugins*
CD_CreatePlugins (struct _CDServer* server)
{
    CDPlugins* self = CD_malloc(sizeof(CDPlugins));

    if (!self) {
        return NULL;
    }

    self->server = server;
    self->item   = NULL;
    self->length = 0;

    lt_dlinit();

    return self;
}

void
CD_DestroyPlugins (CDPlugins* self)
{
    lt_dlexit();

    CD_free(self->item);
    CD_free(self);
}

CDPlugin*
CD_LoadPlugin (CDPlugins* self, const char* path)
{
    CDPlugin* plugin = CD_CreatePlugin(self->server, path);

    if (!plugin) {
        return NULL;
    }

    self->item                   = CD_realloc(self->item, sizeof(CDPlugin*) * ++self->length);
    self->item[self->length - 1] = plugin;

    return plugin;
}
