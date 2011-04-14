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
#include <craftd/Server.h>

#include "include/HTTPd.h"

static
bool
cdhttp_ServerStart (CDServer* server)
{
    CDHTTPd* httpd = (CDHTTPd*) CD_DynamicGet(server, "HTTPd.instance");

    pthread_create(&httpd->thread, &httpd->attributes, (void *(*)(void *)) CD_RunHTTPd, httpd);

    return true;
}

static
bool
cdhttp_ServerStop (CDServer* server)
{
    CDHTTPd* httpd = (CDHTTPd*) CD_DynamicGet(server, "HTTPd.instance");

    CD_StopHTTPd(httpd);

    return true;
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->description = CD_CreateStringFromCString("Web Interface and RPC");

    CD_DynamicPut(self->server, "HTTPd.instance", (CDPointer) CD_CreateHTTPd(self));

    CD_EventRegister(self->server, "Server.start!", cdhttp_ServerStart);
    CD_EventRegister(self->server, "Server.stop!", cdhttp_ServerStop);


    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_DestroyHTTPd((CDHTTPd*) CD_DynamicDelete(self->server, "HTTPd.instance"));

    CD_EventUnregister(self->server, "Server.start!", cdhttp_ServerStart);
    CD_EventUnregister(self->server, "Server.stop!", cdhttp_ServerStop);

    return true;
}
