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

#include "include/common.h"
#include "helpers.c"

bool
cdjs_EventDispatcher (CDServer* server, const char* event, va_list args)
{
    if (!CD_HashHasKey(server->event.provided, event)) {
        return true;
    }

    JSRuntime* runtime   = (JSRuntime*) CD_DynamicGet(server, "JavaScript.runtime");
    CDMap*     contextes = (CDMap*) CD_DynamicGet(server, "JavaScript.contextes");
    JSContext* context   = (JSContext*) CD_MapGet(contextes, pthread_self());
    
    if (!context) {
        CD_MapPut(contextes, pthread_self(), (CDPointer) (context = cdjs_CreateContext(server, runtime)));
    }

    JS_BeginRequest(context);

    JS_EndRequest(context);

    return true;
}

extern
bool
CD_ScriptingEngineInitialize (CDScriptingEngine* self)
{
    self->description = CD_CreateStringFromCString("JavaScript scripting");

    JS_SetCStringsAreUTF8();

    JSRuntime* runtime   = JS_NewRuntime(8L * 1024L * 1024L);
    JSContext* context   = cdjs_CreateContext(self->server, runtime);
    CDMap*     contextes = CD_CreateMap();

    CD_DynamicPut(self->server, "JavaScript.runtime",   (CDPointer) runtime);
    CD_DynamicPut(self->server, "JavaScript.contextes", (CDPointer) contextes);
    CD_DynamicPut(self->server, "JavaScript.context",   (CDPointer) context);

    CD_EventRegister(self->server, "Event.dispatch:before", cdjs_EventDispatcher);

    return true;
}

extern
bool
CD_ScriptingEngineFinalize (CDScriptingEngine* self)
{
    CD_EventUnregister(self->server, "Event.dispatch:before", cdjs_EventDispatcher);

    CDMap* contextes = (CDMap*) CD_DynamicDelete(self->server, "JavaScript.contextes");

    CD_MAP_FOREACH(contextes, it) {
        JS_DestroyContext((JSContext*) CD_MapIteratorValue(it));
    }

    CD_DestroyMap(contextes);

    JS_DestroyContext((JSContext*) CD_DynamicDelete(self->server, "JavaScript.context"));

    JS_DestroyRuntime((JSRuntime*) CD_DynamicDelete(self->server, "JavaScript.runtime"));

    JS_ShutDown();

    return true;
}
