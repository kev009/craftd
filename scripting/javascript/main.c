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

#include <craftd/Server.h>
#include <craftd/Plugin.h>

#include <jsapi.h>

#include "helpers.c"

bool
cdjs_EventDispatcher (CDServer* server, const char* event, va_list args)
{
    return true;
}

extern
bool
CD_ScriptingEngineInitialize (CDScriptingEngine* self)
{
    self->description = CD_CreateStringFromCString("JavaScript scripting");

    JS_SetCStringsAreUTF8();

    JSRuntime* runtime = JS_NewRuntime(8L * 1024L * 1024L);
    JSContext* context = JS_NewContext(runtime, 8192);;

    JS_SetErrorReporter(context, cdjs_ReportError);

    JSObject* global = cdjs_InitializeGlobal(self->server);

    CD_DynamicPut(self->server, "JavaScript.runtime", (CDPointer) runtime);
    CD_DynamicPut(self->server, "JavaScript.context", (CDPointer) context);
    CD_DynamicPut(self->server, "JavaScript.global",  (CDPointer) global);

    CD_EventRegister(self->server, "Event.dispatch:before", cdjs_EventDispatcher);

    return true;
}

extern
bool
CD_ScriptingEngineFinalize (CDScriptingEngine* self)
{
    CD_EventUnregister(self->server, "Event.dispatch:before", cdjs_EventDispatcher);

    return true;
}
