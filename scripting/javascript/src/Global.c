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

#include "../include/Global.h"
#include "../include/Client.h"

JSBool
cdjs_InitializeGlobal (CDServer* server, JSContext* context)
{
    JSObject* self = JS_NewCompartmentAndGlobalObject(context, &Global_class, NULL);

    if (!JS_InitStandardClasses(context, self)) {
        return JS_FALSE;
    }

    #ifdef HAVE_CONST_JS_HAS_CTYPES
    JS_InitCTypesClass(context, self);
    #endif

    if (!JS_DefineFunctions(context, self, Global_functions)) {
        return JS_FALSE;
    }

    cdjs_InitializeClient(server, context);

    JS_DefineProperty(context, self, "Craftd", OBJECT_TO_JSVAL(self),
        JS_PropertyStub, JS_StrictPropertyStub, JSPROP_READONLY);

    return JS_TRUE;
}

JSBool
Global_include (JSContext* context, uintN argc, jsval* argv)
{
    for (uintN i = 0; i < argc; i++) {

    }

    return JS_TRUE;
}
