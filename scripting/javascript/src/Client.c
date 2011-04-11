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

#include "../include/Client.h"

JSBool
cdjs_InitializeClient (CDServer* server, JSContext* context)
{
    JSObject* self = JS_InitClass(context, JS_GetGlobalObject(context), NULL,
        &Client_class, Client_constructor, 1,
        Client_properties, Client_methods,
        Client_static_properties, Client_static_methods);

    if (!self) {
        return JS_FALSE;
    }

    return JS_TRUE;
}

JSBool
Client_constructor (JSContext* context, uintN argc, jsval* argv)
{
    JSObject* self = NULL;

    return JS_TRUE;
}

JSBool
Client_get_ip (JSContext* context, JSObject* owner, jsid id, jsval* value)
{
    CDClient* self = JS_GetPrivate(context, owner);

    *value = STRING_TO_JSVAL(JS_NewStringCopyZ(context, self->ip));

    return JS_TRUE;
}
