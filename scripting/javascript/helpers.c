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

#include "include/Global.h"

void
cdjs_ReportError (JSContext* cx, const char* message, JSErrorReport* report)
{
    ERR("%s:%u > %s\n",
        report->filename ? report->filename : "craftd",
        (unsigned int) report->lineno,
        message
    );
}

jsval
cdjs_EvaluateFile (JSContext* context, const char* path)
{
    JS_BeginRequest(context);

    JSObject* script = JS_CompileFile(context, JS_GetGlobalObject(context), path);
    jsval     result;

    if (script == NULL) {
        result = JSVAL_FALSE;
    }
    else {
        if (!JS_ExecuteScript(context, JS_GetGlobalObject(context), script, &result)) {
            result = JSVAL_FALSE;
        }
        else {
            JS_MaybeGC(context);
        }
    }

    JS_EndRequest(context);

    return result;
}

jsval
cdjs_Evaluate (JSContext* context, const char* format, ...)
{
    va_list ap;

    va_start(ap, format);

    CDString* code = CD_CreateStringFromFormatList(format, ap);
    jsval     result;

    JS_EvaluateUCScript(context, JS_GetGlobalObject(context),
        (const jschar*) CD_StringContent(code), CD_StringSize(code),
        "craftd", 0, &result);


    if (JS_IsExceptionPending(context)) {
        JS_ClearPendingException(context);
        
        result = JSVAL_VOID;
    }   
        

    CD_DestroyString(code);

    va_end(ap);

    return result;
}

JSContext*
cdjs_CreateContext (CDServer* server, JSRuntime* runtime)
{
    JSContext* self = JS_NewContext(runtime, 8192);

    JS_SetOptions(self,
        JSOPTION_VAROBJFIX |
        JSOPTION_JIT       |
        JSOPTION_METHODJIT |
        JSOPTION_XML       |
        JSOPTION_COMPILE_N_GO);

    JS_SetVersion(self, JSVERSION_LATEST);

    JS_SetErrorReporter(self, cdjs_ReportError);

    if (!cdjs_InitializeGlobal(server, self)) {
        JS_DestroyContext(self);

        return NULL;
    }

    return self;
}
