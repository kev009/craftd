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

#include <ecl/ecl.h>

#include "helpers.c"

static
bool
cdcl_EventDispatcher (CDServer* server, const char* event, va_list args)
{
    if (!CD_HashHasKey(server->event.provided, event)) {
        return true;
    }

    CDString* parameters = cdcl_MakeParameters((CDList*) CD_HashGet(server->event.provided, event), args);

    if (!parameters) {
        return true;
    }


    ecl_import_current_thread(Cnil, Cnil);
    cdcl_eval("(craftd:fire :%s %s)", event, CD_StringContent(parameters));
    ecl_release_current_thread();

    CD_DestroyString(parameters);

    return true;
}

extern
bool
CD_ScriptingEngineInitialize (CDScriptingEngine* self)
{
    self->description = CD_CreateStringFromCString("Common LISP scripting");

    int          argc = 1;
    const char** argv = CD_malloc(sizeof(char*));

    argv[0] = "craftd";

    C_FOREACH(option, C_PATH(self->config, "options")) {
        argv           = CD_realloc(argv, argc * sizeof(char*));
        argv[argc - 1] = C_TO_STRING(option);

        argc++;
    }

    ecl_set_option(ECL_OPT_TRAP_INTERRUPT_SIGNAL, FALSE);
    ecl_set_option(ECL_OPT_TRAP_SIGINT, FALSE);
    ecl_set_option(ECL_OPT_TRAP_SIGFPE, FALSE);
    ecl_set_option(ECL_OPT_TRAP_SIGSEGV, FALSE);
    ecl_set_option(ECL_OPT_TRAP_SIGILL, FALSE);

    cl_boot(argc, (char**) argv);

    cdcl_eval("(require 'asdf)");

    C_FOREACH(c_path, C_PATH(self->config, "paths")) {
        CDString* path = CD_CreateStringFromCString(C_TO_STRING(c_path));

        if (CD_StringContent(path)[0] == '/') {
            if (!CD_IsDirectory(CD_StringContent(path))) {
                CD_DestroyString(path);
                continue;
            }
        }
        else {
            char tmp[FILENAME_MAX];
            
            if (getcwd(tmp, FILENAME_MAX)) {
                path = CD_PrependCString(path, "/");
                path = CD_PrependCString(path, tmp);
            }

            if (!CD_IsDirectory(CD_StringContent(path))) {
                CD_DestroyString(path);
                continue;
            }
        }

        if (!CD_StringEndWith(path, "/")) {
            path = CD_AppendCString(path, "/");
        }

        cdcl_eval("(pushnew #P\"%s\" asdf:*central-registry* :test #'equal)", CD_StringContent(path));

        CD_DestroyString(path);
    }

    cdcl_safe_eval("(asdf:load-system :craftd)");

    if (errno == EILSEQ) {
        SERR(self->server, "Failed to load the core LISP");

        return false;
    }

    cdcl_eval("(defparameter craftd::*server* (uffi:make-pointer %ld :void))", (CDPointer) self->server);

    C_FOREACH(script, C_PATH(self->config, "scripts")) {
        cdcl_safe_eval("(asdf:load-system \"%s\")", C_TO_STRING(script));
    }

    CD_EventRegister(self->server, "Event.dispatch:before", cdcl_EventDispatcher);

    return true;
}

extern
bool
CD_ScriptingEngineFinalize (CDScriptingEngine* self)
{
    CD_EventUnregister(self->server, "Event.dispatch:before", cdcl_EventDispatcher);

    cl_shutdown();

    return true;
}
