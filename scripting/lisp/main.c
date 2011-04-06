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

static struct {
    const char* kernel;
} _config;

#include "helpers.c"

bool
cdlisp_EventDispatcher (CDServer* server, const char* event, va_list args)
{
    return true;
}

extern
bool
CD_ScriptingEngineInitialize (CDScriptingEngine* self)
{
    self->description = CD_CreateStringFromCString("Common Lisp scripting");

    int          argc = 1;
    const char** argv = CD_malloc(sizeof(char*));

    argv[0] = "craftd";

    J_DO {
        J_FOREACH(arg, self->config, "options") {
            argv           = CD_realloc(argv, argc * sizeof(char*));
            argv[argc - 1] = J_STRING_CAST(arg);

            argc++;
        }
    }

    ecl_set_option(ECL_OPT_TRAP_INTERRUPT_SIGNAL, FALSE);
    ecl_set_option(ECL_OPT_TRAP_SIGINT, FALSE);

    cl_boot(argc, (char**) argv);

    CD_EventRegister(self->server, "Event.dispatch:before", cdlisp_EventDispatcher);

    cdlisp_eval("(setf *break-on-signals* 'error)");
    cdlisp_eval("(require :asdf)");

    J_DO {
        J_FOREACH(j_path, self->config, "paths") {
            CDString* path = CD_CreateStringFromCString(J_STRING_CAST(j_path));

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

            CDString* code = CD_CreateStringFromFormat("(pushnew #P\"%s\" asdf:*central-registry* :test #'equal)", CD_StringContent(path));

            cdlisp_eval(CD_StringContent(code));

            CD_DestroyString(code);
            CD_DestroyString(path);
        }
    }

    cdlisp_eval("(asdf:load-system :craftd)");

    if (errno == EILSEQ) {
        SERR(self->server, "Failed to load the core LISP stuff");

        return false;
    }

    DO {
        CDString* code = CD_CreateStringFromFormat("(defparameter craftd::*server* (uffi:make-pointer %d :void))", (CDPointer) self->server);

        cdlisp_eval(CD_StringContent(code));

        CD_DestroyString(code);
    }

    return true;
}

extern
bool
CD_ScriptingEngineFinalize (CDScriptingEngine* self)
{
    CD_EventUnregister(self->server, "Event.dispatch:before", cdlisp_EventDispatcher);

    cl_shutdown();

    return true;
}
