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


#define CRAFTD_LOGGER_IGNORE_EXTERN
#include "Logger.h"
#undef CRAFTD_LOGGER_IGNORE_EXTERN

static int cd_mask = 0;

static
void
cd_ConsoleLog (int priority, const char* format, ...)
{
    static const char* names[] = {
        "EMERG", "ALERT", "CRIT", "ERR", "WARNING", "NOTICE", "INFO", "DEBUG"
    };

    va_list ap;

    va_start(ap, format);
  
    /* Return on MASKed log priorities */
    if (LOG_MASK(priority) & cd_mask) {
        return;
    }

    if (priority >= (sizeof(names) / sizeof(char*)) || priority < 0) {
        printf("UNKNOWN: ");
    }
    else {
        printf("%s", names[priority]);
    }

    vprintf(format, ap);
    printf("\n");
  
    va_end(ap);
}

static
int cd_ConsoleSetLogMask (int mask)
{
    int old = cd_mask;

    if (mask != 0) {
        cd_mask = mask;
    }

    return old;
}

static
void cd_ConsoleCloseLog (void)
{
    fflush(stdout);
}

CDLogger CDConsoleLogger = {
    .log        = cd_ConsoleLog,
    .setlogmask = cd_ConsoleSetLogMask,
    .closelog   = cd_ConsoleCloseLog,
};

CDLogger CDDefaultLogger = CDConsoleLogger;
