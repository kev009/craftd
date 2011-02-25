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

#ifndef CRAFTD_LOGGER_H
#define CRAFTD_LOGGER_H

#include <syslog.h>

#include "common.h"

typedef struct _CDLogger {
    void (*log)        (int, const char*, ...);
    int  (*setlogmask) (int);
    void (*closelog)   (void)
} CDLogger;

#ifndef CRAFTD_LOGGER_IGNORE_EXTERN
extern CDLogger CDConsoleLogger;
extern CDLogger CDSystemLogger;
extern CDLogger CDDefaultLogger;
#endif

#define LOG(priority, format, ...) \
    ((CDMainServer != NULL) \
        ? CDMainServer->logger.log(priority, "%s> " format, CD_ServerToString(CDMainServer), ##__VA_ARGS__) \
        : CDDefaultLogger.log(priority, format, ##__VA_ARGS__))

#define ERR(format, ...) LOG(LOG_CRIT, format, ##__VA_ARGS__)

#define LOG_CLOSE() do { \
    if (CDMainServer) CDMainServer->logger.closelog(); \
    CDDefaultLogger.closelog(); \
} while (0)

#define CLOG(priority, format, ...) CDConsoleLogger.log(priority, format, ##__VA_ARGS__)

#define CERR(format, ...) CLOG(LOG_CRIT, format, ##__VA_ARGS__)

#define SLOG(server, priority, format, ...) \
    server->logger.log(priority, "%s> " format, CD_ServerToString(server), ##__VA_ARGS__)

#define SERR(format, ...) SLOG(LOG_CRIT, format, ##__VA_ARGS__)

#define SLOG_CLOSE(server) server->logger.close()

#endif
