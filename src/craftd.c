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
#include <craftd/Logger.h>
#include <craftd/version.h>
#include <signal.h>

int
main (int argc, char** argv)
{
    CDServer* server;
    int       opt;
    bool      noFork    = false;
    bool      debugging = false;
    char*     config    = NULL;

    static const char* configSearchPath[] = {
      "%s/.craftd/craftd.conf", // %s is replaced with $HOME
      "/etc/craftd/craftd.conf",
      "/usr/local/etc/craftd/craftd.conf",
      "craftd.conf", // Current working directory (for devs)
      NULL
    };

    CDDefaultLogger = CDConsoleLogger;

    LOG(LOG_INFO, "%s " CRAFTD_VERSION, argv[0]);

    while ((opt = getopt(argc, argv, "c:dhnv")) != -1) {
        switch (opt) {
            case 'd': {  // debugging mode
                debugging = true;
            } break;

            case 'v': { // print version
                exit(EXIT_SUCCESS); // Version header already printed
            } break;

            case 'n': { // don't fork or daemonize, use stdout for logging
                noFork = true;
            } break;

            case 'c': { // use the specified config file
                config = optarg;
            } break;

            case 'h': // print help message
            default: {
                fprintf(stderr, "\nUsage: %s [OPTION]...\n"
                    "-c <conf file>\tspecify a conf file location\n"
                    "-d\t\tenable verbose debugging messages\n"
                    "-h\t\tdisplay this help and exit\n"
                    "-n\t\tdon't fork/daemonize (overrides config file)\n"
                    "-v\t\toutput version information and exit\n"
                    "\nFor complete documentation, visit the wiki.\n\n", argv[0]);

                exit((opt == 'h') ? EXIT_SUCCESS : EXIT_FAILURE);
            }
        }
    }

    if (!config) {
        char         path[FILENAME_MAX] = { 0 };
        const char** current            = configSearchPath;

        do {
            snprintf(path, FILENAME_MAX, *current, getenv("HOME"));

            current++;
        } while (*current != NULL && access(path, R_OK) != 0);

        if (access(path, R_OK) != 0) {
            ERR("The config file could not be found");

            exit(EXIT_FAILURE);
        }
        else {
            config = path;
        }
    }
    else {
        if (access(config, R_OK) != 0) {
            ERR("%s could not be read", config);
            exit(EXIT_FAILURE);
        }
    }

    #ifdef WIN32
        evthread_use_windows_threads();
    #else
        evthread_use_pthreads();
    #endif

    if (debugging) {
        evthread_enable_lock_debuging();
    }

    CDMainServer = server = CD_CreateServer(config);

    if (!server) {
        ERR("Server couldn't be instantiated");
        exit(EXIT_FAILURE);
    }

    /* By default, mask debugging messages */
    if (!debugging) {
        server->logger.setlogmask(LOG_MASK(LOG_DEBUG));
    }

    CD_RunServer(server);

    LOG(LOG_INFO, "Exiting.");
    LOG_CLOSE();

    CD_DestroyServer(server);
}
