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

#define CD_ADMIN_AUTH_USAGE \
    "Usage: /auth [name] <password>\n" \
    "   name          If omitted Player's username is used\n" \
    "   password    Password to login"


if (CD_StringIsEqual(matches->item[1], "auth")) {
    if (!matches->item[2]) {
        cdadmin_SendUsage(player, CD_ADMIN_AUTH_USAGE);

        goto done;
    }

    DO {
        CDRegexpMatches* old = matches;
        matches = CD_RegexpMatch(regexp, old->item[2]);
        CD_DestroyRegexpMatches(old);
    }

    DO {
        const char* currentName     = NULL;
        const char* currentPassword = NULL;
        const char* name            = NULL;
        const char* password        = NULL;
        const char* level           = NULL;
              bool  authorized      = false;

        if (matches->item[2]) {
            currentName     = CD_StringContent(matches->item[1]);
            currentPassword = CD_StringContent(matches->item[2]);
        }
        else {
            currentName     = CD_StringContent(player->username);
            currentPassword = CD_StringContent(matches->item[1]);
        }

        J_DO {
            J_IN(server, player->client->server->config->data, "server") {
                J_IN(plugin, server, "plugin") {
                    J_FOREACH(plugin, plugin, "plugins") {
                        J_IF_STRING(plugin, "name") {
                            if (CD_CStringIsEqual(J_STRING_VALUE, "admin")) {
                                J_FOREACH(auth, plugin, "authorizations") {
                                    J_STRING(auth, "name", name);
                                    J_STRING(auth, "password", password);
                                    J_STRING(auth, "level", level);

                                    if (CD_CStringIsEqual(currentName, name) && CD_CStringIsEqual(currentPassword, password)) {
                                        cdadmin_SetPlayerAuthLevel(player, level);

                                        cdadmin_SendSuccess(player, CD_CreateStringFromFormat(
                                            "Authorized as %s with level %s", name, level
                                        ));

                                        authorized = true;

                                        break;
                                    }
                                }

                                if (!authorized) {
                                    cdadmin_SendFailure(player, CD_CreateStringFromFormat(
                                        "Failed to authorize as %s", currentName));
                                }

                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    goto done;
}
