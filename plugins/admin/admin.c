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
#include <craftd/Player.h>

#define CD_ADMIN_AUTH_USAGE \
    "Usage: /auth [name] <password>\n" \
    "   name          If omitted Player's username is used\n" \
    "   password    Password to login"

#define CD_ADMIN_PLAYER_USAGE \
    "Usage: /player <command> [options]\n" \
    "   Level:\n" \
    "       Registered Users:\n" \
    "           number    Get the number of connected players\n" \
    "\n" \
    "       Moderator:\n" \
    "           list    Get a list of connected players"

typedef enum _CDAuthLevel {
    CDLevelUser,
    CDLevelRegisteredUser,
    CDLevelModerator,
    CDLevelAdmin
} CDAuthLevel;

static
void
cdadmin_SendResponse (CDPlayer* player, CDString* message)
{
    CD_PlayerSendMessage(player, MC_StringColor(message, MCColorGray));
}

static
void
cdadmin_SendSuccessful (CDPlayer* player, CDString* message)
{
    CD_PlayerSendMessage(player, MC_StringColor(message, MCColorDarkGreen));
}

static
void
cdadmin_SendFailure (CDPlayer* player, CDString* message)
{
    CD_PlayerSendMessage(player, MC_StringColor(message, MCColorDarkRed));
}

static
void
cdadmin_SendUsage (CDPlayer* player, const char* usage)
{
    const char*  current = usage;
          size_t offset  = 1;

    while (*current != '\0') {
        offset++;
        current++;

        if (*current == '\n' || *current == '\0') {
            if (*current == '\n') {
                offset--;
            }

            cdadmin_SendResponse(player, CD_CreateStringFromBufferCopy(usage, offset));

            offset = 0;
            usage  = current + 1;
        }
    }
}

static
void
cdadmin_SetPlayerAuthLevel (CDPlayer* player, const char* level)
{
    CDAuthLevel apply = CDLevelUser;

    if (CD_CStringIsEqual(level, "admin")) {
        apply = CDLevelAdmin;
    }
    else if (CD_CStringIsEqual(level, "moderator")) {
        apply = CDLevelModerator;
    }
    else if (CD_CStringIsEqual(level, "registered")) {
        apply = CDLevelRegisteredUser;
    }

    CD_HashPut(PRIVATE(player), "Authorization.level", apply);
}

static
CDAuthLevel
cdadmin_GetPlayerAuthLevel (CDPlayer* player)
{
    return CD_HashGet(PRIVATE(player), "Authorization.level");
}

static
bool
cdadmin_AuthLevelIsEnough (CDPlayer* player, CDAuthLevel level)
{
    if (cdadmin_GetPlayerAuthLevel(player) < level) {
        cdadmin_SendFailure(player, CD_CreateStringFromCStringCopy(
            "Authorization level not enough"));

        return false;
    }

    return true;
}

static
CDString*
cdadmin_ColoredNick (CDPlayer* player) {
    switch (cdadmin_GetPlayerAuthLevel(player)) {
        case CDLevelAdmin: {
            return MC_StringColor(CD_CloneString(player->username), MCColorRed);
        } break;

        case CDLevelModerator: {
            return MC_StringColor(CD_CloneString(player->username), MCColorBlue);
        } break;

        default: {
            return CD_CloneString(player->username);
        }
    }
}

static
bool
cdadmin_HandleCommand (CDServer* server, CDPlayer* player, CDString* command)
{
    CDRegexpMatches* matches = CD_RegexpMatchString("^(\\w+)(?:\\s+(.*?))?$", CDRegexpNone, command);

    if (!matches) {
        goto error;
    }

    SDEBUG(server, "Command> %s: %s\n", CD_StringContent(matches->item[1]), CD_StringContent(matches->item[2]));

    if (CD_StringIsEqual(matches->item[1], "auth")) {
        if (!matches->item[2]) {
            cdadmin_SendUsage(player, CD_ADMIN_AUTH_USAGE);

            goto done;
        }

        CDRegexpMatches* args = CD_RegexpMatchString("^(.+?)(?:\\s+(.*?))?$", CDRegexpNone, matches->item[2]);

        CD_DO {
            const char* currentName;
            const char* currentPassword;
            const char* name;
            const char* password;
            const char* level;
            bool        authorized = false;

            if (args->item[2]) {
                currentName     = CD_StringContent(args->item[1]);
                currentPassword = CD_StringContent(args->item[2]);
            }
            else {
                currentName     = CD_StringContent(player->username);
                currentPassword = CD_StringContent(args->item[1]);
            }

            J_DO {
                J_IN(server, player->server->config->data, "server") {
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

                                            cdadmin_SendSuccessful(player, CD_CreateStringFromFormat(
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

        CD_DestroyRegexpMatches(args);

        goto done;
    }

    if (CD_StringIsEqual(matches->item[1], "workers")) {
        if (!cdadmin_AuthLevelIsEnough(player, CDLevelAdmin)) {
            goto done;
        }

        if (!matches->item[2]) {
            cdadmin_SendResponse(player, CD_CreateStringFromFormat("There are %d workers running.",
                server->workers->length));
        }
        else {
            int workers = atoi(CD_StringContent(matches->item[2]));

            if (workers >= 1) {
                if (workers > server->workers->length) {
                    CD_free(CD_SpawnWorkers(server->workers, workers - server->workers->length));
                }
                else if (workers < server->workers->length) {
                    for (size_t i = 0; i < server->workers->length; i++) {
                        if (!server->workers->item[i]->job || server->workers->item[i]->job->type != CDPlayerProcessJob) {
                            continue;
                        }

                        if (((CDPlayerProcessJobData*) server->workers->item[i]->job->data)->player == player) {
                            CD_KillWorkersAvoid(server->workers, server->workers->length - workers, server->workers->item[i]);
                            break;
                        }
                    }
                }
            }
        }

        goto done;
    }

    if (CD_StringIsEqual(matches->item[1], "player")) {
        if (!matches->item[2]) {
            cdadmin_SendUsage(player, CD_ADMIN_PLAYER_USAGE);

            goto done;
        }

        if (!cdadmin_AuthLevelIsEnough(player, CDLevelRegisteredUser)) {
            goto done;
        }

        if (CD_StringIsEqual(matches->item[2], "number")) {
            size_t connected = CD_HashLength(server->players);

            if (connected > 1) {
                cdadmin_SendResponse(player, CD_CreateStringFromFormat("There are %d connected players", connected));
            }
            else {
                cdadmin_SendResponse(player, CD_CreateStringFromFormat("There is %d connected player", connected));
            }

            goto done;
        }

        if (!cdadmin_AuthLevelIsEnough(player, CDLevelModerator)) {
            goto done;
        }

        if (CD_StringIsEqual(matches->item[2], "list")) {
            cdadmin_SendResponse(player, CD_CreateStringFromCStringCopy("Players:"));

            CD_HASH_FOREACH(server->players, it) {
                CDPlayer* current = (CDPlayer*) CD_HashIteratorValue(it);
                CDString* name    = cdadmin_ColoredNick(current);

                cdadmin_SendResponse(player, CD_CreateStringFromFormat("%s" MC_COLOR_GRAY " > x: %lf; y:%lf; z:%lf", CD_StringContent(name),
                    current->entity.position.x, current->entity.position.y, current->entity.position.z));
            }

            goto done;
        }

        cdadmin_SendUsage(player, CD_ADMIN_PLAYER_USAGE);
        goto done;
    }

    error: {
        if (matches) {
            CD_DestroyRegexpMatches(matches);
        }

        return true;
    }

    done: {
        if (matches) {
            CD_DestroyRegexpMatches(matches);
        }

        return false;
    }
}

static
bool
cdadmin_HandleChat (CDServer* server, CDPlayer* player, CDString* message)
{
    if (!player->username) {
        return false;
    }

    CDString* name = cdadmin_ColoredNick(player);

    SLOG(server, LOG_NOTICE, "<%s> %s", CD_StringContent(player->username),
            CD_StringContent(message));

    CD_ServerBroadcast(server, CD_CreateStringFromFormat("<%s> %s",
        CD_StringContent(name),
        CD_StringContent(message)));

    CD_DestroyString(name);

    return false;
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->name = CD_CreateStringFromCString("Admin");

    CD_EventRegisterWithPriority(self->server, "Player.command", -10, cdadmin_HandleCommand);
    CD_EventRegisterWithPriority(self->server, "Player.chat", -10, cdadmin_HandleChat);

    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_EventUnregister(self->server, "Player.command", cdadmin_HandleCommand);
    CD_EventUnregister(self->server, "Player.chat", cdadmin_HandleChat);

    return true;
}
