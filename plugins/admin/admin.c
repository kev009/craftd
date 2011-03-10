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

typedef enum _CDAuthLevel {
    CDLevelUser,
    CDLevelModerator,
    CDLevelAdmin
} CDAuthLevel;

static
bool
cdadmin_HandleCommand (CDServer* server, CDPlayer* player, CDString* command, CDString* parameters)
{
    return true;
}

static
bool
cdadmin_HandleChat (CDServer* server, CDPlayer* player, CDString* message)
{
    if (!player->username) {
        return false;
    }

    CDString* name;

    switch ((CDAuthLevel) CD_HashGet(PRIVATE(player), "Admin.level")) {
        case CDLevelAdmin: {
            name = CD_StringColor(CD_CloneString(player->username), CDColorRed);
        } break;

        default: {
            return true;
        }
    }

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
