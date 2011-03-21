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

#include <beta/Player.h>

static struct {
    struct {
        int max;
    } ticket;
} _config;

typedef struct _CDATicket {
    CDPlayer* requester;
    CDPlayer* assignee;

    enum {
        CDTicketOpen,
        CDTicketAssigned
    } status;

    CDString* content;
} CDATicket;

CDATicket*
cdadmin_CreateTicket (CDPlayer* requester, CDString* content)
{
    CDATicket* self = CD_malloc(sizeof(CDATicket));

    assert(self);

    self->requester = requester;
    self->assignee  = NULL;
    self->status    = CDTicketOpen;
    self->content   = content;

    return self;
}

void
cdadmin_DestroyTicket (CDATicket* self)
{
    if (self->content) {
        CD_DestroyString(self->content);
    }

    CD_free(self);
}

static CDList* _tickets;

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
cdadmin_SendSuccess (CDPlayer* player, CDString* message)
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

            cdadmin_SendResponse(player, CD_CreateStringFromBuffer(usage, offset));

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
    return !(cdadmin_GetPlayerAuthLevel(player) < level);
}

static
bool
cdadmin_AuthLevelIsEnoughWithMessage (CDPlayer* player, CDAuthLevel level)
{
    if (!cdadmin_AuthLevelIsEnough(player, level)) {
        cdadmin_SendFailure(player, CD_CreateStringFromCString(
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
    CDRegexp*        regexp  = CD_CreateRegexp("^(\\w+)(?:\\s+(.*?))?$", CDRegexpNone);
    CDRegexpMatches* matches = CD_RegexpMatch(regexp, command);

    if (!matches) {
        goto error;
    }

    SDEBUG(server, "Command> %s: %s\n", CD_StringContent(matches->item[1]), CD_StringContent(matches->item[2]));

    #include "src/auth.c"
    #include "src/workers.c"
//    #include "src/player.c"
//    #include "src/ticket.c"

    error: {
        CD_DestroyRegexpKeepString(regexp);

        if (matches) {
            CD_DestroyRegexpMatches(matches);
        }

        return true;
    }

    done: {
        CD_DestroyRegexpKeepString(regexp);

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

    // TODO: testing/WIP?
    //CD_ServerBroadcast(server, CD_CreateStringFromCString(MCCharset));

    CD_DestroyString(name);

    return false;
}

static
int8_t
cdadmin_CompareTicket (CDPlayer* player, CDATicket* ticket)
{
    return (ticket->requester == player) ? 0 : 1;
}

static
bool
cdadmin_PlayerLogout (CDServer* server, CDPlayer* player, bool status)
{
    CD_ListDeleteIf(_tickets, (CDPointer) player, (CDListCompareCallback) cdadmin_CompareTicket);

    if (cdadmin_AuthLevelIsEnough(player, CDLevelModerator)) {
        CD_LIST_FOREACH(_tickets, it) {
            CDATicket* ticket = (CDATicket*) CD_ListIteratorValue(it);

            if (ticket->assignee == player) {
                ticket->status   = CDTicketOpen;
                ticket->assignee = NULL;
            }
        }
    }

    return true;
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->name = CD_CreateStringFromCString("Admin");

    CD_DO { // Initiailize config cache
        _config.ticket.max = 20;

        J_DO {
            J_IN(server, self->server->config->data, "server") {
                J_IN(plugin, server, "plugin") {
                    J_FOREACH(plugin, plugin, "plugins") {
                        J_IF_STRING(plugin, "name") {
                            if (CD_CStringIsEqual(J_STRING_VALUE, "admin")) {
                                J_IN(ticket, plugin, "ticket") {
                                    J_INT(ticket, "max", _config.ticket.max);
                                }

                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    _tickets = CD_CreateList();

    CD_HashPut(PRIVATE(self->server), "Admin.tickets", (CDPointer) _tickets);

    CD_EventRegisterWithPriority(self->server, "Player.command", -10, cdadmin_HandleCommand);
    CD_EventRegisterWithPriority(self->server, "Player.chat", -10, cdadmin_HandleChat);

    CD_EventRegister(self->server, "Player.logout", (CDEventCallbackFunction) cdadmin_PlayerLogout);

    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_EventUnregister(self->server, "Player.command", cdadmin_HandleCommand);
    CD_EventUnregister(self->server, "Player.chat", cdadmin_HandleChat);

    CD_EventUnregister(self->server, "Player.logout", (CDEventCallbackFunction) cdadmin_PlayerLogout);

    CD_DestroyList((CDList*) CD_HashDelete(PRIVATE(self->server), "Admin.tickets"));

    return true;
}
