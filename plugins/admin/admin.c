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
    "           list     Get a list of connected players\n" \
    "           kick    Kick a player"

#define CD_ADMIN_PLAYER_KICK_USAGE \
    "Usage: /player kick <name> [reason]\n" \
    "    name        The name of the player to kick\n" \
    "    reason    The reason for the kick"

#define CD_ADMIN_TICKET_MODERATOR_USAGE \
    "Usage: /ticket <command> [options]\n" \
    "    list       List the tickets\n" \
    "    assign     Set a ticket as in resolution\n" \
    "    close      Close a ticket"

#define CD_ADMIN_TICKET_MODERATOR_LIST_USAGE \
    "Usage: /ticket list <status>\n" \
    "   status    [all, open, assigned, mine]"

#define CD_ADMIN_TICKET_MODERATOR_ASSIGN_USAGE \
    "Usage: /ticket assign <id> <name>\n" \
    "    id      ID of the ticket\n" \
    "    name    Name of the moderator to assign the ticket"

#define CD_ADMIN_TICKET_MODERATOR_CLOSE_USAGE \
    "Usage: /ticket close <id>\n" \
    "    id    ID of the ticket"

#define CD_ADMIN_TICKET_PLAYER_USAGE \
    "Usage: /ticket <command> [options]\n" \
    "    create    Create a ticket\n" \
    "    status    Check the status of a ticket"

#define CD_ADMIN_TICKET_PLAYER_CREATE_USAGE \
    "Usage: /ticket create <text>"

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

    if (CD_StringIsEqual(matches->item[1], "auth")) {
        if (!matches->item[2]) {
            cdadmin_SendUsage(player, CD_ADMIN_AUTH_USAGE);

            goto done;
        }

        CD_DO {
            CDRegexpMatches* old = matches;
            matches = CD_RegexpMatch(regexp, old->item[2]);
            CD_DestroyRegexpMatches(old);
        }

        CD_DO {
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

    if (CD_StringIsEqual(matches->item[1], "workers")) {
        if (!cdadmin_AuthLevelIsEnoughWithMessage(player, CDLevelAdmin)) {
            goto done;
        }

        if (!matches->item[2]) {
            cdadmin_SendResponse(player, CD_CreateStringFromFormat("There are %d workers running.",
                server->workers->length));
        }
        else {
            int workers = atoi(CD_StringContent(matches->item[2]));

            if (workers <= 0) {
                cdadmin_SendFailure(player, CD_CreateStringFromCString("You can have 1 worker at minimum"));
                goto done;
            }

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

        goto done;
    }

    if (CD_StringIsEqual(matches->item[1], "player")) {
        if (!matches->item[2]) {
            cdadmin_SendUsage(player, CD_ADMIN_PLAYER_USAGE);

            goto done;
        }

        if (!cdadmin_AuthLevelIsEnoughWithMessage(player, CDLevelRegisteredUser)) {
            goto done;
        }

        CD_DO {
            CDRegexpMatches* old = matches;
            matches = CD_RegexpMatch(regexp, old->item[2]);
            CD_DestroyRegexpMatches(old);
        }

        if (CD_StringIsEqual(matches->item[1], "number")) {
            size_t connected = CD_HashLength(server->players);

            if (connected > 1) {
                cdadmin_SendResponse(player, CD_CreateStringFromFormat("There are %d connected players", connected));
            }
            else {
                cdadmin_SendResponse(player, CD_CreateStringFromFormat("There is %d connected player", connected));
            }

            goto done;
        }

        if (!cdadmin_AuthLevelIsEnoughWithMessage(player, CDLevelModerator)) {
            goto done;
        }

        if (CD_StringIsEqual(matches->item[1], "list")) {
            cdadmin_SendResponse(player, CD_CreateStringFromCString("Players:"));

            CD_HASH_FOREACH(server->players, it) {
                CDPlayer* current = (CDPlayer*) CD_HashIteratorValue(it);
                CDString* name    = cdadmin_ColoredNick(current);

                cdadmin_SendResponse(player, CD_CreateStringFromFormat("%s" MC_COLOR_GRAY " > x: %lf; y:%lf; z:%lf", CD_StringContent(name),
                    current->entity.position.x, current->entity.position.y, current->entity.position.z));
            }

            goto done;
        }

        if (CD_StringIsEqual(matches->item[1], "kick")) {
            if (!matches->item[2]) {
                cdadmin_SendUsage(player, CD_ADMIN_PLAYER_KICK_USAGE);
                goto done;
            }

            CD_DO {
                CDRegexpMatches* old = matches;
                matches = CD_RegexpMatch(regexp, old->item[2]);
                CD_DestroyRegexpMatches(old);
            }

            if (CD_HashHas(server->players, CD_StringContent(matches->item[1]))) {
                CD_ServerKick(server, (CDPlayer*) CD_HashGet(server->players, CD_StringContent(matches->item[1])), CD_CloneString(matches->item[2]));
            }
            else {
                cdadmin_SendFailure(player, CD_CreateStringFromFormat("Player %s not found.", CD_StringContent(matches->item[1])));
            }

            goto done;
        }

        cdadmin_SendUsage(player, CD_ADMIN_PLAYER_USAGE);
        goto done;
    }

    if (CD_StringIsEqual(matches->item[1], "ticket")) {
        if (cdadmin_AuthLevelIsEnough(player, CDLevelModerator)) {
            if (!matches->item[2]) {
                cdadmin_SendUsage(player, CD_ADMIN_TICKET_MODERATOR_USAGE);
                goto done;
            }

            CD_DO {
                CDRegexpMatches* old = matches;
                matches = CD_RegexpMatch(regexp, old->item[2]);
                CD_DestroyRegexpMatches(old);
            }

            if (CD_StringIsEqual(matches->item[1], "list")) {
                if (!matches->item[2]) {
                    cdadmin_SendUsage(player, CD_ADMIN_TICKET_MODERATOR_LIST_USAGE);
                    goto done;
                }

                size_t count = 0;
                size_t shown = 0;

                CD_LIST_FOREACH(_tickets, it) {
                    CDATicket* ticket = (CDATicket*) CD_ListIteratorValue(it);
                    bool       show   = false;

                    if (CD_StringIsEqual(matches->item[2], "all")) {
                        show = true;
                    }
                    else if (CD_StringIsEqual(matches->item[2], "open")) {
                        if (ticket->status == CDTicketOpen) {
                            show = true;
                        }
                    }
                    else if (CD_StringIsEqual(matches->item[2], "assigned")) {
                        if (ticket->status == CDTicketAssigned) {
                            show = true;
                        }
                    }
                    else if (CD_StringIsEqual(matches->item[2], "mine")) {
                        if (ticket->assignee == player) {
                            show = true;
                        }
                    }

                    count++;

                    if (!show) {
                        continue;
                    }
                    else {
                        shown++;
                    }

                    if (ticket->assignee) {
                        cdadmin_SendResponse(player, CD_CreateStringFromFormat(
                            MC_COLOR_DARKRED "%d: " MC_COLOR_WHITE "%s (%s)" MC_COLOR_GRAY "> " MC_COLOR_WHITE "%s",
                            count - 1, CD_StringContent(ticket->requester->username),
                            CD_StringContent(ticket->assignee->username), CD_StringContent(ticket->content)));
                    }
                    else {
                        cdadmin_SendResponse(player, CD_CreateStringFromFormat(
                            MC_COLOR_DARKRED "%d: " MC_COLOR_WHITE "%s" MC_COLOR_GRAY "> " MC_COLOR_WHITE "%s",
                            count - 1, CD_StringContent(ticket->requester->username), CD_StringContent(ticket->content)));
                    }

                    if (shown <= 0) {
                        cdadmin_SendResponse(player, CD_CreateStringFromCString("No tickets"));
                    }
                }

                goto done;
            }

            if (CD_StringIsEqual(matches->item[1], "assign")) {
                if (!matches->item[2]) {
                    cdadmin_SendUsage(player, CD_ADMIN_TICKET_MODERATOR_ASSIGN_USAGE);
                    goto done;
                }

                CD_DO {
                    CDRegexpMatches* old = matches;
                    matches = CD_RegexpMatchString("^(\\d+)\\s+(.+)$", CDRegexpNone, old->item[2]);
                    CD_DestroyRegexpMatches(old);
                }

                if (!matches) {
                    cdadmin_SendUsage(player, CD_ADMIN_TICKET_MODERATOR_ASSIGN_USAGE);
                    goto done;
                }

                CDPlayer* assignee = NULL;

                if (!CD_HashHas(server->players, CD_StringContent(matches->item[2]))) {
                    cdadmin_SendFailure(player, CD_CreateStringFromFormat("%s isn't connected",
                        CD_StringContent(matches->item[2])));

                    goto done;
                }
                else {
                    assignee = (CDPlayer*) CD_HashGet(server->players, CD_StringContent(matches->item[2]));

                    if (!cdadmin_AuthLevelIsEnough(assignee, CDLevelModerator)) {
                        cdadmin_SendFailure(player, CD_CreateStringFromFormat("%s isn't a moderator",
                            CD_StringContent(matches->item[2])));

                        goto done;
                    }
                }

                size_t id      = atoi(CD_StringContent(matches->item[1]));
                size_t current = 0;

                CD_LIST_FOREACH(_tickets, it) {
                    if (current == id) {
                        CDATicket* ticket = (CDATicket*) CD_ListIteratorValue(it);

                        ticket->assignee = assignee;
                        ticket->status   = CDTicketAssigned;

                        CD_LIST_BREAK(_tickets);
                    }

                    current++;
                }

                cdadmin_SendSuccess(player, CD_CreateStringFromFormat("Ticket assigned to %s",
                    CD_StringContent(matches->item[2])));

                goto done;
            }

            if (CD_StringIsEqual(matches->item[1], "close")) {
                if (!matches->item[2]) {
                    cdadmin_SendUsage(player, CD_ADMIN_TICKET_MODERATOR_CLOSE_USAGE);
                    goto done;
                }

                size_t     id      = atoi(CD_StringContent(matches->item[1]));
                size_t     current = 0;
                CDATicket* ticket  = NULL;

                CD_LIST_FOREACH(_tickets, it) {
                    if (current == id) {
                        ticket = (CDATicket*) CD_ListIteratorValue(it);

                        CD_LIST_BREAK(_tickets);
                    }

                    current++;
                }

                if (ticket) {
                    CD_ListDelete(_tickets, (CDPointer) ticket);
                    cdadmin_DestroyTicket(ticket);

                    cdadmin_SendSuccess(player, CD_CreateStringFromFormat("Ticket %d closed", id));
                }
                else {
                    cdadmin_SendFailure(player, CD_CreateStringFromFormat("Ticket %d couldn't be found",
                        id));
                }

                goto done;
            }
        }
        else {
            if (!matches->item[2]) {
                cdadmin_SendUsage(player, CD_ADMIN_TICKET_PLAYER_USAGE);
                goto done;
            }

            CD_DO {
                CDRegexpMatches* old = matches;
                matches = CD_RegexpMatch(regexp, old->item[2]);
                CD_DestroyRegexpMatches(old);
            }

            if (CD_StringIsEqual(matches->item[1], "create")) {
                if (!matches->item[2]) {
                    cdadmin_SendUsage(player, CD_ADMIN_TICKET_PLAYER_CREATE_USAGE);
                    goto done;
                }

                if (CD_ListLength(_tickets) > _config.ticket.max) {
                    cdadmin_SendFailure(player, CD_CreateStringFromCString(
                        "There can't be more tickets at this moment, try again later"));

                    goto done;
                }

                CD_LIST_FOREACH(_tickets, it) {
                    if (((CDATicket*) CD_ListIteratorValue(it))->requester == player) {
                        cdadmin_SendFailure(player, CD_CreateStringFromCString(
                            "You can't have more than one ticket open at the same time"));

                        goto done;
                    }
                }

                CD_ListPush(_tickets, (CDPointer) cdadmin_CreateTicket(player, CD_CloneString(matches->item[2])));

                cdadmin_SendSuccess(player, CD_CreateStringFromCString(
                    "The ticket has been added, it will be taken care of as soon as possible."));

                goto done;
            }

            if (CD_StringIsEqual(matches->item[1], "status")) {
                CD_LIST_FOREACH(_tickets, it) {
                    CDATicket* ticket = (CDATicket*) CD_ListIteratorValue(it);

                    if (ticket->requester == player) {
                        if (ticket->assignee) {
                            cdadmin_SendResponse(player, CD_CreateStringFromFormat(
                                "Assigned to %s", CD_StringContent(ticket->assignee->username)));
                        }
                        else {
                            cdadmin_SendResponse(player, CD_CreateStringFromCString(
                                "Nobody is taking care of your ticket at the moment, please be patient"));
                        }

                        goto done;
                    }
                }

                cdadmin_SendFailure(player, CD_CreateStringFromCString("There are no tickets related to you"));

                goto done;
            }
        }
    }

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

    CD_ServerBroadcast(server, CD_CreateStringFromCString(MCCharset));

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
