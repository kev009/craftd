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

if (CD_StringIsEqual(matches->item[1], "ticket")) {
    if (cdadmin_AuthLevelIsEnough(player, CDLevelModerator)) {
        if (!matches->item[2]) {
            cdadmin_SendUsage(player, CD_ADMIN_TICKET_MODERATOR_USAGE);
            goto done;
        }

        DO {
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

            DO {
                CDRegexpMatches* old = matches;
                matches = CD_RegexpMatchString("^(\\d+)\\s+(.+)$", CDRegexpNone, old->item[2]);
                CD_DestroyRegexpMatches(old);
            }

            if (!matches) {
                cdadmin_SendUsage(player, CD_ADMIN_TICKET_MODERATOR_ASSIGN_USAGE);
                goto done;
            }

            CDPlayer* assignee = NULL;

            if (!CD_HashHasKey(server->players, CD_StringContent(matches->item[2]))) {
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

        DO {
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
