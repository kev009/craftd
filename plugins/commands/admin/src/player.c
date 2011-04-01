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

if (CD_StringIsEqual(matches->item[1], "player")) {
    if (!matches->item[2]) {
        cdadmin_SendUsage(player, CD_ADMIN_PLAYER_USAGE);

        goto done;
    }

    if (!cdadmin_AuthLevelIsEnoughWithMessage(player, CDLevelRegisteredUser)) {
        goto done;
    }

    DO {
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

        DO {
            CDRegexpMatches* old = matches;
            matches = CD_RegexpMatch(regexp, old->item[2]);
            CD_DestroyRegexpMatches(old);
        }

        if (CD_HashHasKey(server->players, CD_StringContent(matches->item[1]))) {
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
