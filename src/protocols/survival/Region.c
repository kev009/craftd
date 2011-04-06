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

#include <craftd/protocols/survival/Region.h>

bool
SV_IsCoordInRadius (SVChunkPosition* coord, SVChunkPosition* centerCoord, int radius)
{
    return (coord->x >= centerCoord->x - radius &&
            coord->x <= centerCoord->x + radius &&
            coord->z >= centerCoord->z - radius &&
            coord->z <= centerCoord->z + radius);
}

bool
SV_IsDistanceGreater (SVPrecisePosition a, SVPrecisePosition b, int maxDistance)
{
    return (abs( a.x - b.x ) > maxDistance ||
            abs( a.y - b.y ) > maxDistance ||
            abs( a.z - b.z ) > maxDistance);
}

SVRelativePosition
SV_RelativeMove (SVPrecisePosition* a, SVPrecisePosition* b)
{
    SVAbsolutePosition absoluteA = SV_PrecisePositionToAbsolutePosition(*a);
    SVAbsolutePosition absoluteB = SV_PrecisePositionToAbsolutePosition(*b);

    return (SVRelativePosition) {
        .x = absoluteA.x - absoluteB.x,
        .y = absoluteA.y - absoluteB.y,
        .z = absoluteA.z - absoluteB.z
    };
}

void
SV_RegionBroadcastPacket (SVPlayer* player, SVPacket* packet)
{
    CDList* seenPlayers = (CDList*) CD_DynamicGet(player, "Player.seenPlayers");

    CD_LIST_FOREACH(seenPlayers, it) {
        if (player == (SVPlayer*) CD_ListIteratorValue(it)) {
            continue;
        }

        SV_PlayerSendPacket((SVPlayer*) CD_ListIteratorValue(it), packet);
    }
}


