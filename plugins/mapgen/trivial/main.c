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

#include <beta/minecraft.h>
#include <beta/World.h>

static
bool
cdtrivial_GenerateLevel (CDServer* server, CDWorld* world)
{
    world->spawnPosition = (MCBlockPosition) {
        .x = 0,
        .y = 120,
        .z = 0
    };

    return true;
}

static
bool
cdtrivial_GenerateChunk (CDServer* server, int x, int z, MCChunk* data, const char* seed)
{
    int lightValue = CD_Max(0x0F - ABS(x) - ABS(z), 0);

    // this should only put 1 layer of bedrock
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            data->blocks[(z * 128) + (x * 128 * 16)] = MCBedrock; // one layer bedrock
            data->heightMap[x + (z * 16)] = 1; // max height is 1

            for (int y = 1; y < 128; y++) {
                // full light for first 2 layers
                data->skyLight[((z * 128) + (x * 128 * 16) + y) / 2] = (lightValue | (lightValue << 4));
            }
        }
    }

    return false;
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->description = CD_CreateStringFromCString("Trivial Mapgen");

    CD_EventRegister(self->server, "Mapgen.level", cdtrivial_GenerateLevel);
    CD_EventRegister(self->server, "Mapgen.chunk", cdtrivial_GenerateChunk);

    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_EventUnregister(self->server, "Mapgen.level", cdtrivial_GenerateLevel);
    CD_EventUnregister(self->server, "Mapgen.chunk", cdtrivial_GenerateChunk);

    return true;
}
