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

#include <craftd/protocols/survival.h>

static struct {
    const char* seed;
} _config;

#include "helpers.c"

static
bool
cdclassic_GenerateLevel (CDServer* server, SVWorld* world, const char* seed)
{
    if (seed == NULL) {
        seed = _config.seed;
    }

    world->spawnPosition = (SVBlockPosition) {
        .x = 0,
        .y = 120,
        .z = 0
    };

    return true;
}

static
bool
cdclassic_GenerateChunk (CDServer* server, SVWorld* world, int x, int z, SVChunk* data, const char* seed)
{
    memset(data, 0, sizeof(*data));

    if (seed == NULL) {
        seed = _config.seed;
    }

    cdclassic_GenerateHeightMap(data, x, z);
    cdclassic_GenerateFilledChunk(data, x, z, SVStone);
    cdclassic_DigCaves(data, x, z);
    cdclassic_ErodeLandscape(data, x, z);
    cdclassic_AddMinerals(data, x, z);
    cdclassic_AddSediments(data, x, z);
    cdclassic_FloodWithWater(data, x, z, 64);
    cdclassic_BedrockGround(data, x, z);
    cdclassic_GenerateSkyLight(data, x, z);

    return true;
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->description = CD_CreateStringFromCString("Classic Mapgen");

    DO { // Initiailize config cache
        _config.seed = "^_^";

        J_DO {
            J_STRING(self->config, "seed", _config.seed);
        }
    }


    CD_EventRegister(self->server, "Mapgen.level", cdclassic_GenerateLevel);
    CD_EventRegister(self->server, "Mapgen.chunk", cdclassic_GenerateChunk);

    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_EventUnregister(self->server, "Mapgen.level", cdclassic_GenerateLevel);
    CD_EventUnregister(self->server, "Mapgen.chunk", cdclassic_GenerateChunk);

    return true;
}
