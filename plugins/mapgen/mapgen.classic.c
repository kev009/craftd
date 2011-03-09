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

#include <math.h>
#include "noise/simplexnoise1234.h"
#include "mapgen.h"

static
MCBlockType
cdmg_LookupTypeMountain (float value)
{
    if (value < -0.50) {
        return MCGravel;
    }

    if (value < 0.0) {
        return MCDirt;
    }

    if (value < 0.30) {
        return MCStone;
    }

    if (value < 0.35) {
        return MCCoalOre;
    }

    if (value < 0.50) {
        return MCStone;
    }

    if (value < 0.55) {
        return MCIronOre;
    }

    return MCAir;
}

static
MCBlockType
cdmg_LookupTypeHills (float value)
{
    if (value < -0.50) {
        return MCAir;
    }

    if (value < -0.05) {
        return MCDirt;
    }

    if (value < 0.0) {
        return MCGravel;
    }

    if (value < 0.25) {
        return MCStone;
    }

    if (value < 0.40) {
        return MCAir;
    }

    if (value < 0.50) {
        return MCStone;
    }

    if (value < 0.58) {
        return MCDirt;
    }

    if (value < 0.62) {
        return MCAir;
    }

    if (value < 0.72) {
        return MCDirt;
    }

    if (value < 0.722) {
        return MCCoalOre;
    }

    if (value < 0.84) {
        return MCStone;
    }

    if (value < 0.85) {
        return MCIronOre;
    }

    return MCAir;
}

static
MCBlockType
cdmg_LookupTypeSealevel (float value)
{
    if (value < -0.07) {
        return MCSand;
    }

    if (value < 0.0) {
        return MCStone;
    }

    if (value < 0.07) {
        return MCDirt;
    }

    return MCSand;
}

static
MCBlockType
cdmg_LookupTypeUndergroundFirst (float value)
{
    if (value < -0.2) {
        return MCSand;
    }

    if (value < -0.19) {
        return MCAir;
    }

    if (value < -0.17) {
        return MCStone;
    }

    if (value < -0.15) {
        return MCDirt;
    }

    if (value < -0.05) {
        return MCAir;
    }

    if (value < 0.2) {
        return MCStone;
    }

    if (value < 0.25) {
        return MCAir;
    }

    if (value < 0.5) {
        return MCStone;
    }

    return MCSand;
}

static
MCBlockType
cdmg_LookupTypeUndergroundSecond (float value)
{
    return MCBedrock;
}

static
MCBlockType
cdmg_LookupTypeUndergroundThird (float value)
{
    return MCBedrock;
}

static
MCBlockType
cdmg_LookupTypeBottom (float value)
{
    return MCBedrock;
}

/**
 * Determine a block's type (sand, bedrock, ...)
 *
 * @param value a noise generated value [-1.0, 1.0]
 * @param height the altitude of the block
 *
 * @return the block type
 */
static
MCBlockType
cdmg_LookupType (float value, int height)
{
    if (height <= 1) {
        return cdmg_LookupTypeBottom(value);
    }

    if (height <= 20) {
        return cdmg_LookupTypeUndergroundThird(value);
    }

    if (height <= 35) {
        return cdmg_LookupTypeUndergroundSecond(value);
    }

    if (height <= 50) {
        return cdmg_LookupTypeUndergroundFirst(value);
    }

    if (height <= 66) {
        return cdmg_LookupTypeSealevel(value);
    }

    if (height <= 75) {
        return cdmg_LookupTypeHills(value);
    }

    // really high
    return cdmg_LookupTypeMountain(value);
}

static
MCBlockType
cdmg_BlockType (int chunkX, int chunkZ, int x, int y, int z)
{
    float totalX = ((((float) chunkX) * 16.0) + ((float) x)) * 0.053;
    float totalZ = ((((float) chunkZ) * 16.0) + ((float) z)) * 0.053;
    float totalY = ((float) y) * 0.015;

    float val = snoise3(totalX, totalY, totalZ);

    val += (0.5   * (snoise3(totalX * 2.0, totalY * 2.0, totalZ * 2.0)));
    val += (0.25  * (snoise3(totalX * 4.0, totalY * 4.0, totalZ * 4.0)));
    val += (0.125 * (snoise3(totalX * 8.0, totalY * 8.0, totalZ * 8.0)));

    val /= (1.0 + 0.5 + 0.25 + 0.125);

    return cdmg_LookupType(val, y);
}

static
void
cdmg_GenerateChunk (int chunkX, int chunkZ, CDMapgenData* data)
{
    int lightValue = 0x0F;
    int blockHeight[16][16];

    // step 1: generate the height map
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            float totalX;
            float totalZ;
            float block;

            totalX  = ((((float) chunkX) * 16.0) + ((float) x)) * 0.015;
            totalZ  = ((((float) chunkZ) * 16.0) + ((float) z)) * 0.015;

            block = snoise2(totalX, totalZ);

            block += (snoise2(totalX * 2.0, totalZ * 2.0) * 0.5);
            block += (snoise2(totalX * 4.0, totalZ * 4.0) * 0.25);
            block += (snoise2(totalX * 8.0, totalZ * 8.0) * 0.125);

            block /= (1.0 + 0.5 + 0.25 + 0.125);
            // noise tends to be of repetitive height (-1.0 -> 1.0)
            // which would tend to result in hills of the same height
            // so we need to multiply it by a cos/sin to give more
            // randomness  to it
            block *= (cos(totalZ)+cos(totalX));

            // fill the minimum height, we get flat bottom lakes like that
            block = (block < -0.40 ? -0.40 : block);
            block = ((block / 2.5)*48.0 + 64.0);

            blockHeight[(int) x][(int) z] = block;
        }
    }

    // step 2: fill the chunk with sand according to the height map
    memset(data->blocks, 0, 32768);
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            for (int y = 0; y < blockHeight[x][z]; y++) {
                data->blocks[y + (z * 128) + (x * 128 * 16)] = cdmg_BlockType(chunkX, chunkZ, x, y, z);
            }

            data->heightMap[x + (z * 16)] = blockHeight[x][z]; // max height is 1

            for (int y = 1; y < 128; y++) {
                // full light for first 2 layers
                data->skyLight[((z * 128) + (x * 128 * 16) + y) / 2] = (lightValue | (lightValue << 4));
            }
        }
    }

    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            // step 3: replace top with grass
            int y = data->heightMap[x + (z * 16)];

            while (data->blocks[y + (z * 128) + (x * 128 * 16)] == 0 && y > 64) {
                y--;
            }

            switch (data->blocks[(y) + (z * 128) + (x * 128 * 16)]) {
                case MCAir:
                case MCSand: {
                    break;
                }

                default: {
                    data->blocks[(y + 1) + (z * 128) + (x * 128 * 16)] = MCGrass;
                }
            }

            // step 4: flood with water at level 64
            y = 62;
            while (data->blocks[y + (z * 128) + (x * 128 * 16)] == 0) {
                data->blocks[y + (z * 128) + (x * 128 * 16)] = MCStationaryWater;
                y--;
            }
        }
    }
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->name = CD_CreateStringFromCString("Mapgen.classic");

    CD_EventRegister(self->server, "Mapgen.generateChunk", cdmg_GenerateChunk);

    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_EventUnregister(self->server, "Mapgen.generateChunk", cdmg_GenerateChunk);

    return true;
}
