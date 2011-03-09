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

static
float
cdmg_Multifractal2d (float x, float z, float lacunarity, int octaves)
{
    float exponentArray[octaves];
    float frequency = 1.0;
    float H         = 0.25;
    float offset    = 0.7;
    float weight    = 1.0;
    float result    = 0.0;

    for (int i = 0; i < octaves; i++) {
        exponentArray[i] = pow(frequency, -H);
        frequency       *= lacunarity;
    }

    for (int i = 0; i < octaves; i++) {
        float _signal = (snoise2(x, z) + offset) * exponentArray[i];

        if (weight > 1.0) {
            weight = 1.0;
        }

        result += (weight * _signal);
        weight *= _signal;
        x      *= lacunarity;
        z      *= lacunarity;
    }

    return result;
}

static
float
cdmg_Multifractal3d (float x, float y, float z, float lacunarity, int octaves)
{
    float exponentArray[octaves];
    float frequency   = 1.0;
    float H           = 0.25;
    float offset      = 0.7;
    float weight      = 1.0;
    float totalWeight = 0.0;
    float result      = 0.0;

    for (int i = 0; i < octaves; i++) {
        exponentArray[i] = pow(frequency, -H);
        frequency       *= lacunarity;
    }

    for (int i = 0; i < octaves; i++) {
        float _signal = (snoise3(x, y, z) + offset) * exponentArray[i];

        if (weight > 1.0) {
            weight = 1.0;
        }

        result      += (weight * _signal);
        totalWeight += (exponentArray[i] * weight);

        weight *= _signal;
        x      *= lacunarity;
        z      *= lacunarity;
    }

    return result;
}

static
void
cdmg_GenerateHeightMap (MCChunkData* chunkData, int chunkX, int chunkZ)
{
    // step 1: generate the height map
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            float totalX = ((((float) chunkX) * 16.0) + ((float) x)) * 0.00155; // magic
            float totalZ = ((((float) chunkZ) * 16.0) + ((float) z)) * 0.00155;

            chunkData->heightMap[x + (z * 16)] = cdmg_Multifractal2d(totalX, totalZ, 2.7, 20) * 13.5 + 55;
        }
    }
}

static
void
cdmg_GenerateFilledChunk (MCChunkData* chunkData, int chunkX, int chunkZ, MCBlockType blockType)
{
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            for (int y = 0; y < chunkData->heightMap[x + (z * 16)] && y < 128; y++) {
                // stone is the basis of MC worlds
                chunkData->blocks[y + (z * 128) + (x * 128 * 16)] = blockType;
            }
        }
    }
}

static
void
cdmg_GenerateSkyLight (MCChunkData* chunkData, int chunkX, int chunkZ)
{
    int lightValue = 0x0F;

    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            for (int y = chunkData->heightMap[x + (z * 16)]; y < 128; y++) {
                if (y % 2) {
                    chunkData->skyLight[((z * 128) + (x * 128 * 16) + y) / 2] |= (lightValue << 4);
                }
                else {
                    chunkData->skyLight[((z * 128) + (x * 128 * 16) + y) / 2] |= (lightValue);
                }
            }
        }
    }
}

static
void
cdmg_DigCaves (MCChunkData* chunkData, int chunkX, int chunkZ)
{
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            float totalX = ((((float) chunkX) * 16.0) + ((float) x));
            float totalZ = ((((float) chunkZ) * 16.0) + ((float) z));

            for (int y = 0; y < 54; y++) {
                float result  = (snoise3(totalX / 12.0, y / 12.0, totalZ / 12.0)
                    + (0.5 * snoise3(totalX / 24.0, y / 24.0, totalZ / 24.0))) / 1.5;

                if (result > 0.35) {
                    if (y < 16) {
                        chunkData->blocks[y + (z * 128) + (x * 128 * 16)] = MCLava;
                    }
                    else {
                        // cave
                        chunkData->blocks[y + (z * 128) + (x * 128 * 16)] = MCAir;
                    }
                }
            }

            for (int y = 54; y < chunkData->heightMap[x + (z * 16)] - 4; y++) {
                float result = (snoise3(totalX / 12.0, y / 12.0, totalZ / 12.0)
                    + (0.5 * snoise3(totalX / 24.0, y / 24.0, totalZ / 24.0))) / 1.5;

                if (result > 0.45) {
                    // cave
                    chunkData->blocks[y + (z * 128) + (x * 128 * 16)] = MCAir;
                }
            }

            // update height map
            for (int y = chunkData->heightMap[x + (z * 16)]; y > 0 && chunkData->blocks[y + (z * 128) + (x * 128 * 16)] == MCAir; y--) {
                chunkData->heightMap[x + (z * 16)] = y;
            }
        }
    }
}

static
void
cdmg_ErodeLandscape (MCChunkData* chunkData, int chunkX, int chunkZ)
{
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            float totalX = ((((float) chunkX) * 16.0) + ((float) x));
            float totalZ = ((((float) chunkZ) * 16.0) + ((float) z));

            // erosion (over ground)
            for (int y = 65; y < chunkData->heightMap[x + (z * 16)]; y++) {
                float result = (snoise3(totalX / 40.0, y / 50.0, totalZ / 40.0)
                    + (0.5 * snoise3(totalX / 80.0, y / 100.0, totalZ / 80.0))) / 1.5;

                if (result > 0.50) {
                    // cave
                    chunkData->blocks[y + (z * 128) + (x * 128 * 16)] = MCAir;
                }
            }

            // update height map
            int y = chunkData->heightMap[x + (z * 16)];
            while (y > 0 && chunkData->blocks[y + (z * 128) + (x * 128 * 16)] == MCAir) {
                chunkData->heightMap[x + (z * 16)] = y--;
            }
        }
    }
}

static
void
cdmg_AddSediments (MCChunkData* chunkData, int chunkX, int chunkZ)
{
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            // step 3: replace top with grass / the higher, the less blocks / 0 to 3

            int y              = chunkData->heightMap[x + (z * 16)];
            int sedimentHeight = (128 - chunkData->heightMap[x + (z * 16)]) / 21; // 0 - 3 blocks

            if (y < 64) {
                for (int i = 0; i < sedimentHeight; i++) {
                    // sand underwater
                    chunkData->blocks[y + (z * 128) + (x * 128 * 16) + i] = MCSand;
                }
            }
            else if (y >= 64 && sedimentHeight > 0) {
                for (int i = 0; i < sedimentHeight - 1; i++, sedimentHeight--) {
                    chunkData->blocks[y + (z * 128) + (x * 128 * 16) + i] = MCDirt;
                }

                chunkData->blocks[y + (z * 128) + (x * 128 * 16) + sedimentHeight - 1] = MCGrass;
            }

            chunkData->heightMap[x + (z * 16)] += sedimentHeight;
        }
    }
}

static
void
cdmg_FloodWithWater (MCChunkData* chunkData, int chunkX, int chunkZ, int8_t waterLevel)
{
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            // step 4: flood with water at level 64
            for (int y = waterLevel; chunkData->blocks[y + (z * 128) + (x * 128 * 16)] == MCAir; y--) {
                chunkData->blocks[y + (z * 128) + (x * 128 * 16)] = MCWater;
            }
        }
    }
}

static
void
cdmg_BedrockGround (MCChunkData* chunkData, int chunkX, int chunkZ)
{
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            chunkData->blocks[0 + (z * 128) + (x*128*16)] = MCBedrock;
            chunkData->blocks[1 + (z * 128) + (x*128*16)] = MCBedrock;
            chunkData->heightMap[x+(z * 16)]              = CD_Max(chunkData->heightMap[x + (z * 16)], 2);
        }
    }
}

static
void
cdmg_AddMineral (MCChunkData* chunkData, int x, int z, int y, float totalX, float totalZ, float totalY, MCBlockType blockType, float probability)
{
    if (snoise4(totalX, totalY, totalZ, blockType) + 1.0 <= (0.25 * probability)) {
        chunkData->blocks[y + (z * 128) + (x * 128 * 16)] = blockType;
    }
}

static
void
cdmg_AddMinerals (MCChunkData* chunkData, int chunkX, int chunkZ)
{
    for (int x = 0; x < 16; x++) {
        float totalX = ((((float) chunkX) * 16.0) + ((float) x)) * 0.075;

        for (int z = 0; z < 16; z++) {
            float totalZ = ((((float) chunkZ) * 16.0) + ((float) z)) * 0.075;

            for (int y = 2; y < chunkData->heightMap[x + (z * 16)]; y++) {
                if (chunkData->blocks[y + (z * 128) + (x * 128 * 16)] == MCAir) {
                    continue;
                }

                float totalY = (((float) y)) * 0.075;

                cdmg_AddMineral(chunkData, x, z, y, totalX, totalZ, totalY, MCCoalOre, 1.3);
                cdmg_AddMineral(chunkData, x, z, y, totalX, totalZ, totalY, MCDirt, 2.5);
                cdmg_AddMineral(chunkData, x, z, y, totalX, totalZ, totalY, MCGravel, 2.5);

                // 5 blocks under the surface
                if (y < chunkData->heightMap[x + (z * 16)] - 5) {
                    cdmg_AddMineral(chunkData, x, z, y, totalX, totalZ, totalY, MCIronOre, 1.15);
                }

                if (y < 40) {
                    cdmg_AddMineral(chunkData, x, z, y, totalX, totalZ, totalY, MCLapisLazuliOre, 0.80);
                    cdmg_AddMineral(chunkData, x, z, y, totalX, totalZ, totalY, MCGoldOre, 0.85);
                }

                if (y < 20) {
                    cdmg_AddMineral(chunkData, x, z, y, totalX, totalZ, totalY, MCDiamondOre, 0.80);
                    cdmg_AddMineral(chunkData, x, z, y, totalX, totalZ, totalY, MCRedstoneOre, 1.2);
                }
            }
        }
    }
}

static
bool
cdmg_GenerateChunk (CDServer* server, int chunkX, int chunkZ, MCChunkData* data, CDString* seed)
{
    memset(data, 0, sizeof(MCChunkData));

    cdmg_GenerateHeightMap(data, chunkX, chunkZ);
    cdmg_GenerateFilledChunk(data, chunkX, chunkZ, MCStone);
    cdmg_DigCaves(data, chunkX, chunkZ);
    cdmg_ErodeLandscape(data, chunkX, chunkZ);
    cdmg_AddMinerals(data, chunkX, chunkZ);
    cdmg_AddSediments(data, chunkX, chunkZ);
    cdmg_FloodWithWater(data, chunkX, chunkZ, 64);
    cdmg_BedrockGround(data, chunkX, chunkZ);
    cdmg_GenerateSkyLight(data, chunkX, chunkZ);

    return false;
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
