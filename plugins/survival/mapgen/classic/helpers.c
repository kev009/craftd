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

#include <math.h>
#include <noise/simplexnoise1234.h>

static
float
cdclassic_Multifractal2d (float x, float z, float lacunarity, int octaves)
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
cdclassic_Multifractal3d (float x, float y, float z, float lacunarity, int octaves)
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
cdclassic_GenerateHeightMap (SVChunk* chunk, int chunkX, int chunkZ)
{
    // step 1: generate the height map
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            float totalX = ((((float) chunkX) * 16.0) + ((float) x)) * 0.00155; // magic
            float totalZ = ((((float) chunkZ) * 16.0) + ((float) z)) * 0.00155;

            chunk->heightMap[x + (z * 16)] = cdclassic_Multifractal2d(totalX, totalZ, 2.7, 20) * 13.5 + 55;
        }
    }
}

static
void
cdclassic_GenerateFilledChunk (SVChunk* chunk, int chunkX, int chunkZ, SVBlockType blockType)
{
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            for (int y = 0; y < chunk->heightMap[x + (z * 16)] && y < 128; y++) {
                // stone is the basis of SV worlds
                chunk->blocks[y + (z * 128) + (x * 128 * 16)] = blockType;
            }
        }
    }
}

static
void
cdclassic_GenerateSkyLight (SVChunk* chunk, int chunkX, int chunkZ)
{
    int lightValue = 0x0F;

    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            for (int y = chunk->heightMap[x + (z * 16)]; y < 128; y++) {
                if (y % 2) {
                    chunk->skyLight[((z * 128) + (x * 128 * 16) + y) / 2] |= (lightValue << 4);
                }
                else {
                    chunk->skyLight[((z * 128) + (x * 128 * 16) + y) / 2] |= (lightValue);
                }
            }
        }
    }
}

static
void
cdclassic_DigCaves (SVChunk* chunk, int chunkX, int chunkZ)
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
                        chunk->blocks[y + (z * 128) + (x * 128 * 16)] = SVLava;
                    }
                    else {
                        chunk->blocks[y + (z * 128) + (x * 128 * 16)] = SVAir;
                    }
                }
            }

            for (int y = 54; y < chunk->heightMap[x + (z * 16)] - 4; y++) {
                float result = (snoise3(totalX / 12.0, y / 12.0, totalZ / 12.0)
                    + (0.5 * snoise3(totalX / 24.0, y / 24.0, totalZ / 24.0))) / 1.5;

                if (result > 0.45) {
                    chunk->blocks[y + (z * 128) + (x * 128 * 16)] = SVAir;
                }
            }

            // update height map
            for (int y = chunk->heightMap[x + (z * 16)]; y > 0 && chunk->blocks[y + (z * 128) + (x * 128 * 16)] == SVAir; y--) {
                chunk->heightMap[x + (z * 16)] = y;
            }
        }
    }
}

static
void
cdclassic_ErodeLandscape (SVChunk* chunk, int chunkX, int chunkZ)
{
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            float totalX = ((((float) chunkX) * 16.0) + ((float) x));
            float totalZ = ((((float) chunkZ) * 16.0) + ((float) z));

            // erosion (over ground)
            for (int y = 65; y < chunk->heightMap[x + (z * 16)]; y++) {
                float result = (snoise3(totalX / 40.0, y / 50.0, totalZ / 40.0)
                    + (0.5 * snoise3(totalX / 80.0, y / 100.0, totalZ / 80.0))) / 1.5;

                if (result > 0.50) {
                    // cave
                    chunk->blocks[y + (z * 128) + (x * 128 * 16)] = SVAir;
                }
            }

            // update height map
            int y = chunk->heightMap[x + (z * 16)];
            while (y > 0 && chunk->blocks[y + (z * 128) + (x * 128 * 16)] == SVAir) {
                chunk->heightMap[x + (z * 16)] = y--;
            }
        }
    }
}

static
void
cdclassic_AddSediments (SVChunk* chunk, int chunkX, int chunkZ)
{
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            // step 3: replace top with grass / the higher, the less blocks / 0 to 3

            int y              = chunk->heightMap[x + (z * 16)];
            int sedimentHeight = (128 - chunk->heightMap[x + (z * 16)]) / 21; // 0 - 3 blocks

            if (y < 64) {
                for (int i = 0; i < sedimentHeight; i++) {
                    // sand underwater
                    chunk->blocks[y + (z * 128) + (x * 128 * 16) + i] = SVSand;
                }
            }
            else if (y >= 64 && sedimentHeight > 0) {
                for (int i = 0; i < sedimentHeight - 1; i++, sedimentHeight--) {
                    chunk->blocks[y + (z * 128) + (x * 128 * 16) + i] = SVDirt;
                }

                chunk->blocks[y + (z * 128) + (x * 128 * 16) + sedimentHeight - 1] = SVGrass;
            }

            chunk->heightMap[x + (z * 16)] += sedimentHeight;
        }
    }
}

static
void
cdclassic_FloodWithWater (SVChunk* chunk, int chunkX, int chunkZ, int8_t waterLevel)
{
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            // step 4: flood with water at level 64
            for (int y = waterLevel; chunk->blocks[y + (z * 128) + (x * 128 * 16)] == SVAir; y--) {
                chunk->blocks[y + (z * 128) + (x * 128 * 16)] = SVWater;
            }
        }
    }
}

static
void
cdclassic_BedrockGround (SVChunk* chunk, int chunkX, int chunkZ)
{
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            chunk->blocks[0 + (z * 128) + (x*128*16)] = SVBedrock;
            chunk->blocks[1 + (z * 128) + (x*128*16)] = SVBedrock;
            chunk->heightMap[x+(z * 16)]              = CD_Max(chunk->heightMap[x + (z * 16)], 2);
        }
    }
}

static
void
cdclassic_AddMineral (SVChunk* chunk, int x, int z, int y, float totalX, float totalZ, float totalY, SVBlockType blockType, float probability)
{
    if (snoise4(totalX, totalY, totalZ, blockType) + 1.0 <= (0.25 * probability)) {
        chunk->blocks[y + (z * 128) + (x * 128 * 16)] = blockType;
    }
}

static
void
cdclassic_AddMinerals (SVChunk* chunk, int chunkX, int chunkZ)
{
    for (int x = 0; x < 16; x++) {
        float totalX = ((((float) chunkX) * 16.0) + ((float) x)) * 0.075;

        for (int z = 0; z < 16; z++) {
            float totalZ = ((((float) chunkZ) * 16.0) + ((float) z)) * 0.075;

            for (int y = 2; y < chunk->heightMap[x + (z * 16)]; y++) {
                if (chunk->blocks[y + (z * 128) + (x * 128 * 16)] == SVAir) {
                    continue;
                }

                float totalY = (((float) y)) * 0.075;

                cdclassic_AddMineral(chunk, x, z, y, totalX, totalZ, totalY, SVCoalOre, 1.3);
                cdclassic_AddMineral(chunk, x, z, y, totalX, totalZ, totalY, SVDirt, 2.5);
                cdclassic_AddMineral(chunk, x, z, y, totalX, totalZ, totalY, SVGravel, 2.5);

                // 5 blocks under the surface
                if (y < chunk->heightMap[x + (z * 16)] - 5) {
                    cdclassic_AddMineral(chunk, x, z, y, totalX, totalZ, totalY, SVIronOre, 1.15);
                }

                if (y < 40) {
                    cdclassic_AddMineral(chunk, x, z, y, totalX, totalZ, totalY, SVLapisLazuliOre, 0.80);
                    cdclassic_AddMineral(chunk, x, z, y, totalX, totalZ, totalY, SVGoldOre, 0.85);
                }

                if (y < 20) {
                    cdclassic_AddMineral(chunk, x, z, y, totalX, totalZ, totalY, SVDiamondOre, 0.80);
                    cdclassic_AddMineral(chunk, x, z, y, totalX, totalZ, totalY, SVRedstoneOre, 1.2);
                }
            }
        }
    }
}
