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

#include "Arithmetic.h"


float multifractal_2d(float x, float z, float lacunarity, int octaves)
{
	float *exponent_array = calloc(octaves, sizeof(float));;
	float frequency = 1.0;
	float H = 0.25;
	float offset = 0.7;
	int i;
	float weight = 1.0;

	float res = 0.0;

	for (i = 0 ; i < octaves ; i++) {
		exponent_array[i] = pow(frequency, -H);
		frequency *= lacunarity;
	}

	for (i = 0 ; i < octaves ; i++) {
		float signl = (snoise2(x, z) + offset) * exponent_array[i];
		if (weight > 1.0) weight = 1.0;
		res += (weight * signl);
		weight *= signl;
		x *= lacunarity;
		z *= lacunarity;
	}
	free(exponent_array);
	return res;
}

float multifractal_3d(float x, float y, float z, float lacunarity, int octaves)
{
	float *exponent_array = calloc(octaves, sizeof(float));;
	float frequency = 1.0;
	float H = 0.25;
	float offset = 0.7;
	int i;
	float weight = 1.0;
	float total_weight = 0.0;

	float res = 0.0;

	for (i = 0 ; i < octaves ; i++) {
		exponent_array[i] = pow(frequency, -H);
		frequency *= lacunarity;
	}

	for (i = 0 ; i < octaves ; i++) {
		float signl = (snoise3(x, y, z) + offset) * exponent_array[i];
		if (weight > 1.0) weight = 1.0;
		res += (weight * signl);
		total_weight += (exponent_array[i] * weight);

		weight *= signl;
		x *= lacunarity;
		z *= lacunarity;
	}
	free(exponent_array);

	return res /*/ total_weight*/;
}

static void generate_heightmap(MCChunkData *ch, int chunkX, int chunkZ)
{
	int x, z;
	float lacunarity;
	float a;

	/* step 1: generate the height map */
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			float total_x, total_z;
			total_x = ((((float)chunkX)*16.0) + ((float)x)) * 0.00155; /* magic */
			total_z = ((((float)chunkZ)*16.0) + ((float)z)) * 0.00155;
			lacunarity = (((cos(total_x/5.0) + cos(total_z/5.0))/4.0)+1.0);

			a = multifractal_2d(total_x, total_z, 2.7, 20); /* magic settings */
			a = a * 13.5 + 55; /* magic settings */

			ch->heightMap[x + (z*16)] = a;
		}
	}
}

static void generate_filled_chunk(MCChunkData *ch, int chunkX, int chunkZ, char block_type)
{
	int x, y, z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			for (y = 0 ; y < ch->heightMap[x + (z*16)] && y < 128 ; y++) {
				ch->blocks[y + (z*128) + (x*128*16)] = block_type; /* stone is the basis of MC worlds */
			}
		}
	}
}

static void generate_skyLight(MCChunkData *ch, int chunkX, int chunkZ)
{
	int light_val = 0x0F;
	int x, y, z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			for (y = ch->heightMap[x+(z*16)] ; y < 128 ; y++) {
				if (y%2) {
					ch->skyLight[((z*128) + (x*128*16)+y)/2] |= (light_val << 4);
				} else {
					ch->skyLight[((z*128) + (x*128*16)+y)/2] |= (light_val);
				}
			}
		}
	}
}

static void dig_caves(MCChunkData *ch, int chunkX, int chunkZ)
{
	float res;
	int x, y, z;
	float total_x, total_z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			total_x = ((((float)chunkX)*16.0) + ((float)x));
			total_z = ((((float)chunkZ)*16.0) + ((float)z));
			for (y = 0 ; y < 54 ; y++) {
				res = snoise3(total_x/12.0, y/12.0, total_z/12.0);
				res += (0.5 * snoise3(total_x/24.0, y/24.0, total_z/24.0));
				res /= 1.5;
				if (res > 0.35) {
					if (y < 16)
						ch->blocks[y + (z*128) + (x*128*16)] = MCLava; /* lava */
					else
						ch->blocks[y + (z*128) + (x*128*16)] = MCAir; /* cave */
				}
			}
			for (y = 54 ; y < ch->heightMap[x+(z*16)] - 4 ; y++) {
				res = snoise3(total_x/12.0, y/12.0, total_z/12.0);
				res += (0.5 * snoise3(total_x/24.0, y/24.0, total_z/24.0));
				res /= 1.5;
				if (res > 0.45) {
					ch->blocks[y + (z*128) + (x*128*16)] = MCAir; /* cave */
				}
			}
			/* update height map */
			y = ch->heightMap[x+(z*16)];
			while (y > 0 && ch->blocks[y + (z*128) + (x*128*16)] == MCAir) {
				ch->heightMap[x+(z*16)] = y;
				y--;
			}
		}
	}
}

static void erode_landscape(MCChunkData *ch, int chunkX, int chunkZ)
{
	float res;
	int x, y, z;
	float total_x, total_z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			total_x = ((((float)chunkX)*16.0) + ((float)x));
			total_z = ((((float)chunkZ)*16.0) + ((float)z));

			/* erosion (over ground) */
			for (y = 65 ; y < ch->heightMap[x+(z*16)] ; y++) {
				res = snoise3(total_x/40.0, y/50.0, total_z/40.0);
				res += (0.5 * snoise3(total_x/80.0, y/100.0, total_z/80.0));
				res /= 1.5;
				if (res > 0.50)
					ch->blocks[y + (z*128) + (x*128*16)] = MCAir; /* cave */
			}
			/* update height map */
			y = ch->heightMap[x+(z*16)];
			while (y > 0 && ch->blocks[y + (z*128) + (x*128*16)] == MCAir) {
				ch->heightMap[x+(z*16)] = y;
				y--;
			}
		}
	}
}

static void add_sediments(MCChunkData *ch, int chunkX, int chunkZ)
{
	int x, y, z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			/* step 3: replace top with grass / the higher, the less blocks / 0 to 3 */
			y = ch->heightMap[x+(z*16)];
			int sediment_height = (128 - ch->heightMap[x+(z*16)])/21; /* 0 - 3 blocks */
			int i;
			if (y < 64) {
				for (i = 0 ; i < sediment_height ; i++) {
					ch->blocks[y + (z*128) + (x*128*16)+i] = MCSand; /* sand underwater */
				}
			} else if (y >= 64 && sediment_height > 0) {
				for (i = 0 ; i < sediment_height-1 ; i++) {
					ch->blocks[y + (z*128) + (x*128*16)+i] = MCDirt;
					sediment_height--;
				}
				ch->blocks[y + (z*128) + (x*128*16)+sediment_height-1] = MCGrass; /* grass */
			}
			ch->heightMap[x+(z*16)] += sediment_height;
		}
	}
}

static void flood_with_water(MCChunkData *ch, int chunkX, int chunkZ, char water_level)
{
	int x, y, z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {

			/* step 4: flood with water at level 64 */
			y = water_level;
			while (ch->blocks[y + (z*128) + (x*128*16)] == MCAir) {
				ch->blocks[y + (z*128) + (x*128*16)] = MCWater; /* water */
				y--;
			}
		}
	}
}

static void bedrock_ground(MCChunkData *ch, int chunkX, int chunkZ)
{
	int x, z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			ch->blocks[0 + (z*128) + (x*128*16)] = MCBedrock; // bedrock
			ch->blocks[1 + (z*128) + (x*128*16)] = MCBedrock; // bedrock
			ch->heightMap[x+(z*16)] = CD_Max(ch->heightMap[x+(z*16)], 2);
		}
	}
}

static void add_mineral(MCChunkData *ch, int x, int z, int y, float total_x, float total_z, float total_y, char block_type, float probability)
{
	float res;
	//res = snoise3(total_x + (block_type * 128), total_y + (block_type * 128), total_z + (block_type * 128));
	res = snoise4(total_x, total_y, total_z, block_type);
	if (res+1.0 <= (0.25*probability)) {
		ch->blocks[y + (z*128) + (x*128*16)] = block_type;
	}
	
}

static void add_minerals(MCChunkData *ch, int chunkX, int chunkZ)
{
	int x, y, z;
	float total_x, total_z, total_y;
	for (x = 0 ; x < 16 ; x++) {
		total_x = ((((float)chunkX)*16.0) + ((float)x))* 0.075;
		for (z = 0 ; z < 16 ; z++) {
			total_z = ((((float)chunkZ)*16.0) + ((float)z))* 0.075;
			for (y = 2 ; y < ch->heightMap[x+(z*16)] ; y++) {
				if (ch->blocks[y + (z*128) + (x*128*16)] == 0)
					continue;

				total_y = (((float)y))* 0.075;
				/* coal (16) */
				add_mineral(ch, x, z, y, total_x, total_z, total_y, MCCoalOre, 1.3);
				/* dirt (3) */
				add_mineral(ch, x, z, y, total_x, total_z, total_y, MCDirt, 2.5);
				/* gravel (13), 10 % */
				add_mineral(ch, x, z, y, total_x, total_z, total_y, MCGravel, 2.5);

				/* 5 blocks under the surface */
				if (y < ch->heightMap[x+(z*16)] - 5) {
					/* iron (15) */
					add_mineral(ch, x, z, y, total_x, total_z, total_y, MCIronOre, 1.15);
				}
				/* y < 40 */
				if (y < 40) {
					/* lapis lazulis (22) */
					add_mineral(ch, x, z, y, total_x, total_z, total_y, MCLapisLazuliOre, 0.80);
					/* gold (14) */
					add_mineral(ch, x, z, y, total_x, total_z, total_y, MCGoldOre, 0.85);
				}
				/* y < 20 */
				if (y < 20) {
					/* diamond (56) */
					add_mineral(ch, x, z, y, total_x, total_z, total_y, MCDiamondOre, 0.80);
					/* redstone (73) */
					add_mineral(ch, x, z, y, total_x, total_z, total_y, MCRedstoneOre, 1.2);
				}
			}
		}
	}
}
static
bool
cdmg_GenerateChunk (CDServer* server, int chunkX, int chunkZ, MCChunkData* data, CDString* seed)
{
	generate_heightmap(data, chunkX, chunkZ);
	generate_filled_chunk(data, chunkX, chunkZ, MCStone);
	dig_caves(data, chunkX, chunkZ);
	erode_landscape(data, chunkX, chunkZ);
	add_minerals(data, chunkX, chunkZ);
	add_sediments(data, chunkX, chunkZ);
	flood_with_water(data, chunkX, chunkZ, 64);
	bedrock_ground(data, chunkX, chunkZ);
	generate_skyLight(data, chunkX, chunkZ);

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
