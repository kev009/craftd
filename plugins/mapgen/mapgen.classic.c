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

static unsigned char* _seed              = NULL;
static unsigned char  _blocks[32768]     = { 0 };
static unsigned char  _data[16384]       = { 0 };
static unsigned char  _skyLight[16384]   = { 0 };
static unsigned char  _blockLight[16384] = { 0 };
static unsigned char  _heightMap[256]    = { 0 };

static
int
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
int
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
int
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
int
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
int
cdmg_LookupTypeUndergroundSecond (float value)
{
    return MCBedrock;
}

static
int
cdmg_LookupTypeUndergroundThird (float value)
{
    return MCBedrock;
}

static
int
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
int
cdmg_LookupType (float value, int height)
{
    if (height <= 1) { {
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
int
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
cdmg_InitializeData (int chunkX, int chunkZ)
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
    memset(_blocks, 0, 32768);
    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            for (int y = 0; y < blockHeight[x][z]; y++) {
                _blocks[y + (z * 128) + (x * 128 * 16)] = cdmg_BlockType(chunkX, chunkZ, x, y, z);
            }

            _heightMap[x + (z * 16)] = blockHeight[x][z]; // max height is 1

            for (int y = 1; y < 128; y++) {
                // full light for first 2 layers
                _skyLight[((z * 128) + (x * 128 * 16) + y) / 2] = (lightValue | (lightValue << 4));
            }
        }
    }

    for (int x = 0; x < 16; x++) {
        for (int z = 0; z < 16; z++) {
            // step 3: replace top with grass
            int y = _heightMap[x + (z * 16)];

            while (_blocks[y + (z * 128) + (x * 128 * 16)] == 0 && y > 64) {
                y--;
            }

            switch (_blocks[(y) + (z * 128) + (x * 128 * 16)]) {
                case MCAir:
                case MCSand: {
                    break;
                }

                default: {
                    _blocks[(y + 1) + (z * 128) + (x * 128 * 16)] = MCGrass;
                }
            }

            // step 4: flood with water at level 64
            y = 62;
            while (_blocks[y + (z * 128) + (x * 128 * 16)] == 0) {
                _blocks[y + (z * 128) + (x * 128 * 16)] = MCStationaryWater;
                y--;
            }
        }
    }
}

/**
 * Generate a chunk based on x and z coordinates
 *
 * @remarks this will not return any data, but instead write
 * directly to the disk
 *
 * @param mg the map generator structure where the options are stored
 * @param x the x coordinate of the chunk
 * @param z the z coordinate of the chunk
 */
static void gen_chunk(struct mapgen* mg, int x, int z)
{
    char x_file[8];
    char z_file[8];
    char x_dir[8];
    char z_dir[8];
    char filename[24]; /* c.<x>.<z>.dat = 24 chars*/
    char *full_path = calloc(strlen(mg->path)+(8*2)+(8*2)+8, sizeof(char));
    nbt_file *nbt;
    nbt_tag *level_tag;
    nbt_tag *sub_tag;
    struct stat sbuf;

    itoa(x, x_file, 36);
    itoa(z, z_file, 36);
    itoa(Arith_mod(x, 64), x_dir, 36);
    itoa(Arith_mod(z, 64), z_dir, 36);

    /* test first directory exists or create it */
    strcat(full_path, mg->path);
    /* test if x directory exists or create it */
    strcat(full_path, "/");
    strcat(full_path, x_dir);
    mkdir(full_path, 0755);
    /* test if z directory exists or create it */
    strcat(full_path, "/");
    strcat(full_path, z_dir);
    mkdir(full_path, 0755);
    /* test if file exists */
    strcat(full_path, "/");
    snprintf(filename, 24, "c.%s.%s.dat", x_file, z_file);
    strcat(full_path, filename);
    if (stat(full_path, &sbuf) == 0) {
        printf("File %s already exists\n", full_path);
        return;
    }

    /* we use static data here */
    _init_data(x, z);

    printf("Generating chunk : %s\n", full_path);
    /* generate the chunk */
    nbt_init(&nbt);

    // Level {
    nbt_new_compound(&nbt->root, "");
    nbt_new_compound(&level_tag, "Level");
    nbt_add_tag(level_tag, nbt->root);
    // Blocks
    nbt_new_byte_array(&sub_tag, "Blocks");
    nbt_set_byte_array(sub_tag, _blocks, 32768);
    nbt_add_tag(sub_tag, level_tag);
    // Data
    nbt_new_byte_array(&sub_tag, "Data");
    nbt_set_byte_array(sub_tag, _data, 16384);
    nbt_add_tag(sub_tag, level_tag);
    // SkyLight
    nbt_new_byte_array(&sub_tag, "SkyLight");
    nbt_set_byte_array(sub_tag, _skylight, 16384);
    nbt_add_tag(sub_tag, level_tag);
    // BlockLight
    nbt_new_byte_array(&sub_tag, "BlockLight");
    nbt_set_byte_array(sub_tag, _blocklight, 16384);
    nbt_add_tag(sub_tag, level_tag);
    // HeigtMap
    nbt_new_byte_array(&sub_tag, "HeightMap");
    nbt_set_byte_array(sub_tag, _heightmap, 256);
    nbt_add_tag(sub_tag, level_tag);
    // Entities
    nbt_new_list(&sub_tag, "Entities", TAG_COMPOUND);
    nbt_add_tag(sub_tag, level_tag);
    // TileEntities
    nbt_new_list(&sub_tag, "TileEntities", TAG_COMPOUND);
    nbt_add_tag(sub_tag, level_tag);
    // LastUpdate
    nbt_new_long(&sub_tag, "LastUpdate");
    nbt_set_long(sub_tag, 0);
    nbt_add_tag(sub_tag, level_tag);
    // xPos
    nbt_new_int(&sub_tag, "xPos");
    nbt_set_int(sub_tag, x);
    nbt_add_tag(sub_tag, level_tag);
    // zPos
    nbt_new_int(&sub_tag, "zPos");
    nbt_set_int(sub_tag, z);
    nbt_add_tag(sub_tag, level_tag);
    // TerrainPopulated
    nbt_new_byte(&sub_tag, "TerrainPopulated");
    nbt_set_byte(sub_tag, 0);
    nbt_add_tag(sub_tag, level_tag);
    // }

    /* write the nbt to disk */
    //nbt_write_compound(nbt, nbt_cast_compound(level_tag));
    nbt_write(nbt, full_path);

    free(full_path);
}



extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->name = CD_CreateStringFromCString("Mapgen.classic");

    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{

    return true;
}
