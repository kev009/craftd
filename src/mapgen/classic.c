#include <math.h>

/**
 * A trivial map generator for minecraft
 *
 * This map generator (for testing purposes)
 * will create an infinite plane.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <math.h>

#include "mapgen_plugin.h"
#include "../nbt/nbt.h"
#include "../algos/arith.h"
#include "../noise/simplexnoise1234.h"

static unsigned char _blocks[32768] = {0};
static unsigned char _data[16384] = {0};
static unsigned char _skylight[16384] = {0};
static unsigned char _blocklight[16384] = {0};
static unsigned char _heightmap[256] = {0};


#define OCTAVES 8

/**
 * Initialize byte arrays for a standard chunk
 *
 */
static void _init_data(int ch_x, int ch_z)
{
	int x, z, y;
	float a;
	int light_val = 0x0F;
	/* this should only put 1 layer of bedrock */
	int block_height[16][16];
	float lacunarity;
	float exponent_array[OCTAVES];

	float frequency = 1.0;
	float offset = 0.7;
	float H = 0.25;
	int i;

	/* step 1: generate the height map */
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			float total_x, total_z;
			total_x = ((((float)ch_x)*16.0) + ((float)x));
			total_z = ((((float)ch_z)*16.0) + ((float)z));
			total_x *= 0.015;
			total_z *= 0.015;
			frequency = 1.0;
			lacunarity = (((cos(total_x/5.0) + cos(total_z/5.0))/4.0)+1.0);
			for (i = 0 ; i < OCTAVES ; i++) {
				exponent_array[i] = pow(frequency, -H);
				frequency *= lacunarity;
			}
			float weight = 1.0;
			float signl;
			a = 0.0;
			for (i = 0 ; i < OCTAVES ; i++) {
				signl = (snoise2(total_x, total_z) + offset) * exponent_array[i];
				if (weight > 1.0) weight = 1.0;
				a += (weight * signl);
				weight *= signl;
				total_x *= lacunarity;
				total_z *= lacunarity;
			}
			a = ((a *4.0) + ((-lacunarity*40)+100))*(lacunarity/1.5)+20;
			block_height[(int)x][(int)z] = a;
		}
	}
	/* step 2: fill the chunk with rock according to the height map */
	memset(_blocks, 0, 32768);
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			for (y = 0 ; y < block_height[x][z] && y < 128 ; y++) {
				_blocks[y + (z*128) + (x*128*16)] = 1; /* stone is the basis of MC worlds */
			}
			_heightmap[x+(z*16)] = block_height[x][z]; /* max height is 1 */

			for (y = 1 ; y < 128 ; y++) {
				_skylight[((z*128) + (x*128*16)+y)/2] = (light_val | (light_val << 4)); /* full light for first 2 layers */
			}
		}
	}
	float total_x, total_z;
	float res;
	/* dig caves / erosion */
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			total_x = ((((float)ch_x)*16.0) + ((float)x));
			total_z = ((((float)ch_z)*16.0) + ((float)z));
			/* caves (underground) */
			for (y = 0 ; y < 54 ; y++) {
				res = snoise3(total_x/12.0, y/12.0, total_z/12.0);
				res += (0.5 * snoise3(total_x/24.0, y/24.0, total_z/24.0));
				res /= 1.5;
				if (res > 0.35) {
					if (y < 16)
						_blocks[y + (z*128) + (x*128*16)] = 11; /* lava */
					else
						_blocks[y + (z*128) + (x*128*16)] = 0; /* cave */
				}
			}
			for (y = 54 ; y < _heightmap[x+(z*16)] - 4 ; y++) {
				res = snoise3(total_x/12.0, y/12.0, total_z/12.0);
				res += (0.5 * snoise3(total_x/24.0, y/24.0, total_z/24.0));
				res /= 1.5;
				if (res > 0.45) {
						_blocks[y + (z*128) + (x*128*16)] = 0; /* cave */
				}
			}
			/* erosion (over ground) */
			for (y = 65 ; y < _heightmap[x+(z*16)] ; y++) {
				res = snoise3(total_x/40.0, y/50.0, total_z/40.0);
				res += (0.5 * snoise3(total_x/80.0, y/100.0, total_z/80.0));
				res /= 1.5;
				if (res > 0.50)
					_blocks[y + (z*128) + (x*128*16)] = 0; /* cave */
			}
		}
	}
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			/* step 3: replace top with grass */
			y = _heightmap[x+(z*16)];
			while (_blocks[y + (z*128) + (x*128*16)] == 0) {
				y--;
				_heightmap[x+(z*16)]--;
				
			}
			if (y < 64 && y > 60) {
				_blocks[y + (z*128) + (x*128*16)] = 12; /* sand underwater */
				_blocks[y + (z*128) + (x*128*16)+1] = 12; /* sand underwater */
			} else if (y >= 64) {
				_blocks[y + (z*128) + (x*128*16)] = 3; /* dirt */
				_blocks[y + (z*128) + (x*128*16)+1] = 2; /* dirt */
			}
			_heightmap[x+(z*16)] += 2;

			/* step 4: flood with water at level 64 */
			y = 64;
			while (_blocks[y + (z*128) + (x*128*16)] == 0) {
				_blocks[y + (z*128) + (x*128*16)] = 9; /* water */
				y--;
			}
		}
	}
	/* add 2 layers of bedrock */
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			_blocks[0 + (z*128) + (x*128*16)] = 7; // bedrock
			_blocks[1 + (z*128) + (x*128*16)] = 7; // bedrock
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
	printf("Done\n");
}

/**
 * Initializes the Trivial map generator
 *
 * @remarks plugin specific data may be stored
 * in the generic "data" field of the structure
 *
 * @param seed the random seed used to generate the map (unused)
 * @param path the full path of the world (needs to be suffixed with the dimension)
 * @return an initialized mapgen structure, NULL on failure
 */
struct mapgen *init_mapgen(char *seed, char *path)
{
	struct mapgen *mg;
	if (!seed) {
		return NULL;
	}
	if (!path) {
		return NULL;
	}

	mg = calloc(1, sizeof(struct mapgen));

	mg->seed = strdup(seed);
	mg->path = strdup(path);
	mg->generate_chunk = &gen_chunk;

	return mg;
}

#ifdef MG_STANDALONE
int main()
{
	float x, z, y;
//	for (x=0.0 ; x < 16 ; x+= 0.10) {
//		for (z=0.0 ; z < 16 ; z+= 0.10) {
	for (x=0.0 ; x < 1.6 ; x+= 0.10) {
		for (z=0.0 ; z < 1.6 ; z+= 0.10) {
			y = snoise2(x, z);
			y += (snoise2(x*2.0, z*2.0) * 0.5);
			y += (snoise2(x*4.0, z*4.0) * 0.25);
			//y += (snoise2(x*8.0, z*8.0) * 0.125);
			//y += (snoise2(x*16.0, z*16.0) * 0.0625);
			y *= (cos(z)-sin(x));
			printf("%f %f %f\n", x, z, y);
		}
		printf("\n");
	}
}
#endif
