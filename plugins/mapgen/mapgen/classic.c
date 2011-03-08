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

static int lookup_type_mountain(float value)
{
	if (value < -0.50)
		return 13;	/* gravel */
	if (value < 0.0)
		return 3;	/* dirt */
	if (value < 0.30)
		return 1;	/* stone */
	if (value < 0.35)
		return 16;	/* coal */
	if (value < 0.50)
		return 1;	/* stone */
	if (value < 0.55)
		return 16;	/* iron */
	return 0;
}

static int lookup_type_hills(float value)
{
	if (value < -0.50)
		return 0;
	if (value < -0.05)
		return 3;	/* dirt */
	if (value < 0.0)
		return 13;	/* gravel */
	if (value < 0.25)
		return 1;	/* stone */
	if (value < 0.40)
		return 0;	/* cave */
	if (value < 0.50)
		return 1;	/* stone */
	if (value < 0.58)
		return 3;	/* dirt */
	if (value < 0.62)
		return 0;	/* cave */
	if (value < 0.72)
		return 3;	/* dirt */
	if (value < 0.722)
		return 16;	/* coal */
	if (value < 0.84)
		return 1;	/* stone */
	if (value < 0.85)
		return 16;	/* iron */
	return 0;
}

static int lookup_type_sealevel(float value)
{
	if (value < -0.07)
		return 12;	/* sand */
	if (value < 0.0)
		return 1;	/* rock */
	if (value < 0.07)
		return 3;	/* dirt */
	return 12;		/* sand */
}

static int lookup_type_underground1(float value)
{
	if (value < -0.2)
		return 12;
	if (value < -0.19)
		return 0;
	if (value < -0.17)
		return 1;
	if (value < -0.15)
		return 3;
	if (value < -0.05)
		return 0;
	if (value < 0.2)
		return 1;
	if (value < 0.25)
		return 0;
	if (value < 0.5)
		return 1;
	return 12;
}

static int lookup_type_underground2(float value)
{
	return 7;
}

static int lookup_type_underground3(float value)
{
	return 7;
}

static int lookup_type_bottom(float value)
{
	return 7; /* bedrock */
}

/**
 * Determine a block's type (sand, bedrock, ...)
 *
 * @param value a noise generated value [-1.0, 1.0]
 * @param height the altitude of the block
 *
 * @return the block type
 */
static int lookup_type(float value, int height)
{
	if (height <= 1)
		return lookup_type_bottom(value);
	if (height <= 20)
		return lookup_type_underground3(value);
	if (height <= 35)
		return lookup_type_underground2(value);
	if (height <= 50)
		return lookup_type_underground1(value);
	if (height <= 66)
		return lookup_type_sealevel(value);
	if (height <= 75)
		return lookup_type_hills(value);
	/* really high */
	return lookup_type_mountain(value);
}

static int block_type(int ch_x, int ch_z, int x, int y, int z)
{
	float total_x = ((((float)ch_x)*16.0) + ((float)x)) * 0.053;
	float total_z = ((((float)ch_z)*16.0) + ((float)z)) * 0.053;
	float total_y = ((float)y) * 0.015;

	float val = snoise3(total_x, total_y, total_z);
	val += (0.5*(snoise3(total_x*2.0, total_y*2.0, total_z*2.0)));
	val += (0.25*(snoise3(total_x*4.0, total_y*4.0, total_z*4.0)));
	val += (0.125*(snoise3(total_x*8.0, total_y*8.0, total_z*8.0)));
	//val *= (cos(total_z)+cos(total_x)+cos(total_z));
	//val /= (1.5);
	val /= (1.0 + 0.5 + 0.25 + 0.125);

	return lookup_type(val, y);
}

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

	/* step 1: generate the height map */
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			float total_x, total_z;
			total_x = ((((float)ch_x)*16.0) + ((float)x)); 
			total_z = ((((float)ch_z)*16.0) + ((float)z)); 
			total_x *= 0.015;
			total_z *= 0.015;
			a = snoise2(total_x, total_z);
			a += (snoise2(total_x*2.0, total_z*2.0) * 0.5);
			a += (snoise2(total_x*4.0, total_z*4.0) * 0.25);
			a += (snoise2(total_x*8.0, total_z*8.0) * 0.125);
			a /= (1.0 + 0.5 + 0.25 + 0.125);
			/* noise tends to be of repetitive height (-1.0 -> 1.0)
			 * which would tend to result in hills of the same height
			 * so we need to multiply it by a cos/sin to give more
			 * randomness  to it */
			a *= (cos(total_z)+cos(total_x));
			/* fill the minimum height, we get flat bottom lakes like that */
			a = (a < -0.40 ? -0.40 : a);
			a = ((a / 2.5)*48.0 + 64.0);
			block_height[(int)x][(int)z] = a;
		}
	}
	/* step 2: fill the chunk with sand according to the height map */
	memset(_blocks, 0, 32768);
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			for (y = 0 ; y < block_height[x][z] ; y++) {
				_blocks[y + (z*128) + (x*128*16)] = block_type(ch_x, ch_z, x, y, z);
			}
			_heightmap[x+(z*16)] = block_height[x][z]; /* max height is 1 */

			for (y = 1 ; y < 128 ; y++) {
				_skylight[((z*128) + (x*128*16)+y)/2] = (light_val | (light_val << 4)); /* full light for first 2 layers */
			}
		}
	}
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			/* step 3: replace top with grass */
			y = _heightmap[x+(z*16)];;
			while (_blocks[y + (z*128) + (x*128*16)] == 0 && y > 64) {
				y--;
			}
			switch (_blocks[(y) + (z*128) + (x*128*16)]) {
				case 0: /* empty */
				case 12: /* sand */
					break;
				default:
					_blocks[(y+1) + (z*128) + (x*128*16)] = 2;
			}

			/* step 4: flood with water at level 64 */
			y = 62;
			while (_blocks[y + (z*128) + (x*128*16)] == 0) {
				_blocks[y + (z*128) + (x*128*16)] = 9; /* water */
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
