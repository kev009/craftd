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

struct chunk {
	int x;
	int z;
	unsigned char blocks[16*16*128];
	unsigned char data[16384];
	unsigned char blocklight[16384];
	unsigned char skylight[16384];
	unsigned char heightmap[256];
};

#define OCTAVES 8

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

	for (i = 0 ; i < OCTAVES ; i++) {
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

	for (i = 0 ; i < OCTAVES ; i++) {
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

static void generate_heightmap(struct chunk *ch)
{
	int x, z;
	float lacunarity;
	float a;

	/* step 1: generate the height map */
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			float total_x, total_z;
			total_x = ((((float)ch->x)*16.0) + ((float)x)) * 0.00155; /* magic */
			total_z = ((((float)ch->z)*16.0) + ((float)z)) * 0.00155;
			lacunarity = (((cos(total_x/5.0) + cos(total_z/5.0))/4.0)+1.0);

			a = multifractal_2d(total_x, total_z, 2.7, 20); /* magic settings */
			a = a * 13.5 + 55; /* magic settings */

			ch->heightmap[x + (z*16)] = a;
		}
	}
}

static void generate_filled_chunk(struct chunk *ch, char block_type)
{
	int x, y, z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			for (y = 0 ; y < ch->heightmap[x + (z*16)] && y < 128 ; y++) {
				ch->blocks[y + (z*128) + (x*128*16)] = block_type; /* stone is the basis of MC worlds */
			}
		}
	}
}

static void generate_skylight(struct chunk *ch)
{
	int light_val = 0x0F;
	int x, y, z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			for (y = 1 ; y < 128 ; y++) {
				ch->skylight[((z*128) + (x*128*16)+y)/2] = (light_val | (light_val << 4));
			}
		}
	}
}

static void dig_caves(struct chunk *ch)
{
	float res;
	int x, y, z;
	float total_x, total_z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			total_x = ((((float)ch->x)*16.0) + ((float)x));
			total_z = ((((float)ch->z)*16.0) + ((float)z));
			for (y = 0 ; y < 54 ; y++) {
				res = snoise3(total_x/12.0, y/12.0, total_z/12.0);
				res += (0.5 * snoise3(total_x/24.0, y/24.0, total_z/24.0));
				res /= 1.5;
				if (res > 0.35) {
					if (y < 16)
						ch->blocks[y + (z*128) + (x*128*16)] = 11; /* lava */
					else
						ch->blocks[y + (z*128) + (x*128*16)] = 0; /* cave */
				}
			}
			for (y = 54 ; y < ch->heightmap[x+(z*16)] - 4 ; y++) {
				res = snoise3(total_x/12.0, y/12.0, total_z/12.0);
				res += (0.5 * snoise3(total_x/24.0, y/24.0, total_z/24.0));
				res /= 1.5;
				if (res > 0.45) {
					ch->blocks[y + (z*128) + (x*128*16)] = 0; /* cave */
				}
			}
			/* update height map */
			y = ch->heightmap[x+(z*16)];
			while (y > 0 && ch->blocks[y + (z*128) + (x*128*16)] == 0) {
				ch->heightmap[x+(z*16)] = y;
				y--;
			}
		}
	}
}

static void erode_landscape(struct chunk *ch)
{
	float res;
	int x, y, z;
	float total_x, total_z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			total_x = ((((float)ch->x)*16.0) + ((float)x));
			total_z = ((((float)ch->z)*16.0) + ((float)z));

			/* erosion (over ground) */
			for (y = 65 ; y < ch->heightmap[x+(z*16)] ; y++) {
				res = snoise3(total_x/40.0, y/50.0, total_z/40.0);
				res += (0.5 * snoise3(total_x/80.0, y/100.0, total_z/80.0));
				res /= 1.5;
				if (res > 0.50)
					ch->blocks[y + (z*128) + (x*128*16)] = 0; /* cave */
			}
			/* update height map */
			y = ch->heightmap[x+(z*16)];
			while (y > 0 && ch->blocks[y + (z*128) + (x*128*16)] == 0) {
				ch->heightmap[x+(z*16)] = y;
				y--;
			}
		}
	}
}

static void add_sediments(struct chunk *ch)
{
	int x, y, z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			/* step 3: replace top with grass / the higher, the less blocks / 0 to 3 */
			y = ch->heightmap[x+(z*16)];
			int sediment_height = (128 - ch->heightmap[x+(z*16)])/21; /* 0 - 3 blocks */
			int i;
			if (y < 64) {
				for (i = 0 ; i < sediment_height ; i++) {
					ch->blocks[y + (z*128) + (x*128*16)+i] = 12; /* sand underwater */
				}
			} else if (y >= 64 && sediment_height > 0) {
				for (i = 0 ; i < sediment_height-1 ; i++) {
					ch->blocks[y + (z*128) + (x*128*16)+i] = 3; /* sand underwater */
					sediment_height--;
				}
				ch->blocks[y + (z*128) + (x*128*16)+sediment_height-1] = 2; /* grass */
			}
			ch->heightmap[x+(z*16)] += sediment_height;
		}
	}
}

static void flood_with_water(struct chunk *ch, char water_level)
{
	int x, y, z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {

			/* step 4: flood with water at level 64 */
			y = water_level;
			while (ch->blocks[y + (z*128) + (x*128*16)] == 0) {
				ch->blocks[y + (z*128) + (x*128*16)] = 9; /* water */
				y--;
			}
		}
	}
}

static void bedrock_ground(struct chunk *ch)
{
	int x, z;
	for (x = 0 ; x < 16 ; x++) {
		for (z = 0 ; z < 16 ; z++) {
			ch->blocks[0 + (z*128) + (x*128*16)] = 7; // bedrock
			ch->blocks[1 + (z*128) + (x*128*16)] = 7; // bedrock
			ch->heightmap[x+(z*16)] = Arith_max(ch->heightmap[x+(z*16)], 2);
		}
	}
}

static void add_mineral(struct chunk *ch, int x, int z, int y, float total_x, float total_z, float total_y, char block_type, float probability)
{
	float res;
	//res = snoise3(total_x + (block_type * 128), total_y + (block_type * 128), total_z + (block_type * 128));
	res = snoise4(total_x, total_y, total_z, block_type);
	if (res+1.0 <= (0.25*probability)) {
		ch->blocks[y + (z*128) + (x*128*16)] = block_type;
	}
	
}

static void add_minerals(struct chunk *ch)
{
	int x, y, z;
	float total_x, total_z, total_y;
	for (x = 0 ; x < 16 ; x++) {
		total_x = ((((float)ch->x)*16.0) + ((float)x))* 0.075;
		for (z = 0 ; z < 16 ; z++) {
			total_z = ((((float)ch->z)*16.0) + ((float)z))* 0.075;
			for (y = 2 ; y < ch->heightmap[x+(z*16)] ; y++) {
				if (ch->blocks[y + (z*128) + (x*128*16)] == 0)
					continue;

				total_y = (((float)y))* 0.075;
				/* coal (16) */
				add_mineral(ch, x, z, y, total_x, total_z, total_y, 16, 1.3);
				/* dirt (3) */
				add_mineral(ch, x, z, y, total_x, total_z, total_y, 3, 2.5);
				/* gravel (13), 10 % */
				add_mineral(ch, x, z, y, total_x, total_z, total_y, 13, 2.5);

				/* 5 blocks under the surface */
				if (y < ch->heightmap[x+(z*16)] - 5) {
					/* iron (15) */
					add_mineral(ch, x, z, y, total_x, total_z, total_y, 15, 1.15);
				}
				/* y < 40 */
				if (y < 40) {
					/* lapis lazulis (22) */
					add_mineral(ch, x, z, y, total_x, total_z, total_y, 22, 0.80);
					/* gold (14) */
					add_mineral(ch, x, z, y, total_x, total_z, total_y, 14, 0.85);
				}
				/* y < 20 */
				if (y < 20) {
					/* diamond (56) */
					add_mineral(ch, x, z, y, total_x, total_z, total_y, 56, 0.80);
					/* redstone (73) */
					add_mineral(ch, x, z, y, total_x, total_z, total_y, 73, 1.2);
				}
			}
		}
	}
}

/**
 * Initialize byte arrays for a standard chunk
 *
 */
static struct chunk *_init_data(int ch_x, int ch_z)
{
	struct chunk *ch = calloc(1, sizeof(struct chunk));
	ch->x = ch_x;
	ch->z = ch_z;

	generate_heightmap(ch);
	generate_filled_chunk(ch, 1);
	dig_caves(ch);
	erode_landscape(ch);
	add_minerals(ch);
	add_sediments(ch);
	flood_with_water(ch, 64);
	bedrock_ground(ch);
	generate_skylight(ch);

	return ch;
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
	struct chunk *ch;

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
	ch = _init_data(x, z);

	printf("Generating chunk : %s\n", full_path);
	/* generate the chunk */
	nbt_init(&nbt);

	// Level {
	nbt_new_compound(&nbt->root, "");
	nbt_new_compound(&level_tag, "Level");
	nbt_add_tag(level_tag, nbt->root);
	// Blocks
	nbt_new_byte_array(&sub_tag, "Blocks");
	nbt_set_byte_array(sub_tag, ch->blocks, 32768);
	nbt_add_tag(sub_tag, level_tag);
	// Data
	nbt_new_byte_array(&sub_tag, "Data");
	nbt_set_byte_array(sub_tag, ch->data, 16384);
	nbt_add_tag(sub_tag, level_tag);
	// SkyLight
	nbt_new_byte_array(&sub_tag, "SkyLight");
	nbt_set_byte_array(sub_tag, ch->skylight, 16384);
	nbt_add_tag(sub_tag, level_tag);
	// BlockLight
	nbt_new_byte_array(&sub_tag, "BlockLight");
	nbt_set_byte_array(sub_tag, ch->blocklight, 16384);
	nbt_add_tag(sub_tag, level_tag);
	// HeigtMap
	nbt_new_byte_array(&sub_tag, "HeightMap");
	nbt_set_byte_array(sub_tag, ch->heightmap, 256);
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
	free(ch);
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
