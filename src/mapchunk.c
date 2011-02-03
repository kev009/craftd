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

#include <config.h>

#include <stdio.h>
#include <sys/queue.h>
#include <sys/stat.h>
#include <zlib.h>

#include <khash.h>

#include "craftd-config.h"
#include "craftd.h"
#include "util.h"
#include "mapchunk.h"
#include "timeloop.h"
#include "nbt/nbt.h"
#include "mapgen/mapgen.h"

/* Map Chunk */
// Temp
static const int WORLDBASE = 36;

/**
 * Check two chunk coordinate pairs for equality
 * 
 * @returns 0 if equal, 1 otherwise
 */
int chunkcoordcmp(const void *a, const void *b)
{
  chunk_coord coordA = *(chunk_coord *)a;
  chunk_coord coordB = *(chunk_coord *)b;
  
  if ( coordA.x == coordB.x && coordA.z == coordB.z)
    return 0;
  
  return 1;
}

/**
 * Compute a hash function of a chunk coordinate pair
 */
uint chunkcoordhash(const void *T)
{
  const int HASHMULTIPLIER = 31;
  chunk_coord coord = *(chunk_coord *)T;
  
  return coord.x * (HASHMULTIPLIER) * coord.z;
}

/**
 * Global Active Chunk Table
 *
 * The GACT holds a chunk (x,z) coordinate as the key and a linked list of
 * players in that region as the value.
 * 
 * This association enables broadcasting to in-range players
 */
struct GACT_entry
{
  pthread_spinlock_t spinlock;
  struct PL_entry player;
  SLIST_ENTRY(GACT_entry) GACT_entries; // Pointer to the GACT list entry
};

int valid_chunk(nbt_tag *nbtroot)
{
    /* Check valid root element */
    nbt_tag *root = nbt_find_tag_by_name("Level", nbtroot);

    if ((root != NULL) &&
            (strcmp(root->name, "Level") == 0) && (root->type == TAG_COMPOUND))
    {
        nbt_tag *blocks = nbt_find_tag_by_name("Blocks", root);

        if ((blocks != NULL) && (blocks->type == TAG_BYTE_ARRAY))
        {
            nbt_byte_array *arr = (nbt_byte_array *)blocks->value;

            if (arr->length == 32768)
            {
                return 0; /* Valid at last. */
            }
        }
    }
    
    return 1;
}

/**
 * Run at server startup to initialize Config.SpawnPos struct with default spawn
 * coordinates
 *
 * @returns 0 on success, -1 otherwise
 */
int loadLevelDat()
{
  struct stat buf;
  int worldstat = stat(Config.world_dir, &buf);
  
  if (worldstat == -1)
  {
    LOG(LOG_ERR, "World directory %s does not exist", Config.world_dir);
    return -1;
  }
  
  char leveldatpath[PATH_MAX];
  nbt_file *nf;
  nbt_tag *data;

  if (nbt_init(&nf) != NBT_OK)
  {
    LOG(LOG_ERR, "Cannot initialize level.dat structure");
    return -1;
  }

  evutil_snprintf(leveldatpath, PATH_MAX, "%s/level.dat", Config.world_dir);

  if (nbt_parse(nf, leveldatpath) != NBT_OK)
  {
    LOG(LOG_ERR, "Cannot parse level.dat (%d).", leveldatpath);
    return -1;
  }
  
  data = nbt_find_tag_by_name("Data", nf->root);
  
  //nbt_tag *t_gametime = nbt_find_tag_by_name("Time", data);
  nbt_tag *t_spawnX =  nbt_find_tag_by_name("SpawnX", data);
  nbt_tag *t_spawnY = nbt_find_tag_by_name("SpawnY", data);
  nbt_tag *t_spawnZ = nbt_find_tag_by_name("SpawnZ", data);
  
  //TL_set_gametime(xx);
  Config.spawn.x = *(nbt_cast_int(t_spawnX));
  Config.spawn.y = *(nbt_cast_int(t_spawnY));
  Config.spawn.z = *(nbt_cast_int(t_spawnZ));
  
  nbt_free(nf);
  return 0;
}

int loadChunk(int x, int z, uint8_t *mapdata)
{
  const char bufsize = 8; // Big enough for all base36 int values and -,nul
  char chunkpath[PATH_MAX]; // POSIX maximum path length
  char dir1[bufsize], dir2[bufsize];
  char cname1[bufsize], cname2[bufsize];
  nbt_file *nf;
  struct MG_entry *mapgen_item;

  /* Chunk directory and subdir location */
  itoa((x & 63), dir1, WORLDBASE);
  itoa((z & 63), dir2, WORLDBASE);
  /* Chunk file name */
  itoa(x, cname1, WORLDBASE);
  itoa(z, cname2, WORLDBASE);
  evutil_snprintf(chunkpath, PATH_MAX, "%s/%s/%s/c.%s.%s.dat", 
      Config.world_dir, dir1, dir2, cname1, cname2);
  LOGT(LOG_DEBUG, "Loading chunk %s", chunkpath);

  if (nbt_init(&nf) != NBT_OK)
  {
    LOG(LOG_ERR, "cannot init chunk struct");
    return -1;
  }

  if (nbt_parse(nf, chunkpath) != NBT_OK)
  {
    LOG(LOG_INFO, "chunk '%s' needs to be generated", chunkpath);
    pthread_mutex_lock(&mapgen_cvmutex);
    mapgen_item = calloc(1, sizeof(struct MG_entry));
    mapgen_item->x = x;
    mapgen_item->z = z;
    mapgen_item->dim = "./";
    STAILQ_INSERT_TAIL(&mapgen_head, mapgen_item, entries);
    pthread_mutex_unlock(&mapgen_cvmutex);
    pthread_cond_signal(&mapgen_cv);
  }
  /* FIXME: this ugly hack makes us wait until the
   * generator is done with the chunk, but active
   * waiting should definitely be avoided in some way
   * Maybe we could delay the sending of the chunk? */
  while ((nbt_parse(nf, chunkpath) != NBT_OK) ||
         (valid_chunk(nf->root) != 0));

  if (valid_chunk(nf->root) == 0)
  {
    nbt_byte_array *blocks, *data, *skylight, *blocklight;
    nbt_tag *t_level = nbt_find_tag_by_name("Level", nf->root);
    nbt_tag *t_blocks = nbt_find_tag_by_name("Blocks", t_level);
    nbt_tag *t_data = nbt_find_tag_by_name("Data", t_level);
    nbt_tag *t_skylight = nbt_find_tag_by_name("SkyLight", t_level);
    nbt_tag *t_blocklight = nbt_find_tag_by_name("BlockLight", t_level);

    blocks = nbt_cast_byte_array(t_blocks);
    data = nbt_cast_byte_array(t_data);
    skylight = nbt_cast_byte_array(t_skylight);
    blocklight = nbt_cast_byte_array(t_blocklight);

    int offset = 0;
    memcpy(mapdata, blocks->content, blocks->length);
    offset += blocks->length;
    memcpy(mapdata+offset, data->content, data->length);
    offset += data->length;
    memcpy(mapdata+offset, skylight->content, skylight->length);
    offset += skylight->length;
    memcpy(mapdata+offset, blocklight->content, blocklight->length);
  }
  else
  {
    LOG(LOG_ERR, "bad chunk file '%s'", chunkpath);
    goto CHUNKRDERR;
  }

  // Good read
  nbt_free(nf);
  return 0;

CHUNKRDERR:
  nbt_free(nf);
  return -1;
}
