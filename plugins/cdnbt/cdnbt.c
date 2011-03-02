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

#include <sys/stat.h>

#include <craftd/Server.h>
#include <craftd/Plugin.h>

#include "nbt.h"
#include "itoa.h"

// Temporary hax; load from config
static const int WORLDBASE = 36;
static const char* world_dir;

/**
 * Run at plugin startup to initialize Config.SpawnPos struct with default spawn
 * coordinates and make sure the world directory exists.
 *
 * @return true on success, false otherwise
 */
static
bool
cdnbt_LoadLevelDat (CDPlugin* self)
{
  struct stat buf;
  int worldstat = stat(world_dir = self->server->config->cache.files.world,
                       &buf);

  if (worldstat == -1) {
    ERR("World directory %s does not exist", world_dir);
    return false;
  }

  CDString* leveldatPath;
  nbt_file* nf;
  nbt_tag*  data;

  if (nbt_init(&nf) != NBT_OK) {
    ERR("Cannot initialize level.dat structure");
    return false;
  }

  leveldatPath = CD_CreateStringFromFormat("%s/level.dat", world_dir);

  if (nbt_parse(nf, CD_StringContent(leveldatPath)) != NBT_OK) {
    ERR("Cannot parse level.dat (%s).", CD_StringContent(leveldatPath));
    return false;
  }

  data = nbt_find_tag_by_name("Data", nbt_cast_compound(nf->root));

  nbt_tag* t_gametime = nbt_find_tag_by_name("Time", nbt_cast_compound(data));
  nbt_tag* t_spawnX   = nbt_find_tag_by_name("SpawnX", nbt_cast_compound(data));
  nbt_tag* t_spawnY   = nbt_find_tag_by_name("SpawnY", nbt_cast_compound(data));
  nbt_tag* t_spawnZ   = nbt_find_tag_by_name("SpawnZ", nbt_cast_compound(data));

  CD_ServerSetTime(self->server, *(nbt_cast_long(t_gametime)));

  MCPosition* spawnposition = CD_malloc(sizeof(MCPosition));
  spawnposition->x = *(nbt_cast_int(t_spawnX));
  spawnposition->y = *(nbt_cast_int(t_spawnY));
  spawnposition->z = *(nbt_cast_int(t_spawnZ));

  CD_HashSet(PRIVATE(self->server), "World.spawnposition", (CDPointer) spawnposition);

  nbt_free(nf);

  CD_DestroyString(leveldatPath);

  return true;
}

static
bool
valid_chunk (nbt_tag *nbtroot)
{
  /* Check valid root element */
  nbt_tag* root = nbt_find_tag_by_name("Level", nbt_cast_compound(nbtroot));

  if ((root != NULL) &&
      (strcmp(root->name, "Level") == 0) && (root->type == TAG_COMPOUND)) {
    nbt_tag* blocks = nbt_find_tag_by_name("Blocks", nbt_cast_compound(root));

    if ((blocks != NULL) && (blocks->type == TAG_BYTE_ARRAY)) {
      nbt_byte_array* arr = (nbt_byte_array*) blocks->value;

      if (arr->length == 32768) {
        return true;         /* Valid at last. */
      }
    }
  }

  return false;
}

static
bool
cdnbt_LoadChunk(int x, int z, uint8_t *mapdata)
{
  const char bufsize = 8; // Big enough for all base36 int values and -,nul
  char chunkpath[PATH_MAX]; // POSIX maximum path length
  char dir1[bufsize], dir2[bufsize];
  char cname1[bufsize], cname2[bufsize];
  nbt_file *nf;

  /* Chunk directory and subdir location */
  itoa((x & 63), dir1, WORLDBASE);
  itoa((z & 63), dir2, WORLDBASE);
  /* Chunk file name */
  itoa(x, cname1, WORLDBASE);
  itoa(z, cname2, WORLDBASE);
  evutil_snprintf(chunkpath, PATH_MAX, "%s/%s/%s/c.%s.%s.dat",
                  world_dir, dir1, dir2, cname1, cname2);
  LOG(LOG_DEBUG, "Loading chunk %s", chunkpath);

  if (nbt_init(&nf) != NBT_OK)
  {
    LOG(LOG_ERR, "cannot init chunk struct");
    return -1;
  }

  if (nbt_parse(nf, chunkpath) != NBT_OK)
  {
    LOG(LOG_ERR, "cannot parse chunk '%s'", chunkpath);
    goto CHUNKRDERR;
  }

  if (valid_chunk(nf->root) == 0)
  {
    nbt_byte_array *blocks, *data, *skylight, *blocklight;
    nbt_tag *t_level = nbt_find_tag_by_name("Level", nbt_cast_compound(nf->root));
    nbt_tag *t_blocks =
      nbt_find_tag_by_name("Blocks", nbt_cast_compound(t_level));
    nbt_tag *t_data = nbt_find_tag_by_name("Data", nbt_cast_compound(t_level));
    nbt_tag *t_skylight =
      nbt_find_tag_by_name("SkyLight", nbt_cast_compound(t_level));
    nbt_tag *t_blocklight =
      nbt_find_tag_by_name("BlockLight", nbt_cast_compound(
                             t_level));

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
  return true;

CHUNKRDERR:
  nbt_free(nf);
  return false;
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
  cdnbt_LoadLevelDat(self);

  CD_EventRegister(self->server, "Chunk.load", cdnbt_LoadChunk);

  return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
  CD_EventUnregister(self->server, "Chunk.load", cdnbt_LoadChunk);

  CD_free(CD_HashGet(PRIVATE(self->server), "World.spawnposition"));

  return true;
}
