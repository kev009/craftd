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
static const int WORLD_BASE = 36;

/**
 * Run at plugin startup to initialize World.spawnPosition struct with default spawn
 * coordinates and make sure the world directory exists.
 *
 * @return true on success, false otherwise
 */
static
bool
cdnbt_LoadLevelDat (CDPlugin* self)
{
    struct stat statBuffer;

    if (stat(self->server->config->cache.files.world, &statBuffer) < 0) {
        SERR(self->server, "world directory %s does not exist", self->server->config->cache.files.world);

        return false;
    }

    CDString* leveldatPath;
    nbt_file* nf;
    nbt_tag*  data;

    if (nbt_init(&nf) != NBT_OK) {
        SERR(self->server, "cannot initialize level.dat structure");

        return false;
    }

    leveldatPath = CD_CreateStringFromFormat("%s/level.dat", self->server->config->cache.files.world);

    if (nbt_parse(nf, CD_StringContent(leveldatPath)) != NBT_OK) {
        SERR(self->server, "cannot parse level.dat (%s).", CD_StringContent(leveldatPath));

        return false;
    }

    data = nbt_find_tag_by_name("Data", nbt_cast_compound(nf->root));

    nbt_tag* t_gametime = nbt_find_tag_by_name("Time", nbt_cast_compound(data));
    nbt_tag* t_spawnX   = nbt_find_tag_by_name("SpawnX", nbt_cast_compound(data));
    nbt_tag* t_spawnY   = nbt_find_tag_by_name("SpawnY", nbt_cast_compound(data));
    nbt_tag* t_spawnZ   = nbt_find_tag_by_name("SpawnZ", nbt_cast_compound(data));

    CD_ServerSetTime(self->server, *(nbt_cast_long(t_gametime)));

    MCPosition* spawnPosition = CD_malloc(sizeof(MCPosition));
    spawnPosition->x = *(nbt_cast_int(t_spawnX));
    spawnPosition->y = *(nbt_cast_int(t_spawnY));
    spawnPosition->z = *(nbt_cast_int(t_spawnZ));

    DEBUG("spawn position: (%d, %d, %d)", spawnPosition->x, spawnPosition->y, spawnPosition->z);

    CD_HashPut(PRIVATE(self->server), "World.spawnPosition", (CDPointer) spawnPosition);

    nbt_free(nf);

    CD_DestroyString(leveldatPath);

    return true;
}

static
bool
cdnbt_ValidChunk (nbt_tag* nbtRoot)
{
    /* Check valid root element */
    nbt_tag* root = nbt_find_tag_by_name("Level", nbt_cast_compound(nbtRoot));

    if ((root != NULL) && (strcmp(root->name, "Level") == 0) && (root->type == TAG_COMPOUND)) {
        nbt_tag* blocks = nbt_find_tag_by_name("Blocks", nbt_cast_compound(root));

        if ((blocks != NULL) && (blocks->type == TAG_BYTE_ARRAY)) {
            nbt_byte_array* arr = (nbt_byte_array*) blocks->value;

            if (arr->length == 32768) {
                return true; /* Valid at last. */
            }
        }
    }

    return false;
}

static
bool
cdnbt_LoadChunk (CDServer* server, int x, int z, uint8_t* mapdata)
{
    const char bufferSize = 8; // Big enough for all base36 int values and -,nul

    /* Chunk directory and subdir location */
    char directory1[bufferSize];
    char directory2[bufferSize];

    itoa((x & 63), directory1, WORLD_BASE);
    itoa((z & 63), directory2, WORLD_BASE);

    /* Chunk file name */
    char chunkName1[bufferSize];
    char chunkName2[bufferSize];

    itoa(x, chunkName1, WORLD_BASE);
    itoa(z, chunkName2, WORLD_BASE);

    CDString* chunkPath = CD_CreateStringFromFormat("%s/%s/%s/c.%s.%s.dat",
        server->config->cache.files.world,
        directory1, directory2,
        chunkName1, chunkName2
    );

    SDEBUG(server, "loading chunk %s", CD_StringContent(chunkPath));

    nbt_file* nf = NULL;

    if (nbt_init(&nf) != NBT_OK) {
        SERR(server, "cannot init chunk struct");

        goto error;
    }

    int reasonCode;

    if ((reasonCode = nbt_parse(nf, CD_StringContent(chunkPath))) != NBT_OK) {
        const char* reason;

        switch (reasonCode) {
            case NBT_EGZ:  reason = strerror(errno);      break;
            case NBT_EMEM: reason = "out of memory";      break;
            case NBT_ERR:  reason = "chunk format error"; break;
            default:       reason = "unknown error";      break;
        }

        SERR(server, "cannot parse chunk '%s': %s", CD_StringContent(chunkPath), reason);

        goto error;
    }

    if (cdnbt_ValidChunk(nf->root) == true) {
        nbt_tag* t_level      = nbt_find_tag_by_name("Level", nbt_cast_compound(nf->root));
        nbt_tag* t_blocks     = nbt_find_tag_by_name("Blocks", nbt_cast_compound(t_level));
        nbt_tag* t_data       = nbt_find_tag_by_name("Data", nbt_cast_compound(t_level));
        nbt_tag* t_skyLight   = nbt_find_tag_by_name("SkyLight", nbt_cast_compound(t_level));
        nbt_tag *t_blockLight = nbt_find_tag_by_name("BlockLight", nbt_cast_compound(t_level));

        nbt_byte_array* blocks     = nbt_cast_byte_array(t_blocks);
        nbt_byte_array* data       = nbt_cast_byte_array(t_data);
        nbt_byte_array* skyLight   = nbt_cast_byte_array(t_skyLight);
        nbt_byte_array* blockLight = nbt_cast_byte_array(t_blockLight);

        int offset = 0;

        memcpy(mapdata + offset, blocks->content, blocks->length);
        offset += blocks->length;
        memcpy(mapdata + offset, data->content, data->length);
        offset += data->length;
        memcpy(mapdata + offset, skyLight->content, skyLight->length);
        offset += skyLight->length;
        memcpy(mapdata + offset, blockLight->content, blockLight->length);
    }
    else {
        SERR(server, "bad chunk file '%s'", CD_StringContent(chunkPath));

        goto error;
    }

    done: {
        if (nf) {
            nbt_free(nf);
        }

        CD_DestroyString(chunkPath);

        return true;
    }

    error: {
        if (nf) {
            nbt_free(nf);
        }

        CD_DestroyString(chunkPath);

        return false;
    }
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

    CD_free((void*) CD_HashGet(PRIVATE(self->server), "World.spawnPosition"));

    return true;
}
