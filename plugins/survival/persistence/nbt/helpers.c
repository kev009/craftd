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

static
bool
cdnbt_ValidLevel (nbt_node* root)
{
    static char*    names[] = { ".Data.Time", ".Data.SpawnX", ".Data.SpawnY", ".Data.SpawnZ" };
    static nbt_type types[] = { TAG_LONG,     TAG_INT,        TAG_INT,        TAG_INT };

    nbt_node* node;

    for (size_t i = 0; i < ARRAY_SIZE(names); i++) {
        if ((node = nbt_find_by_path(root, names[i])) == NULL || node->type != types[i]) {
            return false;
        }
    }

    return true;
}

static
bool
cdnbt_ValidChunk (nbt_node* root)
{
    static char*  names[] = { ".Level.HeightMap", ".Level.Blocks", ".Level.Data", ".Level.BlockLight", ".Level.SkyLight" };
    static size_t sizes[] = { 256,                32768,           16384,         16384,               16384 };

    nbt_node* node;

    for (size_t i = 0; i < ARRAY_SIZE(names); i++) {
        if ((node = nbt_find_by_path(root, names[i])) == NULL || node->type != TAG_BYTE_ARRAY || node->payload.tag_byte_array.length != sizes[i]) {
            return false;
        }
    }

    return true;
}

static
CDString*
cdnbt_ChunkPath (SVWorld* world, int x, int z)
{
    // Chunk directory and subdir location
    char directory1[8];
    char directory2[8];

    itoa((x & 63), directory1, _config.base);
    itoa((z & 63), directory2, _config.base);

    // Chunk file name
    char chunkName1[8];
    char chunkName2[8];

    itoa(x, chunkName1, _config.base);
    itoa(z, chunkName2, _config.base);

    return CD_CreateStringFromFormat("%s/%s/%s/%s/c.%s.%s.dat",
        _config.path, CD_StringContent(world->name),
        directory1, directory2,
        chunkName1, chunkName2
    );
}

static
CDError
cdnbt_GenerateChunk (SVWorld* world, int x, int z, SVChunk* chunk, const char* seed)
{
    CDError   status;
    CDString* chunkPath = cdnbt_ChunkPath(world, x, z);
    CDString* directory = CD_StringDirname(chunkPath);

    CD_mkdir(CD_StringContent(directory), 0755);

    CD_EventDispatchWithError(status, world->server, "Mapgen.chunk", world, x, z, chunk, seed);

    // TODO: save the generated chunk

    end: {
        CD_DestroyString(chunkPath);
        CD_DestroyString(directory);
    }

    return status;
}

static
int8_t
cdnbt_ObjectNotWatched (CDList* self, CDPointer data)
{
    int8_t result = 0;

    CD_LIST_FOREACH(self, it) {
        if (data == CD_ListIteratorValue(it)) {
            result = 1;

            CD_LIST_BREAK(self);
        }
    }

    return result;
}

static
int8_t
cdnbt_NameNotObserved (CDList* self, CDPointer data)
{
    int8_t result = 0;

    CD_LIST_FOREACH(self, it) {
        if (CD_CStringIsEqual((char*) data, (char*) CD_ListIteratorValue(it))) {
            result = 1;

            CD_LIST_BREAK(self);
        }
    }

    return result;
}
