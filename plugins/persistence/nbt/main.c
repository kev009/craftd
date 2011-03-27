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
#include <fcntl.h>

#include <craftd/Server.h>
#include <craftd/Plugin.h>

#include <beta/minecraft.h>
#include <beta/World.h>
#include <beta/Logger.h>

#include <nbt/nbt.h>
#include <nbt/itoa.h>

static struct {
    const char* path;

    int base;
} _config;

static
bool
cdnbt_ValidChunk (nbt_node* root)
{
    static char*  names[] = { "Blocks", "Data", "BlockLight", "SkyLight" };
    static size_t sizes[] = { 32768, 16384, 16384, 16384 };

    nbt_node* node;

    for (size_t i = 0; i < ARRAY_SIZE(names); i++) {
        if ((node = nbt_find_by_name(root, names[i])) == NULL) {
            return false;
        }

        if (node->type != TAG_BYTE_ARRAY || node->payload.tag_byte_array.length != sizes[i]) {
            return false;
        }
    }

    return true;
}

static
bool
cdnbt_WorldCreate (CDServer* server, CDWorld* world)
{
    int       error = CDNull;
    CDString* path  = CD_CreateStringFromFormat("%s/%s/level.dat", _config.path, CD_StringContent(world->name));
    nbt_node* root  = nbt_parse_path(CD_StringContent(path));

    if (!root || errno != NBT_OK) {
        goto error;
    }

    world->spawnPosition = (MCBlockPosition) {
        .x = nbt_find_by_name(root, "SpawnX")->payload.tag_int,
        .y = nbt_find_by_name(root, "SpawnY")->payload.tag_int,
        .z = nbt_find_by_name(root, "SpawnZ")->payload.tag_int
    };

    WDEBUG(world, "spawn position: (%d, %d, %d)",
        world->spawnPosition.x,
        world->spawnPosition.y,
        world->spawnPosition.z);

    CD_WorldSetTime(world, nbt_find_by_name(root, "Time")->payload.tag_long);

    world->dimension = nbt_find_by_name(root, "Dimension")->payload.tag_int;

    done: {
        if (root) {
            nbt_free(root);
        }

        return true;
    }

    error: {
        int     old = errno;
        CDError status;

        if (root) {
            nbt_free(root);
        }

        CD_EventDispatchWithError(status, server, "Mapgen.level", world);

        if (status != CDOk) {
            WERR(world, "Couldn't load world base data: %s", nbt_error_to_string(old));
        }
        else {
            WDEBUG(world, "spawn position: (%d, %d, %d)",
                world->spawnPosition.x,
                world->spawnPosition.y,
                world->spawnPosition.z);
        }

        return true;
    }
}

static
bool
cdnbt_WorldGetChunk (CDServer* server, CDWorld* world, int x, int z, MCChunk* chunk)
{
    int fd;

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

    CDString* chunkPath = CD_CreateStringFromFormat("%s/%s/%s/%s/c.%s.%s.dat",
        _config.path, CD_StringContent(world->name),
        directory1, directory2,
        chunkName1, chunkName2
    );

    SDEBUG(server, "loading chunk %s", CD_StringContent(chunkPath));

    nbt_node* root = NULL;

    if (access(CD_StringContent(chunkPath), R_OK) < 0) {
        SDEBUG(server, "Generating chunk: %d,%d\n", x, z );

        CD_DO {
            CDString* dir = CD_CreateStringFromFormat("%s/%s/%s", _config.path, CD_StringContent(world->name),
                directory1);

            mkdir(CD_StringContent(dir), 0755);

            CD_DestroyString(dir);
        }

        CD_DO {
            CDString* dir = CD_CreateStringFromFormat("%s/%s/%s/%s", _config.path, CD_StringContent(world->name),
                directory1, directory2);

            mkdir(CD_StringContent(dir), 0755);

            CD_DestroyString(dir);
        }

        CD_DO {
            struct flock lock = { F_WRLCK, SEEK_SET, 0, 0, 0 };
                         fd   = open(CD_StringContent(chunkPath), O_CREAT, 0755);

            fcntl(fd, F_GETLK, &lock);

            if (lock.l_type == F_UNLCK) {
                lock.l_type = F_WRLCK;
                fcntl(fd, F_SETLKW, &lock);
            }
            else {
                close(fd);
                goto load;
            }
        }

        CD_DO {
            CDError status;

            CD_EventDispatchWithError(status, server, "Mapgen.chunk", x, z, chunk, NULL);

            if (status != CDOk) {
                goto error;
            }
            else {
                goto done;
            }
        }
    }

    load: {
        CD_DO {
            struct flock lock = { F_RDLCK, SEEK_SET, 0, 0, 0 };
                         fd   = open(CD_StringContent(chunkPath), O_RDONLY);

            fcntl(fd, F_SETLKW, &lock);
        }

        root = nbt_parse_path(CD_StringContent(chunkPath));

        if (!root || errno != NBT_OK || !cdnbt_ValidChunk(root)) {
            SERR(server, "bad chunk file '%s'", CD_StringContent(chunkPath));

            goto error;
        }

        nbt_node* node;

        node = nbt_find_by_name(root, "Blocks");
        memcpy(chunk->blocks, node->payload.tag_byte_array.data, 32768);

        node = nbt_find_by_name(root, "Data");
        memcpy(chunk->data, node->payload.tag_byte_array.data, 16384);

        node = nbt_find_by_name(root, "BlockLight");
        memcpy(chunk->blockLight, node->payload.tag_byte_array.data, 16384);

        node = nbt_find_by_name(root, "SkyLight");
        memcpy(chunk->skyLight, node->payload.tag_byte_array.data, 16384);

        CD_DO {
            struct flock lock = { F_UNLCK, SEEK_SET, 0, 0, 0 };
            fcntl(fd, F_SETLKW, &lock);

            close(fd);
        }
    }

    done: {
        if (root) {
            nbt_free(root);
        }

        CD_DestroyString(chunkPath);

        CD_DO {
            struct flock lock = { F_UNLCK, SEEK_SET, 0, 0, 0 };
            fcntl(fd, F_SETLKW, &lock);

            close(fd);
        }

        return true;
    }

    error: {
        if (root) {
            nbt_free(root);
        }

        CD_DestroyString(chunkPath);

        CD_DO {
            struct flock lock = { F_UNLCK, SEEK_SET, 0, 0, 0 };
            fcntl(fd, F_SETLKW, &lock);

            close(fd);
        }

        return false;
    }
}

static
bool
cdnbt_WorldSetChunk (CDServer* server, CDWorld* world, int x, int z, MCChunk* chunk)
{
    return true;
}

static
bool
cdnbt_WorldSave (CDServer* server, CDWorld* world)
{
    return true;
}

static
bool
cdnbt_WorldDestroy (CDServer* server, CDWorld* world)
{
    return true;
}

extern
bool
CD_PluginInitialize (CDPlugin* self)
{
    self->description = CD_CreateStringFromCString("cNBT Persistence");

    CD_DO { // Initialize configuration stuff
        _config.path = "/usr/share/craftd/worlds";
        _config.base = 36;

        J_DO {
            J_STRING(self->config, "path", _config.path);
            J_INT(self->config, "base", _config.base);
        }
    }

    CD_EventRegister(self->server, "World.create",  cdnbt_WorldCreate);
    CD_EventRegister(self->server, "World.chunk",   cdnbt_WorldGetChunk);
    CD_EventRegister(self->server, "World.chunk=",  cdnbt_WorldSetChunk);
    CD_EventRegister(self->server, "World.save",    cdnbt_WorldSave);
    CD_EventRegister(self->server, "World.destroy", cdnbt_WorldDestroy);

    return true;
}

extern
bool
CD_PluginFinalize (CDPlugin* self)
{
    CD_EventUnregister(self->server, "World.create",  cdnbt_WorldCreate);
    CD_EventUnregister(self->server, "World.chunk",   cdnbt_WorldGetChunk);
    CD_EventUnregister(self->server, "World.chunk=",   cdnbt_WorldSetChunk);
    CD_EventUnregister(self->server, "World.save",    cdnbt_WorldSave);
    CD_EventUnregister(self->server, "World.destroy", cdnbt_WorldDestroy);

    return true;
}
