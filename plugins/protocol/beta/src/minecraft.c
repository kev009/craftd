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

#define CRAFTD_MINECRAFT_IGNORE_EXTERN
#include <beta/common.h>
#undef CRAFTD_MINECRAFT_IGNORE_EXTERN

const char* MCCharset =
    " #$%&\"()*+,-./:;<=>!?@[\\]^_'{|}~⌂ªº¿®¬½¼¡«»£×ƒ"
    "0123456789"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    "ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜøØáíóúñÑ";

const char MCCharsetPixel[] = {
    4, 5, 5, 5, 5, 4, 4, 4, 4, 5, 1, 5, 1, 5, 1, 1, 4, 5, 4, 1, 5, 6, 3, 5, 3, 5
};

const MCEntityId MCMaxEntityId = INT_MAX;

void
MC_DestroyString (MCString self)
{
    CD_DestroyString(self);
}

MCMetadata*
MC_CreateMetadata (void)
{
    MCMetadata* self = CD_malloc(sizeof(MCMetadata));

    if (!self) {
        return NULL;
    }

    return self;
}

void
MC_DestroyMetadata (MCMetadata* self)
{
    for (size_t i = 0; i < self->length; i++) {
        MC_DestroyData(self->item[i]);
    }

    CD_free(self->item);
    CD_free(self);
}

MCData*
MC_CreateData (void)
{
    MCData* self = CD_malloc(sizeof(MCData));

    if (!self) {
        return NULL;
    }

    return self;
}

void
MC_DestroyData (MCData* self)
{
    // Destroy the bstring, the other types lay on the stack
    if (self->type == MCTypeString) {
        CD_DestroyString(self->data.S);
    }

    CD_free(self);
}

MCMetadata*
MC_ConcatDatas (MCMetadata* metadata, MCData** items, size_t length)
{
    for (size_t i = 0; i < length; i++) {
        MC_AppendData(metadata, items[i]);
    }

    return metadata;
}

MCMetadata*
MC_AppendData (MCMetadata* metadata, MCData* data)
{
    metadata->item = CD_realloc(metadata->item, sizeof(MCData*) * ++metadata->length);

    metadata->item[metadata->length - 1] = data;

    return metadata;
}

bool
MC_StringIsValid (MCString self)
{
    assert(self);

    for (size_t i = 0, ie = CD_StringLength(self); i < ie; i++) {
        bool      has = false;
        CDString* ch  = CD_CharAt(self, i);

        for (size_t h = 0, he = CD_UTF8_strlen(MCCharset); h < he; h++) {
            const char* che = &MCCharset[CD_UTF8_offset(MCCharset, h)];

            if (strncmp(CD_StringContent(ch), che, CD_StringSize(ch)) == 0) {
                has = true;
                break;
            }
        }

        if (!has && !(strncmp(CD_StringContent(ch), "§", 2) == 0 && i < ie - 2)) {
            CD_DestroyString(ch);
            return false;
        }

        CD_DestroyString(ch);
    }

    return true;
}

MCString
MC_StringSanitize (MCString self)
{
    CDString* result = CD_CreateString();

    assert(self);

    for (size_t i = 0, ie = CD_StringLength(self); i < ie; i++) {
        bool      has = false;
        CDString* ch  = CD_CharAt(self, i);

        for (size_t h = 0, he = CD_UTF8_strlen(MCCharset); h < he; h++) {
            const char* che = &MCCharset[CD_UTF8_offset(MCCharset, h)];

            if (strncmp(CD_StringContent(ch), che, CD_StringSize(ch)) == 0) {
                has = true;
                break;
            }
        }

        if (i == ie - 2 && strncmp(CD_StringContent(ch), "§", 2) == 0){
            CD_DestroyString(ch);
            break;
        }

        if (has || strncmp(CD_StringContent(ch), "§", 2) == 0) {
            CD_AppendString(result, ch);
        }
        else {
            CD_AppendCString(result, "?");
        }

        CD_DestroyString(ch);
    }

    self->length = CD_UTF8_strnlen(CD_StringContent(self), self->raw->slen);

    return result;
}

MCString
MC_StringColorRange (MCString self, MCStringColor color, size_t a, size_t b)
{
    if (self->external) {
        CDString* tmp = self;
        self          = CD_CloneString(tmp);
        CD_DestroyString(tmp);
    }

    CDString* start = CD_CreateStringFromFormat("§%x", color);
    CDString* end   = CD_CreateStringFromCString(MC_COLOR_WHITE);

    assert(self);
    assert(a < b);
    assert(a >= 0 && b <= CD_StringLength(self));

    CD_InsertString(self, end, b);
    CD_InsertString(self, start, a);

    CD_DestroyString(start);
    CD_DestroyString(end);

    return self;
}

MCString
MC_StringColor (MCString self, MCStringColor color)
{
    assert(self);

    if (CD_StringLength(self) > 0) {
        return MC_StringColorRange(self, color, 0, CD_StringLength(self));
    }
    else {
        return self;
    }
}

bool
CD_CompareChunkPosition (CDSet* self, MCChunkPosition* a, MCChunkPosition* b)
{
    assert(self);

    return (a->x == b->x && a->z == b->z);
}

unsigned int
CD_HashChunkPosition (CDSet* self, MCChunkPosition* position)
{
    const int HASHMULTIPLIER = 31;
    const int CHUNKBUCKETS   = 401; // Max chunks to the nearest prime

    assert(self);

    return ((((position->x * HASHMULTIPLIER)) * HASHMULTIPLIER + position->z) * HASHMULTIPLIER) % CHUNKBUCKETS;
}

void
MC_ChunkToByteArray (MCChunk* chunk, uint8_t* array)
{
    size_t offset = 0;

    memcpy(array + offset, chunk->blocks, 32768);
    offset += 32768;
    memcpy(array + offset, chunk->data, 16384);
    offset += 16384;
    memcpy(array + offset, chunk->blockLight, 16384);
    offset += 16384;
    memcpy(array + offset, chunk->skyLight, 16384);
}
