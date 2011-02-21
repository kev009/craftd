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

#include "minecraft.h"
#include "common.h"

void
MC_DestroyString (MCString object)
{
    bdestroy(object);
}

MCMetadata*
MC_CreateMetadata (void)
{
    MCMetadata* object = MC_malloc(sizeof(MCMetadata));

    if (!object) {
        return NULL;
    }
}

void
MC_DestroyMetadata (MCMetadata* object)
{
    size_t i;

    for (i = 0; i < object->length; i++) {
        MC_DestroyData(object->item[i]);
    }

    MC_free(object->item);
    MC_free(object);
}

MCData*
MC_CreateData (void)
{
    MCData* object = CD_malloc(sizeof(MCData));

    if (!object) {
        return NULL;
    }

    return object;
}

void
MC_DestroyData (MCData* object)
{
    // Destroy the bstring, the other types lay on the stack
    if (object->type == MCTypeString) {
        bdestroy(object->data.s);
    }

    CD\_free(object);
}

MCMetadata*
MC_ConcatDatas (MCMetadata* metadata, MCData** items, size_t length)
{
    size_t i;

    for (i = 0; i < length; i++) {
        MC_AppendData(metadata, items[i]);
    }

    return metadata;
}

MCMetadata*
MC_AppendData (MCMetadata* metadata, MCData* data)
{
    metadata->item = MC_realloc(metadata->item, sizeof(MCData*) * ++metadata->length);

    metadata->item[metadata->length - 1] = data;

    return metadata;
}

MCMetadata*
MC_MetadataFromEvent (struct bufferevent* event)
{
    MCMetadata*      metadata = MC_CreateMetadata();
    MCData*          current  = NULL;
    MCByte           type     = 0;
    struct evbuffer* input    = bufferevent_get_input(event);
    MCShort          length;
    char*            buffer;

    // Sizes of the different metadata types
    static char sizes[] = { 1, 2, 4, 4, -1, 5 };

    while (true) {
        evbuffer_remove(input, &type, 1);

        if (type == 127) {
            break;
        }
        
        current       = MC_CreateData();
        current->type = type;

        if (current->type == MCTypeString) {
            evbuffer_remove(input, &length, 2);
            buffer = MC_malloc(length);
            evbuffer_remove(input, buffer, length);
            bassignblk(current->data.s, buffer, length);
        }
        else {
            current->data = evbuffer_remove(input, &current->data, sizes[current->type]);
        }

        MC_AppendData(metadata, current);
    }

    return metadata;
}
