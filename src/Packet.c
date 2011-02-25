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

#include "Error.h"
#include "Packet.h"
#include "common.h"

CDPacket*
CD_PacketFromEvent (struct bufferevent* event)
{
    struct evbuffer* input  = bufferevent_get_input(event);
    size_t           length = evbuffer_get_length(input);

    if (length == 0) {
        CDError = CDNone;
        return NULL;
    }

    CDPacket* self = CD_malloc(sizeof(CDPacket));

    if (!self) {
        CDError = CDFail;
        return NULL;
    }

    evbuffer_remove(input, &self->type, 1);
    self->data = CD_GetPacketDataFromEvent(self, event);

    return self;
}

void
CD_DestroyPacket (CDPacket* self)
{
    switch (self->type) {
        case CDLogin: {
            MC_DestroyString(((CDPacketLogin*) self->data)->username);
            MC_DestroyString(((CDPacketLogin*) self->data)->password);
        } break;

        case CDHandshake: {
            MC_DestroyString(((CDPacketHandshake*) self->data)->username);
        } break;

        case CDChat: {
            MC_DestroyString(((CDPacketChat*) self->data)->message);
        } break;

        case CDNamedEntitySpawn: {
            MC_DestroyString(((CDPacketNamedEntitySpawn*) self->data)->name);
        } break;

        case CDSpawnMob: {
            MC_DestroyMetadata(((CDPacketSpawnMob*) self->data)->metadata);
        } break;

        case CDPainting: {
            MC_DestroyString(((CDPacketPainting*) self->data)->title);
        } break;

        case CDEntityMetadata: {
            MC_DestroyMetadata(((CDPacketEntityMetadata*) self->data)->metadata);
        } break;

        case CDMapChunk: {
            CD_free(((CDPacketMapChunk*) self->data)->item);
        } break;

        case CDMultiBlockChange: {
            CD_free(((CDPacketMultiBlockChange*) self->data)->coordinate);
            CD_free(((CDPacketMultiBlockChange*) self->data)->type);
            CD_free(((CDPacketMultiBlockChange*) self->data)->metadata);
        } break;

        case CDExplosion: {
            CD_free(((CDPacketExplosion*) self->data)->item);
        } break;

        case CDOpenWindow: {
            MC_DestroyString(((CDPacketOpenWindow*) self->data)->title);
        } break;

        case CDWindowItems: {
            CD_free(((CDPacketWindowItems*) self->data)->item);
        } break;

        case CDUpdateSign: {
            MC_DestroyString(((CDPacketUpdateSign*) self->data)->first);
            MC_DestroyString(((CDPacketUpdateSign*) self->data)->second);
            MC_DestroyString(((CDPacketUpdateSign*) self->data)->third);
            MC_DestroyString(((CDPacketUpdateSign*) self->data)->fourth);
        } break;

        case CDDisconnect: {
            MC_DestroyString(((CDPacketDisconnect*) self->data)->reason);
        } break;
    }

    CD_free(self->data);
    CD_free(self);
}

void*
CD_GetPacketDataFromEvent (CDPacket* self, struct bufferevent* buffer)
{
    struct evbuffer* input  = bufferevent_get_input(buffer);
    void*            data   = NULL;
    size_t           length = 0;

    switch (self->type) {
        case CDKeepAlive: {
            evbuffer_drain(input, 1);
        } break;

        case CDLogin: {
            int16_t len;

//            evbuffer_remove(input, data);
        } break;
    }

    return data;
}

bstring
CD_PacketToString (CDPacket* self)
{
    return NULL;
}
