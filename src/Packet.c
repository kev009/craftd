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

    CDPacket* object = CD_malloc(sizeof(CDPacket));

    if (!object) {
        CDError = CDFail;
        return NULL;
    }

    evbuffer_remove(input, &object->type, 1);
    length = len_statemachine(object->type, input);

    if (length < 0) {
        case (-length) {
            case EAGAIN: {
                CDError = CDNone;

                LOGT(LOG_DEBUG, "EAGAIN");
            } break;

            case EILSEQ: {
                CDError = CDFail;

                LOG(LOG_ERR, "EILSEQ in recv buffer!, pkttype: 0x%.2x", object->type);
            } break;

            default: {
                CDError = CDUnknown;
            }
        }

        return CD_DestroyPacket(object);
    }

    object->data = CD_GetPacketDataFromEvent(object, event);

    return object;
}

void
CD_DestroyPacket (CDPacket* object)
{
    switch (object->type) {
        case CDLogin: {
            MC_DestroyString(((CDPacketLogin*) object->data)->username);
            MC_DestroyString(((CDPacketLogin*) object->data)->password);
        } break;

        case CDHandshake: {
            MC_DestroyString(((CDPacketHandshake*) object->data)->username);
        } break;

        case CDChat: {
            MC_DestroyString(((CDPacketChat*) object->data)->message);
        } break;

        case CDNamedEntitySpawn: {
            MC_DestroyString(((CDPacketNamedEntitySpawn*) object->data)->name);
        } break;

        case CDSpawnMob: {
            MC_DestroyMetadata(((CDPacketSpawnMob*) object->data)->metadata);
        } break;

        case CDPainting: {
            MC_DestroyString(((CDPacketPainting*) object->data)->title);
        } break;

        case CDEntityMetadata: {
            MC_DestroyMetadata(((CDPacketEntityMetadata*) object->data)->metadata);
        } break;
    }

    CD_free(object->data);
    CD_free(object);
}

void*
CD_GetPacketDataFromEvent (CDPacket* packet, struct bufferevent* event)
{
    void*  data   = NULL;
    size_t length = 0;

    switch (packet->type) {
        case CDKeepAlive: {
            evbuffer_drain(input, 1);
        } break;

        case CDLogin: {
            int16_t leng;

            evbuffer_remove(input, data);
        } break;
    }

    return data;
}
