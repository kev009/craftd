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

#include <craftd/Packet.h>

CDPacket*
CD_PacketFromEvent (struct bufferevent* event)
{
    struct evbuffer* input  = bufferevent_get_input(event);
    size_t           length = evbuffer_get_length(input);

    if (length <= 1) {
        errno = EAGAIN;

        return NULL;
    }

    CDPacket* self = CD_malloc(sizeof(CDPacket));

    if (!self) {
        return NULL;
    }

    self->chain = CDRequest;
    self->type  = 0;

    evbuffer_remove(input, (MCByte*) &self->type, 1);
    self->data = CD_GetPacketDataFromEvent(self, event);

    return self;
}

void
CD_DestroyPacket (CDPacket* self)
{
    switch (self->chain) {
        case CDRequest: {
            switch (self->type) {
                case CDLogin: {
                    MC_DestroyString(((CDPacketLogin*) self->data)->request.username);
                    MC_DestroyString(((CDPacketLogin*) self->data)->request.password);
                } break;

                case CDHandshake: {
                    MC_DestroyString(((CDPacketHandshake*) self->data)->request.username);
                } break;

                case CDChat: {
                    MC_DestroyString(((CDPacketChat*) self->data)->request.message);
                } break;

                case CDEntityMetadata: {
                    MC_DestroyMetadata(((CDPacketEntityMetadata*) self->data)->request.metadata);
                } break;

                case CDUpdateSign: {
                    MC_DestroyString(((CDPacketUpdateSign*) self->data)->request.first);
                    MC_DestroyString(((CDPacketUpdateSign*) self->data)->request.second);
                    MC_DestroyString(((CDPacketUpdateSign*) self->data)->request.third);
                    MC_DestroyString(((CDPacketUpdateSign*) self->data)->request.fourth);
                } break;

                default: {
                    // wut
                }
            }
        } break;

        case CDResponse: {
            switch (self->type) {
                case CDNamedEntitySpawn: {
                    MC_DestroyString(((CDPacketNamedEntitySpawn*) self->data)->response.name);
                } break;

                case CDSpawnMob: {
                    MC_DestroyMetadata(((CDPacketSpawnMob*) self->data)->response.metadata);
                } break;

                case CDPainting: {
                    MC_DestroyString(((CDPacketPainting*) self->data)->response.title);
                } break;

                case CDEntityMetadata: {
                    MC_DestroyMetadata(((CDPacketEntityMetadata*) self->data)->response.metadata);
                } break;

                case CDMapChunk: {
                    CD_free(((CDPacketMapChunk*) self->data)->response.item);
                } break;

                case CDMultiBlockChange: {
                    CD_free(((CDPacketMultiBlockChange*) self->data)->response.coordinate);
                    CD_free(((CDPacketMultiBlockChange*) self->data)->response.type);
                    CD_free(((CDPacketMultiBlockChange*) self->data)->response.metadata);
                } break;

                case CDExplosion: {
                    CD_free(((CDPacketExplosion*) self->data)->response.item);
                } break;

                case CDOpenWindow: {
                    MC_DestroyString(((CDPacketOpenWindow*) self->data)->response.title);
                } break;

                case CDWindowItems: {
                    CD_free(((CDPacketWindowItems*) self->data)->response.item);
                } break;

                case CDDisconnect: {
                    MC_DestroyString(((CDPacketDisconnect*) self->data)->response.reason);
                } break;

                default: {
                    // wut
                }
            }
        } break;
    }

    CD_free(self->data);
    CD_free(self);
}

static
MCByte
cd_GetMCByteFromBuffer (struct evbuffer* input)
{
    MCByte result = 0;

    evbuffer_remove(input, &result, MCByteSize);

    return result;
}

static
MCShort
cd_GetMCShortFromBuffer (struct evbuffer* input)
{
    MCShort result = 0;

    evbuffer_remove(input, &result, MCShortSize);

    return ntohs(result);
}

static
MCInteger
cd_GetMCIntegerFromBuffer (struct evbuffer* input)
{
    MCInteger result = 0;

    evbuffer_remove(input, &result, MCIntegerSize);

    return ntohl(result);
}

static
MCLong
cd_GetMCLongFromBuffer (struct evbuffer* input)
{
    MCLong result = 0;

    evbuffer_remove(input, &result, MCLongSize);

    return ntohll(result);
}

static
MCString
cd_GetMCStringFromBuffer (struct evbuffer* input)
{
    char*   data   = NULL;
    MCShort length = 0;

    evbuffer_remove(input, &length, MCShortSize);

    length = ntohs(length);
    data   = CD_malloc(length + 1);

    evbuffer_remove(input, data, length);

    data[length] = '\0';

    return CD_CreateStringFromBuffer(data, length + 1);
}

void*
CD_GetPacketDataFromEvent (CDPacket* self, struct bufferevent* buffer)
{
    struct evbuffer* input = bufferevent_get_input(buffer);

    switch (self->type) {
        case CDKeepAlive: {
            return CD_malloc(sizeof(CDPacketKeepAlive));
        }

        case CDLogin: {
            CDPacketLogin* packet = CD_malloc(sizeof(CDPacketLogin));

            packet->request.version   = cd_GetMCIntegerFromBuffer(input);
            packet->request.username  = cd_GetMCStringFromBuffer(input);
            packet->request.password  = cd_GetMCStringFromBuffer(input);
            packet->request.mapSeed   = cd_GetMCLongFromBuffer(input);
            packet->request.dimension = cd_GetMCByteFromBuffer(input);

            return packet;
        }

        case CDHandshake: {
            CDPacketHandshake* packet = CD_malloc(sizeof(CDPacketHandshake));

            packet->request.username = cd_GetMCStringFromBuffer(input);

            return packet;
        }

        default: {
            return NULL;
        }
    }
}

CDString*
CD_PacketToString (CDPacket* self)
{
    char*  data   = CD_malloc(1);
    size_t length = 1;

    data[0] = self->type;

    switch (self->chain) {
        case CDRequest: {
            switch (self->type) {
            }
        } break;

        case CDResponse: {
            switch (self->type) {
                case CDHandshake: {
                    CDPacketHandshake* packet = self->data;

                    length += MCShortSize + CD_StringLength(packet->response.hash);
                    data    = CD_realloc(data, length);

                    *((short*) (data + 1)) = htons(CD_StringLength(packet->response.hash));
                    memcpy((data + 1 + MCShortSize), CD_StringContent(packet->response.hash), CD_StringLength(packet->response.hash));
                } break;
            }
        } break;

        default: {
            CD_free(data);

            return NULL;
        }
    }

    return CD_CreateStringFromBuffer(data, length);
}
