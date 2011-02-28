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

#include <craftd/Logger.h>
#include <craftd/Packet.h>

CDPacket*
CD_PacketFromBuffer (CDBuffer* input)
{
    if (CD_BufferLength(input) <= 1) {
        errno = EAGAIN;

        return NULL;
    }

    CDPacket* self = CD_malloc(sizeof(CDPacket));

    if (!self) {
        return NULL;
    }

    self->chain = CDRequest;
    self->type  = CD_BufferRemoveByte(input);
    self->data  = CD_GetPacketDataFromBuffer(self, input);

    return self;
}

void
CD_DestroyPacket (CDPacket* self)
{
    if (!self->data) {
        CD_free(self);
        return;
    }

    switch (self->chain) {
        case CDRequest: {
            switch (self->type) {
                case CDLogin: {
                    CDPacketLogin* packet = (CDPacketLogin*) self->data;

                    MC_DestroyString(packet->request.username);
                    MC_DestroyString(packet->request.password);
                } break;

                case CDHandshake: {
                    CDPacketHandshake* packet = (CDPacketHandshake*) self->data;

                    MC_DestroyString(packet->request.username);
                } break;

                case CDChat: {
                    CDPacketChat* packet = (CDPacketChat*) self->data;

                    MC_DestroyString(packet->request.message);
                } break;

                case CDEntityMetadata: {
                    CDPacketEntityMetadata* packet = (CDPacketEntityMetadata*) self->data;

                    MC_DestroyMetadata(packet->request.metadata);
                } break;

                case CDUpdateSign: {
                    CDPacketUpdateSign* packet = (CDPacketUpdateSign*) self->data;

                    MC_DestroyString(packet->request.first);
                    MC_DestroyString(packet->request.second);
                    MC_DestroyString(packet->request.third);
                    MC_DestroyString(packet->request.fourth);
                } break;
            }
        } break;

        case CDResponse: {
            switch (self->type) {
                case CDNamedEntitySpawn: {
                    CDPacketNamedEntitySpawn* packet = (CDPacketNamedEntitySpawn*) self->data;

                    MC_DestroyString(packet->response.name);
                } break;

                case CDSpawnMob: {
                    CDPacketSpawnMob* packet = (CDPacketSpawnMob*) self->data;

                    MC_DestroyMetadata(packet->response.metadata);
                } break;

                case CDPainting: {
                    CDPacketPainting* packet = (CDPacketPainting*) self->data;

                    MC_DestroyString(packet->response.title);
                } break;

                case CDEntityMetadata: {
                    CDPacketEntityMetadata* packet = (CDPacketEntityMetadata*) self->data;

                    MC_DestroyMetadata(packet->response.metadata);
                } break;

                case CDMapChunk: {
                    CDPacketMapChunk* packet = (CDPacketMapChunk*) self->data;

                    CD_free(packet->response.item);
                } break;

                case CDMultiBlockChange: {
                    CDPacketMultiBlockChange* packet = (CDPacketMultiBlockChange*) self->data;

                    CD_free(packet->response.coordinate);
                    CD_free(packet->response.type);
                    CD_free(packet->response.metadata);
                } break;

                case CDExplosion: {
                    CDPacketExplosion* packet = (CDPacketExplosion*) self->data;

                    CD_free(packet->response.item);
                } break;

                case CDOpenWindow: {
                    CDPacketOpenWindow* packet = (CDPacketOpenWindow*) self->data;

                    MC_DestroyString(packet->response.title);
                } break;

                case CDWindowItems: {
                    CDPacketWindowItems* packet = (CDPacketWindowItems*) self->data;

                    CD_free(packet->response.item);
                } break;

                case CDDisconnect: {
                    CDPacketDisconnect* packet = (CDPacketDisconnect*) self->data;

                    MC_DestroyString(packet->response.reason);
                } break;
            }
        } break;
    }

    CD_free((void*) self->data);
    CD_free(self);
}

CDPointer
CD_GetPacketDataFromBuffer (CDPacket* self, CDBuffer* input)
{
    switch (self->type) {
        case CDKeepAlive: {
            return (CDPointer) CD_malloc(sizeof(CDPacketKeepAlive));
        }

        case CDLogin: {
            CDPacketLogin* packet = (CDPacketLogin*) CD_malloc(sizeof(CDPacketLogin));

            packet->request.version   = CD_BufferRemoveInteger(input);
            packet->request.username  = CD_BufferRemoveString(input);
            packet->request.password  = CD_BufferRemoveString(input);
            packet->request.mapSeed   = CD_BufferRemoveLong(input);
            packet->request.dimension = CD_BufferRemoveByte(input);

            return (CDPointer) packet;
        }

        case CDHandshake: {
            CDPacketHandshake* packet = (CDPacketHandshake*) CD_malloc(sizeof(CDPacketHandshake));

            packet->request.username = CD_BufferRemoveString(input);

            return (CDPointer) packet;
        }

        case CDChat: {
            CDPacketChat* packet = (CDPacketChat*) CD_malloc(sizeof(CDPacketChat));

            packet->request.message = CD_BufferRemoveString(input);

            return (CDPointer) packet;
        }

        case CDUseEntity: {
            CDPacketUseEntity* packet = (CDPacketUseEntity*) CD_malloc(sizeof(CDPacketUseEntity));

            packet->request.user      = CD_BufferRemoveInteger(input);
            packet->request.target    = CD_BufferRemoveInteger(input);
            packet->request.leftClick = CD_BufferRemoveByte(input);

            return (CDPointer) packet;
        }

        case CDRespawn: {
            return (CDPointer) CD_malloc(sizeof(CDPacketRespawn));
        }

        case CDOnGround: {
            CDPacketOnGround* packet = (CDPacketOnGround*) CD_malloc(sizeof(CDPacketOnGround));

            packet->request.onGround = CD_BufferRemoveBoolean(input);

            return (CDPointer) packet;
        }

        case CDPlayerPosition: {
            CDPacketPlayerPosition* packet = (CDPacketPlayerPosition*) CD_malloc(sizeof(CDPacketPlayerPosition));

            packet->request.position.x  = CD_BufferRemoveDouble(input);
            packet->request.position.y  = CD_BufferRemoveDouble(input);
            packet->request.stance      = CD_BufferRemoveDouble(input);
            packet->request.position.z  = CD_BufferRemoveDouble(input);
            packet->request.is.onGround = CD_BufferRemoveBoolean(input);

            return (CDPointer) packet;
        }

        case CDPlayerLook: {
            CDPacketPlayerLook* packet = (CDPacketPlayerLook*) CD_malloc(sizeof(CDPacketPlayerLook));

            packet->request.yaw         = CD_BufferRemoveFloat(input);
            packet->request.pitch       = CD_BufferRemoveFloat(input);
            packet->request.is.onGround = CD_BufferRemoveBoolean(input);

            return (CDPointer) packet;
        }

        case CDPlayerMoveLook: {
            CDPacketPlayerMoveLook* packet = (CDPacketPlayerMoveLook*) CD_malloc(sizeof(CDPacketPlayerMoveLook));

            packet->request.position.x  = CD_BufferRemoveDouble(input);
            packet->request.stance      = CD_BufferRemoveDouble(input);
            packet->request.position.y  = CD_BufferRemoveDouble(input);
            packet->request.position.z  = CD_BufferRemoveDouble(input);
            packet->request.yaw         = CD_BufferRemoveFloat(input);
            packet->request.pitch       = CD_BufferRemoveFloat(input);
            packet->request.is.onGround = CD_BufferRemoveBoolean(input);

            return (CDPointer) packet;
        }

        default: {
            return (CDPointer) NULL;
        }
    }
}

CDBuffer*
CD_PacketToBuffer (CDPacket* self)
{
    CDBuffer* data = CD_CreateBuffer();

    CD_BufferAddByte(data, self->type);

    switch (self->chain) {
        case CDRequest: {
            switch (self->type) {
            }
        } break;

        case CDResponse: {
            switch (self->type) {
                case CDLogin: {
                    CDPacketLogin* packet = (CDPacketLogin*) self->data;

                    CD_BufferAddInteger(data, packet->response.id);
                    CD_BufferAddString(data, packet->response.serverName);
                    CD_BufferAddString(data, packet->response.motd);
                    CD_BufferAddLong(data, packet->response.mapSeed);
                    CD_BufferAddByte(data, packet->response.dimension);
                } break;

                case CDHandshake: {
                    CDPacketHandshake* packet = (CDPacketHandshake*) self->data;

                    CD_BufferAddString(data, packet->response.hash);
                } break;

                case CDChat: {
                    CDPacketChat* packet = (CDPacketChat*) self->data;

                    CD_BufferAddString(data, packet->response.message);
                } break;
            }
        } break;
    }

    return data;
}
