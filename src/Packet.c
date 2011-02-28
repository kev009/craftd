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
                case CDHandshake: {
                    CDPacketHandshake* packet = (CDPacketHandshake*) self->data;

                    MC_DestroyString(packet->response.hash);
                } break;

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

            CD_BufferRemoveFormat(input, "iSSlb",
                &packet->request.version,
                &packet->request.username,
                &packet->request.password,
                &packet->request.mapSeed,
                &packet->request.dimension
            );

            return (CDPointer) packet;
        }

        case CDHandshake: {
            CDPacketHandshake* packet = (CDPacketHandshake*) CD_malloc(sizeof(CDPacketHandshake));

            CD_BufferRemoveFormat(input, "S",
                &packet->request.username
            );

            return (CDPointer) packet;
        }

        case CDChat: {
            CDPacketChat* packet = (CDPacketChat*) CD_malloc(sizeof(CDPacketChat));

            CD_BufferRemoveFormat(input, "S",
                &packet->request.message
            );

            return (CDPointer) packet;
        }

        case CDUseEntity: {
            CDPacketUseEntity* packet = (CDPacketUseEntity*) CD_malloc(sizeof(CDPacketUseEntity));

            CD_BufferRemoveFormat(input, "iib",
                &packet->request.user,
                &packet->request.target,
                &packet->request.leftClick
            );

            return (CDPointer) packet;
        }

        case CDRespawn: {
            return (CDPointer) CD_malloc(sizeof(CDPacketRespawn));
        }

        case CDOnGround: {
            CDPacketOnGround* packet = (CDPacketOnGround*) CD_malloc(sizeof(CDPacketOnGround));

            CD_BufferRemoveFormat(input, "B",
                &packet->request.onGround
            );

            return (CDPointer) packet;
        }

        case CDPlayerPosition: {
            CDPacketPlayerPosition* packet = (CDPacketPlayerPosition*) CD_malloc(sizeof(CDPacketPlayerPosition));

            CD_BufferRemoveFormat(input, "ddddb",
                &packet->request.position.x,
                &packet->request.position.y,
                &packet->request.stance,
                &packet->request.position.z,
                &packet->request.is.onGround
            );

            return (CDPointer) packet;
        }

        case CDPlayerLook: {
            CDPacketPlayerLook* packet = (CDPacketPlayerLook*) CD_malloc(sizeof(CDPacketPlayerLook));

            CD_BufferRemoveFormat(input, "ffb",
                &packet->request.yaw,
                &packet->request.pitch,
                &packet->request.is.onGround
            );

            return (CDPointer) packet;
        }

        case CDPlayerMoveLook: {
            CDPacketPlayerMoveLook* packet = (CDPacketPlayerMoveLook*) CD_malloc(sizeof(CDPacketPlayerMoveLook));

            CD_BufferRemoveFormat(input, "ddddffb",
                &packet->request.position.x,
                &packet->request.stance,
                &packet->request.position.y,
                &packet->request.position.z,
                &packet->request.yaw,
                &packet->request.pitch,
                &packet->request.is.onGround
            );

            return (CDPointer) packet;
        }

        case CDPlayerDigging: {
            CDPacketPlayerDigging* packet = (CDPacketPlayerDigging*) CD_malloc(sizeof(CDPacketPlayerDigging));

            CD_BufferRemoveFormat(input, "bibib",
                &packet->request.status,
                &packet->request.position.x,
                &packet->request.position.y,
                &packet->request.position.z,
                &packet->request.face
            );

            return (CDPointer) packet;
        }

        case CDPlayerBlockPlacement: {
            CDPacketPlayerBlockPlacement* packet = (CDPacketPlayerBlockPlacement*) CD_malloc(sizeof(CDPacketPlayerBlockPlacement));

            CD_BufferRemoveFormat(input, "ibibsbs",
                &packet->request.position.x,
                &packet->request.position.y,
                &packet->request.position.z,

                &packet->request.direction,
                &packet->request.item.id,
                &packet->request.item.count,
                &packet->request.item.uses
            );

            return (CDPointer) packet;
        }

        case CDHoldChange: {
            CDPacketHoldChange* packet = (CDPacketHoldChange*) CD_malloc(sizeof(CDPacketHoldChange));

            CD_BufferRemoveFormat(input, "s",
                &packet->request.item.id
            );

            return (CDPointer) packet;
        }

        case CDEntityAction: {
            CDPacketEntityAction* packet = (CDPacketEntityAction*) CD_malloc(sizeof(CDPacketEntityAction));

            CD_BufferRemoveFormat(input, "ib",
                &packet->request.entity.id,
                &packet->request.action
            );

            return (CDPointer) packet;
        }

        case CDEntityMetadata: {
            CDPacketEntityMetadata* packet = (CDPacketEntityMetadata*) CD_malloc(sizeof(CDPacketEntityMetadata));

            CD_BufferRemoveFormat(input, "iM",
                &packet->request.entity.id,
                &packet->request.metadata
            );

            return (CDPointer) packet;
        }

        case CDWindowClick: {
            CDPacketWindowClick* packet = (CDPacketWindowClick*) CD_malloc(sizeof(CDPacketWindowClick));

            CD_BufferRemoveFormat(input, "bsBssbs",
                &packet->request.id,
                &packet->request.slot,
                &packet->request.rightClick,
                &packet->request.action,

                &packet->request.item.id,
                &packet->request.item.count,
                &packet->request.item.uses
            );

            return (CDPointer) packet;
        }

        case CDTransaction: {
            CDPacketTransaction* packet = (CDPacketTransaction*) CD_malloc(sizeof(CDPacketTransaction));

            CD_BufferRemoveFormat(input, "bsB",
                &packet->request.id,
                &packet->request.action,
                &packet->request.accepted
            );

            return (CDPointer) packet;
        }

        case CDUpdateSign: {
            CDPacketUpdateSign* packet = (CDPacketUpdateSign*) CD_malloc(sizeof(CDPacketUpdateSign));

            CD_BufferRemoveFormat(input, "iisiSSSS",
                &packet->request.position.x,
                &packet->request.position.y,
                &packet->request.position.z,

                &packet->request.first,
                &packet->request.second,
                &packet->request.third,
                &packet->request.fourth
            );

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

                    CD_BufferAddFormat(data, "iSSlb",
                        packet->response.id,
                        packet->response.serverName,
                        packet->response.motd,
                        packet->response.mapSeed,
                        packet->response.dimension
                    );
                } break;

                case CDHandshake: {
                    CDPacketHandshake* packet = (CDPacketHandshake*) self->data;

                    CD_BufferAddFormat(data, "S",
                        packet->response.hash
                    );
                } break;

                case CDChat: {
                    CDPacketChat* packet = (CDPacketChat*) self->data;

                    CD_BufferAddFormat(data, "S",
                        packet->response.message
                    );
                } break;
            }
        } break;
    }

    return data;
}
