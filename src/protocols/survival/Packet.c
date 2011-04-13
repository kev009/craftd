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

#include <craftd/protocols/survival/Packet.h>

SVPacket*
SV_PacketFromBuffers (CDBuffers* buffers)
{
    SVPacket* self = CD_malloc(sizeof(SVPacket));

    assert(self);

    self->chain = SVRequest;
    self->type  = (uint32_t) (uint8_t) SV_BufferRemoveByte(buffers->input);
    self->data  = SV_GetPacketDataFromBuffer(self, buffers->input);

    if (!self->data) {
        ERR("unparsable packet 0x%.2X", self->type);

        SV_DestroyPacket(self);

        errno = EILSEQ;

        return NULL;
    }

    return self;
}

void
SV_DestroyPacket (SVPacket* self)
{
    assert(self);

    SV_DestroyPacketData(self);

    CD_free((void*) self->data);
    CD_free(self);
}

void
SV_DestroyPacketData (SVPacket* self)
{
    if (!self->data) {
        return;
    }

    switch (self->chain) {
        case SVRequest: {
            switch (self->type) {
                case SVLogin: {
                    SVPacketLogin* packet = (SVPacketLogin*) self->data;

                    SV_DestroyString(packet->request.username);
                    SV_DestroyString(packet->request.password);
                } break;

                case SVHandshake: {
                    SVPacketHandshake* packet = (SVPacketHandshake*) self->data;

                    SV_DestroyString(packet->request.username);
                } break;

                case SVChat: {
                    SVPacketChat* packet = (SVPacketChat*) self->data;

                    SV_DestroyString(packet->request.message);
                } break;

                case SVEntityMetadata: {
                    SVPacketEntityMetadata* packet = (SVPacketEntityMetadata*) self->data;

                    SV_DestroyMetadata(packet->request.metadata);
                } break;

                case SVUpdateSign: {
                    SVPacketUpdateSign* packet = (SVPacketUpdateSign*) self->data;

                    SV_DestroyString(packet->request.first);
                    SV_DestroyString(packet->request.second);
                    SV_DestroyString(packet->request.third);
                    SV_DestroyString(packet->request.fourth);
                } break;

                case SVDisconnect: {
                    SVPacketDisconnect* packet = (SVPacketDisconnect*) self->data;

                    SV_DestroyString(packet->request.reason);
                }

                default: break;
            }
        } break;

        case SVResponse: {
            switch (self->type) {
                case SVLogin: {
                    SVPacketLogin* packet = (SVPacketLogin*) self->data;

                    SV_DestroyString(packet->response.serverName);
                    SV_DestroyString(packet->response.motd);
                } break;

                case SVHandshake: {
                    SVPacketHandshake* packet = (SVPacketHandshake*) self->data;

                    SV_DestroyString(packet->response.hash);
                } break;

                case SVChat: {
                    SVPacketChat* packet = (SVPacketChat*) self->data;

                    SV_DestroyString(packet->request.message);
                } break;

                case SVNamedEntitySpawn: {
                    SVPacketNamedEntitySpawn* packet = (SVPacketNamedEntitySpawn*) self->data;

                    SV_DestroyString(packet->response.name);
                } break;

                case SVSpawnMob: {
                    SVPacketSpawnMob* packet = (SVPacketSpawnMob*) self->data;

                    SV_DestroyMetadata(packet->response.metadata);
                } break;

                case SVPainting: {
                    SVPacketPainting* packet = (SVPacketPainting*) self->data;

                    SV_DestroyString(packet->response.title);
                } break;

                case SVEntityMetadata: {
                    SVPacketEntityMetadata* packet = (SVPacketEntityMetadata*) self->data;

                    SV_DestroyMetadata(packet->response.metadata);
                } break;

                case SVMapChunk: {
                    SVPacketMapChunk* packet = (SVPacketMapChunk*) self->data;

                    CD_free(packet->response.item);
                } break;

                case SVMultiBlockChange: {
                    SVPacketMultiBlockChange* packet = (SVPacketMultiBlockChange*) self->data;

                    CD_free(packet->response.coordinate);
                    CD_free(packet->response.type);
                    CD_free(packet->response.metadata);
                } break;

                case SVExplosion: {
                    SVPacketExplosion* packet = (SVPacketExplosion*) self->data;

                    CD_free(packet->response.item);
                } break;

                case SVOpenWindow: {
                    SVPacketOpenWindow* packet = (SVPacketOpenWindow*) self->data;

                    SV_DestroyString(packet->response.title);
                } break;

                case SVWindowItems: {
                    SVPacketWindowItems* packet = (SVPacketWindowItems*) self->data;

                    CD_free(packet->response.item);
                } break;

                case SVDisconnect: {
                    SVPacketDisconnect* packet = (SVPacketDisconnect*) self->data;

                    SV_DestroyString(packet->response.reason);
                } break;

                default: break;
            }
        } break;
    }
}

CDPointer
SV_GetPacketDataFromBuffer (SVPacket* self, CDBuffer* input)
{
    assert(self);
    assert(input);

    switch (self->type) {
        case SVKeepAlive: {
            return (CDPointer) CD_malloc(sizeof(SVPacketKeepAlive));
        }

        case SVLogin: {
            SVPacketLogin* packet = (SVPacketLogin*) CD_malloc(sizeof(SVPacketLogin));

            SV_BufferRemoveFormat(input, "iSSlb",
                &packet->request.version,
                &packet->request.username,
                &packet->request.password,
                &packet->request.mapSeed,
                &packet->request.dimension
            );

            return (CDPointer) packet;
        }

        case SVHandshake: {
            SVPacketHandshake* packet = (SVPacketHandshake*) CD_malloc(sizeof(SVPacketHandshake));

            packet->request.username = SV_BufferRemoveString(input);

            return (CDPointer) packet;
        }

        case SVChat: {
            SVPacketChat* packet = (SVPacketChat*) CD_malloc(sizeof(SVPacketChat));

            packet->request.message = SV_BufferRemoveString(input);

            return (CDPointer) packet;
        }

        case SVUseEntity: {
            SVPacketUseEntity* packet = (SVPacketUseEntity*) CD_malloc(sizeof(SVPacketUseEntity));

            SV_BufferRemoveFormat(input, "iib",
                &packet->request.user,
                &packet->request.target,
                &packet->request.leftClick
            );

            return (CDPointer) packet;
        }

        case SVRespawn: {
            return (CDPointer) CD_malloc(sizeof(SVPacketRespawn));
        }

        case SVOnGround: {
            SVPacketOnGround* packet = (SVPacketOnGround*) CD_malloc(sizeof(SVPacketOnGround));

            packet->request.onGround = SV_BufferRemoveBoolean(input);

            return (CDPointer) packet;
        }

        case SVPlayerPosition: {
            SVPacketPlayerPosition* packet = (SVPacketPlayerPosition*) CD_malloc(sizeof(SVPacketPlayerPosition));

            SV_BufferRemoveFormat(input, "ddddb",
                &packet->request.position.x,
                &packet->request.position.y,
                &packet->request.stance,
                &packet->request.position.z,
                &packet->request.is.onGround
            );

            return (CDPointer) packet;
        }

        case SVPlayerLook: {
            SVPacketPlayerLook* packet = (SVPacketPlayerLook*) CD_malloc(sizeof(SVPacketPlayerLook));

            SV_BufferRemoveFormat(input, "ffb",
                &packet->request.yaw,
                &packet->request.pitch,
                &packet->request.is.onGround
            );

            return (CDPointer) packet;
        }

        case SVPlayerMoveLook: {
            SVPacketPlayerMoveLook* packet = (SVPacketPlayerMoveLook*) CD_malloc(sizeof(SVPacketPlayerMoveLook));

            SV_BufferRemoveFormat(input, "ddddffb",
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

        case SVPlayerDigging: {
            SVPacketPlayerDigging* packet = (SVPacketPlayerDigging*) CD_malloc(sizeof(SVPacketPlayerDigging));

            packet->request.status = SV_BufferRemoveByte(input);

            SV_BufferRemoveFormat(input, "ibi",
                &packet->request.position.x,
                &packet->request.position.y,
                &packet->request.position.z,
            );

            packet->request.face = SV_BufferRemoveByte(input);

            return (CDPointer) packet;
        }

        case SVPlayerBlockPlacement: {
            SVPacketPlayerBlockPlacement* packet = (SVPacketPlayerBlockPlacement*) CD_malloc(sizeof(SVPacketPlayerBlockPlacement));

            SV_BufferRemoveFormat(input, "ibibs",
                &packet->request.position.x,
                &packet->request.position.y,
                &packet->request.position.z,

                &packet->request.direction,
                &packet->request.item.id
            );

            if (packet->request.item.id != -1) {
                SV_BufferRemoveFormat(input, "bs",
                    &packet->request.item.count,
                    &packet->request.item.uses
                );
            }

            return (CDPointer) packet;
        }

        case SVHoldChange: {
            SVPacketHoldChange* packet = (SVPacketHoldChange*) CD_malloc(sizeof(SVPacketHoldChange));

            packet->request.item.id = SV_BufferRemoveShort(input);

            return (CDPointer) packet;
        }

        case SVAnimation: {
            SVPacketAnimation* packet = (SVPacketAnimation*) CD_malloc(sizeof(SVPacketAnimation));

            SV_BufferRemoveFormat(input, "ib",
                &packet->request.entity.id,
                &packet->request.type
            );

            return (CDPointer) packet;
        }

        case SVEntityAction: {
            SVPacketEntityAction* packet = (SVPacketEntityAction*) CD_malloc(sizeof(SVPacketEntityAction));

            packet->request.entity.id = SV_BufferRemoveInteger(input);
            packet->request.action    = SV_BufferRemoveByte(input);

            return (CDPointer) packet;
        }

        case SVEntityMetadata: {
            SVPacketEntityMetadata* packet = (SVPacketEntityMetadata*) CD_malloc(sizeof(SVPacketEntityMetadata));

            SV_BufferRemoveFormat(input, "iM",
                &packet->request.entity.id,
                &packet->request.metadata
            );

            return (CDPointer) packet;
        }

        case SVCloseWindow: {
            SVPacketCloseWindow* packet = (SVPacketCloseWindow*) CD_malloc(sizeof(SVPacketCloseWindow));

            packet->request.id = SV_BufferRemoveByte(input);

            return (CDPointer) packet;
        }

        case SVWindowClick: {
            SVPacketWindowClick* packet = (SVPacketWindowClick*) CD_malloc(sizeof(SVPacketWindowClick));

            SV_BufferRemoveFormat(input, "bsBss",
                &packet->request.id,
                &packet->request.slot,
                &packet->request.rightClick,
                &packet->request.action,
                &packet->request.item.id
            );

            if (packet->request.item.id != -1) {
                SV_BufferRemoveFormat(input, "bs",
                    &packet->request.item.count,
                    &packet->request.item.uses
                );
            }

            return (CDPointer) packet;
        }

        case SVTransaction: {
            SVPacketTransaction* packet = (SVPacketTransaction*) CD_malloc(sizeof(SVPacketTransaction));

            SV_BufferRemoveFormat(input, "bsB",
                &packet->request.id,
                &packet->request.action,
                &packet->request.accepted
            );

            return (CDPointer) packet;
        }

        case SVUpdateSign: {
            SVPacketUpdateSign* packet = (SVPacketUpdateSign*) CD_malloc(sizeof(SVPacketUpdateSign));

            SV_BufferRemoveFormat(input, "iisiSSSS",
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

        case SVDisconnect: {
            SVPacketDisconnect* packet = (SVPacketDisconnect*) CD_malloc(sizeof(SVPacketDisconnect));

            packet->request.reason = SV_BufferRemoveString(input);

            return (CDPointer) packet;
        }

        default: {
            return (CDPointer) NULL;
        }
    }
}

CDBuffer*
SV_PacketToBuffer (SVPacket* self)
{
    CDBuffer* data = CD_CreateBuffer();

    assert(self);

    SV_BufferAddByte(data, self->type);

    switch (self->chain) {
        case SVRequest: {
            switch (self->type) {
                default: break;
            }
        } break;

        case SVResponse: {
            switch (self->type) {
                case SVLogin: {
                    SVPacketLogin* packet = (SVPacketLogin*) self->data;

                    SV_BufferAddFormat(data, "iSSlb",
                        packet->response.id,
                        packet->response.serverName,
                        packet->response.motd,
                        packet->response.mapSeed,
                        packet->response.dimension
                    );
                } break;

                case SVHandshake: {
                    SVPacketHandshake* packet = (SVPacketHandshake*) self->data;

                    SV_BufferAddString(data, packet->response.hash);
                } break;

                case SVChat: {
                    SVPacketChat* packet = (SVPacketChat*) self->data;

                    SV_BufferAddString(data, packet->response.message);
                } break;

                case SVTimeUpdate: {
                    SVPacketTimeUpdate* packet = (SVPacketTimeUpdate*) self->data;

                    SV_BufferAddLong(data, packet->response.time);
                } break;

                case SVEntityEquipment: {
                    SVPacketEntityEquipment* packet = (SVPacketEntityEquipment*) self->data;

                    SV_BufferAddFormat(data, "isss",
                        packet->response.entity.id,
                        packet->response.slot,
                        packet->response.item,
                        packet->response.damage
                    );
                } break;

                case SVSpawnPosition: {
                    SVPacketSpawnPosition* packet = (SVPacketSpawnPosition*) self->data;

                    SV_BufferAddFormat(data, "iii",
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z
                    );
                } break;

                case SVUpdateHealth: {
                    SVPacketUpdateHealth* packet = (SVPacketUpdateHealth*) self->data;

                    SV_BufferAddShort(data, packet->response.health);
                } break;

                case SVPlayerMoveLook: {
                    SVPacketPlayerMoveLook* packet = (SVPacketPlayerMoveLook*) self->data;

                    SV_BufferAddFormat(data, "ddddffB",
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.stance,
                        packet->response.position.z,
                        packet->response.yaw,
                        packet->response.pitch,
                        packet->response.is.onGround
                    );
                } break;

                case SVUseBed: {
                    SVPacketUseBed* packet = (SVPacketUseBed*) self->data;

                    SV_BufferAddFormat(data, "ibibi",
                        packet->response.entity.id,
                        packet->response.inBed,
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z
                    );
                } break;

                case SVAnimation: {
                    SVPacketAnimation* packet = (SVPacketAnimation*) self->data;

                    SV_BufferAddFormat(data, "ib",
                        packet->response.entity.id,
                        packet->response.type
                    );
                } break;

                case SVNamedEntitySpawn: {
                    SVPacketNamedEntitySpawn* packet = (SVPacketNamedEntitySpawn*) self->data;

                    SV_BufferAddFormat(data, "iSiiibbs",
                        packet->response.entity.id,
                        packet->response.name,
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z,
                        packet->response.rotation,
                        packet->response.pitch,
                        packet->response.item
                    );
                } break;

                case SVPickupSpawn: {
                    SVPacketPickupSpawn* packet = (SVPacketPickupSpawn*) self->data;

                    SV_BufferAddFormat(data, "isbsiiibbb",
                        packet->response.entity.id,
                        packet->response.item.id,
                        packet->response.item.count,
                        packet->response.item.uses,
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z,
                        packet->response.rotation,
                        packet->response.pitch,
                        packet->response.roll
                    );
                } break;

                case SVCollectItem: {
                    SVPacketCollectItem* packet = (SVPacketCollectItem*) self->data;

                    SV_BufferAddFormat(data, "ii",
                        packet->response.collected,
                        packet->response.collector
                    );
                } break;

                case SVSpawnObject: {
                    SVPacketSpawnObject* packet = (SVPacketSpawnObject*) self->data;

                    SV_BufferAddFormat(data, "ibiii",
                        packet->response.entity.id,
                        packet->response.type,
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z
                    );
                } break;

                case SVSpawnMob: {
                    SVPacketSpawnMob* packet = (SVPacketSpawnMob*) self->data;

                    SV_BufferAddFormat(data, "ibiiibbM",
                        packet->response.id,
                        packet->response.type,
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z,
                        packet->response.yaw,
                        packet->response.pitch,
                        packet->response.metadata
                    );
                } break;

                case SVPainting: {
                    SVPacketPainting* packet = (SVPacketPainting*) self->data;

                    SV_BufferAddFormat(data, "iSiiii",
                        packet->response.entity.id,
                        packet->response.title,
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z,
                        packet->response.type
                    );
                } break;

                case SVEntityVelocity: {
                    SVPacketEntityVelocity* packet = (SVPacketEntityVelocity*) self->data;

                    SV_BufferAddFormat(data, "isss",
                        packet->response.entity.id,
                        packet->response.velocity.x,
                        packet->response.velocity.y,
                        packet->response.velocity.z
                    );
                } break;

                case SVEntityDestroy: {
                    SVPacketEntityDestroy* packet = (SVPacketEntityDestroy*) self->data;

                    SV_BufferAddInteger(data, packet->response.entity.id);
                } break;

                case SVEntityCreate: {
                    SVPacketEntityCreate* packet = (SVPacketEntityCreate*) self->data;

                    SV_BufferAddInteger(data, packet->response.entity.id);
                } break;

                case SVEntityRelativeMove: {
                    SVPacketEntityRelativeMove* packet = (SVPacketEntityRelativeMove*) self->data;

                    SV_BufferAddFormat(data, "ibbb",
                        packet->response.entity.id,
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z
                    );
                } break;

                case SVEntityLook: {
                    SVPacketEntityLook* packet = (SVPacketEntityLook*) self->data;

                    SV_BufferAddFormat(data, "ibb",
                        packet->response.entity.id,
                        packet->response.yaw,
                        packet->response.pitch
                    );
                } break;

                case SVEntityLookMove: {
                    SVPacketEntityLookMove* packet = (SVPacketEntityLookMove*) self->data;

                    SV_BufferAddFormat(data, "ibbbbb",
                        packet->response.entity.id,
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z,
                        packet->response.yaw,
                        packet->response.pitch
                    );
                } break;

                case SVEntityTeleport: {
                    SVPacketEntityTeleport* packet = (SVPacketEntityTeleport*) self->data;

                    SV_BufferAddFormat(data, "iiiibb",
                        packet->response.entity.id,
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z,
                        packet->response.rotation,
                        packet->response.pitch
                    );
                } break;

                case SVEntityStatus: {
                    SVPacketEntityStatus* packet = (SVPacketEntityStatus*) self->data;

                    SV_BufferAddFormat(data, "ib",
                        packet->response.entity.id,
                        packet->response.status
                    );
                } break;

                case SVEntityAttach: {
                    SVPacketEntityAttach* packet = (SVPacketEntityAttach*) self->data;

                    SV_BufferAddFormat(data, "ii",
                        packet->response.entity.id,
                        packet->response.vehicle.id
                    );
                } break;

                case SVEntityMetadata: {
                    SVPacketEntityMetadata* packet = (SVPacketEntityMetadata*) self->data;

                    SV_BufferAddFormat(data, "iM",
                        packet->response.entity.id,
                        packet->response.metadata
                    );
                } break;

                case SVPreChunk: {
                    SVPacketPreChunk* packet = (SVPacketPreChunk*) self->data;

                    SV_BufferAddFormat(data, "iiB",
                        packet->response.position.x,
                        packet->response.position.z,
                        packet->response.mode
                    );
                } break;

                case SVMapChunk: {
                    SVPacketMapChunk* packet = (SVPacketMapChunk*) self->data;

                    SV_BufferAddFormat(data, "isibbb",
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z,

                        packet->response.size.x - 1,
                        packet->response.size.y - 1,
                        packet->response.size.z - 1
                    );

                    SV_BufferAddInteger(data, packet->response.length);

                    CD_BufferAdd(data, (CDPointer) packet->response.item, packet->response.length * SVByteSize);
                } break;

                case SVMultiBlockChange: {
                    SVPacketMultiBlockChange* packet = (SVPacketMultiBlockChange*) self->data;

                    SV_BufferAddFormat(data, "ii",
                        packet->response.position.x,
                        packet->response.position.z
                    );

                    SV_BufferAddShort(data, packet->response.length);

                    CD_BufferAdd(data, (CDPointer) packet->response.coordinate, packet->response.length * SVShortSize);
                    CD_BufferAdd(data, (CDPointer) packet->response.type,       packet->response.length * SVByteSize);
                    CD_BufferAdd(data, (CDPointer) packet->response.metadata,   packet->response.length * SVByteSize);
                } break;

                case SVBlockChange: {
                    SVPacketBlockChange* packet = (SVPacketBlockChange*) self->data;

                    SV_BufferAddFormat(data, "ibibb",
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z,

                        packet->response.type,
                        packet->response.metadata
                    );
                } break;

                case SVPlayNoteBlock: {
                    SVPacketPlayNoteBlock* packet = (SVPacketPlayNoteBlock*) self->data;

                    SV_BufferAddFormat(data, "isibb",
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z,

                        packet->response.instrument,
                        packet->response.pitch
                    );
                } break;

                case SVExplosion: {
                    SVPacketExplosion* packet = (SVPacketExplosion*) self->data;

                    SV_BufferAddFormat(data, "dddf",
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z,

                        packet->response.radius
                    );

                    SV_BufferAddInteger(data, packet->response.length);

                    CD_BufferAdd(data, (CDPointer) packet->response.item, packet->response.length * 3 * SVByteSize);
                } break;

                case SVOpenWindow: {
                    SVPacketOpenWindow* packet = (SVPacketOpenWindow*) self->data;

                    SV_BufferAddFormat(data, "bbSb",
                        packet->response.id,
                        packet->response.type,
                        packet->response.title,
                        packet->response.slots
                    );
                } break;

                case SVCloseWindow: {
                    SVPacketCloseWindow* packet = (SVPacketCloseWindow*) self->data;

                    SV_BufferAddByte(data, packet->response.id);
                } break;

                case SVSetSlot: {
                    SVPacketSetSlot* packet = (SVPacketSetSlot*) self->data;

                    SV_BufferAddFormat(data, "bss",
                        packet->response.id,
                        packet->response.slot,
                        packet->response.item.id
                    );

                    if (packet->response.item.id != -1) {
                        SV_BufferAddFormat(data, "bs",
                            packet->response.item.count,
                            packet->response.item.uses
                        );
                    }
                } break;

                case SVWindowItems: {
                    SVPacketWindowItems* packet = (SVPacketWindowItems*) self->data;

                    SV_BufferAddByte(data, packet->response.id);
                    SV_BufferAddShort(data, packet->response.length);

                    for (size_t i = 0; i < packet->response.length; i++) {
                        if (packet->response.item[i].id == -1) {
                            SV_BufferAddShort(data, -1);
                        }
                        else {
                            SV_BufferAddFormat(data, "sbs",
                                packet->response.item[i].id,
                                packet->response.item[i].count,
                                packet->response.item[i].uses
                            );
                        }
                    }
                } break;

                case SVUpdateProgressBar: {
                    SVPacketUpdateProgressBar* packet = (SVPacketUpdateProgressBar*) self->data;

                    SV_BufferAddFormat(data, "bss",
                        packet->response.id,
                        packet->response.bar,
                        packet->response.value
                    );
                } break;

                case SVTransaction: {
                    SVPacketTransaction* packet = (SVPacketTransaction*) self->data;

                    SV_BufferAddFormat(data, "bsB",
                        packet->response.id,
                        packet->response.action,
                        packet->response.accepted
                    );
                } break;

                case SVUpdateSign: {
                    SVPacketUpdateSign* packet = (SVPacketUpdateSign*) self->data;

                    SV_BufferAddFormat(data, "isiSSSS",
                        packet->response.position.x,
                        packet->response.position.y,
                        packet->response.position.z,

                        packet->response.first,
                        packet->response.second,
                        packet->response.third,
                        packet->response.fourth
                    );
                } break;

                case SVDisconnect: {
                    SVPacketDisconnect* packet = (SVPacketDisconnect*) self->data;

                    SV_BufferAddString(data, packet->response.reason);
                } break;

                default: break;
            }
        } break;
    }

    return data;
}
