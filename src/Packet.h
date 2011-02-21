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

#ifndef CRAFTD_PACKET_H
#define CRAFTD_PACKET_H

typedef enum _CDPacketType {
    CDKeepAlive            = 0x00,
    CDLogin                = 0x01,
    CDHandshake            = 0x02,
    CDChat                 = 0x03,
    CDTimeUpdate           = 0x04,
    CDEntityEquipment      = 0x05,
    CDSpawnPosition        = 0x06,
    CDUseEntity            = 0x07,
    CDUpdateHealth         = 0x08,
    CDRespawn              = 0x09,
    CDOnGround             = 0x0A,
    CDPlayerPosition       = 0x0B,
    CDPlayerLook           = 0x0C,
    CDPlayerMoveLook       = 0x0D,
    CDPlayerDigging        = 0x0E,
    CDPlayerBlockPlacement = 0x0F,
    CDHoldChange           = 0x10,
    CDArmAnimate           = 0x12,
    CDEntityAction         = 0x13,
    CDNamedEntitySpawn     = 0x14,
    CDPickupSpawn          = 0x15,
    CDCollectItem          = 0x16,
    CDSpawnObject          = 0x17,
    CDSpawnMob             = 0x18,
    CDPainting             = 0x19,
    CDEntityVelocity       = 0x1C,
    CDEntityDestroy        = 0x1D,
    CDEntityInit           = 0x1E,
    CDEntityRelativeMove   = 0x1F,
    CDEntityLook           = 0x20,
    CDEntityLookMove       = 0x21,
    CDEntityTeleport       = 0x22,
    CDEntityStatus         = 0x26,
    CDEntityAttach         = 0x27,
    CDEntityMetadata       = 0x28,
    CDPreChunk             = 0x32,
    CDMapChunk             = 0x33,
    CDMultiBlockChange     = 0x34,
    CDBlockChange          = 0x35,
    CDExplosion            = 0x3C,
    CDOpenWindow           = 0x64,
    CDCloseWindow          = 0x65,
    CDWindowClick          = 0x66,
    CDSetSlot              = 0x67,
    CDWindowItems          = 0x68,
    CDUpdateProgressBar    = 0x69,
    CDTransaction          = 0x6A,
    CDUpdateSign           = 0x82,
    CDDisconnect           = 0xFF
} CDPacketType;

typedef struct _CDPacket {
    CDPacketType type;
    size_t       length;
    void*        data;
} CDPacket;

CDPacket* CD_PacketFromEvent (struct bufferevent* event);

void* CD_GetPacketDataFromEvent (CDPacket* packet, struct bufferevent* event);

#endif
