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

#include "Entity.h"
#include "Position.h"
#include "Item.h"

#include "minecraft.h"

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
    CDEntityCreate         = 0x1E,
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

typedef struct _MCEntity {
    MCInteger id;
} MCEntity;

typedef struct _MCItem {
    MCShort id;
} MCItem;

typedef struct _CDPacketKeepAlive {
    // This packet has no data
} CDPacketKeepAlive;

typedef struct _CDPacketLogin {
    MCInteger version;

    MCString username;
    MCString password;

    MCLong mapSeed;
    MCByte dimension;
} CDPacketLogin;

typedef struct _CDPacketHandshake {
    MCString username;
} CDPacketHandshake;

typedef struct _CDPacketChat {
    MCString message;
} CDPacketChat;

typedef struct _CDPacketTimeUpdate {
    MCLong time;
} CDPacketTimeUpdate;

typedef struct _CDPacketEntityEquipment {
    MCEntity entity;

    MCShort   slot;
    MCShort   item;
    MCShort   damage; // Still not sure about it
} MCEntityEquipment;

typedef struct _CDPacketSpawnPosition {
    MCPosition position;
} CDPacketSpawnPosition;

typedef struct _CDPacketUseEntity {
    MCInteger user;
    MCInteger target;
    MCBoolean leftClick;
} CDPacketUseEntity;

typedef struct _CDPacketUpdateHealth {
    MCShort health;
} CDPacketUpdateHealth;

typedef struct _CDPacketRespawn {
    // This packet has no data
} CDPacketRespawn;

typedef struct _CDPacketOnGround {
    MCBoolean onGround;
} CDPacketOnGround;

typedef struct _CDPacketPlayerPosition {
    MCPrecisePosition position;

    MCDouble stance;

    CDPacketOnGround is;
} CDPacketPlayerPosition;

typedef struct _CDPacketPlayerLook {
    MCFloat yaw;
    MCFloat pitch;

    CDPacketOnGround is;
} CDPacketPlayerLook;

typedef struct _CDPacketPlayerMoveLook {
    CDPacketPlayerPosition position;
    CDPacketPlayerLook     look;

    CDPacketOnGround is;
} CDPacketPlayerMoveLook;

typedef struct _CDPacketDigging {
    enum {
        CDStartedDigging,
        CDDigging,
        CDStoppedDigging,
        CDBlockBroken,
        CDDropItem
    } status;

    MCPosition position;

    enum {
        CDFaceNegativeY,
        CDFacePositiveY,
        CDFaceNegativeZ,
        CDFacePositiveZ,
        CDFaceNegativeX,
        CDFacePositiveX
    } face;
} CDPacketDigging;

typedef struct _CDPacketPlayerBlockPlacement {
    MCPosition position;

    MCByte  direction;
    MCItem  item;
    MCByte  amount;
    MCShort damage;
} CDPacketPlayerBlockPlacement;

typedef struct _CDPacketHoldChange {
    MCItem item;
} CDPacketHoldChange;

typedef struct _CDPacketArmAnimate {
    MCEntity entity;

    enum {
        CDNoAnimation,
        CDSwingArm
        
        CDUnknownAnimation = 102,

        CDCrouchAnimation = 104,
        CDUncrouchAnimation
    } animate;
} CDPacketArmAnimate;

typedef struct _CDPacketEntityAction {
    MCEntity entity;

    enum {
        CDCrouchAction = 1,
        CDUncrouchAction
    } action;
} CDPacketEntityAction;

typedef struct _CDPacketNamedEntitySpawn {
    MCEntity entity;
    MCString name;

    MCPosition position;

    MCByte rotation;
    MCByte pitch;

    MCItem item;
} CDPacketNamedEntitySpawn;

typedef struct _CDPacketPickupSpawn {
    MCEntity         entity;
    MCItem           item;
    CDPacketPosition position;

    MCByte rotation;
    MCByte pitch;
    MCByte roll;
} CDPacketPickupSpawn;

typedef struct _CDPacketCollectItem {
    MCInteger collected;
    MCInteger collector;
} CDPacketCollectItem;

typedef struct _CDPacketSpawnObject {
    MCEntity entity;
    
    enum {
        CDBoat = 1,

        CDMinecart = 10,
        CDStorageCart,
        CDPoweredCart,

        CDActivatedTNT = 50,

        CDArrow = 60,
        CDThrownSnowball,
        CDThrownEgg,

        CDFallingSand = 70,
        CDFallingGravel,

        CDFishingFloat = 90
    } type;

    MCPosition position;
} CDPacketSpawnObject;

typedef struct _CDPacketSpawnMob {
    MCInteger id;

    enum {
        CDCreeper = 50, // metadata: possible values -1, 1
        CDSkeleton,
        CDSpider,
        CDGiantZombie,
        CDZombie,
        CDSlime,
        CDGhast,
        CDZombiePigman,

        CDPig = 90, // metadata: possible values 0, 1
        CDSheep,    // metadata: bit 0x10 indicates shearedness, the lower 4 bits indicate wool color.
        CDCow,
        CDHen,
        CDSquid
    } type;

    MCMetadata* metadata;
} CDPacketSpawnMob;

typedef struct _CDPacketPainting { // Verify type and coordiates
    MCEntity entity;
    MCString title;

    MCPosition position;

    MCInteger type;
} CDPacketPainting;


typedef struct _CDPacketEntityVelocity {
    MCEntity   entity;
    MCPosition velocity;
} CDPacketEntityVelocity;

typedef struct _CDPacketEntityDestroy {
    MCEntity entity;
} CDPacketEntityDestroy;

typedef struct _CDPacketEntityCreate {
    MCEntity entity;
} CDPacketEntityCreate;

typedef struct _CDPacketEntityRelativeMove {
    MCEntity entity;

    MCRelativePosition position;
} CDPacketEntityRelativeMove;

typedef struct _CDPacketEntityLook {
    MCEntity entity;

    MCByte yaw;
    MCByte pitch;
} CDPacketEntityLook;

typedef struct _CDPacketEntityLookMove {
    MCEntity entity;

    MCRelativePosition position;

    MCByte yaw;
    MCByte pitch;
} CDPacketEntityLookMove;

typedef struct _CDPacketEntityTeleport {
    MCEntity   entity;
    MCPosition position;

    MCByte rotation;
    MCByte pitch;
} CDPacketEntityTeleport;

typedef struct _CDPacketEntityStatus { // Not sure yet
    MCEntity entity;

    enum {
        CDDrowning = 2,
        CDDead
    } status;
} CDPacketEntityStatus;

typedef struct _CDPacketEntityAttach {
    MCEntity entity;
    MCEntity vehicle;
} CDPacketEntityAttach;

typedef struct _CDPacketEntityMetadata {
    MCEntity    entity;
    MCMetadata* metadata;
}

CDPacket* CD_PacketFromEvent (struct bufferevent* event);

void CD_DestroyPacket (CDPacket* packet);

void* CD_GetPacketDataFromEvent (CDPacket* packet, struct bufferevent* event);

MCString CD_PacketToRaw (CDPacket* packet);

#endif
