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

#include <craftd/common.h>

typedef enum _CDPacketChain {
    CDRequest,
    CDResponse
} CDPacketChain;

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
    CDUseBed               = 0x11,
    CDAnimation            = 0x12,
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
    CDPlayNoteBlock        = 0x36,
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
    CDPacketChain chain;
    CDPacketType  type;
    CDPointer     data;
} CDPacket;

typedef union _CDPacketKeepAlive {
    char empty;
} CDPacketKeepAlive;

typedef union _CDPacketLogin {
    struct {
        MCInteger version;

        MCString username;
        MCString password;

        MCLong mapSeed;
        MCByte dimension;
    } request;

    struct {
        MCInteger id;

        MCString serverName;
        MCString motd;

        MCLong mapSeed;
        MCByte dimension;
    } response;
} CDPacketLogin;

typedef union _CDPacketHandshake {
    struct {
        MCString username;
    } request;

    struct {
        MCString hash;
    } response;
} CDPacketHandshake;

typedef union _CDPacketChat {
    struct {
        MCString message;
    } request;

    struct {
        MCString message;
    } response;
} CDPacketChat;

typedef union _CDPacketTimeUpdate {
    struct {
        MCLong time;
    } response;
} CDPacketTimeUpdate;

typedef union _CDPacketEntityEquipment {
    struct {
        MCEntity entity;

        MCShort   slot;
        MCShort   item;
        MCShort   damage; // Still not sure about it
    } response;
} CDPacketEntityEquipment;

typedef union _CDPacketSpawnPosition {
    struct {
        MCPosition position;
    } response;
} CDPacketSpawnPosition;

typedef union _CDPacketUseEntity {
    struct {
        MCInteger user;
        MCInteger target;
        MCBoolean leftClick;
    } request;
} CDPacketUseEntity;

typedef union _CDPacketUpdateHealth {
    struct {
        MCShort health;
    } response;
} CDPacketUpdateHealth;

typedef union _CDPacketRespawn {
    char empty;
} CDPacketRespawn;

typedef union _CDPacketOnGround {
    struct {
        MCBoolean onGround;
    } request;
} CDPacketOnGround;

typedef union _CDPacketPlayerPosition {
    struct {
        MCPrecisePosition position;

        MCDouble stance;

        struct {
            MCBoolean onGround;
        } is;
    } request;
} CDPacketPlayerPosition;

typedef union _CDPacketPlayerLook {
    struct {
        MCFloat yaw;
        MCFloat pitch;

        struct {
            MCBoolean onGround;
        } is;
    } request;
} CDPacketPlayerLook;

typedef union _CDPacketPlayerMoveLook {
    struct {
        MCPrecisePosition position;

        MCDouble stance;
        MCFloat yaw;
        MCFloat pitch;

        struct {
            MCBoolean onGround;
        } is;
    } request;

    struct {
        MCPrecisePosition position;

        MCDouble stance;
        MCFloat yaw;
        MCFloat pitch;

        struct {
            MCBoolean onGround;
        } is;
    } response;
} CDPacketPlayerMoveLook;

typedef union _CDPacketPlayerDigging {
    struct {
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
    } request;
} CDPacketPlayerDigging;

typedef union _CDPacketPlayerBlockPlacement {
    struct {
        MCPosition position;

        MCByte  direction;
        MCItem  item;
        MCByte  amount;
        MCShort damage;
    } request;
} CDPacketPlayerBlockPlacement;

typedef union _CDPacketHoldChange {
    struct {
        MCItem item;
    } request;
} CDPacketHoldChange;

typedef union _CDPacketUseBed {
    struct {
        MCEntity entity;

        MCByte inBed;

        MCPosition position;
    } response;
} CDPacketUseBed;

typedef enum _CDAnimationType {
    CDNoAnimation,
    CDSwingArm,

    CDUnknownAnimation = 102,

    CDCrouchAnimation = 104,
    CDUncrouchAnimation
} CDAnimationType;

typedef union _CDPacketAnimation {
    struct {
        MCEntity entity;

        CDAnimationType type;
    } request;

    struct {
        MCEntity entity;

        CDAnimationType type;
    } response;
} CDPacketAnimation;

typedef union _CDPacketEntityAction {
    struct {
        MCEntity entity;

        enum {
            CDCrouchAction = 1,
            CDUncrouchAction
        } action;
    } request;
} CDPacketEntityAction;

typedef union _CDPacketNamedEntitySpawn {
    struct {
        MCEntity entity;
        MCString name;

        MCPosition position;

        MCByte rotation;
        MCByte pitch;

        MCItem item;
    } response;
} CDPacketNamedEntitySpawn;

typedef union _CDPacketPickupSpawn {
    struct {
        MCEntity   entity;
        MCItem     item;
        MCPosition position;

        MCByte rotation;
        MCByte pitch;
        MCByte roll;
    } response;
} CDPacketPickupSpawn;

typedef union _CDPacketCollectItem {
    struct {
        MCInteger collected;
        MCInteger collector;
    } response;
} CDPacketCollectItem;

typedef union _CDPacketSpawnObject {
    struct {
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
    } response;
} CDPacketSpawnObject;

typedef union _CDPacketSpawnMob {
    struct {
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

        MCPosition position;

        MCByte yaw;
        MCByte pitch;

        MCMetadata* metadata;
    } response;
} CDPacketSpawnMob;

typedef union _CDPacketPainting { // Verify type and coordiates
    struct {
        MCEntity entity;
        MCString title;

        MCPosition position;

        MCInteger type;
    } response;
} CDPacketPainting;

typedef union _CDPacketEntityVelocity {
    struct {
        MCEntity   entity;
        MCVelocity velocity;
    } response;
} CDPacketEntityVelocity;

typedef union _CDPacketEntityDestroy {
    struct {
        MCEntity entity;
    } response;
} CDPacketEntityDestroy;

typedef union _CDPacketEntityCreate {
    struct {
        MCEntity entity;
    } response;
} CDPacketEntityCreate;

typedef union _CDPacketEntityRelativeMove {
    struct {
        MCEntity entity;

        MCRelativePosition position;
    } response;
} CDPacketEntityRelativeMove;

typedef union _CDPacketEntityLook {
    struct {
        MCEntity entity;

        MCByte yaw;
        MCByte pitch;
    } response;
} CDPacketEntityLook;

typedef union _CDPacketEntityLookMove {
    struct {
        MCEntity entity;

        MCRelativePosition position;

        MCByte yaw;
        MCByte pitch;
    } response;
} CDPacketEntityLookMove;

typedef union _CDPacketEntityTeleport {
    struct {
        MCEntity   entity;
        MCPosition position;

        MCByte rotation;
        MCByte pitch;
    } response;
} CDPacketEntityTeleport;

typedef union _CDPacketEntityStatus { // Not sure yet
    struct {
        MCEntity entity;

        enum {
            CDDrowning = 2,
            CDDead
        } status;
    } response;
} CDPacketEntityStatus;

typedef union _CDPacketEntityAttach {
    struct {
        MCEntity entity;
        MCEntity vehicle;
    } response;
} CDPacketEntityAttach;

typedef union _CDPacketEntityMetadata {
    struct {
        MCEntity    entity;
        MCMetadata* metadata;
    } request;

    struct {
        MCEntity    entity;
        MCMetadata* metadata;
    } response;
} CDPacketEntityMetadata;

typedef union _CDPacketPreChunk {
    struct {
        MCInteger x;
        MCInteger z;

        MCBoolean mode;
    } response;
} CDPacketPreChunk;

typedef union _CDPacketMapChunk {
    struct {
        MCPosition position;
        MCSize     size;

        MCInteger length;
        MCByte*   item;
    } response;
} CDPacketMapChunk;

typedef union _CDPacketMultiBlockChange {
    struct {
        MCInteger x;
        MCInteger z;

        MCShort length;

        MCShort* coordinate;
        MCByte*  type;
        MCByte*  metadata;
    } response;
} CDPacketMultiBlockChange;

typedef union _CDPacketBlockChange {
    struct {
        MCPosition position;

        MCByte type;
        MCByte metadata;
    } response;
} CDPacketBlockChange;

typedef union _CDPacketPlayNoteBlock {
    struct {
        MCPosition position;

        enum {
            CDHarp,
            CDDoubleBass,
            CDSnareDrum,
            CDClicksSticks,
            CDBassDrum
        } instrument;

        MCByte pitch;
    } response;
} CDPacketPlayNoteBlock;

typedef union _CDPacketExplosion { // Not sure yet
    struct {
        MCPrecisePosition position;

        MCFloat radius; // unsure

        MCInteger           length;
        MCRelativePosition* item;
    } response;
} CDPacketExplosion;

typedef union _CDPacketOpenWindow {
    struct {
        MCByte id;

        enum {
            CDChest,
            CDWorkbench,
            CDFurnace,
            CDDispenser
        } type;

        MCString title;

        MCByte slots;
    } response;
} CDPacketOpenWindow;

typedef union _CDPacketCloseWindow {
    struct {
        MCByte id;
    } request;

    struct {
        MCByte id;
    } response;
} CDPacketCloseWindow;

typedef union _CDPacketWindowClick {
    struct {
        MCByte    id;
        MCShort   slot;
        MCBoolean rightClick;
        MCShort   action;

        MCItem item; // if the first of the 3 values is -1 the packet ends there
    } request;
} CDPacketWindowClick;

typedef union _CDPacketSetSlot {
    struct {
        MCByte  id;
        MCShort slot;

        MCItem item; // if the first of the 3 values is -1 the packet ends there
    } response;
} CDPacketSetSlot;

typedef union _CDPacketWindowItems {
    struct {
        MCByte id;

        MCShort length;
        MCItem* item;
    } response;
} CDPacketWindowItems;

typedef union _CDPacketUpdateProgressBar {
    struct {
        MCByte  id;
        MCShort bar;
        MCShort value;
    } response;
} CDPacketUpdateProgressBar;

typedef union _CDPacketTransaction {
    struct {
        MCByte    id;
        MCShort   action;
        MCBoolean accepted;
    } request;

    struct {
        MCByte    id;
        MCShort   action;
        MCBoolean accepted;
    } response;
} CDPacketTransaction;

typedef union _CDPacketUpdateSign {
    struct {
        MCPosition position;

        MCString first;
        MCString second;
        MCString third;
        MCString fourth;
    } request;

    struct {
        MCPosition position;

        MCString first;
        MCString second;
        MCString third;
        MCString fourth;
    } response;
} CDPacketUpdateSign;

typedef union _CDPacketDisconnect {
    struct {
        MCString reason;
    } request;

    struct {
        MCString reason;
    } response;
} CDPacketDisconnect;

/**
 * Create a Packet from a Buffer
 *
 * @param input The Buffer to read from
 *
 * @return The instantiated Packet object
 */
CDPacket* CD_PacketFromBuffer (CDBuffer* input);

/**
 * Destroy a Packet object
 */
void CD_DestroyPacket (CDPacket* self);

/**
 * Generate a CDPacket* object from the given bufferevent and return it.
 *
 * This is used internally by CD_PacketFromEvent but can be used in other situations.
 *
 * @param input The Buffer where the input lays
 *
 * @return The instantiated Object cast to (CDPointer)
 */
CDPointer CD_GetPacketDataFromBuffer (CDPacket* self, CDBuffer* input);

/**
 * Generate a Buffer version of the packet to send through the net
 *
 * @return The raw packet data
 */
CDBuffer* CD_PacketToBuffer (CDPacket* self);

#define CD_PACKET_DO \
    for (void* __check__ = NULL; __check__ == NULL; __check__++)

#endif
