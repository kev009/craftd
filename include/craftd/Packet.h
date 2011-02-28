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
    char noDataYet;
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

typedef struct _CDPacketTimeUpdate {
    MCLong time;
} CDPacketTimeUpdate;

typedef struct _CDPacketEntityEquipment {
    MCEntity entity;

    MCShort   slot;
    MCShort   item;
    MCShort   damage; // Still not sure about it
} MCEntityEquipment;

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

typedef struct _CDPacketUpdateHealth {
    MCShort health;
} CDPacketUpdateHealth;

typedef struct _CDPacketRespawn {
    char noDataYet;
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
        CDSwingArm,

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

typedef struct _CDPacketPickupSpawn {
    MCEntity   entity;
    MCItem     item;
    MCPosition position;

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

typedef struct _CDPacketPreChunk {
    MCInteger x;
    MCInteger z;

    MCBoolean mode;
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

typedef struct _CDPacketBlockChange {
    MCPosition position;

    MCByte type;
    MCByte metadata;
} CDPacketBlockChange;

typedef struct _CDPacketPlayNoteBlock {
    MCPosition position;

    enum {
        CDHarp,
        CDDoubleBass,
        CDSnareDrum,
        CDClicksSticks,
        CDBassDrum
    } instrument;

    MCByte pitch;
} CDPacketPlayNoteBlock;

typedef union _CDPacketExplosion { // Not sure yet
    struct {
        MCPrecisePosition position;

        MCFloat radius; // unsure

        MCInteger   length;
        MCPosition* item;
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
    } response;
} CDPacketCloseWindow;

typedef struct _CDPacketWindowClick {
    MCByte    id;
    MCShort   slot;
    MCBoolean rightClick;
    MCShort   action;

    MCItem item; // if the first of the 3 values is -1 the packet ends there
} CDPacketWindowClick;

typedef struct _CDPacketSetSlot {
    MCByte  id;
    MCShort slot;

    MCItem item; // if the first of the 3 values is -1 the packet ends there
} CDPacketSetSlot;

typedef union _CDPacketWindowItems {
    struct {
        MCByte id;

        MCShort length;
        MCItem* item;
    } response;
} CDPacketWindowItems;

typedef struct _CDPacketUpdateProgressBar {
    MCByte  id;
    MCShort bar;
    MCShort value;
} CDPacketUpdateProgressBar;

typedef struct _CDPacketTransaction {
    MCByte    id;
    MCShort   action;
    MCBoolean accepted;
} CDPacketTransaction;

typedef union _CDPacketUpdateSign {
    struct {
        MCPosition position;

        MCString first;
        MCString second;
        MCString third;
        MCString fourth;
    } request;
} CDPacketUpdateSign;

typedef union _CDPacketDisconnect {
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

#endif
