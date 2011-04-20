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

#ifndef CRAFTD_SURVIVAL_PACKET_H
#define CRAFTD_SURVIVAL_PACKET_H

#include <craftd/protocols/survival/common.h>

#define CRAFTD_PROTOCOL_VERSION (11)

typedef enum _SVPacketChain {
    SVRequest,
    SVResponse
} SVPacketChain;

typedef enum _SVPacketType {
    SVKeepAlive            = 0x00,
    SVLogin                = 0x01,
    SVHandshake            = 0x02,
    SVChat                 = 0x03,
    SVTimeUpdate           = 0x04,
    SVEntityEquipment      = 0x05,
    SVSpawnPosition        = 0x06,
    SVUseEntity            = 0x07,
    SVUpdateHealth         = 0x08,
    SVRespawn              = 0x09,
    SVOnGround             = 0x0A,
    SVPlayerPosition       = 0x0B,
    SVPlayerLook           = 0x0C,
    SVPlayerMoveLook       = 0x0D,
    SVPlayerDigging        = 0x0E,
    SVPlayerBlockPlacement = 0x0F,
    SVHoldChange           = 0x10,
    SVUseBed               = 0x11,
    SVAnimation            = 0x12,
    SVEntityAction         = 0x13,
    SVNamedEntitySpawn     = 0x14,
    SVPickupSpawn          = 0x15,
    SVCollectItem          = 0x16,
    SVSpawnObject          = 0x17,
    SVSpawnMob             = 0x18,
    SVPainting             = 0x19,
    SVEntityVelocity       = 0x1C,
    SVEntityDestroy        = 0x1D,
    SVEntityCreate         = 0x1E,
    SVEntityRelativeMove   = 0x1F,
    SVEntityLook           = 0x20,
    SVEntityLookMove       = 0x21,
    SVEntityTeleport       = 0x22,
    SVEntityStatus         = 0x26,
    SVEntityAttach         = 0x27,
    SVEntityMetadata       = 0x28,
    SVPreChunk             = 0x32,
    SVMapChunk             = 0x33,
    SVMultiBlockChange     = 0x34,
    SVBlockChange          = 0x35,
    SVPlayNoteBlock        = 0x36,
    SVExplosion            = 0x3C,
    SVOpenWindow           = 0x64,
    SVCloseWindow          = 0x65,
    SVWindowClick          = 0x66,
    SVSetSlot              = 0x67,
    SVWindowItems          = 0x68,
    SVUpdateProgressBar    = 0x69,
    SVTransaction          = 0x6A,
    SVUpdateSign           = 0x82,
    SVIncrementStatistic   = 0xC8,
    SVDisconnect           = 0xFF
} SVPacketType;

typedef struct _SVPacket {
    SVPacketChain chain;
    SVPacketType  type;
    CDPointer     data;
} SVPacket;

typedef union _SVPacketKeepAlive {
    char empty;
} SVPacketKeepAlive;

typedef union _SVPacketLogin {
    struct {
        SVInteger version;

        SVString username;

        SVLong mapSeed;
        SVByte dimension;
    } request;

    struct {
        SVInteger id;

        SVString serverName;

        SVLong mapSeed;
        SVByte dimension;
    } response;
} SVPacketLogin;

typedef union _SVPacketHandshake {
    struct {
        SVString username;
    } request;

    struct {
        SVString hash;
    } response;
} SVPacketHandshake;

typedef union _SVPacketChat {
    struct {
        SVString message;
    } request;

    struct {
        SVString message;
    } response;
} SVPacketChat;

typedef union _SVPacketTimeUpdate {
    struct {
        SVLong time;
    } response;
} SVPacketTimeUpdate;

typedef union _SVPacketEntityEquipment {
    struct {
        SVEntity entity;

        SVShort   slot;
        SVShort   item;
        SVShort   damage; // Still not sure about it
    } response;
} SVPacketEntityEquipment;

typedef union _SVPacketSpawnPosition {
    struct {
        SVBlockPosition position;
    } response;
} SVPacketSpawnPosition;

typedef union _SVPacketUseEntity {
    struct {
        SVInteger user;
        SVInteger target;
        SVBoolean leftClick;
    } request;
} SVPacketUseEntity;

typedef union _SVPacketUpdateHealth {
    struct {
        SVShort health;
    } response;
} SVPacketUpdateHealth;

typedef union _SVPacketRespawn {
    char empty;
} SVPacketRespawn;

typedef union _SVPacketOnGround {
    struct {
        SVBoolean onGround;
    } request;
} SVPacketOnGround;

typedef union _SVPacketPlayerPosition {
    struct {
        SVPrecisePosition position;

        SVDouble stance;

        struct {
            SVBoolean onGround;
        } is;
    } request;
} SVPacketPlayerPosition;

typedef union _SVPacketPlayerLook {
    struct {
        SVFloat yaw;
        SVFloat pitch;

        struct {
            SVBoolean onGround;
        } is;
    } request;
} SVPacketPlayerLook;

typedef union _SVPacketPlayerMoveLook {
    struct {
        SVPrecisePosition position;

        SVDouble stance;
        SVFloat yaw;
        SVFloat pitch;

        struct {
            SVBoolean onGround;
        } is;
    } request;

    struct {
        SVPrecisePosition position;

        SVDouble stance;
        SVFloat yaw;
        SVFloat pitch;

        struct {
            SVBoolean onGround;
        } is;
    } response;
} SVPacketPlayerMoveLook;

typedef union _SVPacketPlayerDigging {
    struct {
        enum {
            SVStartedDigging,
            SVDigging,
            SVStoppedDigging,
            SVBlockBroken,
            SVDropItem
        } status;

        SVBlockPosition position;

        enum {
            SVFaceNegativeY,
            SVFacePositiveY,
            SVFaceNegativeZ,
            SVFacePositiveZ,
            SVFaceNegativeX,
            SVFacePositiveX
        } face;
    } request;
} SVPacketPlayerDigging;

typedef union _SVPacketPlayerBlockPlacement {
    struct {
        SVBlockPosition position;

        SVByte  direction;
        SVItem  item;
        SVByte  amount;
        SVShort damage;
    } request;
} SVPacketPlayerBlockPlacement;

typedef union _SVPacketHoldChange {
    struct {
        SVItem item;
    } request;
} SVPacketHoldChange;

typedef union _SVPacketUseBed {
    struct {
        SVEntity entity;

        SVByte inBed;

        SVBlockPosition position;
    } response;
} SVPacketUseBed;

typedef enum _SVAnimationType {
    SVNoAnimation,
    SVSwingArm,

    SVUnknownAnimation = 102,

    SVCrouchAnimation = 104,
    SVUncrouchAnimation
} SVAnimationType;

typedef union _SVPacketAnimation {
    struct {
        SVEntity entity;

        SVAnimationType type;
    } request;

    struct {
        SVEntity entity;

        SVAnimationType type;
    } response;
} SVPacketAnimation;

typedef union _SVPacketEntityAction {
    struct {
        SVEntity entity;

        enum {
            SVCrouchAction = 1,
            SVUncrouchAction
        } action;
    } request;
} SVPacketEntityAction;

typedef union _SVPacketNamedEntitySpawn {
    struct {
        SVEntity entity;
        SVString name;

        SVAbsolutePosition position;

        SVByte rotation;
        SVByte pitch;

        SVItem item;
    } response;
} SVPacketNamedEntitySpawn;

typedef union _SVPacketPickupSpawn {
    struct {
        SVEntity   entity;
        SVItem     item;
        SVAbsolutePosition position;

        SVByte rotation;
        SVByte pitch;
        SVByte roll;
    } response;
} SVPacketPickupSpawn;

typedef union _SVPacketCollectItem {
    struct {
        SVInteger collected;
        SVInteger collector;
    } response;
} SVPacketCollectItem;

typedef union _SVPacketSpawnObject {
    struct {
        SVEntity entity;

        enum {
            SVBoat = 1,

            SVMinecart = 10,
            SVStorageCart,
            SVPoweredCart,

            SVActivatedTNT = 50,

            SVArrow = 60,
            SVThrownSnowball,
            SVThrownEgg,

            SVFallingSand = 70,
            SVFallingGravel,

            SVFishingFloat = 90
        } type;

        SVAbsolutePosition position;
    } response;
} SVPacketSpawnObject;

typedef union _SVPacketSpawnMob {
    struct {
        SVInteger id;

        enum {
            SVCreeper = 50, // metadata: possible values -1, 1
            SVSkeleton,
            SVSpider,
            SVGiantZombie,
            SVZombie,
            SVSlime,
            SVGhast,
            SVZombiePigman,

            SVPig = 90, // metadata: possible values 0, 1
            SVSheep,    // metadata: bit 0x10 indicates shearedness, the lower 4 bits indicate wool color.
            SVCow,
            SVHen,
            SVSquid
        } type;

        SVAbsolutePosition position;

        SVByte yaw;
        SVByte pitch;

        SVMetadata* metadata;
    } response;
} SVPacketSpawnMob;

typedef union _SVPacketPainting { // Verify type and coordiates
    struct {
        SVEntity entity;
        SVString title;

        SVBlockPosition position;

        SVInteger type;
    } response;
} SVPacketPainting;

typedef union _SVPacketEntityVelocity {
    struct {
        SVEntity   entity;
        SVVelocity velocity;
    } response;
} SVPacketEntityVelocity;

typedef union _SVPacketEntityDestroy {
    struct {
        SVEntity entity;
    } response;
} SVPacketEntityDestroy;

typedef union _SVPacketEntityCreate {
    struct {
        SVEntity entity;
    } response;
} SVPacketEntityCreate;

typedef union _SVPacketEntityRelativeMove {
    struct {
        SVEntity entity;

        SVRelativePosition position;
    } response;
} SVPacketEntityRelativeMove;

typedef union _SVPacketEntityLook {
    struct {
        SVEntity entity;

        SVByte yaw;
        SVByte pitch;
    } response;
} SVPacketEntityLook;

typedef union _SVPacketEntityLookMove {
    struct {
        SVEntity entity;

        SVRelativePosition position;

        SVByte yaw;
        SVByte pitch;
    } response;
} SVPacketEntityLookMove;

typedef union _SVPacketEntityTeleport {
    struct {
        SVEntity   entity;
        SVAbsolutePosition position;

        SVByte rotation;
        SVByte pitch;
    } response;
} SVPacketEntityTeleport;

typedef union _SVPacketEntityStatus { // Not sure yet
    struct {
        SVEntity entity;

        enum {
            SVDrowning = 2,
            SVDead
        } status;
    } response;
} SVPacketEntityStatus;

typedef union _SVPacketEntityAttach {
    struct {
        SVEntity entity;
        SVEntity vehicle;
    } response;
} SVPacketEntityAttach;

typedef union _SVPacketEntityMetadata {
    struct {
        SVEntity    entity;
        SVMetadata* metadata;
    } request;

    struct {
        SVEntity    entity;
        SVMetadata* metadata;
    } response;
} SVPacketEntityMetadata;

typedef union _SVPacketPreChunk {
    struct {
        SVChunkPosition position;

        SVBoolean mode;
    } response;
} SVPacketPreChunk;

typedef union _SVPacketMapChunk {
    struct {
        SVBlockPosition position;
        SVSize     size;

        SVInteger length;
        SVByte*   item;
    } response;
} SVPacketMapChunk;

typedef union _SVPacketMultiBlockChange {
    struct {
        SVChunkPosition position;

        SVShort length;

        SVShort* coordinate;
        SVByte*  type;
        SVByte*  metadata;
    } response;
} SVPacketMultiBlockChange;

typedef union _SVPacketBlockChange {
    struct {
        SVBlockPosition position;

        SVByte type;
        SVByte metadata;
    } response;
} SVPacketBlockChange;

typedef union _SVPacketPlayNoteBlock {
    struct {
        SVBlockPosition position;

        enum {
            SVHarp,
            SVDoubleBass,
            SVSnareDrum,
            SVClicksSticks,
            SVBassDrum
        } instrument;

        SVByte pitch;
    } response;
} SVPacketPlayNoteBlock;

typedef union _SVPacketExplosion { // Not sure yet
    struct {
        SVPrecisePosition position;

        SVFloat radius; // unsure

        SVInteger           length;
        SVRelativePosition* item;
    } response;
} SVPacketExplosion;

typedef union _SVPacketOpenWindow {
    struct {
        SVByte id;

        enum {
            SVChest,
            SVWorkbench,
            SVFurnace,
            SVDispenser
        } type;

        SVString title;

        SVByte slots;
    } response;
} SVPacketOpenWindow;

typedef union _SVPacketCloseWindow {
    struct {
        SVByte id;
    } request;

    struct {
        SVByte id;
    } response;
} SVPacketCloseWindow;

typedef union _SVPacketWindowClick {
    struct {
        SVByte    id;
        SVShort   slot;
        SVBoolean rightClick;
        SVShort   action;

        SVItem item; // if the first of the 3 values is -1 the packet ends there
    } request;
} SVPacketWindowClick;

typedef union _SVPacketSetSlot {
    struct {
        SVByte  id;
        SVShort slot;

        SVItem item; // if the first of the 3 values is -1 the packet ends there
    } response;
} SVPacketSetSlot;

typedef union _SVPacketWindowItems {
    struct {
        SVByte id;

        SVShort length;
        SVItem* item;
    } response;
} SVPacketWindowItems;

typedef union _SVPacketUpdateProgressBar {
    struct {
        SVByte  id;
        SVShort bar;
        SVShort value;
    } response;
} SVPacketUpdateProgressBar;

typedef union _SVPacketTransaction {
    struct {
        SVByte    id;
        SVShort   action;
        SVBoolean accepted;
    } request;

    struct {
        SVByte    id;
        SVShort   action;
        SVBoolean accepted;
    } response;
} SVPacketTransaction;

typedef union _SVPacketUpdateSign {
    struct {
        SVBlockPosition position;

        SVString first;
        SVString second;
        SVString third;
        SVString fourth;
    } request;

    struct {
        SVBlockPosition position;

        SVString first;
        SVString second;
        SVString third;
        SVString fourth;
    } response;
} SVPacketUpdateSign;

typedef union _SVPacketIncrementStatistic {
    struct {
        SVInteger id;
        SVByte    amount;
    } request;
} SVPacketIncrementStatistic;

typedef union _SVPacketDisconnect {
    struct {
        SVString reason;
    } request;

    struct {
        SVString reason;
    } response;
} SVPacketDisconnect;

/**
 * Create a Packet from a Buffers object
 *
 * @param input The Buffer to read from
 *
 * @return The instantiated Packet object
 */
SVPacket* SV_PacketFromBuffers (CDBuffers* buffers);

/**
 * Destroy a Packet object
 */
void SV_DestroyPacket (SVPacket* self);

/**
 * Destroy Packet data
 */
void SV_DestroyPacketData (SVPacket* self);

/**
 * Generate a SVPacket* object from the given bufferevent and return it.
 *
 * This is used internally by SV_PacketFromEvent but can be used in other situations.
 *
 * @param input The Buffer where the input lays
 *
 * @return The instantiated Object cast to (CDPointer)
 */
CDPointer SV_GetPacketDataFromBuffer (SVPacket* self, CDBuffer* input);

/**
 * Generate a Buffer version of the packet to send through the net
 *
 * @return The raw packet data
 */
CDBuffer* SV_PacketToBuffer (SVPacket* self);

#endif
