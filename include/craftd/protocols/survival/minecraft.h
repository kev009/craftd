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

#ifndef CRAFTD_SURVIVAL_MINECRAFT_H
#define CRAFTD_SURVIVAL_MINECRAFT_H

#include <craftd/common.h>

typedef int8_t    SVBoolean;
typedef SVBoolean SVBool;

typedef int8_t SVByte;

typedef int16_t SVShort;

typedef int32_t   SVInteger;
typedef SVInteger SVInt;

typedef int64_t SVLong;

typedef float SVFloat;

typedef double SVDouble;

typedef CDString* SVString;

#ifndef CRAFTD_SURVIVAL_MINECRAFT_IGNORE_EXTERN
extern const char* SVCharset;
extern const char  SVCharsetPixel[];
#endif

void SV_DestroyString (SVString self);

#define SV_COLOR_BLACK      "§0"
#define SV_COLOR_DARKBLUE   "§1"
#define SV_COLOR_DARKGREEN  "§2"
#define SV_COLOR_DARKCYAN   "§3"
#define SV_COLOR_DARKRED    "§4"
#define SV_COLOR_PURPLE     "§5"
#define SV_COLOR_GOLD       "§6"
#define SV_COLOR_GRAY       "§7"
#define SV_COLOR_DARKGRAY   "§8"
#define SV_COLOR_BLUE       "§9"
#define SV_COLOR_LIGHTGREEN "§a"
#define SV_COLOR_CYAN       "§b"
#define SV_COLOR_RED        "§c"
#define SV_COLOR_PINK       "§d"
#define SV_COLOR_YELLOW     "§e"
#define SV_COLOR_WHITE      "§f"

typedef enum _SVStringColor {
    SVColorBlack,
    SVColorDarkBlue,
    SVColorDarkGreen,
    SVColorDarkCyan,
    SVColorDarkRed,
    SVColorPurple,
    SVColorGold,
    SVColorGray,
    SVColorDarkGray,
    SVColorBlue,
    SVColorLightGreen,
    SVColorCyan,
    SVColorRed,
    SVColorPink,
    SVColorYellow,
    SVColorWhite
} SVStringColor;

/**
 * Check if a String is valid for Minecraft
 *
 * @return true if valid, false otherwise
 */
bool SV_StringIsValid (CDString* self);

/**
 * Get a sanitized String to send to Minecraft clients, replaces unknown characters
 * with ?.
 *
 * @return The sanitized String
 */
SVString SV_StringSanitize (SVString self);

SVString SV_StringColorRange (CDString* self, SVStringColor color, size_t a, size_t b);

SVString SV_StringColor (CDString* self, SVStringColor color);

static const char SVBooleanSize = 1;
static const char SVByteSize    = 1;
static const char SVShortSize   = 2;
static const char SVIntegerSize = 4;
static const char SVLongSize    = 8;
static const char SVFloatSize   = 4;
static const char SVDoubleSize  = 8;

typedef struct _SVSize {
    SVByte x;
    SVByte y;
    SVByte z;
} SVSize;

typedef struct _SVVelocity {
    SVShort x;
    SVShort y;
    SVShort z;
} SVVelocity;

typedef struct _SVChunkPosition {
    SVInteger x;
    SVInteger z;
} SVChunkPosition;

typedef struct _SVBlockPosition {
    SVInteger x;
    SVInteger y;
    SVInteger z;
} SVBlockPosition;

typedef struct _SVAbsolutePosition {
    SVInteger x;
    SVInteger y;
    SVInteger z;
} SVAbsolutePosition;

typedef struct _SVPrecisePosition {
    SVDouble x;
    SVDouble y;
    SVDouble z;
} SVPrecisePosition;

typedef struct _SVRelativePosition {
    SVByte x;
    SVByte y;
    SVByte z;
} SVRelativePosition;

typedef struct _SVChunk {
    SVChunkPosition position;

    uint8_t heightMap[256];

    uint8_t blocks[32768];
    uint8_t data[16384];
    uint8_t blockLight[16384];
    uint8_t skyLight[16384];
} SVChunk;

typedef enum _SVItemType {
    SVIronShovel          = 0x100,
    SVIronPickaxe         = 0x101,
    SVIronAxe             = 0x102,
    SVFlintAndSteel       = 0x103,
    SVApple               = 0x104,
    SVBow                 = 0x105,
    SVArrowItem           = 0x106,
    SVCoal                = 0x107,
    SVDiamond             = 0x108,
    SVIronIngot           = 0x109,
    SVGoldIngot           = 0x10A,
    SVIronSword           = 0x10B,
    SVWoodenSword         = 0x10C,
    SVWoodenShovel        = 0x10D,
    SVWoodenPickaxe       = 0x10E,
    SVWoodenAxe           = 0x10F,
    SVStoneSword          = 0x110,
    SVStoneShovel         = 0x111,
    SVStonePickaxe        = 0x112,
    SVStoneAxe            = 0x113,
    SVDiamondSword        = 0x114,
    SVDiamondShovel       = 0x115,
    SVDiamondPickaxe      = 0x116,
    SVDiamondAxe          = 0x117,
    SVStick               = 0x118,
    SVBowl                = 0x119,
    SVMushroomSoup        = 0x11A,
    SVGoldSword           = 0x11B,
    SVGoldShovel          = 0x11C,
    SVGoldPickaxe         = 0x11D,
    SVGoldAxe             = 0x11E,
    SVStringItem          = 0x11F,
    SVFeather             = 0x120,
    SVGunpowder           = 0x121,
    SVWoodenHoe           = 0x122,
    SVStoneHoe            = 0x123,
    SVIronHoe             = 0x124,
    SVDiamondHoe          = 0x125,
    SVGoldHoe             = 0x126,
    SVSeeds               = 0x127,
    SVWheat               = 0x128,
    SVBread               = 0x129,
    SVLeatherHelmet       = 0x12A,
    SVLeatherChestPlate   = 0x12B,
    SVLeatherLeggings     = 0x12C,
    SVLeatherBoots        = 0x12D,
    SVChainmailHelmet     = 0x12E,
    SVChainmailChestPlate = 0x12F,
    SVChainmailLeggings   = 0x130,
    SVChainmailBoots      = 0x131,
    SVIronHelmet          = 0x132,
    SVIronChestPlate      = 0x133,
    SVIronLeggings        = 0x134,
    SVIronBoots           = 0x135,
    SVDiamondHelmet       = 0x136,
    SVDiamondChestPlate   = 0x137,
    SVDiamondLeggings     = 0x138,
    SVDiamondBoots        = 0x139,
    SVGoldHelmet          = 0x13A,
    SVGoldChestPlate      = 0x13B,
    SVGoldLeggings        = 0x13C,
    SVGoldBoots           = 0x13D,
    SVFlint               = 0x13E,
    SVRawPorkchop         = 0x13F,
    SVCookedPortchop      = 0x140,
    SVPaintingItem        = 0x141,
    SVGoldApple           = 0x142,
    SVSign                = 0x143,
    SVWoodenDoorBlock     = 0x144,
    SVBucket              = 0x145,
    SVBucketWithWater     = 0x146,
    SVBucketWithLava      = 0x147,
    SVMineCart            = 0x148,
    SVSaddle              = 0x149,
    SVIronDoor            = 0x14A,
    SVRedstone            = 0x14B,
    SVSnowball            = 0x14C,
    SVBoatItem            = 0x14D,
    SVLeather             = 0x14E,
    SVBucketWithMilk      = 0x14F,
    SVClayBrick           = 0x150,
    SVClayBalls           = 0x151,
    SVSugarCane           = 0x152,
    SVPaper               = 0x153,
    SVBook                = 0x154,
    SVSlimeball           = 0x155,
    SVStorageMinecart     = 0x156,
    SVPoweredMinecart     = 0x157,
    SVEgg                 = 0x158,
    SVCompass             = 0x159,
    SVFishingRod          = 0x15A,
    SVClock               = 0x15B,
    SVGlowstoneDust       = 0x15C,
    SVRawFish             = 0x15D,
    SVCookedFish          = 0x15E,
    SVDye                 = 0x15F,
    SVBone                = 0x160,
    SVSugar               = 0x161,
    SVCake                = 0x162,
    SVBedBlock            = 0x163,
    SVRedstoneRepeater    = 0x164,
    SVGoldMusicDisc       = 0x8D0,
    SVGreenMusicDisc      = 0x8D1
} SVItemType;

typedef enum _SVBlockType {
    SVAir                 = 0x00,
    SVStone               = 0x01,
    SVGrass               = 0x02,
    SVDirt                = 0x03,
    SVCobblestone         = 0x04,
    SVWoodenPlank         = 0x05,
    SVSapling             = 0x06,
    SVBedrock             = 0x07,
    SVWater               = 0x08,
    SVStationaryWater     = 0x09,
    SVLava                = 0x0A,
    SVStationaryLava      = 0x0B,
    SVSand                = 0x0C,
    SVGravel              = 0x0D,
    SVGoldOre             = 0x0E,
    SVIronOre             = 0x0F,
    SVCoalOre             = 0x10,
    SVWood                = 0x11,
    SVLeaves              = 0x12,
    SVSponge              = 0x13,
    SVGlass               = 0x14,
    SVLapisLazuliOre      = 0x15,
    SVLapisLazuliBlock    = 0x16,
    SVDispenserBlock      = 0x17,
    SVSandstone           = 0x18,
    SVNote                = 0x19,
    SVBed                 = 0x1A,
    SVWool                = 0x23,
    SVYellowFlower        = 0x25,
    SVRedRose             = 0x26,
    SVBrownMushroom       = 0x27,
    SVRedMushroom         = 0x28,
    SVGoldBlock           = 0x29,
    SVIronBlock           = 0x2A,
    SVDoubleSlab          = 0x2B,
    SVSlab                = 0x2C,
    SVBrickBlock          = 0x2D,
    SVTNT                 = 0x2E,
    SVBookshelf           = 0x2F,
    SVMossStone           = 0x30,
    SVObsidian            = 0x31,
    SVTorch               = 0x32,
    SVFire                = 0x33,
    SVMonsterSpawner      = 0x34,
    SVWoodenStairs        = 0x35,
    SVChestBlock          = 0x36,
    SVRedstoneWire        = 0x37,
    SVDiamondOre          = 0x38,
    SVDiamondBlock        = 0x39,
    SVCraftingTable       = 0x3A,
    SVCrops               = 0x3B,
    SVFarmland            = 0x3C,
    SVFurnaceBlock        = 0x3E,
    SVSignPost            = 0x3F,
    SVWoodenDoor          = 0x40,
    SVLadder              = 0x41,
    SVRails               = 0x42,
    SVCobblestoneStairs   = 0x43,
    SVWallSign            = 0x44,
    SVLever               = 0x45,
    SVStonePressurePlate  = 0x46,
    SVIronDoorBlock       = 0x47,
    SVWoodenPressurePlate = 0x48,
    SVRedstoneOre         = 0x49,
    SVGlowingRedstoneOre  = 0x4A,
    SVRedstoneTorchOff    = 0x4B,
    SVRedstoneTorchOn     = 0x4C,
    SVStoneButton         = 0x4D,
    SVSnow                = 0x4E,
    SVIce                 = 0x4F,
    SVSnowBlock           = 0x50,
    SVCactus              = 0x51,
    SVClayBlock           = 0x52,
    SVSugarCaneBlock      = 0x53,
    SVJukebox             = 0x54,
    SVFence               = 0x55,
    SVPumpkin             = 0x56,
    SVNetherrack          = 0x57,
    SVSoulSand            = 0x58,
    SVGlowstoneBlock      = 0x59,
    SVPortal              = 0x5A,
    SVJackOLantern        = 0x5B,
    SVCackeBlock          = 0x5C,
    SVRedstoneRepeaterOff = 0x5D,
    SVRedstoneRepeaterOn  = 0x5E
} SVBlockType;

typedef enum _SVWoolColor {
    SVWhiteWool,
    SVOrangeWool,
    SVMagentWool,
    SVLightBlueWool,
    SVYellowWool,
    SVLimeWool,
    SVPinkWool,
    SVGrayWool,
    SVSilverWool,
    SVCyanWool,
    SVPurpleWool,
    SVBlueWool,
    SVBrownWool,
    SVGreenWool,
    SVRedWool,
    SVBlackWool
} SVWoolColor;

typedef SVInteger SVEntityId;

#ifndef CRAFTD_MINECRAFT_IGNORE_EXTERN
extern const SVEntityId SVMaxEntityId;
#endif

typedef enum _SVEntityType {
    SVEntityPlayer,
    SVEntityPickup,
    SVEntityMob,
    SVEntityObject
} SVEntityType;

typedef struct _SVEntity {
    SVEntityId        id;
    SVEntityType      type;
    SVPrecisePosition position;
} SVEntity;

typedef struct _SVItem {
    SVShort id;
    SVByte  count;

    SVShort uses;
    SVShort data;
} SVItem;

typedef struct _SVData {
    enum {
        SVTypeByte,
        SVTypeShort,
        SVTypeInteger,
        SVTypeFloat,
        SVTypeString,
        SVTypeShortByteShort
    } type;

    union {
        SVByte    b;
        SVShort   s;
        SVInteger i;
        SVFloat   f;
        SVString  S;

        struct {
            SVShort first;
            SVByte  second;
            SVShort third;
        } sbs;
    } data;
} SVData;

typedef struct _SVMetadata {
    size_t   length;
    SVData** item;
} SVMetadata;

SVMetadata* SV_CreateMetadata (void);

void SV_DestroyMetadata (SVMetadata* self);

SVData* SV_CreateData (void);

void SV_DestroyData (SVData* self);

SVMetadata* SV_ConcatDatas (SVMetadata* metadata, SVData** data, size_t length);

SVMetadata* SV_AppendData (SVMetadata* metadata, SVData* data);

SVMetadata* SV_MetadataFromEvent (struct bufferevent* event);

void SV_ChunkToByteArray (SVChunk* chunk, uint8_t* array);

static inline
SVBlockPosition
SV_ChunkPositionToBlockPosition (SVChunkPosition position)
{
    return (SVBlockPosition) {
        .x = (position.x << 4),
        .y = 0,
        .z = (position.z << 4)
    };
}

static inline
SVAbsolutePosition
SV_ChunkPositionToAbsolutePosition (SVChunkPosition position)
{
    return (SVAbsolutePosition) {
        .x = (position.x << 9),
        .y = 0,
        .z = (position.z << 9)
    };
}

static inline
SVPrecisePosition
SV_ChunkPositionToPrecisePosition (SVChunkPosition position)
{
    return (SVPrecisePosition) {
        .x = (position.x << 4),
        .y = 0,
        .z = (position.z << 4)
    };
}

static inline
SVChunkPosition
SV_BlockPositionToChunkPosition (SVBlockPosition position)
{
    return (SVChunkPosition) {
        .x = (position.x >> 4),
        .z = (position.z >> 4)
    };
}

static inline
SVAbsolutePosition
SV_BlockPositionToAbsolutePosition (SVBlockPosition position)
{
    return (SVAbsolutePosition) {
        .x = (position.x << 5),
        .y = (position.y << 5),
        .z = (position.z << 5)
    };
}

static inline
SVPrecisePosition
SV_BlockPositionToPrecisePosition (SVBlockPosition position)
{
    return (SVPrecisePosition) {
        .x = position.x,
        .y = position.y,
        .z = position.z
    };
}

static inline
SVChunkPosition
SV_AbsolutePositionToChunkPosition (SVAbsolutePosition position)
{
    return (SVChunkPosition) {
        .x = (position.x >> 9),
        .z = (position.z >> 9)
    };
}

static inline
SVBlockPosition
SV_AbsolutePositionToBlockPosition (SVAbsolutePosition position)
{
    return (SVBlockPosition) {
        .x = (position.x >> 5),
        .y = (position.y >> 5),
        .z = (position.z >> 5)
    };
}

static inline
SVPrecisePosition
SV_AbsolutePositionToPrecisePosition (SVAbsolutePosition position)
{
    return (SVPrecisePosition) {
        .x = (position.x / 32.0),
        .y = (position.y / 32.0),
        .z = (position.z / 32.0)
    };
}

static inline
SVChunkPosition
SV_PrecisePositionToChunkPosition (SVPrecisePosition position)
{
    return (SVChunkPosition) {
        .x = (((SVInteger) position.x) >> 4),
        .z = (((SVInteger) position.z) >> 4)
    };
}

static inline
SVBlockPosition
SV_PrecisePositionToBlockPosition (SVPrecisePosition position)
{
    return (SVBlockPosition) {
        .x = ((SVInteger) position.x),
        .y = ((SVInteger) position.y),
        .z = ((SVInteger) position.z)
    };
}

static inline
SVAbsolutePosition
SV_PrecisePositionToAbsolutePosition(SVPrecisePosition position)
{
    return (SVAbsolutePosition) {
        .x = ((SVInteger) (position.x * 32.0)),
        .y = ((SVInteger) (position.y * 32.0)),
        .z = ((SVInteger) (position.z * 32.0))
    };
}

#define SV_ChunkPositionEqual(a, b)     ((a.x == b.x) && (a.z == b.z))
#define SV_BlockPositionEqual(a, b)     ((a.x == b.x) && (a.y == b.y) && (a.z == b.z))
#define SV_AbsolutePositionEqueal(a, b) ((a.x == b.x) && (a.y == b.y) && (a.z == b.z))
#define SV_PrecisePositionEqual(a, b)   ((a.x == b.x) && (a.y == b.y) && (a.z == b.z))

bool SV_CompareChunkPosition (CDSet* self, SVChunkPosition* a, SVChunkPosition* b);

unsigned int SV_HashChunkPosition (CDSet* self, SVChunkPosition* position);

#endif
