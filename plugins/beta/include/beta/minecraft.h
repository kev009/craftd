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

#ifndef CRAFTD_MINECRAFT_H
#define CRAFTD_MINECRAFT_H

#include <craftd/common.h>

typedef int8_t    MCBoolean;
typedef MCBoolean MCBool;

typedef int8_t MCByte;

typedef int16_t MCShort;

typedef int32_t   MCInteger;
typedef MCInteger MCInt;

typedef int64_t MCLong;

typedef float MCFloat;

typedef double MCDouble;

typedef CDString* MCString;

#ifndef CRAFTD_MINECRAFT_IGNORE_EXTERN
extern const char* MCCharset;
extern const char  MCCharsetPixel[];
#endif

void MC_DestroyString (MCString self);

#define MC_COLOR_BLACK      "§0"
#define MC_COLOR_DARKBLUE   "§1"
#define MC_COLOR_DARKGREEN  "§2"
#define MC_COLOR_DARKCYAN   "§3"
#define MC_COLOR_DARKRED    "§4"
#define MC_COLOR_PURPLE     "§5"
#define MC_COLOR_GOLD       "§6"
#define MC_COLOR_GRAY       "§7"
#define MC_COLOR_DARKGRAY   "§8"
#define MC_COLOR_BLUE       "§9"
#define MC_COLOR_LIGHTGREEN "§a"
#define MC_COLOR_CYAN       "§b"
#define MC_COLOR_RED        "§c"
#define MC_COLOR_PINK       "§d"
#define MC_COLOR_YELLOW     "§e"
#define MC_COLOR_WHITE      "§f"

typedef enum _MCStringColor {
    MCColorBlack,
    MCColorDarkBlue,
    MCColorDarkGreen,
    MCColorDarkCyan,
    MCColorDarkRed,
    MCColorPurple,
    MCColorGold,
    MCColorGray,
    MCColorDarkGray,
    MCColorBlue,
    MCColorLightGreen,
    MCColorCyan,
    MCColorRed,
    MCColorPink,
    MCColorYellow,
    MCColorWhite
} MCStringColor;

/**
 * Check if a String is valid for Minecraft
 *
 * @return true if valid, false otherwise
 */
bool MC_StringIsValid (CDString* self);

/**
 * Get a sanitized String to send to Minecraft clients, replaces unknown characters
 * with ?.
 *
 * @return The sanitized String
 */
MCString MC_StringSanitize (MCString self);

MCString MC_StringColorRange (CDString* self, MCStringColor color, size_t a, size_t b);

MCString MC_StringColor (CDString* self, MCStringColor color);

static const char MCBooleanSize = 1;
static const char MCByteSize    = 1;
static const char MCShortSize   = 2;
static const char MCIntegerSize = 4;
static const char MCLongSize    = 8;
static const char MCFloatSize   = 4;
static const char MCDoubleSize  = 8;

typedef struct _MCChunkData {
    uint8_t blocks[32768];
    uint8_t data[16384];
    uint8_t blockLight[16384];
    uint8_t skyLight[16384];
    uint8_t heightMap[256];
} MCChunkData;

typedef struct _MCSize {
    MCByte x;
    MCByte y;
    MCByte z;
} MCSize;

typedef struct _MCVelocity {
    MCShort x;
    MCShort y;
    MCShort z;
} MCVelocity;

typedef struct _MCChunkPosition {
    MCInteger x;
    MCInteger z;
} MCChunkPosition;

typedef struct _MCBlockPosition {
    MCInteger x;
    MCInteger y;
    MCInteger z;
} MCBlockPosition;

typedef struct _MCAbsolutePosition {
    MCInteger x;
    MCInteger y;
    MCInteger z;
} MCAbsolutePosition;

typedef struct _MCPrecisePosition {
    MCDouble x;
    MCDouble y;
    MCDouble z;
} MCPrecisePosition;

typedef struct _MCRelativePosition {
    MCByte x;
    MCByte y;
    MCByte z;
} MCRelativePosition;

typedef enum _MCItemType {
    MCIronShovel          = 0x100,
    MCIronPickaxe         = 0x101,
    MCIronAxe             = 0x102,
    MCFlintAndSteel       = 0x103,
    MCApple               = 0x104,
    MCBow                 = 0x105,
    MCArrow               = 0x106,
    MCCoal                = 0x107,
    MCDiamond             = 0x108,
    MCIronIngot           = 0x109,
    MCGoldIngot           = 0x10A,
    MCIronSword           = 0x10B,
    MCWoodenSword         = 0x10C,
    MCWoodenShovel        = 0x10D,
    MCWoodenPickaxe       = 0x10E,
    MCWoodenAxe           = 0x10F,
    MCStoneSword          = 0x110,
    MCStoneShovel         = 0x111,
    MCStonePickaxe        = 0x112,
    MCStoneAxe            = 0x113,
    MCDiamondSword        = 0x114,
    MCDiamondShovel       = 0x115,
    MCDiamondPickaxe      = 0x116,
    MCDiamondAxe          = 0x117,
    MCStick               = 0x118,
    MCBowl                = 0x119,
    MCMushroomSoup        = 0x11A,
    MCGoldSword           = 0x11B,
    MCGoldShovel          = 0x11C,
    MCGoldPickaxe         = 0x11D,
    MCGoldAxe             = 0x11E,
    MCStringItem          = 0x11F,
    MCFeather             = 0x120,
    MCGunpowder           = 0x121,
    MCWoodenHoe           = 0x122,
    MCStoneHoe            = 0x123,
    MCIronHoe             = 0x124,
    MCDiamondHoe          = 0x125,
    MCGoldHoe             = 0x126,
    MCSeeds               = 0x127,
    MCWheat               = 0x128,
    MCBread               = 0x129,
    MCLeatherHelmet       = 0x12A,
    MCLeatherChestPlate   = 0x12B,
    MCLeatherLeggings     = 0x12C,
    MCLeatherBoots        = 0x12D,
    MCChainmailHelmet     = 0x12E,
    MCChainmailChestPlate = 0x12F,
    MCChainmailLeggings   = 0x130,
    MCChainmailBoots      = 0x131,
    MCIronHelmet          = 0x132,
    MCIronChestPlate      = 0x133,
    MCIronLeggings        = 0x134,
    MCIronBoots           = 0x135,
    MCDiamondHelmet       = 0x136,
    MCDiamondChestPlate   = 0x137,
    MCDiamondLeggings     = 0x138,
    MCDiamondBoots        = 0x139,
    MCGoldHelmet          = 0x13A,
    MCGoldChestPlate      = 0x13B,
    MCGoldLeggings        = 0x13C,
    MCGoldBoots           = 0x13D,
    MCFlint               = 0x13E,
    MCRawPorkchop         = 0x13F,
    MCCookedPortchop      = 0x140,
    MCPainting            = 0x141,
    MCGoldApple           = 0x142,
    MCSign                = 0x143,
    MCWoodenDoorBlock     = 0x144,
    MCBucket              = 0x145,
    MCBucketWithWater     = 0x146,
    MCBucketWithLava      = 0x147,
    MCMineCart            = 0x148,
    MCSaddle              = 0x149,
    MCIronDoor            = 0x14A,
    MCRedstone            = 0x14B,
    MCSnowball            = 0x14C,
    MCBoat                = 0x14D,
    MCLeather             = 0x14E,
    MCBucketWithMilk      = 0x14F,
    MCClayBrick           = 0x150,
    MCClayBalls           = 0x151,
    MCSugarCane           = 0x152,
    MCPaper               = 0x153,
    MCBook                = 0x154,
    MCSlimeball           = 0x155,
    MCStorageMinecart     = 0x156,
    MCPoweredMinecart     = 0x157,
    MCEgg                 = 0x158,
    MCCompass             = 0x159,
    MCFishingRod          = 0x15A,
    MCClock               = 0x15B,
    MCGlowstoneDust       = 0x15C,
    MCRawFish             = 0x15D,
    MCCookedFish          = 0x15E,
    MCDye                 = 0x15F,
    MCBone                = 0x160,
    MCSugar               = 0x161,
    MCCake                = 0x162,
    MCBedBlock            = 0x163,
    MCRedstoneRepeater    = 0x164,
    MCGoldMusicDisc       = 0x8D0,
    MCGreenMusicDisc      = 0x8D1
} MCItemType;

typedef enum _MCBlockType {
    MCAir                 = 0x00,
    MCStone               = 0x01,
    MCGrass               = 0x02,
    MCDirt                = 0x03,
    MCCobblestone         = 0x04,
    MCWoodenPlank         = 0x05,
    MCSapling             = 0x06,
    MCBedrock             = 0x07,
    MCWater               = 0x08,
    MCStationaryWater     = 0x09,
    MCLava                = 0x0A,
    MCStationaryLava      = 0x0B,
    MCSand                = 0x0C,
    MCGravel              = 0x0D,
    MCGoldOre             = 0x0E,
    MCIronOre             = 0x0F,
    MCCoalOre             = 0x10,
    MCWood                = 0x11,
    MCLeaves              = 0x12,
    MCSponge              = 0x13,
    MCGlass               = 0x14,
    MCLapisLazuliOre      = 0x15,
    MCLapisLazuliBlock    = 0x16,
    MCDispenser           = 0x17,
    MCSandstone           = 0x18,
    MCNote                = 0x19,
    MCBed                 = 0x1A,
    MCWool                = 0x23,
    MCYellowFlower        = 0x25,
    MCRedRose             = 0x26,
    MCBrownMushroom       = 0x27,
    MCRedMushroom         = 0x28,
    MCGoldBlock           = 0x29,
    MCIronBlock           = 0x2A,
    MCDoubleSlab          = 0x2B,
    MCSlab                = 0x2C,
    MCBrickBlock          = 0x2D,
    MCTNT                 = 0x2E,
    MCBookshelf           = 0x2F,
    MCMossStone           = 0x30,
    MCObsidian            = 0x31,
    MCTorch               = 0x32,
    MCFire                = 0x33,
    MCMonsterSpawner      = 0x34,
    MCWoodenStairs        = 0x35,
    MCChest               = 0x36,
    MCRedstoneWire        = 0x37,
    MCDiamondOre          = 0x38,
    MCDiamondBlock        = 0x39,
    MCCraftingTable       = 0x3A,
    MCCrops               = 0x3B,
    MCFarmland            = 0x3C,
    MCFurnace             = 0x3E,
    MCSignPost            = 0x3F,
    MCWoodenDoor          = 0x40,
    MCLadder              = 0x41,
    MCRails               = 0x42,
    MCCobblestoneStairs   = 0x43,
    MCWallSign            = 0x44,
    MCLever               = 0x45,
    MCStonePressurePlate  = 0x46,
    MCIronDoorBlock       = 0x47,
    MCWoodenPressurePlate = 0x48,
    MCRedstoneOre         = 0x49,
    MCGlowingRedstoneOre  = 0x4A,
    MCRedstoneTorchOff    = 0x4B,
    MCRedstoneTorchOn     = 0x4C,
    MCStoneButton         = 0x4D,
    MCSnow                = 0x4E,
    MCIce                 = 0x4F,
    MCSnowBlock           = 0x50,
    MCCactus              = 0x51,
    MCClayBlock           = 0x52,
    MCSugarCaneBlock      = 0x53,
    MCJukebox             = 0x54,
    MCFence               = 0x55,
    MCPumpkin             = 0x56,
    MCNetherrack          = 0x57,
    MCSoulSand            = 0x58,
    MCGlowstoneBlock      = 0x59,
    MCPortal              = 0x5A,
    MCJackOLantern        = 0x5B,
    MCCackeBlock          = 0x5C,
    MCRedstoneRepeaterOff = 0x5D,
    MCRedstoneRepeaterOn  = 0x5E
} MCBlockType;

typedef enum _MCWoolColor {
    MCWhiteWool,
    MCOrangeWool,
    MCMagentWool,
    MCLightBlueWool,
    MCYellowWool,
    MCLimeWool,
    MCPinkWool,
    MCGrayWool,
    MCSilverWool,
    MCCyanWool,
    MCPurpleWool,
    MCBlueWool,
    MCBrownWool,
    MCGreenWool,
    MCRedWool,
    MCBlackWool
} MCWoolColor;

typedef MCInteger MCEntityId;

#ifndef CRAFTD_MINECRAFT_IGNORE_EXTERN
extern const MCEntityId MCMaxEntityId;
#endif

typedef enum _MCEntityType {
    MCEntityPlayer,
    MCEntityPickup,
    MCEntityMob,
    MCEntityObject
} MCEntityType;

typedef struct _MCEntity {
    MCEntityId        id;
    MCEntityType      type;
    MCPrecisePosition position;
} MCEntity;

typedef struct _MCItem {
  MCShort id;
  MCByte  count;
  MCShort uses;
} MCItem;

typedef struct _MCData {
    enum {
        MCTypeByte,
        MCTypeShort,
        MCTypeInteger,
        MCTypeFloat,
        MCTypeString,
        MCTypeShortByteShort
    } type;

    union {
        MCByte    b;
        MCShort   s;
        MCInteger i;
        MCFloat   f;
        MCString  S;

        struct {
            MCShort first;
            MCByte  second;
            MCShort third;
        } sbs;
    } data;
} MCData;

typedef struct _MCMetadata {
    size_t   length;
    MCData** item;
} MCMetadata;

MCMetadata* MC_CreateMetadata (void);

void MC_DestroyMetadata (MCMetadata* self);

MCData* MC_CreateData (void);

void MC_DestroyData (MCData* self);

MCMetadata* MC_ConcatDatas (MCMetadata* metadata, MCData** data, size_t length);

MCMetadata* MC_AppendData (MCMetadata* metadata, MCData* data);

MCMetadata* MC_MetadataFromEvent (struct bufferevent* event);

static inline
MCBlockPosition
MC_ChunkPositionToBlockPosition (MCChunkPosition position)
{
    MCBlockPosition result = {
        .x = (position.x << 4),
        .y = 0,
        .z = (position.z << 4)
    };

    return result;
}

static inline
MCAbsolutePosition
MC_ChunkPositionToAbsolutePosition (MCChunkPosition position)
{
    MCAbsolutePosition result = {
        .x = (position.x << 9),
        .y = 0,
        .z = (position.z << 9)
    };

    return result;
}

static inline
MCPrecisePosition
MC_ChunkPositionToPrecisePosition (MCChunkPosition position)
{
    MCPrecisePosition result = {
        .x = (position.x << 4),
        .y = 0,
        .z = (position.z << 4)
    };

    return result;
}

static inline
MCChunkPosition
MC_BlockPositionToChunkPosition (MCBlockPosition position)
{
    MCChunkPosition result = {
        .x = (position.x >> 4),
        .z = (position.z >> 4)
    };

    return result;
}

static inline
MCAbsolutePosition
MC_BlockPositionToAbsolutePosition (MCBlockPosition position)
{
    MCAbsolutePosition result = {
        .x = (position.x << 5),
        .y = (position.y << 5),
        .z = (position.z << 5)
    };
    
    return result;
}

static inline
MCPrecisePosition
MC_BlockPositionToPrecisePosition (MCBlockPosition position)
{
    MCPrecisePosition result = {
        .x = position.x,
        .y = position.y,
        .z = position.z
    };
  
    return result;
}

static inline
MCChunkPosition
MC_AbsolutePositionToChunkPosition (MCAbsolutePosition position)
{
    MCChunkPosition result = {
        .x = (position.x >> 9),
        .z = (position.z >> 9)
    };

    return result;
}

static inline
MCBlockPosition
MC_AbsolutePositionToBlockPosition (MCAbsolutePosition position)
{
    MCBlockPosition result = {
        .x = (position.x >> 5),
        .y = (position.y >> 5),
        .z = (position.z >> 5)
    };

    return result;
}

static inline
MCPrecisePosition
MC_AbsolutePositionToPrecisePosition (MCAbsolutePosition position)
{
    MCPrecisePosition result = {
        .x = (position.x / 32.0),
        .y = (position.y / 32.0),
        .z = (position.z / 32.0)
    };

    return result;
}

static inline
MCChunkPosition
MC_PrecisePositionToChunkPosition (MCPrecisePosition position)
{
    MCChunkPosition result = {
        .x = (((MCInteger) position.x) >> 4),
        .z = (((MCInteger) position.z) >> 4)
    };

    return result;
}

static inline
MCBlockPosition
MC_PrecisePositionToBlockPosition (MCPrecisePosition position)
{
    MCBlockPosition result = {
        .x = ((MCInteger) position.x),
        .y = ((MCInteger) position.y),
        .z = ((MCInteger) position.z)
    };

    return result;
}

static inline
MCAbsolutePosition
MC_PrecisePositionToAbsolutePosition(MCPrecisePosition position)
{
    MCAbsolutePosition result = {
        .x = ((MCInteger) (position.x * 32.0)),
        .y = ((MCInteger) (position.y * 32.0)),
        .z = ((MCInteger) (position.z * 32.0))
    };

    return result;
}

#define MC_ChunkPositionEqual(a, b)     ((a.x == b.x) && (a.z == b.z))
#define MC_BlockPositionEqual(a, b)     ((a.x == b.x) && (a.y == b.y) && (a.z == b.z))
#define MC_AbsolutePositionEqueal(a, b) ((a.x == b.x) && (a.y == b.y) && (a.z == b.z))
#define MC_PrecisePositionEqual(a, b)   ((a.x == b.x) && (a.y == b.y) && (a.z == b.z))

#endif
