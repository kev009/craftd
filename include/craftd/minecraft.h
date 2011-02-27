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

void MC_DestroyString (MCString self);

static const char MCByteSize    = 1;
static const char MCShortSize   = 2;
static const char MCIntegerSize = 4;
static const char MCLongSize    = 8;
static const char MCFloatSize   = 4;
static const char MCDoubleSize  = 8;

typedef struct _MCSize {
    MCByte x;
    MCByte y;
    MCByte z;
} MCSize;

typedef struct _MCPosition {
    MCInteger x;
    MCInteger y;
    MCInteger z;
} MCPosition;

typedef struct _MCRelativePosition {
    MCByte x;
    MCByte y;
    MCByte z;
} MCRelativePosition;

typedef struct _MCPrecisePosition {
    MCDouble x;
    MCDouble y;
    MCDouble z;
} MCPrecisePosition;

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
        MCShort   w;
        MCInteger i;
        MCFloat   f;
        MCString  s;

        struct {
            MCShort first;
            MCByte  second;
            MCShort third;
        } wbw;
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

#endif
