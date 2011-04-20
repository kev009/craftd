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

#ifndef CRAFTD_SURVIVAL_PACKETLENGTH_H
#define CRAFTD_SURVIVAL_PACKETLENGTH_H

#include <craftd/Buffers.h>

/**
 * Check if the buffer has enough/right data to parse a Packet
 *
 * @param input The buffer to read from
 *
 * @return true if parsable, false otherwise, errno is set with the following possible values:
 *
 *         EAGAIN: not enough data yet
 *         EILSEQ: bad data packet
 */
bool SV_PacketParsable (CDBuffers* buffers);

static const size_t SVPacketLength[] = {
/* 0x00 */  1,      // Keep Alive
/* 0x01 */  15,     // Login
/* 0x02 */  3,      // Handshake
/* 0x03 */  3,      // Chat
/* 0x04 */  9,      // Time Update
/* 0x05 */  11,     // Entity Equipment
/* 0x06 */  13,     // Spawn Position
/* 0x07 */  10,     // Use Entity
/* 0x08 */  3,      // Update Health
/* 0x09 */  1,      // Respawn
/* 0x0A */  2,      // Player
/* 0x0B */  34,     // Player Position
/* 0x0C */  10,     // Player Look
/* 0x0D */  42,     // Player Position & Look
/* 0x0E */  12,     // Player Digging
/* 0x0F */  13,     // Player Block Placement
/* 0x10 */  3,      // Holding Change
/* 0x11 */  15,     // Use Bed
/* 0x12 */  6,      // Animation
/* 0x13 */  6,      // Entity Action
/* 0x14 */  23,     // Named Entity Spawn
/* 0x15 */  25,     // Pickup Spawn
/* 0x16 */  9,      // Collect Item
/* 0x17 */  18,     // Add Object/Vehicle
/* 0x18 */  21,     // Mob Spawn
/* 0x19 */  23,     // Painting
/* 0x1A */  CDNull,
/* 0x1B */  19,     // ???
/* 0x1C */  11,     // Entity Velocity
/* 0x1D */  5,      // Destroy Entity
/* 0x1E */  5,      // Entity
/* 0x1F */  8,      // Entity Relative Move
/* 0x20 */  7,      // Entity Look
/* 0x21 */  10,     // Entity Look & Relative Move
/* 0x22 */  19,     // Entity Teleport
/* 0x23 */  CDNull,
/* 0x24 */  CDNull,
/* 0x25 */  CDNull,
/* 0x26 */  6,      // Entity Status
/* 0x27 */  9,      // Attach Entity
/* 0x28 */  6,      // Entity Metadata
/* 0x29 */  CDNull,
/* 0x2A */  CDNull,
/* 0x2B */  CDNull,
/* 0x2C */  CDNull,
/* 0x2D */  CDNull,
/* 0x2E */  CDNull,
/* 0x2F */  CDNull,
/* 0x30 */  CDNull,
/* 0x31 */  CDNull,
/* 0x32 */  10,     // Pre-Chunk
/* 0x33 */  18,     // Map Chunk
/* 0x34 */  11,     // Multi Block Change
/* 0x35 */  12,     // Block Change
/* 0x36 */  13,     // Play Note Block
/* 0x37 */  CDNull,
/* 0x38 */  CDNull,
/* 0x39 */  CDNull,
/* 0x3A */  CDNull,
/* 0x3B */  CDNull,
/* 0x3C */  33,      // Explosion
/* 0x3D */  CDNull,
/* 0x3E */  CDNull,
/* 0x3F */  CDNull,
/* 0x40 */  CDNull,
/* 0x41 */  CDNull,
/* 0x42 */  CDNull,
/* 0x43 */  CDNull,
/* 0x44 */  CDNull,
/* 0x45 */  CDNull,
/* 0x46 */  CDNull,
/* 0x47 */  CDNull,
/* 0x48 */  CDNull,
/* 0x49 */  CDNull,
/* 0x4A */  CDNull,
/* 0x4B */  CDNull,
/* 0x4C */  CDNull,
/* 0x4D */  CDNull,
/* 0x4E */  CDNull,
/* 0x4F */  CDNull,
/* 0x50 */  CDNull,
/* 0x51 */  CDNull,
/* 0x52 */  CDNull,
/* 0x53 */  CDNull,
/* 0x54 */  CDNull,
/* 0x55 */  CDNull,
/* 0x56 */  CDNull,
/* 0x57 */  CDNull,
/* 0x58 */  CDNull,
/* 0x59 */  CDNull,
/* 0x5A */  CDNull,
/* 0x5B */  CDNull,
/* 0x5C */  CDNull,
/* 0x5D */  CDNull,
/* 0x5E */  CDNull,
/* 0x5F */  CDNull,
/* 0x60 */  CDNull,
/* 0x61 */  CDNull,
/* 0x62 */  CDNull,
/* 0x63 */  CDNull,
/* 0x64 */  6,      // Open Window
/* 0x65 */  2,      // Close Window
/* 0x66 */  9,      // Window Click
/* 0x67 */  6,      // Set Slot
/* 0x68 */  4,      // Window Items
/* 0x69 */  6,      // Update Progress Bar
/* 0x6A */  5,      // Transaction
/* 0x6B */  CDNull,
/* 0x6C */  CDNull,
/* 0x6D */  CDNull,
/* 0x6E */  CDNull,
/* 0x6F */  CDNull,
/* 0x70 */  CDNull,
/* 0x71 */  CDNull,
/* 0x72 */  CDNull,
/* 0x73 */  CDNull,
/* 0x74 */  CDNull,
/* 0x75 */  CDNull,
/* 0x76 */  CDNull,
/* 0x77 */  CDNull,
/* 0x78 */  CDNull,
/* 0x79 */  CDNull,
/* 0x7A */  CDNull,
/* 0x7B */  CDNull,
/* 0x7C */  CDNull,
/* 0x7D */  CDNull,
/* 0x7E */  CDNull,
/* 0x7F */  CDNull,
/* 0x80 */  CDNull,
/* 0x81 */  CDNull,
/* 0x82 */  19,     // Update Sign
/* 0x83 */  CDNull,
/* 0x84 */  CDNull,
/* 0x85 */  CDNull,
/* 0x86 */  CDNull,
/* 0x87 */  CDNull,
/* 0x88 */  CDNull,
/* 0x89 */  CDNull,
/* 0x8A */  CDNull,
/* 0x8B */  CDNull,
/* 0x8C */  CDNull,
/* 0x8D */  CDNull,
/* 0x8E */  CDNull,
/* 0x8F */  CDNull,
/* 0x90 */  CDNull,
/* 0x91 */  CDNull,
/* 0x92 */  CDNull,
/* 0x93 */  CDNull,
/* 0x94 */  CDNull,
/* 0x95 */  CDNull,
/* 0x96 */  CDNull,
/* 0x97 */  CDNull,
/* 0x98 */  CDNull,
/* 0x99 */  CDNull,
/* 0x9A */  CDNull,
/* 0x9B */  CDNull,
/* 0x9C */  CDNull,
/* 0x9D */  CDNull,
/* 0x9E */  CDNull,
/* 0x9F */  CDNull,
/* 0xA0 */  CDNull,
/* 0xA1 */  CDNull,
/* 0xA2 */  CDNull,
/* 0xA3 */  CDNull,
/* 0xA4 */  CDNull,
/* 0xA5 */  CDNull,
/* 0xA6 */  CDNull,
/* 0xA7 */  CDNull,
/* 0xA8 */  CDNull,
/* 0xA9 */  CDNull,
/* 0xAA */  CDNull,
/* 0xAB */  CDNull,
/* 0xAC */  CDNull,
/* 0xAD */  CDNull,
/* 0xAE */  CDNull,
/* 0xAF */  CDNull,
/* 0xB0 */  CDNull,
/* 0xB1 */  CDNull,
/* 0xB2 */  CDNull,
/* 0xB3 */  CDNull,
/* 0xB4 */  CDNull,
/* 0xB5 */  CDNull,
/* 0xB6 */  CDNull,
/* 0xB7 */  CDNull,
/* 0xB8 */  CDNull,
/* 0xB9 */  CDNull,
/* 0xBA */  CDNull,
/* 0xBB */  CDNull,
/* 0xBC */  CDNull,
/* 0xBD */  CDNull,
/* 0xBE */  CDNull,
/* 0xBF */  CDNull,
/* 0xC0 */  CDNull,
/* 0xC1 */  CDNull,
/* 0xC2 */  CDNull,
/* 0xC3 */  CDNull,
/* 0xC4 */  CDNull,
/* 0xC5 */  CDNull,
/* 0xC6 */  CDNull,
/* 0xC7 */  CDNull,
/* 0xC8 */  6,
/* 0xC9 */  CDNull,
/* 0xCA */  CDNull,
/* 0xCB */  CDNull,
/* 0xCC */  CDNull,
/* 0xCD */  CDNull,
/* 0xCE */  CDNull,
/* 0xCF */  CDNull,
/* 0xD0 */  CDNull,
/* 0xD1 */  CDNull,
/* 0xD2 */  CDNull,
/* 0xD3 */  CDNull,
/* 0xD4 */  CDNull,
/* 0xD5 */  CDNull,
/* 0xD6 */  CDNull,
/* 0xD7 */  CDNull,
/* 0xD8 */  CDNull,
/* 0xD9 */  CDNull,
/* 0xDA */  CDNull,
/* 0xDB */  CDNull,
/* 0xDC */  CDNull,
/* 0xDD */  CDNull,
/* 0xDE */  CDNull,
/* 0xDF */  CDNull,
/* 0xE0 */  CDNull,
/* 0xE1 */  CDNull,
/* 0xE2 */  CDNull,
/* 0xE3 */  CDNull,
/* 0xE4 */  CDNull,
/* 0xE5 */  CDNull,
/* 0xE6 */  CDNull,
/* 0xE7 */  CDNull,
/* 0xE8 */  CDNull,
/* 0xE9 */  CDNull,
/* 0xEA */  CDNull,
/* 0xEB */  CDNull,
/* 0xEC */  CDNull,
/* 0xED */  CDNull,
/* 0xEE */  CDNull,
/* 0xEF */  CDNull,
/* 0xF0 */  CDNull,
/* 0xF1 */  CDNull,
/* 0xF2 */  CDNull,
/* 0xF3 */  CDNull,
/* 0xF4 */  CDNull,
/* 0xF5 */  CDNull,
/* 0xF6 */  CDNull,
/* 0xF7 */  CDNull,
/* 0xF8 */  CDNull,
/* 0xF9 */  CDNull,
/* 0xFA */  CDNull,
/* 0xFB */  CDNull,
/* 0xFC */  CDNull,
/* 0xFD */  CDNull,
/* 0xFE */  CDNull,
/* 0xFF */  3       // Disconnect
};

#endif
