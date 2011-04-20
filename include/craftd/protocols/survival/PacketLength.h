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
    1,      // Keep Alive
    15,     // Login
    3,      // Handshake
    3,      // Chat
    9,      // Time Update
    11,     // Entity Equipment
    13,     // Spawn Position
    10,     // Use Entity
    3,      // Update Health
    1,      // Respawn
    2,      // Player
    34,     // Player Position
    10,     // Player Look
    42,     // Player Position & Look
    12,     // Player Digging
    13,     // Player Block Placement
    3,      // Holding Change
    15,     // Use Bed
    6,      // Animation
    6,      // Entity Action
    23,     // Named Entity Spawn
    25,     // Pickup Spawn
    9,      // Collect Item
    18,     // Add Object/Vehicle
    21,     // Mob Spawn
    23,     // Painting
    CDNull,
    19,     // ???
    11,     // Entity Velocity
    5,      // Destroy Entity
    5,      // Entity
    8,      // Entity Relative Move
    7,      // Entity Look
    10,     // Entity Look & Relative Move
    19,     // Entity Teleport
    CDNull,
    CDNull,
    CDNull,
    6,      // Entity Status
    9,      // Attach Entity
    6,      // Entity Metadata
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    10,     // Pre-Chunk
    18,     // Map Chunk
    11,     // Multi Block Change
    12,     // Block Change
    13,     // Play Note Block
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    33,      // Explosion
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    6,      // Open Window
    2,      // Close Window
    9,      // Window Click
    6,      // Set Slot
    4,      // Window Items
    6,      // Update Progress Bar
    5,      // Transaction
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    19,     // Update Sign
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    CDNull,
    3       // Disconnect
};

#endif
