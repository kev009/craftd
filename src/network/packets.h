#ifndef CRAFTD_PACKETS_H
#define CRAFTD_PACKETS_H

/*
 * Copyright (c) 2010 Kevin M. Bowling, <kevin.bowling@kev009.com>, USA
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

#include <config.h>

#ifdef HAVE_STDBOOL_H
#include <stdbool.h>
#endif

#include "util.h"

/* This file defines structs for different packet types
 * PROTOCOL_VERSION controls the allowed client version
 */

#define PROTOCOL_VERSION (6)

#define MAX_CHUNKARRAY (81920)

enum packetid
{
  PID_KEEPALIVE         = 0x00,
  PID_LOGIN             = 0x01,
  PID_HANDSHAKE         = 0x02,
  PID_CHAT              = 0x03,
  PID_TIMEUPDATE        = 0x04,
  PID_PINVENTORY        = 0x05,
  PID_SPAWNPOS          = 0x06,
  PID_USEENTITY         = 0x07,
  PID_RESPAWN		= 0x09,
  PID_PLAYERFLY         = 0x0A,
  PID_PLAYERPOS         = 0x0B,
  PID_PLAYERLOOK        = 0x0C,
  PID_PLAYERMOVELOOK    = 0x0D,
  PID_PLAYERDIG         = 0x0E,
  PID_BLOCKPLACE        = 0x0F,
  PID_HOLDCHANGE        = 0x10,
  PID_ADDINVENTORY      = 0x11,
  PID_ARMANIMATE        = 0x12,
  PID_PICKUPSPAWN       = 0x15,
  PID_COLLECTITEM       = 0x16,
  PID_ADDOBJVEHICLE     = 0x17,
  PID_MOBSPAWN          = 0x18,
  PID_ENTITYVELOCITY    = 0x1C,
  PID_DESTROYENTITY     = 0x1D,
  PID_ENTITY            = 0x1E,
  PID_ENTITYRELMOVE     = 0x1F,
  PID_ENTITYLOOK        = 0x20,
  PID_ENTITYLOOKRELMOVE = 0x21,
  PID_ENTITYTELEPORT    = 0x22,
  PID_ATTACHENTITY      = 0x27,
  PID_PRECHUNK          = 0x32,
  PID_MAPCHUNK	        = 0x33,
  PID_MULTIBLOCKCHANGE  = 0x34,
  PID_BLOCKCHANGE	= 0x35,
  PID_COMPLEXENTITY     = 0x3B,
  PID_DISCONNECT        = 0xFF
};

/* pid 0x00 */
struct packet_keepalive
{
  uint8_t pid;
};
static const int packet_keepalivesz = sizeof(uint8_t);

/* pid 0x01 */
struct packet_login
{
  uint8_t pid;
  uint32_t version;
  bstring username;
  bstring password;
  uint64_t mapseed;
  uint8_t dimension;
};
// Login size/offset information (C99 initialized struct)
static const struct
{
  const int base;
  const int str1offset;
  const int str2offset;
} packet_loginsz = {
  .base       = sizeof(uint8_t) + sizeof(uint32_t) + sizeof(uint16_t)
	      + sizeof(uint16_t) + sizeof(uint64_t) + sizeof(uint8_t),
  .str1offset = sizeof(uint8_t) + sizeof(uint32_t),
  .str2offset = sizeof(uint8_t) + sizeof(uint32_t) + sizeof(uint16_t)
};

/* pid 0x02 */
struct packet_handshake
{
  uint8_t pid;
  bstring username;
};
// Handshake size/offset information (C99 initialized struct)
static const struct
{
  const int base;
  const int str1offset;
} packet_handshakesz = {
  .base       = sizeof(uint8_t) + sizeof(uint16_t),
  .str1offset = sizeof(uint8_t)
};

/* pid 0x03 */
struct packet_chat
{
  uint8_t pid;
  bstring message;
};
// Chat size/offset information (C99 initialized struct)
static const struct
{
  const int base;
  const int str1offset;
} packet_chatsz = {
  .base       = sizeof(uint8_t) + sizeof(uint16_t),
  .str1offset = sizeof(uint8_t)
};

/* pid 0x04 */
struct packet_time
{
  int8_t pid;
  int64_t time;
};

/* pid 0x05 */
struct packet_inventory
{
  int8_t pid;
  int32_t type;
  int16_t count;
  void *payload; //FIXME: void ptr necessary?
};
// Inventory offset information (C99 initialized struct)
static const struct
{
  const int base;
  const int typeoffset;
  const int countoffset;
  const int payloadoffset;
} packet_inventorysz = {
  .base       	 = sizeof(int8_t) + sizeof(int32_t) + sizeof(int16_t),
  .typeoffset	 = sizeof(int8_t),
  .countoffset   = sizeof(int8_t) + sizeof(int32_t),
  .payloadoffset = sizeof(int8_t) + sizeof(int32_t) + sizeof(int16_t)
};

/* pid 0x06 */
struct packet_spawnpos
{
  int8_t pid;
  int32_t x;
  int32_t y;
  int32_t z;
};

/* pid 0x07 */
struct packet_useentity
{
  int8_t pid;
  int32_t user;
  int32_t target;
  bool leftclick;
};
static const int packet_useentity = sizeof(int8_t) + 2 * sizeof(int32_t)
		       + sizeof(int8_t); // Use int8_t to ensure bool is 1 byte

/* pid 0x09 */
struct packet_respawn
{
  int8_t pid;
};
static const int packet_respawnsz = sizeof(int8_t);

/* pid 0x0A */
struct packet_playerfly
{
  int8_t pid;
  bool onground;
};
static const int packet_playerflysz = sizeof(int8_t)
		      + sizeof(uint8_t); // Use int8_t to ensure bool is 1 byte

/* pid 0x0B */
struct packet_playerpos
{
  int8_t pid;
  double x;
  double y;
  double stance;
  double z;
  bool flying;
};
static const int packet_playerpossz = sizeof(int8_t) + 4 * sizeof(double)
		       + sizeof(int8_t); // Use int8_t to ensure bool is 1 byte

/* pid 0x0C */
struct packet_look
{
  int8_t pid;
  float yaw;
  float pitch;
  bool flying;
};
static const int packet_looksz = sizeof(int8_t) + 2 * sizeof(float) 
		       + sizeof(int8_t); // Use int8_t to ensure bool is 1 byte

/* pid 0x0D */
struct packet_movelook
{
  int8_t pid;
  double x;
  double y;
  double stance;
  double z;
  float yaw;
  float pitch;
  bool flying;
};
static const int packet_movelooksz = sizeof(int8_t) + 4 * sizeof(double)
   + 2 * sizeof(float) + sizeof(int8_t); // Use int8_t to ensure bool is 1 byte

/* pid 0x0E */
struct packet_dig
{
 int8_t pid;
 int8_t status;
 int32_t x;
 int8_t y;
 int32_t z;
 int8_t face;
};
static const int packet_digsz = 4 * sizeof(int8_t) + 2 * sizeof(int32_t);

/* pid 0x0F */
struct packet_blockplace
{
  int8_t pid;
  int16_t itemid;
  int32_t x;
  int8_t y;
  int32_t z;
  int8_t direction;
};
static const int packet_blockplacesz = 3 * sizeof(int8_t) + sizeof(int16_t)
				     + 2 * sizeof(int32_t);

/* pid 0x10 */
struct packet_holdchange
{
  int8_t pid;
  int32_t unused;
  int16_t itemid;
};
static const int packet_holdchangesz = sizeof(int8_t) + sizeof(int32_t)
				     + sizeof(int16_t);

/* pid 0x12 */
struct packet_armanimate
{
  int8_t pid;
  int32_t eid;
  bool animate;
};
static const int packet_armanimatesz = sizeof(int8_t) + sizeof(int32_t)
		       + sizeof(int8_t); // Use int8_t to ensure bool is 1 byte

/* pid 0x15 */
struct packet_pickupspawn
{
  int8_t pid;
  int32_t eid;
  int16_t item;
  int8_t count;
  int32_t x;
  int32_t y;
  int32_t z;
  int8_t rotation;
  int8_t pitch;
  int8_t roll;
};
static const int packet_pickupspawnsz = 5 * sizeof(int8_t) 
				      + 4 * sizeof(int32_t) + sizeof(int16_t);

/* pid 0x32 */
struct packet_prechunk
{
  int8_t pid;
  int32_t x;
  int32_t z;
  bool mode;
};

/* pid 0x33 */
struct packet_mapchunk
{
  int8_t pid;
  int32_t x;
  int16_t y;
  int32_t z;
  uint8_t sizex;
  uint8_t sizey;
  uint8_t sizez;
  int32_t bytearraysize;
  int8_t *bytearray;
};

/* pid 0xFF */
struct packet_disconnect
{
  uint8_t pid;
  int16_t slen;
  bstring message;
};
// Chat size/offset information (C99 initialized struct)
static const struct
{
  const int base;
  const int str1offset;
} packet_disconnectsz = {
  .base       = sizeof(uint8_t) + sizeof(int16_t),
  .str1offset = sizeof(uint8_t)
};

#endif
