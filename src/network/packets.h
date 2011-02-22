#ifndef CRAFTD_PACKETS_H
#define CRAFTD_PACKETS_H

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

#include <config.h>

#ifdef HAVE_STDBOOL_H
#include <stdbool.h>
#endif

#include "util.h"

/* This file defines structs for different packet types
 * PROTOCOL_VERSION controls the allowed client version
 */

#define PROTOCOL_VERSION (9)

#define MAX_CHUNKARRAY (81920)

enum packetid
{
  PID_KEEPALIVE         = 0x00,
  PID_LOGIN             = 0x01,
  PID_HANDSHAKE         = 0x02,
  PID_CHAT              = 0x03,
  PID_TIMEUPDATE        = 0x04,
  PID_ENTITYEQUIPMENT   = 0x05,
  PID_SPAWNPOS          = 0x06,
  PID_USEENTITY         = 0x07,
  PID_PLAYERHEALTH      = 0x08,
  PID_RESPAWN		= 0x09,
  PID_PLAYERFLY         = 0x0A,
  PID_PLAYERPOS         = 0x0B,
  PID_PLAYERLOOK        = 0x0C,
  PID_PLAYERMOVELOOK    = 0x0D,
  PID_PLAYERDIG         = 0x0E,
  PID_BLOCKPLACE        = 0x0F,
  PID_HOLDCHANGE        = 0x10,
  PID_ARMANIMATE        = 0x12,
  PID_ENTITYACTION	= 0x13,
  PID_NAMEDENTITYSPAWN  = 0x14,
  PID_PICKUPSPAWN       = 0x15,
  PID_COLLECTITEM       = 0x16,
  PID_SPAWNOBJECT       = 0x17,
  PID_SPAWNMOB          = 0x18,
  PID_PAINTING		= 0x19,
  PID_ENTITYVELOCITY    = 0x1C,
  PID_ENTITYDESTROY     = 0x1D,
  PID_ENTITYINIT        = 0x1E,
  PID_ENTITYRELMOVE     = 0x1F,
  PID_ENTITYLOOK        = 0x20,
  PID_ENTITYLOOKMOVE 	= 0x21,
  PID_ENTITYPOS		= 0x22,
  PID_ENTITYSTATUS      = 0x26,
  PID_ENTITYATTACH      = 0x27,
  PID_ENTITYMETA	= 0x28,
  PID_PRECHUNK          = 0x32,
  PID_MAPCHUNK	        = 0x33,
  PID_MULTIBLOCKCHANGE  = 0x34,
  PID_BLOCKCHANGE	= 0x35,
  PID_EXPLOSION         = 0x3C,
  PID_OPENWINDOW        = 0x64,
  PID_CLOSEWINDOW       = 0x65,
  PID_WINDOWCLICK       = 0x66,
  PID_SETSLOT           = 0x67,
  PID_WINDOWITEMS       = 0x68,
  PID_UPDATEPROGBAR     = 0x69,
  PID_TRANSACTION       = 0x6A,
  PID_UPDATESIGN        = 0x82,
  PID_DISCONNECT        = 0xFF
};

/* pid 0x00 */
struct packet_keepalive
{
  MCbyte pid;
};
static const int packet_keepalivesz = sizeof(MCbyte);

/* pid 0x01 */
struct packet_login
{
  MCbyte pid;
  MCint version;
  bstring username;
  bstring password;
  MClong mapseed;
  MCbyte dimension;
};
// Login size/offset information (C99 initialized struct)
static const struct
{
  const int base;
  const int str1offset;
  const int str2offset;
} packet_loginsz = {
  .base       = sizeof(MCbyte) + sizeof(MCint) + sizeof(MCshort)
	      + sizeof(MCshort) + sizeof(MClong) + sizeof(MCbyte),
  .str1offset = sizeof(MCbyte) + sizeof(MCint),
  .str2offset = sizeof(MCbyte) + sizeof(MCint) + sizeof(MCshort)
};

/* pid 0x02 */
struct packet_handshake
{
  MCbyte pid;
  bstring username;
};
// Handshake size/offset information (C99 initialized struct)
static const struct
{
  const int base;
  const int str1offset;
} packet_handshakesz = {
  .base       = sizeof(MCbyte) + sizeof(MCshort),
  .str1offset = sizeof(MCbyte)
};

/* pid 0x03 */
struct packet_chat
{
  MCbyte pid;
  bstring message;
};
// Chat size/offset information (C99 initialized struct)
static const struct
{
  const int base;
  const int str1offset;
} packet_chatsz = {
  .base       = sizeof(MCbyte) + sizeof(MCshort),
  .str1offset = sizeof(MCbyte)
};

/* pid 0x04 */
struct packet_time
{
  MCbyte pid;
  MClong time;
};
static const int packet_timesz = sizeof(MCbyte) + sizeof(MClong);

/* pid 0x05 */
struct packet_entityequipment
{
  MCbyte pid;
  MCint eid;
  MCshort eslot;
  MCshort eitem;
  MCshort dmg; //Don't really know yet
};
static const int packet_entityequipmentsz = sizeof(MCbyte) + sizeof(MCint)
		      + 3 * sizeof(MCshort);

/* pid 0x06 */
struct packet_spawnpos
{
  MCbyte pid;
  MCint x;
  MCint y;
  MCint z;
};
static const int packet_spawnpossz = sizeof(MCbyte) + 3 * sizeof(MCint);

/* pid 0x07 */
struct packet_useentity
{
  MCbyte pid;
  MCint user;
  MCint target;
  bool leftclick;
};
static const int packet_useentitysz = sizeof(MCbyte) + 2 * sizeof(MCint)
		       + sizeof(MCbyte); // Use MCbyte to ensure bool is 1 byte

/* pid 0x08 */
struct packet_playerhealth
{
  MCbyte pid;
  MCshort health;
};
static const int packet_playerhealthsz = sizeof(MCbyte) + sizeof(MCshort);

/* pid 0x09 */
struct packet_respawn
{
  MCbyte pid;
};
static const int packet_respawnsz = sizeof(MCbyte);

/* pid 0x0A */
struct packet_playerfly
{
  MCbyte pid;
  bool onground;
};
static const int packet_playerflysz = sizeof(MCbyte)
		      + sizeof(MCbyte); // Use MCbyte to ensure bool is 1 byte

/* pid 0x0B */
struct packet_playerpos
{
  MCbyte pid;
  MCdouble x;
  MCdouble y;
  MCdouble stance;
  MCdouble z;
  bool flying;
};
static const int packet_playerpossz = sizeof(MCbyte) + 4 * sizeof(MCdouble)
		       + sizeof(MCbyte); // Use MCbyte to ensure bool is 1 byte

/* pid 0x0C */
struct packet_look
{
  MCbyte pid;
  MCfloat yaw;
  MCfloat pitch;
  bool flying;
};
static const int packet_looksz = sizeof(MCbyte) + 2 * sizeof(MCfloat) 
		       + sizeof(MCbyte); // Use MCbyte to ensure bool is 1 byte

/* pid 0x0D */
struct packet_movelook
{
  MCbyte pid;
  MCdouble x;
  MCdouble y;
  MCdouble stance;
  MCdouble z;
  MCfloat yaw;
  MCfloat pitch;
  bool flying;
};
static const int packet_movelooksz = sizeof(MCbyte) + 4 * sizeof(MCdouble)
   + 2 * sizeof(MCfloat) + sizeof(MCbyte); // Use MCbyte to ensure bool is 1 byte

/* pid 0x0E */
struct packet_dig
{
 MCbyte pid;
 MCbyte status;
 MCint x;
 MCbyte y;
 MCint z;
 MCbyte face;
};
enum dig_status
{
	STATUS_STARTED = 0x00,
	STATUS_DIGGING = 0x01,
	STATUS_STOPPED = 0x02,
	STATUS_BROKEN = 0x03,
	STATUS_ITEMDROP = 0x04
};
enum block_faces
{
	FACE_Y_NEG = 0x00,
	FACE_Y_POS = 0x01,
	FACE_Z_NEG = 0x02,
	FACE_Z_POS = 0x03,
	FACE_X_NEG = 0x04,
	FACE_X_POS = 0x05
};
static const int packet_digsz = 4 * sizeof(MCbyte) + 2 * sizeof(MCint);

/* pid 0x0F */
struct packet_blockplace
{
  MCbyte pid;
  MCint x;
  MCbyte y;
  MCint z;
  MCbyte direction;
  MCshort itemid;
  MCbyte amount;
  MCshort damage;
};
// Block/item place size information (C99 initialized struct)
static const struct
{
  const int itemidoffset;
  const int emptyplace;
  const int place;
} packet_blockplacesz = {
  .itemidoffset = 3 * sizeof(MCbyte) + 2 * sizeof(MCint),
  .emptyplace = 3 * sizeof(MCbyte) + 2 * sizeof(MCint) + sizeof(MCshort),
  .place =  4 * sizeof(MCbyte) + 2 * sizeof(MCint) + 2 * sizeof(MCshort)
};

/* pid 0x10 */
struct packet_holdchange
{
  MCbyte pid;
  MCshort itemid;
};
static const int packet_holdchangesz = sizeof(MCbyte) + sizeof(MCshort);

/* pid 0x12 */
struct packet_armanimate
{
  MCbyte pid;
  MCint eid;
  MCbyte animate;
};
static const int packet_armanimatesz = 2 * sizeof(MCbyte) + sizeof(MCint);

/* pid 0x13 */
struct packet_entityaction
{
  MCbyte pid;
  MCint eid;
  MCbyte action;
};
static const int packet_entityactionsz = 2 * sizeof(MCbyte) + sizeof(MCint);

/* pid 0x14 */
struct packet_namedentityspawn
{
  MCbyte pid;
  MCint eid;
  bstring entityname;
  MCint x;
  MCint y;
  MCint z;
  MCbyte rotation;
  MCbyte pitch;
  MCshort currentitem;
};
// Named Entity size information (C99 initialized struct)
static const struct
{
  const int base;
  const int str1offset;
} packet_namedentityspawnsz = {
  .base       = 3 * sizeof(MCbyte) + 4 * sizeof(MCint) + 2 * sizeof(MCshort),
  .str1offset = sizeof(MCbyte) +  sizeof(MCint)
};

/* pid 0x15 */
struct packet_pickupspawn
{
  MCbyte pid;
  MCint eid;
  MCshort item;
  MCbyte count;
  MCshort damage; //Verify
  MCint x;
  MCint y;
  MCint z;
  MCbyte rotation;
  MCbyte pitch;
  MCbyte roll;
};
static const int packet_pickupspawnsz = 5 * sizeof(MCbyte) 
				      + 4 * sizeof(MCint) + 2 * sizeof(MCshort);

/* pid 0x16 */
struct packet_collectitem
{
  MCbyte pid;
  MCint eidcollected;
  MCint eidcollector;
};
static const int packet_collectitemsz = sizeof(MCbyte) + 2 * sizeof(MCint);
  
/* pid 0x17 */
struct packet_spawnobject
{
  MCbyte pid;
  MCint eid;
  MCbyte otype;
  MCint x;
  MCint y;
  MCint z;
};
static const int packet_spawnobjectsz = 2 * sizeof(MCbyte) + 4 * sizeof(MCint);

/* pid 0x18 */
struct packet_spawnmob
{
  MCbyte pid;
  MCint eid;
  MCbyte mtype;
  MCint x;
  MCint y;
  MCint z;
  MCbyte rotation;
  MCbyte pitch;
  void* metadata;
};
static const int packet_spawnmobszbase = 4 * sizeof(MCbyte) + 4 * sizeof(MCint);

/* pid 0x19 */
struct packet_painting //TODO Verify packet details
{
  MCbyte pid;
  MCint eid;
  bstring title;
  MCint x;
  MCint y;
  MCint z;
  MCint type;
};
// painting packet size information (C99 initialized struct)
static const struct
{
  const int base;
  const int str1offset;
} packet_paintingsz = {
  .base       = sizeof(MCbyte) + 5 * sizeof(MCint) + sizeof(MCshort),
  .str1offset = sizeof(MCbyte) +  sizeof(MCint)
};

/* pid 0x1c */
struct packet_entityvelocity
{
  MCbyte pid;
  MCint eid;
  MCshort vx;
  MCshort vy;
  MCshort vz;
};
static const int packet_entityvelocitysz = sizeof(MCbyte) + sizeof(MCint) 
					   + 3 * sizeof(MCshort);

/* pid 0x1d */
struct packet_entitydestroy
{
  MCbyte pid;
  MCint eid;
};
static const int packet_entitydestroysz = sizeof(MCbyte) + sizeof(MCint);

/* pid 0x1e */
struct packet_entityinit
{
  MCbyte pid;
  MCint eid;
};
static const int packet_entityinitsz = sizeof(MCbyte) + sizeof(MCint);

/* pid 0x1f */
struct packet_entityrelmove
{
  MCbyte pid;
  MCint eid;
  MCbyte rx;
  MCbyte ry;
  MCbyte rz;
};
static const int packet_entityrelmovesz = 4 * sizeof(MCbyte) + sizeof(MCint);

/* pid 0x20 */
struct packet_entitylook
{
  MCbyte pid;
  MCint eid;
  MCbyte rotation;
  MCbyte pitch;
};
static const int packet_entitylooksz = 3 * sizeof(MCbyte) + sizeof(MCint);

/* pid 0x21 */
struct packet_entitylookmove
{
  MCbyte pid;
  MCint eid;
  MCbyte rx;
  MCbyte ry;
  MCbyte rz;
  MCbyte rotation;
  MCbyte pitch;
};
static const int packet_entitylookmovesz = 6 * sizeof(MCbyte) + sizeof(MCint);

struct packet_entitypos
{
  MCbyte pid;
  MCint eid;
  MCint x;
  MCint y;
  MCint z;
  MCbyte rotation;
  MCbyte pitch;
};
static const int packet_entitypossz = 3 * sizeof(MCbyte) + 4 * sizeof(MCint);

/* pid 0x26 */
struct packet_entitystatus
{
  MCbyte pid;
  MCint eid;
  MCbyte status;
};
static const int packet_entitystatussz = 2 * sizeof(MCbyte) + sizeof(MCint);

/* pid 0x27 */
struct packet_entityattach
{
  MCbyte pid;
  MCint eid;
  MCint vid; //eid of vehicle to be attached to
};
static const int packet_entityattachsz = sizeof(MCbyte) + 2 * sizeof(MCint);

/* pid 0x28 */
struct packet_entitymeta
{
  MCbyte pid;
  MCint eid;
  MCbyte *metadata;//TODO add metadata field
};
static const int packet_entitymetaszbase = sizeof(MCbyte) + 1 * sizeof(MCint);

/* pid 0x32 */
struct packet_prechunk
{
  MCbyte pid;
  MCint x;
  MCint z;
  bool mode;
};
static const int packet_prechunksz = 2 * sizeof(MCbyte) + 2 * sizeof(MCint);

/* pid 0x33 */
struct packet_mapchunk
{
  MCbyte pid;
  MCint x;
  MCshort y;
  MCint z;
  MCbyte sizex;
  MCbyte sizey;
  MCbyte sizez;
  MCint bytearraysize;
  MCbyte *bytearray;
};
static const struct
{
  const int base;
  const int sizelocation;
} packet_mapchunksz = {
  .base         = 4 * sizeof(MCbyte) + 3 * sizeof(MCint) + sizeof(MCshort),
  .sizelocation = 4 * sizeof(MCbyte) + 2 * sizeof(MCint) + sizeof(MCshort)
};

/* pid 0x34 */
struct packet_multiblockchange 
{
  MCbyte pid;
  MCint x;
  MCint z;
  MCshort asize;
  MCshort *cord;
  MCbyte *type;
  MCbyte *metadata;
};
static const int packet_multiblockchangeszbase = sizeof(MCbyte)
                                               + 2 * sizeof(MCint);

/* pid 0x35 */
struct packet_blockchange
{
  MCbyte pid;
  MCint x;
  MCbyte y;
  MCint z;
  MCbyte btype;
  MCbyte metadata;
};
static const int packet_blockchangesz = 4 * sizeof(MCbyte) + 2 * sizeof(MCint);

/* pid 0x36 */
struct packet_noteblockplay
{
  MCbyte pid;
  MCint x;
  MCshort y;
  MCint z;
  MCbyte itype;
  MCbyte pitch;
};
static const int packet_noteblockplaysz = 3 * sizeof(MCbyte) + sizeof(MCshort)
				        + 2 * sizeof(MCint);

/* pid 0x3c */
struct packet_explosion
{
  MCbyte pid;
  MCdouble x;
  MCdouble y;
  MCdouble z;
  MCfloat animradius;
  MCint recordc;
  MCbyte *records;
};
//TODO determine size of packet_explosion

/* pid 0x64 */
struct packet_openwindow
{
  MCbyte pid;
  MCbyte winid;
  MCbyte type;
  bstring title;
  MCbyte slots;
};
// Open window size/offset information (C99 initialized struct)
static const struct
{
  const int base;
  const int str1offset;
} packet_openwindowsz = {
  .base       = 4 * sizeof(MCbyte) + sizeof(MCshort),
  .str1offset = 3 * sizeof(MCbyte)
};

/* pid 0x65 */
struct packet_closewindow
{
  MCbyte pid;
  MCbyte winid;
};
static const int packet_closewindowsz = 2 * sizeof(MCbyte);

/* pid 0x66 */
/* Note: this packet is variable length.  If itemid is -1, itemcount and
 * itemuses are not sent
 */
struct packet_windowclick
{
  MCbyte pid;
  MCbyte winid;
  MCshort slot;
  MCbyte rightclick;
  MCshort action;
  MCshort itemid;
  MCbyte itemcount;
  MCshort itemuses;
};
// Window click size information (C99 initialized struct)
static const struct
{
  const int itemidoffset;
  const int clicknull;
  const int click;
} packet_windowclicksz = {
  .itemidoffset = 3 * sizeof(MCbyte) + 2 * sizeof(MCshort),
  .clicknull = 3 * sizeof(MCbyte) + 3 * sizeof(MCshort),
  .click = 4 * sizeof(MCbyte) + 4 * sizeof(MCshort)
};

/* pid 0x67 */
struct packet_setslot
{
  MCbyte pid;
  MCbyte winid;
  MCshort slot;
  MCitem item;
};
static const struct
{
  const int itemidoffset;
  const int itemnull;
  const int item;
} packet_setslotsz = {
  .itemidoffset = 2 * sizeof(MCbyte) + 1 * sizeof(MCshort),
  .itemnull     = 2 * sizeof(MCbyte) + 2 * sizeof(MCshort),
  .item         = 3 * sizeof(MCbyte) + 3 * sizeof(MCshort)
};

/* pid 0x68 */
struct packet_windowitems //The size of this packet depends on two varaibles
{			  //The amount of items and the if the items are null or not
  MCbyte pid;
  MCbyte winid;
  MCshort icount;
  MCitem *payload;
};
static const struct
{
  const int itemcountoffset;
  const int itemnullsize;
  const int itemsize;
} packet_windowitemssz = {
  .itemcountoffset = 2 * sizeof(MCbyte),
  .itemnullsize    = 1 * sizeof(MCshort),
  .itemsize        = 1 * sizeof(MCbyte) + 2 * sizeof(MCshort)
};

/* pid 0x69 */
struct packet_updateprogbar
{
  MCbyte pid;
  MCbyte winid;
  MCshort pbar;
  MCshort value;
};
static const int packet_updateprogbarsz = 2 * sizeof(MCbyte) + 2 * sizeof(MCshort);

/* pid 0x6a */
struct packet_transaction
{
  MCbyte pid;
  MCbyte winid;
  MCshort actionnum;
  bool accepted;
};
static const int packet_transactionsz = 3 * sizeof(MCbyte) + sizeof(MCshort);

/* pid 0x82 */
struct packet_updatesign
{
  MCbyte pid;
  MCint x;
  MCshort y;
  MCint z;
  bstring line1;
  bstring line2;
  bstring line3;
  bstring line4;
};
// Login size/offset information (C99 initialized struct)
static const struct
{
  const int base;
  const int str1offset;
  const int str2offset;
  const int str3offset;
  const int str4offset;
} packet_updatesignsz = {
  .base       = sizeof(MCbyte) + 2 * sizeof(MCint) + 5 * sizeof(MCshort),
  .str1offset = sizeof(MCbyte) + 2 * sizeof(MCint) + 1 * sizeof(MCshort),
  .str2offset = sizeof(MCbyte) + 2 * sizeof(MCint) + 2 * sizeof(MCshort),
  .str3offset = sizeof(MCbyte) + 2 * sizeof(MCint) + 3 * sizeof(MCshort),
  .str4offset = sizeof(MCbyte) + 2 * sizeof(MCint) + 4 * sizeof(MCshort)
};

/* pid 0xFF */
struct packet_disconnect
{
  MCbyte pid;
  MCshort rlen;
  bstring reason;
};
// Chat size/offset information (C99 initialized struct)
static const struct
{
  const int base;
  const int str1offset;
} packet_disconnectsz = {
  .base       = sizeof(MCbyte) + sizeof(MCshort),
  .str1offset = sizeof(MCbyte)
};

#endif
