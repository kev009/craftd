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

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "craftd-config.h"
#include "network/network.h"
#include "network/network-private.h"
#include "network/packets.h"
#include "mapchunk.h"

// Hack zlib in to test chunk sending
#include <stdio.h>
#include <zlib.h>
#include <fcntl.h>
#include <sys/stat.h>
/**
 * Send the specified chunk to the player
 * 
 * @remarks Scope: public API method
 * @remarks Internally and over the network size{x,y,z} are -1 over the wire
 * 
 * @param player Player List player pointer
 * @param x global chunk x coordinate
 * @param y global chunk y coordinate
 * @param z global chunk z coordinate
 * @param sizex actual x size
 * @param sizey actual y size
 * @param sizez actual z size
 */
void
send_chunk(struct PL_entry *player, int32_t x, int16_t y, int32_t z,
	   uint8_t sizex, uint8_t sizey, uint8_t sizez)
{
  struct evbuffer *output = bufferevent_get_output(player->bev);
  struct evbuffer *tempbuf = evbuffer_new();
  int8_t pid = PID_MAPCHUNK;
  int32_t n_x = htonl(x * 16);
  int16_t n_y = htons(y);
  int32_t n_z = htonl(z * 16);
 
  /* Check that the chunk size is greater than zero since the protocol must
   * subtract one before sending.  If so, do it.
   */
  assert(sizex > 0 && sizey > 0 && sizez > 0);
  assert(sizex <= 128 && sizey <= 128 && sizez <= 128);
  --sizex;
  --sizey;
  --sizez;

  uint8_t mapdata[MAX_CHUNKARRAY];
  if (loadChunk(x, z, &mapdata[0]) != 0)
  {
    /* TODO: bad read.  add to mapgen queue if file DNE */
    return;
  }

  uLongf written = MAX_CHUNKARRAY;
  Bytef *buffer = (Bytef*)Malloc(MAX_CHUNKARRAY);
  if (compress(buffer, &written, &mapdata[0], MAX_CHUNKARRAY) != Z_OK)
    assert(false);
  int32_t n_written = htonl(written);
 
  evbuffer_add(tempbuf, &pid, sizeof(pid));
  evbuffer_add(tempbuf, &n_x, sizeof(n_x));
  evbuffer_add(tempbuf, &n_y, sizeof(n_y));
  evbuffer_add(tempbuf, &n_z, sizeof(n_z));
  evbuffer_add(tempbuf, &sizex, sizeof(sizex));
  evbuffer_add(tempbuf, &sizey, sizeof(sizey));
  evbuffer_add(tempbuf, &sizez, sizeof(sizez));
  evbuffer_add(tempbuf, &n_written, sizeof(n_written));
  evbuffer_add(tempbuf, buffer, written);

  /* TODO: swap to this zero copy method */
  //evbuffer_add_reference(tempbuf, buffer, written, chunkfree_cb, buffer);
  
  evbuffer_add_buffer(output, tempbuf);
  evbuffer_free(tempbuf);
  
  free(buffer);
 
  return;
}