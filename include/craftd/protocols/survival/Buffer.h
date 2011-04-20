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

#ifndef CRAFTD_SURVIVAL_BUFFER_H
#define CRAFTD_SURVIVAL_BUFFER_H

#include <craftd/Buffer.h>

#include <craftd/protocols/survival/minecraft.h>

/**
 * Add data to a buffer with data from the given format.
 *
 * Format types:
 *     b: SVByte
 *     s: SVShort
 *     i: SVInteger
 *     l: SVLong
 *
 *     f: SVFloat
 *     d: SVDouble
 *
 *     B: SVBoolean
 *     S: SVString
 *     U: SVString (UCS-2)
 *     M: SVMetadata
 *
 * @param format The format string
 */
void SV_BufferAddFormat (CDBuffer* self, const char* format, ...);

void SV_BufferAddByte (CDBuffer* self, SVByte data);

void SV_BufferAddShort (CDBuffer* self, SVShort data);

void SV_BufferAddInteger (CDBuffer* self, SVInteger data);

void SV_BufferAddLong (CDBuffer* self, SVLong data);

void SV_BufferAddFloat (CDBuffer* self, SVFloat data);

void SV_BufferAddDouble (CDBuffer* self, SVDouble data);

void SV_BufferAddBoolean (CDBuffer* self, SVBoolean data);

void SV_BufferAddString (CDBuffer* self, SVString data);

void SV_BufferAddString16 (CDBuffer* self, SVString data);

void SV_BufferAddMetadata (CDBuffer* self, SVMetadata* data);




/**
 * Remove data from a buffer with data from the given format.
 *
 * Format types:
 *     b: SVByte
 *     s: SVShort
 *     i: SVInteger
 *     l: SVLong
 *
 *     f: SVFloat
 *     d: SVDouble
 *
 *     B: SVBoolean
 *     S: SVString
 *     U: SVString (UCS-2)
 *     M: SVMetadata
 *
 * @param format The format string
 */
void SV_BufferRemoveFormat (CDBuffer* self, const char* format, ...);

SVByte SV_BufferRemoveByte (CDBuffer* self);

SVShort SV_BufferRemoveShort (CDBuffer* self);

SVInteger SV_BufferRemoveInteger (CDBuffer* self);

SVLong SV_BufferRemoveLong (CDBuffer* self);

SVFloat SV_BufferRemoveFloat (CDBuffer* self);

SVDouble SV_BufferRemoveDouble (CDBuffer* self);

SVBoolean SV_BufferRemoveBoolean (CDBuffer* self);

SVString SV_BufferRemoveString (CDBuffer* self);

SVString SV_BufferRemoveString16 (CDBuffer* self);

SVMetadata* SV_BufferRemoveMetadata (CDBuffer* self);

#endif
