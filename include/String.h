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

#ifndef CRAFTD_STRING_H
#define CRAFTD_STRING_H

#include <stdbool.h>

#include <craftd/bstring/bstrlib.h>
#include <craftd/bstring/bstraux.h>

typedef bstring CDRawString;

/**
 * The String class.
 */
typedef struct _CDString {
    CDRawString raw;

    bool external;
} CDString;

/**
 * Create a String object from a C null terminated string
 *
 * @param string The C string.
 *
 * @return The instantiated String object
 */
CDString* CD_CreateStringFromCString (const char* string);

/**
 * Create a String object from a length given buffer.
 *
 * Note that the buffer is NOT copied and NOT freed when the String is destroyed, use CD_CreateStringFromBufferCopy for that.
 *
 * @param buffer The buffer with the data
 * @param length The length of the data you want to convert in a String
 *
 * @return The intantiated String object
 */
CDString* CD_CreateStringFromBuffer (const char* buffer, size_t length);

/**
 * Create a String object from a length given buffer.
 *
 * Note that the buffer IS copied, use CD_CreateStringFromBuffer if you don't want it to be copied.
 *
 * @param buffer The buffer with the data
 * @param length The length of the data you want to convert in a String
 *
 * @return The intantiated String object
 */
CDString* CD_CreateStringFromBufferCopy (const char* buffer, size_t length);

/**
 * Destroy the String object AND the raw string
 */
void CD_DestroyString (CDString* self);

/**
 * Destroy the String object and return the raw string
 *
 * @return The raw String
 */
CDRawString CD_DestroyStringKeepData (CDString* self);

/**
 * Get the String content as a C string
 */
const char* CD_StringContent (CDString* self);

/**
 * Get the String length
 */
const size_t CD_StringLength (CDString* self);

#endif
