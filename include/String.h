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
    size_t      length;
    bool        external;
} CDString;

/**
 * Create an empty String object
 *
 * @return The instantiated obnject
 */
CDString* CD_CreateString (void);

/**
 * Create a String object from a C null terminated string
 *
 * @param string The C string.
 *
 * @return The instantiated String object
 */
CDString* CD_CreateStringFromCString (const char* string);

/**
 * Create a String object from a C null terminated string
 *
 * @param string The C string.
 *
 * @return The instantiated String object
 */
CDString* CD_CreateStringFromCStringCopy (const char* string);

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
 * Create a String object from a printf-like format string
 *
 * @param format The format to use
 *
 * @return The instantiated String object
 */
CDString* CD_CreateStringFromFormat (const char* format, ...);

/**
 * Create a String object from a printf-like format string and a passed va_list
 *
 * @param format The format to use
 * @param ap A va_started va_list
 *
 * @return The instantiated String object
 */
CDString* CD_CreateStringFromFormatList (const char* format, va_list ap);

/**
 * Create a String object from a substring
 *
 * @param offset The offset where to start
 * @param limit How many chars to include in the result
 *
 * @return The instantiated String object
 */
CDString* CD_CreateStringFromOffset (CDString* self, size_t offset, size_t limit);

/**
 * Clone a String object.
 *
 * @return The cloned object
 */
CDString* CD_CloneString (CDString* self);

/**
 * Replace the String content and destroy the passed object
 */
CDString* CD_ReplaceString (CDString* self, CDString* string);

CDString* CD_ReplaceCString (CDString* self, const char* string);

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

CDString* CD_AppendString (CDString* self, CDString* append);

CDString* CD_AppendCString (CDString* self, const char* append);

CDString* CD_PrependString (CDString* self, CDString* append);

CDString* CD_PrependCString (CDString* self, const char* append);

/**
 * Get the char at the given index
 *
 * @param index The position of the char you want to get
 *
 * @return A String with the wanted char (unicode char)
 */
CDString* CD_CharAt (CDString* self, size_t index);

CDString* CD_InsertString (CDString* self, CDString* insert, size_t position);

/**
 * Get the String content as a C string
 */
const char* CD_StringContent (CDString* self);

/**
 * Get the String length
 */
size_t CD_StringLength (CDString* self);

/**
 * Get raw String length
 */
size_t CD_StringSize (CDString* self);

/**
 * Check if a String is empty (length 0) or not
 *
 * @return true when empty, false otherwise
 */
bool CD_StringEmpty (CDString* self);

/**
 * Check if a String is blank (/^\s*$/) or not
 *
 * @return true when blank, false otherwise
 */
bool CD_StringBlank (CDString* self);

/**
 * Check if a String starts with the given value
 *
 * @param check The string which the String object has to start
 *
 * @return true if it starts with it, false otherwise
 */
bool CD_StringStartWith (CDString* self, const char* check);

/**
 * Check if a String ends with the given value
 *
 * @param check The string which the String object has to end
 *
 * @return true if it ends with it, false otherwise
 */
bool CD_StringEndWith (CDString* self, const char* check);

bool CD_StringIsEqual (CDString* a, const char* b);

bool CD_CStringIsEqual (const char* a, const char* b);

size_t CD_UTF8_strlen (const char* data);

size_t CD_UTF8_strnlen (const char* data, size_t limit);

size_t CD_UTF8_offset (const char* data, size_t offset);

CDString* CD_StringDirname (CDString* self);

CDString* CD_StringBasename (CDString* self);

#endif
