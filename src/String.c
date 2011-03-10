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

#define CRAFTD_STRING_IGNORE_EXTERN
#include <craftd/String.h>
#undef CRAFTD_STRING_IGNORE_EXTERN

#include <craftd/common.h>

const char* MCCharset =
    " #$%&\"()*+,-./:;<=>!?@[\\]^_'{|}~⌂ªº¿®¬½¼¡«»£×ƒ"
    "0123456789"
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    "ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜøØáíóúñÑ";

static inline
size_t
cd_UTF8_nextCharLength (char data)
{
    if ((data & 0x80) == 0x00) {
        return 1;
    }
    else if ((data & 0xE0) == 0xC0) {
        return 2;
    }
    else if ((data & 0xF0) == 0xE0) {
        return 3;
    }
    else if ((data & 0xF8) == 0xF0) {
        return 4;
    }
    else {
        return 0;
    }
}

static
size_t
cd_UTF8_strlen (const char* data)
{
    size_t result  = 0;
    size_t i       = 0;

    while (data[i] != '\0') {
        i += cd_UTF8_nextCharLength(data[i]);
        result++;
    }

    return result;
}

static
size_t
cd_UTF8_offset (const char* data, size_t offset)
{
    size_t result = 0;

    for (size_t i = 0; i < offset; i++) {
        result += cd_UTF8_nextCharLength(data[result]);
    }

    return result;
}

static
void
cd_UpdateLength (CDString* self)
{
    assert(self);

    self->length = cd_UTF8_strlen(CD_StringContent(self));
}

CDString*
CD_CreateString (void)
{
    CDString* self = CD_malloc(sizeof(CDString));

    assert(self);

    self->raw      = bfromcstr("");
    self->length   = 0;
    self->external = false;

    assert(self->raw);

    return self;
}

CDString*
CD_CreateStringFromCString (const char* string)
{
    CDString* self = CD_malloc(sizeof(CDString));

    assert(self);

    self->raw = CD_malloc(sizeof(*self->raw));

    assert(self->raw);

    if (string == NULL) {
        self->raw->data = (unsigned char*) "";
    }
    else {
        self->raw->data = (unsigned char*) string;
    }

    self->raw->slen = strlen(self->raw->data);
    self->raw->mlen = self->raw->slen;

    self->external = true;

    cd_UpdateLength(self);

    return self;
}

CDString*
CD_CreateStringFromCStringCopy (const char* string)
{
    CDString* self = CD_malloc(sizeof(CDString));

    assert(self);

    self->raw      = bfromcstr(string);
    self->external = false;

    assert(self->raw);

    cd_UpdateLength(self);

    return self;
}

CDString*
CD_CreateStringFromBuffer (const char* buffer, size_t length)
{
    CDString* self = CD_malloc(sizeof(CDString));

    assert(self);

    self->raw      = CD_malloc(sizeof(*self->raw));
    self->external = true;

    assert(self->raw);

    self->raw->data = (unsigned char*) buffer;
    self->raw->mlen = length;
    self->raw->slen = length;

    cd_UpdateLength(self);

    return self;
}

CDString*
CD_CreateStringFromBufferCopy (const char* buffer, size_t length)
{
    CDString* self = CD_malloc(sizeof(CDString));

    assert(self);

    self->raw      = blk2bstr(buffer, length);
    self->external = false;

    assert(self->raw);

    cd_UpdateLength(self);

    return self;
}

CDString*
CD_CreateStringFromFormat (const char* format, ...)
{
    va_list ap;
    va_start(ap, format);

    CDString* self = CD_CreateStringFromFormatList(format, ap);

    va_end(ap);

    self->length = cd_UTF8_strlen(CD_StringContent(self));

    return self;
}

CDString*
CD_CreateStringFromFormatList (const char* format, va_list ap)
{
    CDString* self = CD_CreateString();

    bvcformata(self->raw, 9001, format, ap);

    cd_UpdateLength(self);

    return self;
}

CDString*
CD_CreateStringFromOffset (CDString* self, int offset, int limit)
{
    unsigned char* data;

    assert(self);

    if (offset >= 0 && ((unsigned) offset) > self->length) {
        return NULL;
    }

    data = self->raw->data;

    if (offset >= 0) {
        data += cd_UTF8_offset((const char*) self->raw->data, offset);
    }

    self           = CD_CreateStringFromCString(strndup((const char*) data, cd_UTF8_offset((const char*) data, limit)));
    self->external = false;

    cd_UpdateLength(self);

    return self;
}

CDString*
CD_CloneString (CDString* self)
{
    CDString* cloned = CD_CreateString();

    bdestroy(cloned->raw);
    cloned->raw = (CDRawString) bstrcpy(self->raw);

    assert(cloned->raw);

    cd_UpdateLength(cloned);

    return cloned;
}

void
CD_DestroyString (CDString* self)
{
    assert(self);

    if (self->external) {
        CD_free(self->raw);
    }
    else {
        bdestroy(self->raw);
    }

    CD_free(self);
}

CDRawString
CD_DestroyStringKeepData (CDString* self)
{
    CDRawString result = self->raw;

    CD_free(self);

    return result;
}

bool
CD_StringIsValidForMinecraft (CDString* self)
{
    assert(self);

    for (size_t i = 0, ie = CD_StringLength(self); i < ie; i++) {
        bool      has = false;
        CDString* ch  = CD_CharAt(self, i);

        for (size_t h = 0, he = cd_UTF8_strlen(MCCharset); h < he; h++) {
            const char* che = &MCCharset[cd_UTF8_offset(MCCharset, h)];

            if (strncmp(CD_StringContent(ch), che, CD_StringSize(ch)) == 0) {
                has = true;
                break;
            }
        }

        if (!has && !(strncmp(CD_StringContent(ch), "§", 2) == 0 && i < ie - 2)) {
            CD_DestroyString(ch);
            return false;
        }

        CD_DestroyString(ch);
    }

    return true;
}

CDString*
CD_StringSanitizeForMinecraft (CDString* self)
{
    CDString* result = CD_CreateString();

    assert(self);

    for (size_t i = 0, ie = CD_StringLength(self); i < ie; i++) {
        bool      has = false;
        CDString* ch  = CD_CharAt(self, i);

        for (size_t h = 0, he = cd_UTF8_strlen(MCCharset); h < he; h++) {
            const char* che = &MCCharset[cd_UTF8_offset(MCCharset, h)];

            if (strncmp(CD_StringContent(ch), che, CD_StringSize(ch)) == 0) {
                has = true;
                break;
            }
        }

        if (i == ie - 2 && strncmp(CD_StringContent(ch), "§", 2) == 0){
            CD_DestroyString(ch);
            break;
        }

        if (has || strncmp(CD_StringContent(ch), "§", 2) == 0) {
            CD_AppendString(result, ch);
        }
        else {
            CD_AppendCString(result, "?");
        }

        CD_DestroyString(ch);
    }

    cd_UpdateLength(self);

    return result;
}

CDString*
CD_CharAt (CDString* self, size_t index)
{
    assert(self);

    return CD_CreateStringFromOffset(self, index, 1);
}

CDString*
CD_CharAtSet (CDString* self, size_t index, CDString* set)
{
    assert(self);

    if (self->external) {
        return NULL;
    }

    size_t offset = cd_UTF8_offset((const char*) self->raw->data, index);

    if (breplace(self->raw, offset, cd_UTF8_nextCharLength(self->raw->data[offset]), set->raw, '\0') == BSTR_OK) {
        cd_UpdateLength(self);
    }
    else {
        self = NULL;
    }

    return self;
}

CDString*
CD_InsertString (CDString* self, CDString* insert, size_t position)
{
    assert(self);
    assert(insert);

    if (self->external) {
        return NULL;
    }

    if (binsert(self->raw, cd_UTF8_offset(CD_StringContent(self), position), insert->raw, '\0') == BSTR_OK) {
        cd_UpdateLength(self);
    }
    else {
        self = NULL;
    }

    return self;
}

CDString*
CD_AppendString (CDString* self, CDString* append)
{
    assert(self);
    assert(append);

    if (self->external) {
        return NULL;
    }

    if (binsert(self->raw, self->raw->slen, append->raw, '\0') == BSTR_OK) {
        cd_UpdateLength(self);
    }
    else {
        self = NULL;
    }

    return self;
}

CDString*
CD_AppendCString (CDString* self, const char* append)
{
    assert(self);
    assert(append);

    if (self->external) {
        return NULL;
    }

    CDString* tmp = CD_CreateStringFromCString(append);

    if (!CD_AppendString(self, tmp)) {
        self = NULL;
    }

    CD_DestroyString(tmp);

    return self;
}

const char*
CD_StringContent (CDString* self)
{
    if (!self) {
        return NULL;
    }
    else {
        return (const char*) self->raw->data;
    }
}

const size_t
CD_StringLength (CDString* self)
{
    if (!self) {
        return 0;
    }
    else {
        return self->length;
    }
}

const size_t
CD_StringSize (CDString* self)
{
    if (!self) {
        return 0;
    }
    else {
        return self->raw->slen;
    }
}

bool
CD_StringEmpty (CDString* self)
{
    return (self == NULL || self->raw == NULL || CD_StringLength(self) == 0);
}

bool
CD_StringBlank (CDString* self)
{
    for (size_t i = 0; i < self->raw->slen; i++) {
        if (!isspace(self->raw->data[i])) {
            return false;
        }
    }

    return true;
}

bool
CD_StringStartWith (CDString* self, const char* check)
{
    return strncmp(CD_StringContent(self), check, strlen(check)) == 0;
}

bool
CD_StringEndWith (CDString* self, const char* check)
{
    return strncmp(CD_StringContent(self) + strlen(check), check, strlen(check));
}

CDString*
CD_StringColorRange (CDString* self, CDStringColor color, size_t a, size_t b)
{
    if (self->external) {
        return NULL;
    }

    CDString* start = CD_CreateStringFromFormat("§%x", color);
    CDString* end   = CD_CreateStringFromFormat("§%x", CDColorWhite);

    assert(self);
    assert(a < b);
    assert(a >= 0 && b <= CD_StringLength(self));

    CD_InsertString(self, end, b);
    CD_InsertString(self, start, a);

    CD_DestroyString(start);
    CD_DestroyString(end);

    return self;
}

CDString*
CD_StringColor (CDString* self, CDStringColor color)
{
    assert(self);

    return CD_StringColorRange(self, color, 0, CD_StringLength(self));
}
