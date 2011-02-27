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

#include <craftd/memory.h>

#include <craftd/String.h>

CDString*
CD_CreateString (void)
{
    CDString* self = CD_malloc(sizeof(CDString));

    if (!self) {
        return NULL;
    }

    self->raw      = bfromcstr("");
    self->external = false;

    return self;
}

CDString*
CD_CreateStringFromCString (const char* string)
{
    CDString* self = CD_malloc(sizeof(CDString));

    if (!self) {
        return NULL;
    }

    self->raw      = bfromcstr(string);
    self->external = false;

    return self;
}

CDString*
CD_CreateStringFromBuffer (const char* buffer, size_t length)
{
    CDString* self = CD_malloc(sizeof(CDString));

    self->raw      = CD_malloc(sizeof(struct tagbstring));
    self->external = true;

    self->raw->slen = length;
    self->raw->data = buffer;

    return self;
}

CDString*
CD_CreateStringFromBufferCopy (const char* buffer, size_t length)
{
    CDString* self = CD_malloc(sizeof(CDString));

    self->raw      = blk2bstr(buffer, length);
    self->external = false;

    return self;
}

CDString*
CD_CloneString (CDString* self)
{
    CDString* cloned = CD_CreateString();

    cloned->raw = bstrcpy(self->raw);

    return cloned;
}

void
CD_DestroyString (CDString* self)
{
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
    bstring result = self->raw;

    CD_free(self);

    return result;
}

const char*
CD_StringContent (CDString* self)
{
    return self->raw->data;
}

const size_t
CD_StringLength (CDString* self)
{
    return self->raw->slen;
}
