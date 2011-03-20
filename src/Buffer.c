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

#include <craftd/Buffer.h>

CDBuffer*
CD_CreateBuffer (void)
{
    CDBuffer* self = CD_malloc(sizeof(CDBuffer));

    assert(self);

    self->raw      = evbuffer_new();
    self->external = false;

    evbuffer_enable_locking(self->raw, NULL);

    return self;
}

CDBuffer*
CD_WrapBuffer (CDRawBuffer buffer)
{
    CDBuffer* self = CD_malloc(sizeof(CDBuffer));

    assert(self);

    self->raw      = buffer;
    self->external = true;

    evbuffer_enable_locking(self->raw, NULL);

    return self;
}

void
CD_DestroyBuffer (CDBuffer* self)
{
    if (!self->external) {
        evbuffer_free(self->raw);
    }

    CD_free(self);
}

CDPointer
CD_BufferContent (CDBuffer* self)
{
    CDPointer data = (CDPointer) CD_malloc(CD_BufferLength(self));

    evbuffer_copyout(self->raw, (void*) data, CD_BufferLength(self));

    return data;
}

size_t
CD_BufferLength (CDBuffer* self)
{
    return evbuffer_get_length(self->raw);
}

bool
CD_BufferEmpty (CDBuffer* self)
{
    return CD_BufferLength(self) == 0;
}

int
CD_BufferDrain (CDBuffer* self, size_t length)
{
    return evbuffer_drain(self->raw, length);
}

void
CD_BufferAdd (CDBuffer* self, CDPointer data, size_t length)
{
    evbuffer_add(self->raw, (void*) data, length);
}

void
CD_BufferAddBuffer (CDBuffer* self, CDBuffer* data)
{
    CDPointer stuff = CD_BufferContent(data);

    evbuffer_add(self->raw, (void*) stuff, CD_BufferLength(data));

    CD_free((void*) stuff);
}

CDPointer
CD_BufferRemove (CDBuffer* self, size_t length)
{
    CDPointer result = (CDPointer) CD_malloc(length);

    evbuffer_remove(self->raw, (void*) result, length);

    return result;
}

CDBuffer*
CD_BufferRemoveBuffer (CDBuffer* self)
{
    struct evbuffer* buffer = evbuffer_new();
    CDBuffer*        result;

    evbuffer_remove_buffer(self->raw, buffer, CD_BufferLength(self));

    result = CD_WrapBuffer(buffer);

    result->external = false;

    return result;
}
