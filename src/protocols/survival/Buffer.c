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

#include <craftd/protocols/survival/Buffer.h>

void
SV_BufferAddFormat (CDBuffer* self, const char* format, ...)
{
    va_list ap;

    va_start(ap, format);

    while (*format != '\0') {
        switch (*format) {
            case 'b': SV_BufferAddByte(self,    va_arg(ap, int));  break;
            case 's': SV_BufferAddShort(self,   va_arg(ap, int));  break;
            case 'i': SV_BufferAddInteger(self, va_arg(ap, int));  break;
            case 'l': SV_BufferAddLong(self,    va_arg(ap, long)); break;

            case 'f': SV_BufferAddFloat(self,  va_arg(ap, double)); break;
            case 'd': SV_BufferAddDouble(self, va_arg(ap, double)); break;

            case 'B': SV_BufferAddBoolean(self, va_arg(ap, int));          break;
            case 'S': SV_BufferAddString(self,  va_arg(ap, CDString*));    break;
            case 'M': SV_BufferAddMetadata(self, va_arg(ap, SVMetadata*)); break;
        }

        format++;
    }

    va_end(ap);
}

void
SV_BufferAddByte (CDBuffer* self, SVByte data)
{
    evbuffer_add(self->raw, &data, SVByteSize);
}

void
SV_BufferAddShort (CDBuffer* self, SVShort data)
{
    data = htons(data);

    evbuffer_add(self->raw, &data, SVShortSize);
}

void
SV_BufferAddInteger (CDBuffer* self, SVInteger data)
{
    data = htonl(data);

    evbuffer_add(self->raw, &data, SVIntegerSize);
}

void
SV_BufferAddLong (CDBuffer* self, SVLong data)
{
    data = htonll(data);

    evbuffer_add(self->raw, &data, SVLongSize);
}

void
SV_BufferAddFloat (CDBuffer* self, SVFloat data)
{
    data = htonf(data);

    evbuffer_add(self->raw, &data, SVFloatSize);
}

void
SV_BufferAddDouble (CDBuffer* self, SVDouble data)
{
    data = htond(data);

    evbuffer_add(self->raw, &data, SVDoubleSize);
}

void
SV_BufferAddBoolean (CDBuffer* self, SVBoolean data)
{
    evbuffer_add(self->raw, &data, SVBooleanSize);
}

void
SV_BufferAddString (CDBuffer* self, CDString* data)
{
    CDString* sanitized = SV_StringSanitize(data);

    SVShort size = htons(CD_StringSize(sanitized));

    evbuffer_add(self->raw, &size, SVShortSize);
    evbuffer_add(self->raw, CD_StringContent(sanitized), CD_StringSize(sanitized));

    SV_DestroyString(sanitized);
}

void
SV_BufferAddMetadata (CDBuffer* self, SVMetadata* data)
{
    // Format strings of the different metadata types
    static char* formats[] = { "b", "s", "i", "f", "S" };

    for (size_t i = 0; i < data->length; i++) {
        if (data->item[i]->type == SVTypeShortByteShort) {
            SV_BufferAddFormat(self, "sbs",
                data->item[i]->data.sbs.first,
                data->item[i]->data.sbs.second,
                data->item[i]->data.sbs.third
            );
        }
        else {
            SV_BufferAddFormat(self, formats[data->item[i]->type], data->item[i]->data);
        }
    }

    SV_BufferAddByte(self, 127);
}

void
SV_BufferRemoveFormat (CDBuffer* self, const char* format, ...)
{
    va_list ap;

    va_start(ap, format);

    while (*format != '\0') {
        CDPointer pointer = va_arg(ap, CDPointer);

        switch (*format) {
            case 'b': *((SVByte*) pointer)    = SV_BufferRemoveByte(self);    break;
            case 's': *((SVShort*) pointer)   = SV_BufferRemoveShort(self);   break;
            case 'i': *((SVInteger*) pointer) = SV_BufferRemoveInteger(self); break;
            case 'l': *((SVLong*) pointer)    = SV_BufferRemoveLong(self);    break;

            case 'f': *((SVFloat*)  pointer) = SV_BufferRemoveFloat(self);  break;
            case 'd': *((SVDouble*) pointer) = SV_BufferRemoveDouble(self); break;

            case 'B': *((SVBoolean*) pointer)   = SV_BufferRemoveBoolean(self);  break;
            case 'S': *((SVString*) pointer)    = SV_BufferRemoveString(self);   break;
            case 'M': *((SVMetadata**) pointer) = SV_BufferRemoveMetadata(self); break;
        }

        format++;
    }

    va_end(ap);
}

SVBoolean
SV_BufferRemoveBoolean (CDBuffer* self)
{
    SVBoolean result = false;

    evbuffer_remove(self->raw, &result, SVBooleanSize);

    return result;
}

SVByte
SV_BufferRemoveByte (CDBuffer* self)
{
    SVByte result = 0;

    evbuffer_remove(self->raw, &result, SVByteSize);

    return result;
}

SVShort
SV_BufferRemoveShort (CDBuffer* self)
{
    SVShort result = 0;

    evbuffer_remove(self->raw, &result, SVShortSize);

    return ntohs(result);
}

SVInteger
SV_BufferRemoveInteger (CDBuffer* self)
{
    SVInteger result = 0;

    evbuffer_remove(self->raw, &result, SVIntegerSize);

    return ntohl(result);
}

SVLong
SV_BufferRemoveLong (CDBuffer* self)
{
    SVLong result = 0;

    evbuffer_remove(self->raw, &result, SVLongSize);

    return ntohll(result);
}

SVFloat
SV_BufferRemoveFloat (CDBuffer* self)
{
    SVFloat result = 0;

    evbuffer_remove(self->raw, &result, SVFloatSize);

    return ntohf(result);
}

SVDouble
SV_BufferRemoveDouble (CDBuffer* self)
{
    SVDouble result = 0;

    evbuffer_remove(self->raw, &result, SVDoubleSize);

    return ntohd(result);
}

SVString
SV_BufferRemoveString (CDBuffer* self)
{
    char*     data   = NULL;
    SVShort   length = 0;
    CDString* result;

    evbuffer_remove(self->raw, &length, SVShortSize);

    length = ntohs(length);
    data   = CD_malloc(length + 1);

    evbuffer_remove(self->raw, data, length);

    data[length] = '\0';

    result           = CD_CreateStringFromBuffer(data, length + 1);
    result->external = false;

    return result;
}

SVMetadata*
SV_BufferRemoveMetadata (CDBuffer* self)
{
    SVMetadata* metadata = SV_CreateMetadata();
    SVData*     current  = NULL;
    SVByte      type     = 0;

    // Format strings of the different metadata types
    static char* formats[] = { "b", "s", "i", "f", "S" };

    while (!CD_BufferEmpty(self)) {
        type = SV_BufferRemoveByte(self);

        if (type == 127) {
            break;
        }

        current       = SV_CreateData();
        current->type = type >> 5;

        if (current->type == SVTypeShortByteShort) {
            SV_BufferRemoveFormat(self, "sbs",
                &current->data.sbs.first,
                &current->data.sbs.second,
                &current->data.sbs.third
            );
        }
        else {
            SV_BufferRemoveFormat(self, formats[current->type], &current->data);
        }

        SV_AppendData(metadata, current);
    }

    return metadata;
}
