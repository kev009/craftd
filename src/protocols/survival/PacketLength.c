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

#include <craftd/protocols/survival/PacketLength.h>
#include <craftd/protocols/survival/Packet.h>

#define CHECK (length < (SVPacketLength[type] + variable))

bool
SV_PacketParsable (CDBuffers* buffers)
{
    size_t       length   = evbuffer_get_length(buffers->input->raw);
    SVPacketType type     = 0;
    size_t       variable = 0;
    size_t       offset   = SVByteSize;
                 errno    = 0;

    if (length < 1) {
        goto error;
    }

    evbuffer_copyout(buffers->input->raw, &type, 1);

    if (length < SVPacketLength[type]) {
        goto error;
    }

    unsigned char* data = evbuffer_pullup(buffers->input->raw, -1);

    switch (type) {
        case SVLogin: {
            variable += ntohs(*((SVShort*) (data + (offset += SVIntegerSize)))) * 2;

            goto check;
        }

        case SVHandshake: {
            variable += ntohs(*((SVShort*) (data + offset))) * 2;

            goto check;
        }

        case SVChat: {
            variable += ntohs(*((SVShort*) (data + offset))) * 2;

            goto check;
        }

        case SVPlayerBlockPlacement: {
            offset += SVIntegerSize + SVByteSize + SVIntegerSize + SVByteSize;

            if (ntohs(*((SVShort*) (data + offset))) != -1) {
                variable += 3;
            }

            goto check;
        }

        case SVEntityMetadata: {
            offset += SVIntegerSize;

            while (length > offset && *((SVByte*) data + offset) != 127) {
                switch (*((SVByte*) (data + offset)) >> 5) {
                    case SVTypeByte:           offset += SVByteSize;                                break;
                    case SVTypeShort:          offset += SVShortSize;                               break;
                    case SVTypeInteger:        offset += SVIntegerSize;                             break;
                    case SVTypeFloat:          offset += SVFloatSize;                               break;
                    case SVTypeString:         offset += *((SVShort*) (data + offset)) + SVShortSize; break;
                    case SVTypeShortByteShort: offset += SVByteSize + SVShortSize + SVByteSize;     break;
                }
            }

            if (length >= offset) {
                goto done;
            }
            else {
                goto error;
            }
        }

        case SVWindowClick: {
            offset += SVByteSize + SVShortSize + SVByteSize + SVShortSize;

            if (ntohs(*((SVShort*) (data + offset))) != -1) {
                variable += 3;
            }

            goto check;
        }

        case SVUpdateSign: {
            offset += SVIntegerSize + SVShortSize + SVIntegerSize;

            variable += ntohs(*((SVShort*) (data + offset))) * 2;

            if (CHECK) {
                goto error;
            }

            variable += ntohs(*((SVShort*) (data + (offset += SVShortSize + variable)))) * 2;

            if (CHECK) {
                goto error;
            }

            variable += ntohs(*((SVShort*) (data + (offset += SVShortSize + variable)))) * 2;

            if (CHECK) {
                goto error;
            }

            variable += ntohs(*((SVShort*) (data + (offset += SVShortSize + variable)))) * 2;

            goto check;
        }

        case SVDisconnect: {
            variable += ntohs(*((SVShort*) (data + offset))) * 2;

            goto check;
        }

        default: {
            goto done;
        }
    }

    check: {
        if (CHECK) {
            goto error;
        }
    }

    done: {
        return true;
    }

    error: {
        if (errno != EILSEQ) {
            errno = EAGAIN;

            CD_BufferReadIn(buffers, SVPacketLength[type] + variable, CDNull);
        }

        return false;
    }
}
