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

#include <craftd/PacketLength.h>
#include <craftd/Packet.h>

#define CHECK (length < (CDPacketLength[type] + variable))

bool
CD_PacketParsable (CDBuffers* buffers)
{
    unsigned int   length = evbuffer_get_length(buffers->input->raw);
    unsigned char* data   = evbuffer_pullup(buffers->input->raw, -1);

    if (length == 0 || !data) {
        return false;
    }

    CDPacketType type     = data[0];
    size_t       variable = 0;
    size_t       offset   = MCByteSize;
                 errno    = 0;

    if (length < CDPacketLength[type]) {
        goto error;
    }

    switch (type) {
        case CDKeepAlive: {
            goto done;
        }

        case CDLogin: {
            variable += ntohs(*((MCShort*) (data + (offset += MCIntegerSize))));

            if (CHECK) {
                goto error;
            }

            variable += ntohs(*((MCShort*) (data + (offset += MCShortSize + variable))));

            goto check;
        }

        case CDHandshake: {
            variable += ntohs(*((MCShort*) (data + offset)));

            goto check;
        }

        case CDChat: {
            variable += ntohs(*((MCShort*) (data + offset)));

            goto check;
        }

        case CDUseEntity: {
            goto done;
        }

        case CDRespawn: {
            goto done;
        }

        case CDOnGround: {
            goto done;
        }

        case CDPlayerPosition: {
            goto done;
        }

        case CDPlayerLook: {
            goto done;
        }

        case CDPlayerMoveLook: {
            goto done;
        }

        case CDPlayerDigging: {
            goto done;
        }

        case CDPlayerBlockPlacement: {
            offset += MCIntegerSize + MCByteSize + MCIntegerSize + MCByteSize;

            if (ntohs(*((MCShort*) (data + offset))) != -1) {
                variable += 3;
            }

            goto check;
        }

        case CDHoldChange: {
            goto done;
        }

        case CDAnimation: {
            goto done;
        }

        case CDEntityAction: {
            goto done;
        }

        case CDEntityMetadata: {
            offset += MCIntegerSize;

            while (length > offset && *((MCByte*) data + offset) != 127) {
                switch (*((MCByte*) (data + offset)) >> 5) {
                    case MCTypeByte:           offset += MCByteSize;                                break;
                    case MCTypeShort:          offset += MCShortSize;                               break;
                    case MCTypeInteger:        offset += MCIntegerSize;                             break;
                    case MCTypeFloat:          offset += MCFloatSize;                               break;
                    case MCTypeString:         offset += *((MCShort*) (data + offset)) + MCShortSize; break;
                    case MCTypeShortByteShort: offset += MCByteSize + MCShortSize + MCByteSize;     break;
                }
            }

            if (length >= offset) {
                goto done;
            }
            else {
                goto error;
            }
        }

        case CDCloseWindow: {
            goto done;
        }

        case CDWindowClick: {
            offset += MCByteSize + MCShortSize + MCByteSize + MCShortSize;

            if (ntohs(*((MCShort*) (data + offset))) != -1) {
                variable += 3;
            }

            goto check;
        }

        case CDTransaction: {
            goto done;
        }

        case CDUpdateSign: {
            offset += MCIntegerSize + MCShortSize + MCIntegerSize;

            variable += ntohs(*((MCShort*) (data + offset)));

            if (CHECK) {
                goto error;
            }

            variable += ntohs(*((MCShort*) (data + (offset += MCShortSize + variable))));

            if (CHECK) {
                goto error;
            }

            variable += ntohs(*((MCShort*) (data + (offset += MCShortSize + variable))));

            if (CHECK) {
                goto error;
            }

            variable += ntohs(*((MCShort*) (data + (offset += MCShortSize + variable))));

            goto check;
        }

        case CDDisconnect: {
            variable += ntohs(*((MCShort*) (data + offset)));

            goto check;
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

            CD_BufferReadIn(buffers, CDPacketLength[type] + variable, CDNull);
        }

        return false;
    }
}
