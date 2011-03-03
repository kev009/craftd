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

bool
CD_PacketParsable (CDBuffers* buffers)
{
    unsigned int length = evbuffer_get_length(buffers->input->raw);
    unsigned char* data = evbuffer_pullup(buffers->input->raw, -1);

    CDPacketType type     = data[0];
    size_t       variable = 0;

    errno = 0;

    if (length < CDPacketLength[type]) {
        goto PACKET_PARSABLE_ERROR;
    }

    switch (type) {
        // TODO: trololololol
    }

    PACKET_PARSABLE_DONE: {
        return true;
    }

    PACKET_PARSABLE_ERROR: {
        if (errno != EILSEQ) {
            errno = EAGAIN;

            CD_BufferReadIn(buffers, CDPacketLength[type] + variable, CDNull);
        }

        return false;
    }
}
