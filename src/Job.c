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

#include <craftd/Job.h>

CDJob*
CD_CreateJob (CDJobType type, CDPointer data)
{
    CDJob* self = CD_malloc(sizeof(CDJob));

    assert(self);

    self->type = type;
    self->data = data;

    return self;
}

void
CD_DestroyJob (CDJob* self)
{
    assert(self);

    switch (self->type) {
        case CDCustomJob: {
            CD_free((CDCustomJobData*) self->data);
        } break;

        case CDPlayerProcessJob: {
            CD_DestroyPacket(((CDPlayerProcessJobData*) self->data)->packet);
        } break;
    }

    CD_free(self);
}

CDPointer
CD_DestroyJobKeepData (CDJob* self)
{
    assert(self);

    CDPointer result = self->data;

    CD_free(self);

    return result;
}

CDCustomJobData*
CD_CreateCustomJob (CDCustomJobCallback callback, CDPointer data)
{
    CDCustomJobData* self = CD_malloc(sizeof(CDCustomJobData));

    assert(self);

    self->callback = callback;
    self->data     = data;

    return self;
}

CDPlayerProcessJobData*
CD_CreatePlayerProcessJob (CDPlayer* player, CDPacket* packet)
{
    CDPlayerProcessJobData* self = CD_malloc(sizeof(CDPlayerProcessJobData));

    assert(self);

    self->player = player;
    self->packet = packet;

    return self;
}
