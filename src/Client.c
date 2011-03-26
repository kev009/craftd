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

#include <craftd/Client.h>
#include <craftd/Server.h>

CDClient*
CD_CreateClient (CDServer* server)
{
    CDClient* self = CD_malloc(sizeof(CDClient));

    if (pthread_rwlock_init(&self->lock.status, NULL) != 0) {
        CD_abort("pthread rwlock failed to initialize");
    }

    self->server = server;

    self->status = CDClientConnect;
    self->jobs   = 0;

    self->buffers = NULL;

    DYNAMIC(self) = CD_CreateDynamic();
    ERROR(self)   = CDNull;

    return self;
}

void
CD_DestroyClient (CDClient* self)
{
    CD_EventDispatch(self->server, "Client.destroy", self);

    if (self->buffers) {
        bufferevent_flush(self->buffers->raw, EV_READ | EV_WRITE, BEV_FINISHED);
        bufferevent_disable(self->buffers->raw, EV_READ | EV_WRITE);
        bufferevent_free(self->buffers->raw);

        CD_DestroyBuffers(self->buffers);
    }

    CD_DestroyDynamic(DYNAMIC(self));

    pthread_rwlock_destroy(&self->lock.status);

    CD_free(self);
}

void
CD_ClientSendBuffer (CDClient* self, CDBuffer* buffer)
{
    if (!self->buffers) {
        return;
    }

    CD_BufferAddBuffer(self->buffers->output, buffer);

    CD_BuffersFlush(self->buffers);
}
