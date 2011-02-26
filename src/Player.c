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

#include <craftd/Player.h>
#include <craftd/Server.h>

CDPlayer*
CD_CreatePlayer (struct _CDServer* server)
{
    CDPlayer* self = CD_malloc(sizeof(CDPlayer));

    if (!self) {
        return NULL;
    }

    pthread_rwlock_init(&self->lock.status, NULL);
    pthread_rwlock_init(&self->lock.pending, NULL);

    self->server = server;

    self->entity.id         = CD_ServerGenerateEntityId(server);
    self->entity.type       = MCEntityPlayer;
    self->entity.position.x = 0;
    self->entity.position.y = 0;
    self->entity.position.z = 0;

    self->status  = CDPlayerIdle;
    self->pending = false;

    PRIVATE(self) = CD_CreateHash();

    return self;
}

void
CD_DestroyPlayer (CDPlayer* self)
{
    bdestroy(self->name);

    CD_EventDispatch(self->server, "Player.destroy", self);

    pthread_rwlock_destroy(&self->lock.status);
    pthread_rwlock_destroy(&self->lock.pending);

    CD_DestroyHash(PRIVATE(self));

    CD_free(self);
}

void
CD_PlayerSendPacket (CDPlayer* self, CDPacket* packet)
{
    CDString* data = CD_PacketToString(packet);

    CD_PlayerSendRaw(self, data);

    CD_DestroyString(data);
}

void
CD_PlayerSendRaw (CDPlayer* self, CDString* data)
{
    evbuffer_add(bufferevent_get_output(self->buffer), CD_StringContent(data), CD_StringLength(data));
}
