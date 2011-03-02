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

#include <craftd/Worker.h>
#include <craftd/Server.h>
#include <craftd/Workers.h>
#include <craftd/Player.h>
#include <craftd/Logger.h>

CDWorker*
CD_CreateWorker (CDServer* server)
{
    CDWorker* self = CD_malloc(sizeof(CDWorker));

    if (!self) {
        return NULL;
    }

    self->server  = server;
    self->id      = 0;
    self->working = false;
    self->job     = NULL;

    return self;
}

void
CD_DestroyWorker (CDWorker* self)
{
    if (self->thread) {
        self->working = false;
        pthread_cancel(self->thread);
    }

    if (self->job) {
        CD_DestroyJob(self->job);
    }

    CD_free(self);
}

void*
CD_RunWorker (CDWorker* self)
{
    SLOG(self->server, LOG_INFO, "worker %d started", self->id);

    while (self->working) {
        pthread_mutex_lock(&self->workers->lock.mutex);

        SDEBUG(self->server, "worker %d ready", self->id);

        pthread_cond_wait(&self->workers->lock.condition, &self->workers->lock.mutex);

        if (!self->working) {
            pthread_mutex_unlock(&self->workers->lock.mutex);
            break;
        }

        if (!CD_HasJobs(self->workers)) {
            SDEBUG(self->server, "no jobs for %d :<", self->id);
            pthread_mutex_unlock(&self->workers->lock.mutex);
            continue;
        }

        self->job = CD_NextJob(self->workers);

        pthread_mutex_unlock(&self->workers->lock.mutex);

        if (!self->job) {
            continue;
        }

        if (CD_JOB_IS_CUSTOM(self->job)) {
            CDCustomJobData* data = (CDCustomJobData*) self->job->data;

            data->callback(data->data);

            CD_free(data);
            CD_DestroyJob(self->job);
        }
        else if (CD_JOB_IS_PLAYER(self->job)) {
            CDPlayer* player = (CDPlayer*) self->job->data;

            if (!player) {
                CD_DestroyJob(self->job);
                continue;
            }

            pthread_mutex_lock(&player->lock.status);
            pthread_rwlock_wrlock(&player->lock.jobs);
            if (player->status == CDPlayerDisconnect) {
                if (self->job->type != CDPlayerDisconnectJob) {
                    CD_DestroyJob(self->job);
                    self->job = NULL;
                    player->jobs--;
                }
            }
            pthread_rwlock_unlock(&player->lock.jobs);
            pthread_mutex_unlock(&player->lock.status);

            if (!self->job) {
                continue;
            }

            if (self->job->type == CDPlayerProcessJob) {
                CD_EventDispatch(self->server, "Player.process", player);

                pthread_mutex_lock(&player->lock.status);
                if (player->status != CDPlayerDisconnect) {
                    player->status = CDPlayerIdle;
                }
                pthread_mutex_unlock(&player->lock.status);

                pthread_rwlock_wrlock(&player->lock.jobs);
                player->jobs--;
                pthread_rwlock_unlock(&player->lock.jobs);

                CD_DestroyJob(self->job);

                if (CD_BufferLength(player->buffers->input) > 0) {
                    event_base_loop(self->server->event.base, EVLOOP_NONBLOCK);
                }
            }
            else if (self->job->type == CDPlayerDisconnectJob) {
                while (true) {
                    pthread_rwlock_rdlock(&player->lock.jobs);

                    if (player->jobs < 1) {
                        break;
                    }

                    pthread_rwlock_unlock(&player->lock.jobs);
                }

                CD_MapDelete(self->server->entities, player->entity.id);

                if (player->username) {
                    CD_HashDelete(self->server->players, CD_StringContent(player->username));
                }

                CD_DestroyPlayer(player);
            }
        }
        else if (CD_JOB_IS_SERVER(self->job)) {
            if (self->job->type == CDServerBroadcastJob) {
                CDPacketChat pkt;
                pkt.response.message = (CDString*) self->job->data;

                CDPacket response = { CDResponse, CDChat, (CDPointer) &pkt };

                CD_HASH_FOREACH(self->server->players, it) {
                    CDPlayer* player = (CDPlayer*) CD_HashIteratorValue(self->server->players, it);

                    pthread_mutex_lock(&player->lock.status);
                    if (player->status != CDPlayerDisconnect) {
                        CD_PlayerSendPacket(player, &response);
                    }
                    pthread_mutex_unlock(&player->lock.status);
                }

                CD_DestroyString((CDString*) self->job->data);
            }
        }
    }

    SLOG(self->server, LOG_INFO, "worker %d stopped", self->id);

    return NULL;
}
