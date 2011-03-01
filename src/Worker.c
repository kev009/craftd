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
            self->job->running = true;
            ((void (*)(CDServer*)) self->job->data)(self->server);
            self->job->running = false;
        }
        else if (CD_JOB_IS_PLAYER(self->job)) {
            CDPlayer* player = self->job->data;

            if (!player) {
                CD_DestroyJob(self->job);
                continue;
            }

            pthread_mutex_lock(&player->lock.status);
            if (player->status == CDPlayerDisconnect) {
                if (self->job->type != CDPlayerDisconnectJob) {
                    self->job = NULL;
                }
            }
            else {
                if (self->job->type == CDPlayerInputJob && player->status != CDPlayerIdle) {
                    CD_AddJob(self->workers, self->job);
                    self->job = NULL;
                }

                if (self->job->type == CDPlayerProcessJob && player->status != CDPlayerInput) {
                    CD_AddJob(self->workers, self->job);
                    self->job = NULL;
                }
            }

            if (!self->job) {
                pthread_mutex_unlock(&player->lock.status);
                continue;
            }

            self->job->running = true;

            if (self->job->type == CDPlayerInputJob) {
                CDPacket* packet = CD_PacketFromBuffer(player->buffers->input);

                if (packet == NULL) {
                    if (errno == EAGAIN) {
                        player->pending = false;
                        goto PLAYER_JOB_DONE;
                    }
                }

                SDEBUG(self->server, "received packet 0x%.2X from %s", packet->type, player->ip);

                packet = (CDPacket*) CD_HashSet(PRIVATE(player), "packet", (CDPointer) packet);

                if (packet) {
                    CD_DestroyPacket(packet);
                }

                player->status = CDPlayerInput;

                if (CD_BufferLength(player->buffers->input) > 0) {
                    player->pending = true;

                    CD_AddJob(self->workers, self->job);
                    self->job = NULL;
                }
                else {
                    player->pending = false;
                }

                CD_AddJob(self->workers, CD_CreateJob(CDPlayerProcessJob, player));
            }
            else if (self->job->type == CDPlayerProcessJob) {
                player->status = CDPlayerProcess;
                pthread_mutex_unlock(&player->lock.status);

                CD_EventDispatch(self->server, "Player.process", player);

                pthread_mutex_lock(&player->lock.status);
                player->status = CDPlayerIdle;
            }

            PLAYER_JOB_DONE: {
                pthread_mutex_unlock(&player->lock.status);

                if (self->job && self->job->type != CDPlayerDisconnectJob) {
                    CD_ListDeleteAll(player->jobs, (CDPointer) self->job);
                    CD_DestroyJob(self->job);
                }

                if (self->job->type == CDPlayerDisconnectJob) {
                    CD_MapDelete(self->server->entities, player->entity.id);

                    if (player->username) {
                        CD_HashDelete(self->server->players, CD_StringContent(player->username));
                    }

                    CD_LIST_FOREACH(player->jobs, it) {
                        CDJob* job = (CDJob*) CD_ListIteratorValue(it);

                        if (job->type != CDPlayerDisconnectJob) {
                            while (job->running) {
                                continue;
                            }
                        }

                        CD_DestroyJob(job);
                    }

                    CD_DestroyPlayer(player);
                }

                self->job->running = false;
            }
        }
        else if (CD_JOB_IS_SERVER(self->job)) {
            self->job->running = true;

            if (self->job->type == CDServerBroadcastJob) {
                CDPacketChat pkt;
                pkt.response.message = self->job->data;

                CDPacket response = { CDResponse, CDChat, (CDPointer) &pkt };

                CD_HASH_FOREACH(self->server->players, it) {
                    CDPlayer* player = (CDPlayer*) CD_HashIteratorValue(self->server->players, it);

                    pthread_mutex_lock(&player->lock.status);
                    if (player->status != CDPlayerDisconnect) {
                        CD_PlayerSendPacket(player, &response);
                    }
                    pthread_mutex_unlock(&player->lock.status);
                }

                CD_DestroyString(self->job->data);
            }

            self->job->running = false;
        }
    }

    SLOG(self->server, LOG_INFO, "worker %d stopped", self->id);

    return NULL;
}
