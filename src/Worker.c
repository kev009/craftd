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

        if (CD_ListLength(self->workers->jobs) == 0) {
            SDEBUG(self->server, "no jobs for me (%d) :<", self->id);
            pthread_mutex_unlock(&self->workers->lock.mutex);
            continue;
        }

        if (!self->working) {
            SDEBUG(self->server, "worker %d stopped", self->id);
            pthread_mutex_unlock(&self->workers->lock.mutex);
            break;
        }

        do {
            self->job = CD_NextJob(self->workers);

            if (CD_JOB_IS_PLAYER(self->job)) {
                CDPlayer* player = self->job->data;

                pthread_rwlock_rdlock(&player->lock.status);

                switch (self->job->type) {
                    case CDPlayerInputJob: {
                        if (player->status != CDPlayerIdle) {
                            CD_AddJob(self->workers, self->job);
                            self->job = NULL;
                        }
                    } break;

                    case CDPlayerProcessJob: {
                        if (player->status != CDPlayerInput) {
                            CD_AddJob(self->workers, self->job);
                            self->job = NULL;
                        }
                    } break;
                }

                pthread_rwlock_unlock(&player->lock.status);
            }
        } while (!self->job);

        pthread_mutex_unlock(&self->workers->lock.mutex);

        SDEBUG(self->server, "doing job type: %d", self->job->type);

        if (CD_JOB_IS_PLAYER(self->job)) {
            CDPlayer* player = self->job->data;

            pthread_rwlock_rdlock(&player->lock.disconnecting);
            if (player->disconnecting) {
                goto PLAYER_JOB_DONE;
            }
            pthread_rwlock_unlock(&player->lock.disconnecting);

            SDEBUG(self->server, "working on player %s (%s)", player->username, player->ip);

            pthread_rwlock_wrlock(&player->lock.pending);
            pthread_rwlock_wrlock(&player->lock.status);

            switch (self->job->type) {
                case CDPlayerInputJob: {
                    CDPacket* packet = NULL;

                    if (!self->job) {
                        SERR(self->server, "Aaack, null event or context in worker?");

                        goto PLAYER_JOB_ERROR;
                    }

                    packet = CD_PacketFromBuffer(player->buffers->input);

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

                    if (CD_BufferLength(player->buffers->input) > 0) {
                        player->pending = true;

                        CD_AddJob(self->workers, self->job);
                    }
                    else {
                        player->pending = false;
                    }

                    player->status = CDPlayerInput;

                    CD_AddJob(self->workers, CD_CreateJob(CDPlayerProcessJob, player));
                } break;

                case CDPlayerProcessJob: {
                    SDEBUG(self->server, "processing player %d", player->entity.id);

                    player->status = CDPlayerProcess;
                    pthread_rwlock_unlock(&player->lock.pending);
                    pthread_rwlock_unlock(&player->lock.status);

                    CD_EventDispatch(self->server, "Player.process", player);

                    pthread_rwlock_wrlock(&player->lock.pending);
                    pthread_rwlock_wrlock(&player->lock.status);
                    player->status = CDPlayerIdle;
                } break;
            }

            PLAYER_JOB_DONE: {
                pthread_rwlock_rdlock(&player->lock.disconnecting);
                if (player->disconnecting) {
                    CD_DestroyJob(self->job);

                    if (player->username) {
                        CD_AddJob(player->server->workers, CD_CreateJob(
                            CDServerBroadcastJob, CD_CreateStringFromFormat(
                                "Player %s has left the game.", CD_StringContent(player->username)
                            )
                        ));

                        CD_HashDelete(player->server->players, CD_StringContent(player->username));
                    }

                    CD_MapDelete(player->server->entities, player->entity.id);

                    CD_DestroyPlayer(player);

                    continue;
                }
                pthread_rwlock_unlock(&player->lock.disconnecting);

                if (!player->pending && self->job) {
                    CD_DestroyJob(self->job);
                }

                self->job = NULL;

                pthread_rwlock_unlock(&player->lock.pending);
                pthread_rwlock_unlock(&player->lock.status);

                continue;
            }

            PLAYER_JOB_ERROR: {
                self->job = NULL;

                pthread_rwlock_unlock(&player->lock.pending);
                pthread_rwlock_unlock(&player->lock.status);

                continue;
            }
        }
        else if (CD_JOB_IS_SERVER(self->job)) {
            switch (self->job->type) {
                case CDServerBroadcastJob: {
                    CDPacketChat pkt;
                    pkt.response.message = self->job->data;

                    CDPacket response = { CDResponse, CDChat, (CDPointer) &pkt };

                    CD_HASH_FOREACH(self->server->players, it) {
                        CDPlayer* player = (CDPlayer*) CD_HashIteratorValue(self->server->players, it);

                        pthread_rwlock_rdlock(&player->lock.disconnecting);
                        if (!player->disconnecting) {
                            CD_PlayerSendPacket(player, &response);
                        }
                        pthread_rwlock_unlock(&player->lock.disconnecting);
                    }

                    CD_DestroyString(self->job->data);
                } break;
            }

            SERVER_JOB_DONE: {
                CD_DestroyJob(self->job);
                self->job = NULL;

                continue;
            }

            SERVER_JOB_ERROR: {
                CD_DestroyJob(self->job);
                self->job = NULL;

                continue;
            }

        }
        else {
            SERR(self->server, "unknown job (%d)", self->job->type);

            CD_DestroyJob(self->job);

            self->job = NULL;
        }
    }

    return NULL;
}
