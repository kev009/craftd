#ifndef CRAFTD_CRAFTD_H
#define CRAFTD_CRAFTD_H

/*
 * Copyright (c) 2010 Kevin M. Bowling, <kevin.bowling@kev009.com>, USA
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

#include <pthread.h>
#include <sys/queue.h>

#include "util.h"

/* Public data */

/**
 * Declare the player list as a singly linked list 
 */
pthread_rwlock_t PL_rwlock;
int PL_count;
SLIST_HEAD(PL_slisthead, PL_entry) PL_head;


/**
 * This is a player entry in the singly linked list
 */

struct PL_entry
{
    pthread_rwlock_t rwlock;
    evutil_socket_t fd;
    char ip[128];
    uint32_t clientver;
    uint64_t mapseed;
    uint8_t dimension;
    mcstring_t username;
    SLIST_ENTRY(PL_entry) PL_entries; // Pointer to the next player entry
};

/* Public methods */
void craftd_version(const char *progname);

/* Private */
//static void do_read(evutil_socket_t fd, short events, void *arg);
//static void do_write(evutil_socket_t fd, short evets, void *arg);

#endif
