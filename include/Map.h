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

#ifndef CRAFTD_MAP_H
#define CRAFTD_MAP_H

#include <stdbool.h>
#include "khash.h"

KHASH_MAP_INIT_INT(cdMap, void*);

typedef struct _CDMap {
    khash_t(cdMap)* map;

    pthread_rwlock_t lock;
} CDMap;

typedef khiter_t CDMapIterator;

CDMap* CD_CreateMap (void);

CDMap* CD_CloneMap (CDMap* self);

void CD_DestroyMap (CDMap* self);

CDMapIterator CD_MapBegin (CDMap* self);

CDMapIterator CD_MapEnd (CDMap* self);

CDHashIterator CD_HashNext (CDHash* self, CDHashIterator iterator);

CDHashIterator CD_HashPrevious (CDHash* self, CDHashIterator iterator);

size_t CD_MapLength (CDMap* self);

int CD_MapIteratorKey (CDMapIterator iterator);

void* CD_MapIteratorValue (CDMapIterator iterator);

bool CD_MapIteratorValid (CDMap* self, CDHashIterator iterator);

void* CD_MapGet (CDMap* self, int id);

void* CD_MapSet (CDMap* self, int id, void* data);

void* CD_MapDelete (CDMap* self, int id);

void* CD_MapFirst (CDMap* self);

void* CD_MapLast (CDMap* self);

void** CD_MapClear (CDMap* self);

#define CD_MAP_FOREACH(map, it) \
    for (CDMapIterator it = CD_MapBegin(hash), end = CD_MapEnd(hash); it != end; it = CD_MapNext(hash, it))

#endif
