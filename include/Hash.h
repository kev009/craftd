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

#ifndef CRAFTD_HASH_H
#define CRAFTD_HASH_H

#include <stdbool.h>
#include "khash.h"

KHASH_MAP_INIT_STR(cdHash, void*);

typedef struct _CDHash {
    khash_t(cdHash)* hash;

    pthread_rwlock_t lock;
} CDHash;

typedef khiter_t CDHashIterator;

CDHash* CD_CreateHash (void);

CDHash* CD_CloneHash (CDHash* self);

void CD_DestroyHash (CDHash* self);

CDHashIterator CD_HashBegin (CDHash* self);

CDHashIterator CD_HashEnd (CDHash* self);

CDHashIterator CD_HashNext (CDHash* self, CDHashIterator iterator);

CDHashIterator CD_HashPrevious (CDHash* self, CDHashIterator iterator);

size_t CD_HashLength (CDHash* self);

const char* CD_HashIteratorKey (CDHash* self, CDHashIterator iterator);

void* CD_HashIteratorValue (CDHash* self, CDHashIterator iterator);

bool CD_HashIteratorValid (CDHash* self, CDHashIterator iterator);

void* CD_HashGet (CDHash* self, const char* name);

void* CD_HashSet (CDHash* self, const char* name, void* data);

void* CD_HashDelete (CDHash* self, const char* name);

void* CD_HashFirst (CDHash* self);

void* CD_HashLast (CDHash* self);

void** CD_HashClear (CDHash* self);

#define CD_HASH_FOREACH(hash, it) \
    if (hash) for (CDHashIterator it = CD_HashBegin(hash), end = CD_HashEnd(hash); it != end; it = CD_HashNext(hash, it))

#endif
