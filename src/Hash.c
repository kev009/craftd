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

#include "Hash.h"

CDHash*
CD_CreateHash (void)
{
    CDHash* self = CD_malloc(sizeof(CDHash));

    if (!self) {
        ERR("could not allocate a Hash object");
        return NULL;
    }

    self->hash = kh_init(cdHash);

    pthread_rwlock_init(&self->lock, NULL);

    return self;
}

void
CD_DestroyHash (CDHash* self)
{
    kh_destroy(cdHash, self->hash);

    pthread_rwlock_destroy(&self->lock);

    CD_free(self);
}

CDHashIterator
CD_HashBegin (CDHash* self)
{
    CDHashIterator it;

    pthread_rwlock_rdlock(&self->lock);
    it = kh_begin(self->hash);
    pthread_rwlock_unlock(&self->lock);

    return it;
}

CDHashIterator
CD_HashEnd (CDHash* self)
{
    CDHashIterator it;

    pthread_rwlock_rdlock(&self->lock);
    it = kh_end(self->hash);
    pthread_rwlock_unlock(&self->lock);

    return it;
}

size_t
CD_HashLength (CDHash* self)
{
    size_t result;

    pthread_rwlock_rdlock(&self->lock);
    result = kh_size(self->hash);
    pthread_rwlock_unlock(&self->lock);

    return result;
}

const char*
CD_HashIteratorKey (CDHashIterator iterator)
{
    const char* result = NULL;

    pthread_rwlock_rdlock(&self->lock);
    result = kh_key(self->hash, iterator);
    pthread_rwlock_unlock(&self->lock);

    return result;
}

void*
CD_HashIteratorValue (CDHashIterator iterator)
{
    void* result = NULL;

    pthread_rwlock_rdlock(&self->lock);
    result = kh_value(self->hash, iterator);
    pthread_rwlock_unlock(&self->lock);

    return result;
}

void*
CD_HashGet (CDHash* self, const char* name)
{
    void*    result = NULL;
    khiter_t it;

    pthread_rwlock_rdlock(&self->lock);
    it = kh_get(cdHash, self->hash, name);

    if (it != kh_end(self->hash) && kh_exist(self->hash, it)) {
        result = kh_value(self->hash, it)
    }
    pthread_rwlock_unlock(&self->lock);

    return result;
}

void*
CD_HashSet (CDHash* self, const char* name, void* data)
{
    void*    old = NULL;
    khiter_t it;
    int      ret;

    pthread_rwlock_wrlock(&self->lock);
    it                       = kh_put(cdHash, self->hash, name, &ret);
    old                      = kh_value(self->hash, it);
    kh_value(self->hash, it) = data;
    pthread_rwlock_unlock(&self->lock);

    return old;
}

void*
CD_HashDelete (CDHash* self, const char* name)
{
    void*    old = NULL;
    khiter_t it;

    pthread_rwlock_rdlock(&self->lock);
    it = kh_get(cdHash, self->hash, name);

    if (it != kh_end(self->hash) && kh_exist(self->hash, it)) {
        old = kh_value(self->hash, it)
    }

    kh_del(cdHash, self->hash, it);
    pthread_rwlock_unlock(&self->lock);

    return old;
}

void**
CD_HashClear (CDHash* self)
{
    void**   result = NULL;
    size_t   i      = 0;
    khiter_t it;

    pthread_rwlock_wrlock(&self->lock);

    for (it = kh_begin(self->hash); it != kh_end(self->hash); k++) {
        if (kh_exist(self->hash, it)) { i++;
            result        = CD_realloc(result, sizeof(void*) * (i + 1));
            result[i - 1] = kh_value(self->hash, it);
        }
    }

    result[i] = NULL;

    kh_clear(cdHash, self->hash);
    pthread_rwlock_unlock(&self->lock);

    return result;
}
