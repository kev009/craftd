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

#include <craftd/common.h>
#include <craftd/Hash.h>

CDHash*
CD_CreateHash (void)
{
    CDHash* self = CD_malloc(sizeof(CDHash));

    assert(self);

    self->raw = kh_init(cdHash);

    assert(self->raw);
    assert(pthread_rwlock_init(&self->lock, NULL) == 0);

    return self;
}

CDHash*
CD_CloneHash (CDHash* self)
{
    CDHash* cloned = CD_CreateHash();

    assert(self);

    CD_HASH_FOREACH(self, it) {
        CD_HashPut(cloned, CD_HashIteratorKey(it), CD_HashIteratorValue(it));
    }

    return cloned;
}

void
CD_DestroyHash (CDHash* self)
{
    assert(self);

    kh_destroy(cdHash, self->raw);

    pthread_rwlock_destroy(&self->lock);

    CD_free(self);
}

size_t
CD_HashLength (CDHash* self)
{
    size_t result;

    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    result = kh_size(self->raw);
    pthread_rwlock_unlock(&self->lock);

    return result;
}

CDHashIterator
CD_HashBegin (CDHash* self)
{
    CDHashIterator it;

    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    it.raw    = kh_end(self->raw);
    it.parent = self;

    if (!kh_exist(self->raw, it.raw)) {
        it = CD_HashNext(it);
    }
    pthread_rwlock_unlock(&self->lock);

    return it;
}

CDHashIterator
CD_HashEnd (CDHash* self)
{
    CDHashIterator it;

    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    it.raw    = kh_begin(self->raw) - 1;
    it.parent = self;
    pthread_rwlock_unlock(&self->lock);

    return it;
}

CDHashIterator
CD_HashNext (CDHashIterator it)
{
    if (it.raw == kh_begin(it.parent->raw)) {
        return CD_HashEnd(it.parent);
    }

    it.raw--;

    pthread_rwlock_rdlock(&it.parent->lock);
    for (; it.raw != kh_begin(it.parent->raw) && !kh_exist(it.parent->raw, it.raw); it.raw--) {
        continue;
    }

    if (!kh_exist(it.parent->raw, it.raw)) {
        it = CD_HashEnd(it.parent);
    }
    pthread_rwlock_unlock(&it.parent->lock);

    return it;
}

CDHashIterator
CD_HashPrevious (CDHashIterator it)
{
    if (it.raw == kh_end(it.parent->raw)) {
        return CD_HashBegin(it.parent);
    }

    it.raw++;

    pthread_rwlock_rdlock(&it.parent->lock);
    for (; it.raw != kh_end(it.parent->raw) && !kh_exist(it.parent->raw, it.raw); it.raw++) {
        continue;
    }

    if (!kh_exist(it.parent->raw, it.raw)) {
        it = CD_HashBegin(it.parent);
    }
    pthread_rwlock_unlock(&it.parent->lock);

    return it;
}

bool
CD_HashIteratorIsEqual (CDHashIterator a, CDHashIterator b)
{
    return a.raw == b.raw && a.parent == b.parent;
}

const char*
CD_HashIteratorKey (CDHashIterator it)
{
    const char* result = NULL;

    pthread_rwlock_rdlock(&it.parent->lock);
    result = kh_key(it.parent->raw, it.raw);
    pthread_rwlock_unlock(&it.parent->lock);

    return result;
}

CDPointer
CD_HashIteratorValue (CDHashIterator it)
{
    CDPointer result = CDNull;

    pthread_rwlock_rdlock(&it.parent->lock);
    result = kh_value(it.parent->raw, it.raw);
    pthread_rwlock_unlock(&it.parent->lock);

    return result;
}

bool
CD_HashIteratorValid (CDHashIterator it)
{
    bool result = false;

    pthread_rwlock_rdlock(&it.parent->lock);
    result = kh_exist(it.parent->raw, it.raw);
    pthread_rwlock_unlock(&it.parent->lock);

    return result;
}

CDPointer
CD_HashGet (CDHash* self, const char* name)
{
    CDPointer result = (CDPointer) NULL;
    khiter_t  it;

    assert(self);
    assert(name);

    pthread_rwlock_rdlock(&self->lock);
    it = kh_get(cdHash, self->raw, name);

    if (it != kh_end(self->raw) && kh_exist(self->raw, it)) {
        result = kh_value(self->raw, it);
    }
    pthread_rwlock_unlock(&self->lock);

    return result;
}

CDPointer
CD_HashPut (CDHash* self, const char* name, CDPointer data)
{
    CDPointer old = (CDPointer) NULL;
    khiter_t  it;
    int       ret;

    assert(self);
    assert(name);

    pthread_rwlock_wrlock(&self->lock);
    it = kh_get(cdHash, self->raw, name);

    if (it != kh_end(self->raw) && kh_exist(self->raw, it)) {
        old = kh_value(self->raw, it);
    }
    else {
        it = kh_put(cdHash, self->raw, name, &ret);
    }

    kh_value(self->raw, it) = data;
    pthread_rwlock_unlock(&self->lock);

    return old;
}

CDPointer
CD_HashDelete (CDHash* self, const char* name)
{
    CDPointer old = (CDPointer) NULL;
    khiter_t  it;

    assert(self);
    assert(name);

    pthread_rwlock_wrlock(&self->lock);
    it = kh_get(cdHash, self->raw, name);

    if (it != kh_end(self->raw) && kh_exist(self->raw, it)) {
        old = kh_value(self->raw, it);
    }

    kh_del(cdHash, self->raw, it);
    pthread_rwlock_unlock(&self->lock);

    return old;
}

CDPointer
CD_HashFirst (CDHash* self)
{
    return CD_HashIteratorValue(CD_HashBegin(self));
}

CDPointer
CD_HashLast (CDHash* self)
{
    return CD_HashIteratorValue(CD_HashPrevious(CD_HashEnd(self)));
}

CDPointer*
CD_HashClear (CDHash* self)
{
    CDPointer* result = CD_malloc(sizeof(CDPointer) * (CD_HashLength(self) + 1));
    size_t     i      = 0;
    khiter_t   it;

    assert(self);
    assert(result);

    pthread_rwlock_wrlock(&self->lock);
    for (it = kh_begin(self->raw); it != kh_end(self->raw); it++) {
        if (kh_exist(self->raw, it)) {
            result[i++] = kh_value(self->raw, it);
        }
    }

    result[i] = CDNull;

    kh_clear(cdHash, self->raw);
    pthread_rwlock_unlock(&self->lock);

    return result;
}

bool
CD_HashStartIterating (CDHash* self)
{
    assert(self);

    pthread_rwlock_rdlock(&self->lock);

    return true;
}

bool
CD_HashStopIterating (CDHash* self, bool stop)
{
    assert(self);

    if (!stop) {
        pthread_rwlock_unlock(&self->lock);
    }

    return stop;
}
