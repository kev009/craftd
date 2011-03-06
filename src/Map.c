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

#include <craftd/Map.h>

CDMap*
CD_CreateMap (void)
{
    CDMap* self = CD_malloc(sizeof(CDMap));

    assert(self);

    self->raw = kh_init(cdMap);

    assert(self->raw);
    assert(pthread_rwlock_init(&self->lock, NULL) == 0);

    return self;
}

CDMap*
CD_CloneMap (CDMap* self)
{
    CDMap* cloned = CD_CreateMap();

    assert(self);

    CD_MAP_FOREACH(self, it) {
        CD_MapSet(cloned, CD_MapIteratorKey(it), CD_MapIteratorValue(it));
    }

    return cloned;
}

void
CD_DestroyMap (CDMap* self)
{
    assert(self);

    kh_destroy(cdMap, self->raw);

    pthread_rwlock_destroy(&self->lock);

    CD_free(self);
}

size_t
CD_MapLength (CDMap* self)
{
    size_t result;

    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    result = kh_size(self->raw);
    pthread_rwlock_unlock(&self->lock);

    return result;
}

CDMapIterator
CD_MapBegin (CDMap* self)
{
    CDMapIterator it;

    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    it.raw    = kh_end(self->raw);
    it.parent = self;

    if (!kh_exist(self->raw, it.raw)) {
        it = CD_MapNext(it);
    }
    pthread_rwlock_unlock(&self->lock);

    return it;
}

CDMapIterator
CD_MapEnd (CDMap* self)
{
    CDMapIterator it;

    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    it.raw    = kh_begin(self->raw) - 1;
    it.parent = self;
    pthread_rwlock_unlock(&self->lock);

    return it;
}

CDMapIterator
CD_MapNext (CDMapIterator it)
{
    if (it.raw == kh_begin(it.parent->raw)) {
        return CD_MapEnd(it.parent);
    }

    it.raw--;

    pthread_rwlock_rdlock(&it.parent->lock);
    for (; it.raw != kh_begin(it.parent->raw) && !kh_exist(it.parent->raw, it.raw); it.raw--) {
        continue;
    }

    if (!kh_exist(it.parent->raw, it.raw)) {
        it = CD_MapEnd(it.parent);
    }
    pthread_rwlock_unlock(&it.parent->lock);

    return it;
}

CDMapIterator
CD_MapPrevious (CDMapIterator it)
{
    if (it.raw == kh_end(it.parent->raw)) {
        return CD_MapBegin(it.parent);
    }

    it.raw++;

    pthread_rwlock_rdlock(&it.parent->lock);
    for (; it.raw != kh_end(it.parent->raw) && !kh_exist(it.parent->raw, it.raw); it.raw++) {
        continue;
    }

    if (!kh_exist(it.parent->raw, it.raw)) {
        it = CD_MapBegin(it.parent);
    }
    pthread_rwlock_unlock(&it.parent->lock);

    return it;
}

bool
CD_MapIteratorIsEqual (CDMapIterator a, CDMapIterator b)
{
    return a.raw == b.raw && a.parent == b.parent;
}

int
CD_MapIteratorKey (CDMapIterator it)
{
    int result = 0;

    pthread_rwlock_rdlock(&it.parent->lock);
    result = kh_key(it.parent->raw, it.raw);
    pthread_rwlock_unlock(&it.parent->lock);

    return result;
}

CDPointer
CD_MapIteratorValue (CDMapIterator it)
{
    CDPointer result = CDNull;

    pthread_rwlock_rdlock(&it.parent->lock);
    result = kh_value(it.parent->raw, it.raw);
    pthread_rwlock_unlock(&it.parent->lock);

    return result;
}

bool
CD_MapIteratorValid (CDMapIterator it)
{
    bool result = false;

    pthread_rwlock_rdlock(&it.parent->lock);
    result = kh_exist(it.parent->raw, it.raw);
    pthread_rwlock_unlock(&it.parent->lock);

    return result;
}

CDPointer
CD_MapGet (CDMap* self, int id)
{
    CDPointer result = (CDPointer) NULL;
    khiter_t  it;

    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    it = kh_get(cdMap, self->raw, id);

    if (it != kh_end(self->raw) && kh_exist(self->raw, it)) {
        result = kh_value(self->raw, it);
    }
    pthread_rwlock_unlock(&self->lock);

    return result;
}

CDPointer
CD_MapSet (CDMap* self, int id, CDPointer data)
{
    CDPointer old = (CDPointer) NULL;
    khiter_t  it;
    int       ret;

    assert(self);

    pthread_rwlock_wrlock(&self->lock);
    it = kh_get(cdMap, self->raw, id);

    if (it != kh_end(self->raw) && kh_exist(self->raw, it)) {
        old = kh_value(self->raw, it);
    }
    else {
        it = kh_put(cdMap, self->raw, id, &ret);
    }

    kh_value(self->raw, it) = data;
    pthread_rwlock_unlock(&self->lock);

    return old;
}

CDPointer
CD_MapDelete (CDMap* self, int id)
{
    CDPointer old = (CDPointer) NULL;
    khiter_t  it;

    assert(self);

    pthread_rwlock_wrlock(&self->lock);
    it = kh_get(cdMap, self->raw, id);

    if (it != kh_end(self->raw) && kh_exist(self->raw, it)) {
        old = kh_value(self->raw, it);
    }

    kh_del(cdMap, self->raw, it);
    pthread_rwlock_unlock(&self->lock);

    return old;
}

CDPointer
CD_MapFirst (CDMap* self)
{
    return CD_MapIteratorValue(CD_MapBegin(self));
}

CDPointer
CD_MapLast (CDMap* self)
{
    return CD_MapIteratorValue(CD_MapPrevious(CD_MapEnd(self)));
}

CDPointer*
CD_MapClear (CDMap* self)
{
    CDPointer* result = CD_malloc(sizeof(CDPointer) * (CD_MapLength(self) + 1));
    size_t     i      = 0;
    khiter_t   it;

    assert(self);

    pthread_rwlock_wrlock(&self->lock);
    for (it = kh_begin(self->raw); it != kh_end(self->raw); it++) {
        if (kh_exist(self->raw, it)) {
            result[i++] = kh_value(self->raw, it);
        }
    }

    result[i] = CDNull;

    kh_clear(cdMap, self->raw);
    pthread_rwlock_unlock(&self->lock);

    return result;
}

bool
CD_MapStartIterating (CDMap* self)
{
    assert(self);

    pthread_rwlock_rdlock(&self->lock);

    return true;
}

bool
CD_MapStopIterating (CDMap* self, bool stop)
{
    assert(self);

    if (!stop) {
        pthread_rwlock_unlock(&self->lock);
    }

    return stop;
}
