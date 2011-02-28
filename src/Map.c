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

    if (!self) {
        return NULL;
    }

    self->map = kh_init(cdMap);

    pthread_rwlock_init(&self->lock, NULL);

    return self;
}

CDMap*
CD_CloneMap (CDMap* self)
{
    CDMap* cloned = CD_CreateMap();

    CD_MAP_FOREACH(self, it) {
        CD_MapSet(cloned, CD_MapIteratorKey(self, it), CD_MapIteratorValue(self, it));
    }

    return cloned;
}

void
CD_DestroyMap (CDMap* self)
{
    kh_destroy(cdMap, self->map);

    pthread_rwlock_destroy(&self->lock);

    CD_free(self);
}

CDMapIterator
CD_MapBegin (CDMap* self)
{
    CDMapIterator it;

    pthread_rwlock_rdlock(&self->lock);
    it = kh_end(self->map);

    if (!kh_exist(self->map, it)) {
        it = CD_MapNext(self, it);
    }
    pthread_rwlock_unlock(&self->lock);

    return it;
}

CDMapIterator
CD_MapEnd (CDMap* self)
{
    CDMapIterator it;

    pthread_rwlock_rdlock(&self->lock);
    it = kh_begin(self->map) - 1;
    pthread_rwlock_unlock(&self->lock);

    return it;
}

CDMapIterator
CD_MapNext (CDMap* self, CDMapIterator iterator)
{
    iterator--;

    pthread_rwlock_rdlock(&self->lock);
    for (; iterator != kh_begin(self->map) && !kh_exist(self->map, iterator); iterator--) {
        continue;
    }
    pthread_rwlock_unlock(&self->lock);

    if (!kh_exist(self->map, iterator)) {
        iterator = CD_MapEnd(self);
    }

    return iterator;
}

CDMapIterator
CD_MapPrevious (CDMap* self, CDMapIterator iterator)
{
    iterator++;

    pthread_rwlock_rdlock(&self->lock);
    for (; iterator != CD_MapBegin(self) && !CD_MapIteratorValid(self, iterator); iterator++) {
        continue;
    }
    pthread_rwlock_unlock(&self->lock);

    if (!kh_exist(self->map, iterator)) {
        iterator = CD_MapBegin(self);
    }

    return iterator;
}

size_t
CD_MapLength (CDMap* self)
{
    size_t result;

    pthread_rwlock_rdlock(&self->lock);
    result = kh_size(self->map);
    pthread_rwlock_unlock(&self->lock);

    return result;
}

int
CD_MapIteratorKey (CDMap* self, CDMapIterator iterator)
{
    int result = 0;

    pthread_rwlock_rdlock(&self->lock);
    result = kh_key(self->map, iterator);
    pthread_rwlock_unlock(&self->lock);

    return result;
}

CDPointer
CD_MapIteratorValue (CDMap* self, CDMapIterator iterator)
{
    CDPointer result = (CDPointer) NULL;

    pthread_rwlock_rdlock(&self->lock);
    result = kh_value(self->map, iterator);
    pthread_rwlock_unlock(&self->lock);

    return result;
}

bool
CD_MapIteratorValid (CDMap* self, CDMapIterator iterator)
{
    bool result = false;

    pthread_rwlock_rdlock(&self->lock);
    result = kh_exist(self->map, iterator);
    pthread_rwlock_unlock(&self->lock);

    return result;
}

CDPointer
CD_MapGet (CDMap* self, int id)
{
    CDPointer result = (CDPointer) NULL;
    khiter_t  it;

    pthread_rwlock_rdlock(&self->lock);
    it = kh_get(cdMap, self->map, id);

    if (it != kh_end(self->map) && kh_exist(self->map, it)) {
        result = kh_value(self->map, it);
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

    pthread_rwlock_wrlock(&self->lock);
    it                      = kh_put(cdMap, self->map, id, &ret);
    old                     = kh_value(self->map, it);
    kh_value(self->map, it) = data;
    pthread_rwlock_unlock(&self->lock);

    return old;
}

CDPointer
CD_MapDelete (CDMap* self, int id)
{
    CDPointer old = (CDPointer) NULL;
    khiter_t  it;

    pthread_rwlock_rdlock(&self->lock);
    it = kh_get(cdMap, self->map, id);

    if (it != kh_end(self->map) && kh_exist(self->map, it)) {
        old = kh_value(self->map, it);
    }

    kh_del(cdMap, self->map, it);
    pthread_rwlock_unlock(&self->lock);

    return old;
}

CDPointer
CD_MapFirst (CDMap* self)
{
    return CD_MapIteratorValue(self, CD_MapBegin(self));
}

CDPointer
CD_MapLast (CDMap* self)
{
    return CD_MapIteratorValue(self, CD_MapPrevious(self, CD_MapEnd(self)));
}

CDPointer*
CD_MapClear (CDMap* self)
{
    CDPointer* result = (CDPointer*) NULL;
    size_t     i      = 0;
    khiter_t   it;

    pthread_rwlock_wrlock(&self->lock);

    for (it = kh_begin(self->map); it != kh_end(self->map); it++) {
        if (kh_exist(self->map, it)) {
            i++;

            result        = CD_realloc(result, sizeof(CDPointer) * (i + 1));
            result[i - 1] = kh_value(self->map, it);
        }
    }

    result[i] = (CDPointer) NULL;

    kh_clear(cdMap, self->map);
    pthread_rwlock_unlock(&self->lock);

    return result;
}
