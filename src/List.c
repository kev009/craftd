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

#include <craftd/List.h>

CDList*
CD_CreateList (void)
{
    CDList* self = CD_malloc(sizeof(CDList));

    if (!self) {
        return NULL;
    }

    self->raw     = kl_init(cdList);
    self->changed = false;
    self->length  = 0;

    pthread_rwlock_init(&self->lock.rw, NULL);
    pthread_mutex_init(&self->lock.iterating, NULL);
    pthread_mutex_init(&self->lock.length, NULL);

    return self;
}

CDList*
CD_CloneList (CDList* self)
{
    CDList* cloned = CD_CreateList();

    CD_LIST_FOREACH(self, it) {
        CD_ListPush(cloned, CD_ListIteratorValue(it));
    }

    return cloned;
}

void
CD_DestroyList (CDList* self)
{
    pthread_mutex_unlock(&self->lock.iterating);

    kl_destroy(cdList, self->raw);

    pthread_mutex_destroy(&self->lock.iterating);
    pthread_rwlock_destroy(&self->lock.rw);

    CD_free(self);
}

CDListIterator
CD_ListBegin (CDList* self)
{
    CDListIterator it;

    pthread_rwlock_rdlock(&self->lock.rw);

    it.raw    = kl_begin(self->raw);
    it.parent = self;

    pthread_rwlock_unlock(&self->lock.rw);

    return it;
}

CDListIterator
CD_ListEnd (CDList* self)
{
    CDListIterator it;

    pthread_rwlock_rdlock(&self->lock.rw);

    it.raw    = kl_end(self->raw);
    it.parent = self;

    pthread_rwlock_unlock(&self->lock.rw);

    return it;
}

CDListIterator
CD_ListNext (CDListIterator it)
{
    if (it.raw != NULL) {
        it.raw = kl_next(it.raw);
    }

    return it;
}

size_t
CD_ListLength (CDList* self)
{
    pthread_mutex_lock(&self->lock.length);
    if (self->changed) {
        size_t         result = 0;
        CDListIterator it;

        for (it.raw = kl_begin(self->raw); it.raw != kl_end(self->raw); it.raw = kl_next(it.raw)) {
            result++;
        }

        self->length  = result;
        self->changed = false;
    }
    pthread_mutex_unlock(&self->lock.length);

    return self->length;
}

bool
CD_ListIteratorIsEqual (CDListIterator a, CDListIterator b)
{
    return a.raw == b.raw && a.parent == b.parent;
}


CDPointer
CD_ListIteratorValue (CDListIterator it)
{
    return kl_val(it.raw);
}

CDList*
CD_ListPush (CDList* self, CDPointer data)
{
    pthread_rwlock_wrlock(&self->lock.rw);
    pthread_mutex_lock(&self->lock.iterating);
    pthread_mutex_lock(&self->lock.length);

    *kl_pushp(cdList, self->raw) = data;

    self->changed = true;

    pthread_mutex_unlock(&self->lock.length);
    pthread_mutex_unlock(&self->lock.iterating);
    pthread_rwlock_unlock(&self->lock.rw);

    return self;
}

CDPointer
CD_ListShift (CDList* self)
{
    CDPointer result = CDNull;

    pthread_rwlock_wrlock(&self->lock.rw);
    pthread_mutex_lock(&self->lock.iterating);
    pthread_mutex_lock(&self->lock.length);

    kl_shift(cdList, self->raw, &result);

    self->changed = true;

    pthread_mutex_unlock(&self->lock.length);
    pthread_mutex_unlock(&self->lock.iterating);
    pthread_rwlock_unlock(&self->lock.rw);

    return result;
}

CDPointer
CD_ListFirst (CDList* self)
{
    CDPointer result = CDNull;

    pthread_rwlock_rdlock(&self->lock.rw);
    result = kl_val(kl_begin(self->raw));
    pthread_rwlock_unlock(&self->lock.rw);

    return result;
}

CDPointer
CD_ListLast (CDList* self)
{
    CDPointer result = CDNull;

    pthread_rwlock_rdlock(&self->lock.rw);
    result = kl_val(kl_begin(self->raw));
    pthread_rwlock_unlock(&self->lock.rw);

    return result;
}

// FIXME: This looks like it can go wrong, maybe set wrlock and use backend stuff
CDPointer
CD_ListDelete (CDList* self, CDPointer data)
{
    CDPointer      result = CDNull;
    CDListIterator del;

    pthread_mutex_lock(&self->lock.length);

    CD_LIST_FOREACH(self, it) {
        if (CD_ListIteratorValue(del = CD_ListNext(it)) == data) {
            result       = CD_ListIteratorValue(del);
            it.raw->next = CD_ListNext(del).raw;
            kmp_free(cdList, self->raw->mp, del.raw);

            break;
        }
    }

    self->changed = true;
    pthread_mutex_unlock(&self->lock.length);

    return result;
}

// FIXME: same as above
CDPointer
CD_ListDeleteAll (CDList* self, CDPointer data)
{
    CDPointer      result = CDNull;
    CDListIterator del;

    CD_LIST_FOREACH(self, it) {
        if (CD_ListNext(it).raw && CD_ListIteratorValue(del = CD_ListNext(it)) == data) {
            pthread_mutex_lock(&self->lock.length);

            result       = CD_ListIteratorValue(del);
            it.raw->next = del.raw->next;
            kmp_free(cdList, self->raw->mp, del.raw);

            self->changed = true;
            pthread_mutex_unlock(&self->lock.length);
        }
    }

    return result;
}

// FIXME: same as above
CDPointer*
CD_ListClear (CDList* self)
{
    CDPointer* result = (CDPointer*) CD_malloc(sizeof(CDPointer) * (CD_ListLength(self) + 1));
    size_t     i      = 0;

    while (CD_ListLength(self) > 0) {
        result[i++] = CD_ListDeleteAll(self, CD_ListFirst(self));
    }

    result[i] = CDNull;

    return result;
}

bool
CD_ListStartIterating (CDList* self)
{
    pthread_mutex_lock(&self->lock.iterating);

    return true;
}

bool
CD_ListStopIterating (CDList* self, bool stop)
{
    if (!stop) {
        pthread_mutex_unlock(&self->lock.iterating);
    }

    return stop;
}
