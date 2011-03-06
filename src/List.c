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
#include <craftd/List.h>

CDList*
CD_CreateList (void)
{
    CDList* self = CD_malloc(sizeof(CDList));

    assert(self);

    self->raw     = kl_init(cdList);
    self->changed = false;
    self->length  = 0;

    assert(self->raw);
    assert(pthread_rwlock_init(&self->lock, NULL) != 0);

    return self;
}

CDList*
CD_CloneList (CDList* self)
{
    CDList* cloned = CD_CreateList();

    assert(self);

    CD_LIST_FOREACH(self, it) {
        CD_ListPush(cloned, CD_ListIteratorValue(it));
    }

    return cloned;
}

void
CD_DestroyList (CDList* self)
{
    assert(self);

    kl_destroy(cdList, self->raw);

    pthread_rwlock_destroy(&self->lock);

    CD_free(self);
}

CDListIterator
CD_ListBegin (CDList* self)
{
    CDListIterator it;

    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    it.raw    = kl_begin(self->raw);
    it.parent = self;
    pthread_rwlock_unlock(&self->lock);

    return it;
}

CDListIterator
CD_ListEnd (CDList* self)
{
    CDListIterator it;

    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    it.raw    = kl_end(self->raw);
    it.parent = self;
    pthread_rwlock_unlock(&self->lock);

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
    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    if (self->changed) {
        size_t         result = 0;
        CDListIterator it;

        for (it.raw = kl_begin(self->raw); it.raw != kl_end(self->raw); it.raw = kl_next(it.raw)) {
            result++;
        }

        self->length  = result;
        self->changed = false;
    }
    pthread_rwlock_unlock(&self->lock);

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
    assert(self);

    pthread_rwlock_wrlock(&self->lock);
    *kl_pushp(cdList, self->raw) = data;
    self->changed                = true;
    pthread_rwlock_unlock(&self->lock);

    return self;
}

CDPointer
CD_ListShift (CDList* self)
{
    CDPointer result = CDNull;

    assert(self);

    pthread_rwlock_wrlock(&self->lock);
    kl_shift(cdList, self->raw, &result);
    self->changed = true;
    pthread_rwlock_unlock(&self->lock);

    return result;
}

CDPointer
CD_ListFirst (CDList* self)
{
    CDPointer result = CDNull;

    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    result = kl_val(kl_begin(self->raw));
    pthread_rwlock_unlock(&self->lock);

    return result;
}

CDPointer
CD_ListLast (CDList* self)
{
    CDPointer result = CDNull;

    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    result = kl_val(kl_begin(self->raw));
    pthread_rwlock_unlock(&self->lock);

    return result;
}

// FIXME: This looks like it can go wrong, maybe set wrlock and use backend stuff
CDPointer
CD_ListDelete (CDList* self, CDPointer data)
{
    CDPointer      result = CDNull;
    CDListIterator del;

    assert(self);

    CD_LIST_FOREACH(self, it) {
        if (CD_ListIteratorValue(del = CD_ListNext(it)) == data) {
            result       = CD_ListIteratorValue(del);
            it.raw->next = CD_ListNext(del).raw;
            kmp_free(cdList, self->raw->mp, del.raw);

            break;
        }
    }

    self->changed = true;

    return result;
}

// FIXME: same as above
CDPointer
CD_ListDeleteAll (CDList* self, CDPointer data)
{
    CDPointer      result = CDNull;
    CDListIterator del;

    assert(self);

    CD_LIST_FOREACH(self, it) {
        if (CD_ListNext(it).raw && CD_ListIteratorValue(del = CD_ListNext(it)) == data) {
            result       = CD_ListIteratorValue(del);
            it.raw->next = del.raw->next;
            kmp_free(cdList, self->raw->mp, del.raw);

            self->changed = true;
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

    assert(self);
    assert(result);

    while (CD_ListLength(self) > 0) {
        result[i++] = CD_ListDeleteAll(self, CD_ListFirst(self));
    }

    result[i] = CDNull;

    return result;
}

bool
CD_ListStartIterating (CDList* self)
{
    assert(self);

    pthread_rwlock_rdlock(&self->lock);

    return true;
}

bool
CD_ListStopIterating (CDList* self, bool stop)
{
    assert(self);

    if (!stop) {
        pthread_rwlock_unlock(&self->lock);
    }

    return stop;
}
