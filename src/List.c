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

static
int8_t
cd_ListCompare (CDPointer a, CDPointer b)
{
    if (a > b) {
        return 1;
    }
    else if (a < b) {
        return -1;
    }
    else {
        return 0;
    }
}

static
CDListItem*
cd_ListCreateItem (CDPointer data)
{
    CDListItem* item = (CDListItem*) CD_malloc(sizeof(CDListItem));

    assert(item);

    item->next  = NULL;
    item->prev  = NULL;
    item->value = data;

    return item;
}

static
void
cd_ListInsertSorted (CDList* self, CDPointer data, CDListCompareCallback callback)
{
    CDListItem* item = cd_ListCreateItem(data);

    assert(self);

    if (self->head == NULL) {
        self->head = item;
        self->tail = item;
    }
    else if (callback(data, self->head->value) <= 0) {
        item->next       = self->head;
        self->head->prev = item;
        self->head       = item;
    }
    else {
        CDListItem* walker = self->head;

        while (walker->next) {
            if (callback(data, walker->next->value) <= 0) {
                item->next = walker->next;
                item->prev = walker;
                walker->next->prev = item;
                walker->next = item;
                walker = NULL;
                break;
            }

            walker = walker->next;
        }

        if (walker != NULL) {
            item->prev = walker;
            walker->next = item;
        }
    }

    self->changed = true;
}

static
void
cd_ListSortInsert (CDList* self, CDListCompareCallback callback)
{
    CDListItem* walker = self->head;

    self->head = NULL;
    self->tail = NULL;

    while (walker) {
        CDListItem* toDestroy = walker;

        cd_ListInsertSorted(self, walker->value, callback);

        walker = walker->next;

        CD_free(toDestroy);
    }
}

static
CDPointer
cd_ListDelete (CDList* self, CDPointer data, CDListCompareCallback callback)
{
    CDPointer result = CDNull;

    assert(self);

    if (!self->head) {
        return CDNull;
    }

    if (callback(data, self->head->value) == 0) {
        CDListItem* item = self->head;
        result           = item->value;
        self->head       = item->next;

        if (self->head) {
            self->head->prev = NULL;
        }

        self->changed = true;

        CD_free(item);
    }
    else {
        CDListItem *item = self->head;

        while (item->next) {
            if (callback(data, item->next->value) == 0) {
                            result     = item->value;
                CDListItem* toDelete   = item->next;
                            item->next = toDelete->next;

                if (item->next) {
                    item->next->prev = item;
                }

                CD_free(toDelete);

                self->changed = true;

                break;
            }
            else {
                item = item->next;
            }
        }
    }

    return result;
}

CDList*
CD_CreateList (void)
{
    CDList* self = CD_malloc(sizeof(CDList));

    assert(self);

    self->head    = NULL;
    self->tail    = NULL;

    self->changed = false;
    self->length  = 0;

    assert(pthread_rwlock_init(&self->lock, NULL) == 0);

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

    /* Is this neccesary, only when somebody is still
       reading/writing but that should already be
       stopped before you call this. */
    pthread_rwlock_wrlock(&self->lock);

    while (self->head) {
        CDListItem* next = self->head->next;
        CD_free(self->head);
        self->head = next;
    }

    self->tail = self->head = NULL;

    pthread_rwlock_unlock(&self->lock);
    pthread_rwlock_destroy(&self->lock);

    CD_free(self);
}

CDListIterator
CD_ListBegin (CDList* self)
{
    CDListIterator it;

    assert(self);

    it.raw    = self->head;
    it.parent = self;

    return it;
}

CDListIterator
CD_ListEnd (CDList* self)
{
    CDListIterator it;

    assert(self);

    it.raw    = CDNull;
    it.parent = self;

    return it;
}

CDListIterator
CD_ListNext (CDListIterator it)
{
    if (it.raw != NULL) {
        it.raw = it.raw->next;
    }

    return it;
}

CDListIterator
CD_ListPrevious (CDListIterator it)
{
    if (it.raw != NULL) {
        it.raw = it.raw->prev;
    }

    return it;
}

size_t
CD_ListLength (CDList* self)
{
    assert(self);

    pthread_rwlock_rdlock(&self->lock);
    if (self->changed) {
        size_t      result = 0;
        CDListItem* runner = self->head;

        while (runner) {
            result++;
            runner = runner->next;
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
    if (it.raw == NULL) {
      return CDNull;
    }

    return it.raw->value;
}

CDList*
CD_ListPush (CDList* self, CDPointer data)
{
    assert(self);
    assert(data);

    pthread_rwlock_wrlock(&self->lock);

    CDListItem* item = (CDListItem *) CD_malloc(sizeof(CDListItem));

    assert(item);

    item->next  = NULL;
    item->prev  = NULL;
    item->value = data;

    if (self->head == NULL) {
        self->head = item;
        self->tail = item;
    }
    else {
        item->prev       = self->tail;
        self->tail->next = item;
        self->tail       = item;
    }

    self->changed  = true;

    pthread_rwlock_unlock(&self->lock);

    return self;
}

CDPointer
CD_ListShift (CDList* self)
{
    CDPointer result = CDNull;

    assert(self);

    pthread_rwlock_wrlock(&self->lock);

    if (self->head == NULL) {
        result = CDNull;
    }
    else {
        result = cd_ListDelete(self, self->head->value, cd_ListCompare);
    }
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

    if (self->head) {
        result = self->head->value;
    }

    pthread_rwlock_unlock(&self->lock);

    return result;
}

CDPointer
CD_ListLast (CDList* self)
{
    CDPointer result = CDNull;

    assert(self);

    pthread_rwlock_rdlock(&self->lock);

    if (self->tail) {
      result = self->tail->value;
    }

    pthread_rwlock_unlock(&self->lock);

    return result;
}

CDList *
CD_ListSortedPush (CDList* self, CDPointer data, CDListCompareCallback callback)
{
    pthread_rwlock_wrlock(&self->lock);

    cd_ListInsertSorted(self, data, callback);

    pthread_rwlock_unlock(&self->lock);

    return self;
}

CDList *
CD_ListSort (CDList* self, CDListSortAlgorithm algorithm, CDListCompareCallback callback)
{
    pthread_rwlock_wrlock(&self->lock);

    switch (algorithm) {
        case CDSortInsert: {
            cd_ListSortInsert(self, callback);
        } break;
    }

    pthread_rwlock_unlock(&self->lock);

    return self;
}


CDPointer
CD_ListDelete (CDList* self, CDPointer data)
{
    CDPointer result = CDNull;

    assert(self);
    assert(data);

    pthread_rwlock_wrlock(&self->lock);

    result = cd_ListDelete(self, data, cd_ListCompare);

    pthread_rwlock_unlock(&self->lock);

    return result;
}

CDPointer
CD_ListDeleteIf (CDList* self, CDPointer data, CDListCompareCallback callback)
{
    CDPointer result = CDNull;

    assert(self);
    assert(data);

    pthread_rwlock_wrlock(&self->lock);

    result = cd_ListDelete(self, data, callback);

    pthread_rwlock_unlock(&self->lock);

    return result;
}


CDPointer
CD_ListDeleteAll (CDList* self, CDPointer data)
{
    CDPointer result = CDNull;

    pthread_rwlock_wrlock(&self->lock);

    result = cd_ListDelete(self, data, cd_ListCompare);

    if (result) {
        while (cd_ListDelete(self, data, cd_ListCompare) != CDNull ) {
            continue;
        }
    }

    pthread_rwlock_unlock(&self->lock);

    return result;
}

CDPointer
CD_ListDeleteAllIf (CDList* self, CDPointer data, CDListCompareCallback callback)
{
    CDPointer result = CDNull;

    pthread_rwlock_wrlock(&self->lock);

    result = cd_ListDelete(self, data, callback);

    if (result) {
        while (cd_ListDelete(self, data, callback) != CDNull ) {
            continue;
        }
    }

    pthread_rwlock_unlock(&self->lock);

    return result;
}


// TODO: This doesn't work as it should.
CDPointer*
CD_ListClear (CDList* self)
{
    CDPointer* result = (CDPointer*) CD_malloc(sizeof(CDPointer) * (CD_ListLength(self) + 1));
    size_t     i      = 0;

    assert(self);
    assert(result);

    pthread_rwlock_wrlock(&self->lock);

    while (self->head) {
        CDPointer value = result[i++] = self->head->value;

        while ((value = cd_ListDelete(self, value, cd_ListCompare)) != CDNull ) {
            continue;
        }
    }

    pthread_rwlock_unlock(&self->lock);

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
