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

#ifndef CRAFTD_LIST_H
#define CRAFTD_LIST_H

#include <craftd/common.h>

typedef struct _CDListItem {
    struct _CDListItem* next;
    struct _CDListItem* prev;

    CDPointer value;
} CDListItem;

/**
 * The List class.
 */
typedef struct _CDList {
    CDListItem* head;
    CDListItem* tail;

    size_t length;
    bool   changed;

    pthread_rwlock_t lock;
} CDList;

typedef struct _CDListIterator {
    CDListItem* raw;
    CDList*     parent;
} CDListIterator;

/* Sorting the list */
typedef enum _CDListSortAlgorithm {
    CDSortInsert
} CDListSortAlgorithm;

typedef int8_t (*CDListCompareCallback) (CDPointer a, CDPointer b);

/**
 * Create a List object
 *
 * @return The list object
 */
CDList* CD_CreateList (void);

/**
 * Shallow clone a List object
 *
 * @return The cloned List
 */
CDList* CD_CloneList (CDList* self);

/**
 * Destroy a List object and return its remaining data as a NULL terminated CDPointer array.
 */
void CD_DestroyList (CDList* self);

/**
 * Get an iterator to the beginning of the List.
 *
 * If used CD_ListIterator(Value|Key) on this iterator it will refer to the first
 * element of the List.
 *
 * @return The iterator
 */
CDListIterator CD_ListBegin (CDList* self);

/**
 * Get an iterator to the end of the List.
 *
 * This iterator points *AFTER* the last element.
 *
 * @return The iterator
 */
CDListIterator CD_ListEnd (CDList* self);

/**
 * Get the next iterator with content, it automagically jumps empty buckets.
 *
 * @param iterator The iterator to the current position
 *
 * @return The iterator to the next element
 */
CDListIterator CD_ListNext (CDListIterator it);

/**
 * Get the prev iterator with content, it automagically jumps empty buckets.
 *
 * @param iterator The iterator to the current position
 *
 * @return The iterator to the prev element
 */
CDListIterator CD_ListPrevious (CDListIterator it);

/**
 * Get the number of elements in the List
 *
 * @return The number of elements in the List
 */
size_t CD_ListLength (CDList* self);

/**
 * Check if an iterator is equal to another
 */
bool CD_ListIteratorIsEqual (CDListIterator a, CDListIterator b);

/**
 * Get the value of the given iterator position.
 *
 * @param iterator The iterator to the current position
 *
 * @return The value (whatever) value
 */
CDPointer CD_ListIteratorValue (CDListIterator iterator);

/**
 * Push a value into the List.
 *
 * @param data The value to push
 *
 * @return self
 */
CDList* CD_ListPush (CDList* self, CDPointer data);

/**
 * Push a value if the callback returns 0
 *
 * @param data The value to push
 * @param callback The callback to test data
 *
 * @return self if pushed, NULL otherwise
 */
CDList* CD_ListPushIf (CDList* self, CDPointer data, CDListCompareCallback callback);

/**
 * Insert the data sorted in the list. Please only use with alread sorted
 * list (e.g. A list where item are only inserted using this function or after a
 * call to CD_ListSort()
 *
 * @parameter data the data that needs to be inserted in the list
 * @parameter callback a strcmp like callback function
 */
CDList* CD_ListSortedPush (CDList* self, CDPointer data, CDListCompareCallback callback);

/**
 * Shift a value from the List.
 *
 * @return The shifted value
 */
CDPointer CD_ListShift (CDList* self);

/**
 * Sorts the provided list using the algorith specified in the parameters
 *
 * @parameter algorithm The algorithm to use when sorting the list.
 * @parameter callback a strcmp like callback function
 */
CDList* CD_ListSort (CDList* self, CDListSortAlgorithm algorithm, CDListCompareCallback callback);

bool CD_ListIsEqual (CDList* a, CDList* b, CDListCompareCallback callback);

/**
 * Delete the first value matching the passed one from the List.
 *
 * @param data The value to remove
 *
 * @return The removed data
 */
CDPointer CD_ListDelete (CDList* self, CDPointer data);

CDPointer CD_ListDeleteIf (CDList* self, CDPointer data, CDListCompareCallback callback);

/**
 * Delete all the items matching the passed one from the List.
 *
 * @param data The value to remove
 *
 * @return The reoved data
 */
CDPointer CD_ListDeleteAll (CDList* self, CDPointer data);

CDPointer CD_ListDeleteAllIf (CDList* self, CDPointer data, CDListCompareCallback callback);

/**
 * Empty the List and return an array of the contained data
 *
 * @return An array of the contained data
 */
CDPointer* CD_ListClear (CDList* self);

/**
 * Get the first element in the List.
 *
 * @return The first element in the list.
 */
CDPointer CD_ListFirst (CDList* self);

/**
 * Get the last element in the List.
 *
 * @return The last element in the list.
 */
CDPointer CD_ListLast (CDList* self);

bool CD_ListContains (CDList* self, CDPointer data);

bool CD_ListContainsIf (CDList* self, CDPointer data, CDListCompareCallback callback);

bool CD_ListStartIterating (CDList* self);

bool CD_ListStopIterating (CDList* self, bool stop);

/**
 * Iterate over the given map
 *
 * @parameter it The name of the iterator variable
 */
#define CD_LIST_FOREACH(self, it)                                                   \
    if (self && CD_ListLength(self) > 0 && CD_ListStartIterating(self))             \
        for (CDListIterator it = CD_ListBegin(self), __end__ = CD_ListEnd(self);    \
                                                                                    \
        CD_ListStopIterating(self, !CD_ListIteratorIsEqual(it, __end__) && it.raw); \
                                                                                    \
        it = CD_ListNext(it))

#define CD_LIST_BREAK(self) \
    CD_ListStopIterating(self, false); break

#endif
