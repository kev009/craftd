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

#include <craftd/common.h>
#include <craftd/klib/khash.h>

KHASH_MAP_INIT_STR(cdHash, CDPointer);

/**
 * The Hash class
 */
typedef struct _CDHash {
    khash_t(cdHash)* raw;

    pthread_rwlock_t lock;
} CDHash;

/**
 * The Hash iterator type
 */
typedef struct _CDHashIterator {
    khiter_t raw;
    CDHash*  parent;
} CDHashIterator;

/**
 * Create an Hash object
 *
 * @return The Hash object
 */
CDHash* CD_CreateHash (void);

/**
 * Shallow clone a Hash object.
 *
 * It's useful to iterate over a Hash that you want to change during the iteration.
 *
 * @return The cloned Hash object
 */
CDHash* CD_CloneHash (CDHash* self);

/**
 * Destroy a Hash object.
 *
 * Keep in mind that you have to destroy the saved data yourself.
 */
void CD_DestroyHash (CDHash* self);

/**
 * Get the number of elements in the Hash
 *
 * @return The number of elements in the Hash
 */
size_t CD_HashLength (CDHash* self);

/**
 * Get an iterator to the beginning of the Hash.
 *
 * If used CD_HashIterator(Value|Key) on this iterator it will refer to the first
 * element of the Hash.
 *
 * @return The iterator
 */
CDHashIterator CD_HashBegin (CDHash* self);

/**
 * Get an iterator to the end of the Hash.
 *
 * This iterator points *AFTER* the last element, so you have to use CD_HashPrevious to get the
 * iterator pointing to the last element.
 *
 * @return The iterator
 */
CDHashIterator CD_HashEnd (CDHash* self);

/**
 * Get the next iterator with content, it automagically jumps empty buckets.
 *
 * @param iterator The iterator to the current position
 *
 * @return The iterator to the next element
 */
CDHashIterator CD_HashNext (CDHashIterator iterator);

/**
 * Get the previous iterator with content, it automagically jumps empty buckets.
 *
 * @param iterator The iterator to the current position
 *
 * @return The iterator to the previous element
 */
CDHashIterator CD_HashPrevious (CDHashIterator iterator);

/**
 * Check if an iterator is equal to another
 */
bool CD_HashIteratorIsEqual (CDHashIterator a, CDHashIterator b);

/**
 * Get the key of the given iterator position.
 *
 * @param iterator The iterator to the current position
 *
 * @return The key (string) value
 */
const char* CD_HashIteratorKey (CDHashIterator iterator);

/**
 * Get the value of the given iterator position.
 *
 * @param iterator The iterator to the current position
 *
 * @return The value (whatever) value
 */
CDPointer CD_HashIteratorValue (CDHashIterator iterator);

/**
 * Checks if a iterator is valid (points to an existing element)
 *
 * @param iterator The iterator to the current position
 *
 * @return true if there's an element, false otherwise.
 */
bool CD_HashIteratorValid (CDHashIterator iterator);

bool CD_HashHasKey (CDHash* self, const char* name);

/**
 * Get the value of the element with the given name.
 *
 * @param name A string with the name you want to get
 *
 * @return The value or NULL
 */
CDPointer CD_HashGet (CDHash* self, const char* name);

/**
 * Set the value of the element with the given name.
 *
 * @param name A string with the name you want to set
 * @param data The pointer to the data you want to set
 *
 * @return The old data if present or NULL
 */
CDPointer CD_HashPut (CDHash* self, const char* name, CDPointer data);

/**
 * Delete the element with the given name
 *
 * @param name A string with the name you want to delete
 *
 * @return The delete data if present or NULL
 */
CDPointer CD_HashDelete (CDHash* self, const char* name);

/**
 * Get the value of the first element in the Hash
 *
 * @return The first element in the Hash
 */
CDPointer CD_HashFirst (CDHash* self);

/**
 * Get the value of the last element in the Hash
 *
 * @return The last element in the Hash
 */
CDPointer CD_HashLast (CDHash* self);

/**
 * Empty the Hash and return an array of the contained data
 *
 * @return An array of the contained data
 */
CDPointer* CD_HashClear (CDHash* self);

bool CD_HashStartIterating (CDHash* self);

bool CD_HashStopIterating (CDHash* self, bool stop);

/**
 * Iterate over the given hash
 *
 * @parameter it The name of the iterator variable
 */
#define CD_HASH_FOREACH(self, it)                                                   \
    if (self && CD_HashLength(self) > 0 && CD_HashStartIterating(self))             \
        for (CDHashIterator it = CD_HashBegin(self), __end__ = CD_HashEnd(self);    \
                                                                                    \
        CD_HashStopIterating(self, !CD_HashIteratorIsEqual(it, __end__));           \
                                                                                    \
        it = CD_HashNext(it))

#define CD_HASH_BREAK(self) \
    CD_HashStopIterating(self, false); break

#endif
