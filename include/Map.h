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

#ifndef CRAFTD_MAP_H
#define CRAFTD_MAP_H

#include <craftd/common.h>
#include <craftd/klib/khash.h>

KHASH_MAP_INIT_INT(cdMap, void*);

/**
 * The Map class
 */
typedef struct _CDMap {
    khash_t(cdMap)* map;

    pthread_rwlock_t lock;
} CDMap;

/**
 * The Map iterator type
 */
typedef khiter_t CDMapIterator;

/**
 * Create an Map object
 *
 * @return The Map object
 */
CDMap* CD_CreateMap (void);

/**
 * Shallow clone a Map object.
 *
 * It's useful to iterate over a Map that you want to change during the iteration.
 *
 * @return The cloned Map object
 */
CDMap* CD_CloneMap (CDMap* self);

/**
 * Destroy a Map object.
 *
 * Keep in mind that you have to destroy the saved data yourself.
 */
void CD_DestroyMap (CDMap* self);

/**
 * Get an iterator to the beginning of the Map.
 *
 * If used CD_MapIterator(Value|Key) on this iterator it will refer to the first
 * element of the Map.
 *
 * @return The iterator
 */
CDMapIterator CD_MapBegin (CDMap* self);

/**
 * Get an iterator to the end of the Map.
 *
 * This iterator points *AFTER* the last element, so you have to use CD_MapPrevious to get the
 * iterator pointing to the last element.
 *
 * @return The iterator
 */
CDMapIterator CD_MapEnd (CDMap* self);

/**
 * Get the next iterator with content, it automagically jumps empty buckets.
 *
 * @param iterator The iterator to the current position
 *
 * @return The iterator to the next element
 */
CDMapIterator CD_MapNext (CDMap* self, CDMapIterator iterator);

/**
 * Get the previous iterator with content, it automagically jumps empty buckets.
 *
 * @param iterator The iterator to the current position
 *
 * @return The iterator to the previous element
 */
CDMapIterator CD_MapPrevious (CDMap* self, CDMapIterator iterator);

/**
 * Get the number of elements in the Map
 *
 * @return The number of elements in the Map
 */
size_t CD_MapLength (CDMap* self);

/**
 * Get the key of the given iterator position.
 *
 * @param iterator The iterator to the current position
 *
 * @return The key (string) value
 */
int CD_MapIteratorKey (CDMap* self, CDMapIterator iterator);

/**
 * Get the value of the given iterator position.
 *
 * @param iterator The iterator to the current position
 *
 * @return The value (whatever) value
 */
void* CD_MapIteratorValue (CDMap* self, CDMapIterator iterator);

/**
 * Checks if a iterator is valid (points to an existing element)
 *
 * @param iterator The iterator to the current position
 *
 * @return true if there's an element, false otherwise.
 */
bool CD_MapIteratorValid (CDMap* self, CDMapIterator iterator);

/**
 * Get the value of the element with the given name.
 *
 * @param name A string with the name you want to get
 *
 * @return The value or NULL
 */
void* CD_MapGet (CDMap* self, int id);

/**
 * Set the value of the element with the given name.
 *
 * @param name A string with the name you want to set
 * @param data The pointer to the data you want to set
 *
 * @return The old data if present or NULL
 */
void* CD_MapSet (CDMap* self, int id, void* data);

/**
 * Delete the element with the given name
 *
 * @param name A string with the name you want to delete
 *
 * @return The delete data if present or NULL
 */
void* CD_MapDelete (CDMap* self, int id);

/**
 * Get the value of the first element in the Map
 *
 * @return The first element in the Map
 */
void* CD_MapFirst (CDMap* self);

/**
 * Get the value of the last element in the Map
 *
 * @return The last element in the Map
 */
void* CD_MapLast (CDMap* self);

/**
 * Empty the Map and return an array of the contained data
 *
 * @return An array of the contained data
 */
void** CD_MapClear (CDMap* self);

/**
 * Iterate over the given map
 *
 * @parameter it The name of the iterator variable
 */
#define CD_MAP_FOREACH(self, it) \
    if (self) for (CDMapIterator it = CD_MapBegin(self), __end__ = CD_MapEnd(self); it != __end__; it = CD_MapNext(self, it))

#endif
