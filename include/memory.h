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

#ifndef CRAFTD_MEMORY_H
#define CRAFTD_MEMORY_H

#include <stdlib.h>

/**
 * Simple free wrapper
 *
 * @param pointer The pointer to free
 */
void CD_free (void* pointer);

/**
 * Simple calloc wrapper w/error handling
 *
 * @param number number of objects to allocate
 * @param size size of each object
 *
 * @return valid pointer to heap memory
 */
void* CD_calloc (size_t number, size_t size);

/**
 * Simple malloc wrapper w/error handling
 *
 * @param size allocation size
 *
 * @return valid pointer to heap memory
 */
void* CD_malloc (size_t size);

/**
 * Simple malloc wrapper that sets the memory to zero
 */
void* CD_alloc (size_t size);

/**
 * Simple realloc wrapper w/error handling.  Mimics glibc's implementation
 *
 * @param pointer pointer to the heap address to reallocate
 * @param size reallocation size
 *
 * @return resized valid pointer to heap memory
 */
void* CD_realloc (void* pointer, size_t size);

#endif
