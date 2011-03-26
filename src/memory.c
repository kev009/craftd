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

#include <craftd/Logger.h>
#include <craftd/memory.h>

void
CD_free (void* pointer)
{
    if (pointer) {
        free(pointer);
    }
}

void*
CD_calloc (size_t number, size_t size)
{
    void* pointer;

    if ((pointer = calloc(number, size)) == NULL && number > 0 && size > 0) {
        CD_abort("could not allocate memory with a calloc");
    }

    return pointer;
}

void*
CD_malloc (size_t size)
{
    void* pointer;

    if ((pointer = malloc(size)) == NULL) {
        CD_abort("could not allocate memory with a malloc");
    }

    return pointer;
}

void*
CD_alloc (size_t size)
{
    void* pointer = CD_malloc(size);

    if (pointer) {
        memset(pointer, 0, size);
    }

    return pointer;
}

void*
CD_realloc (void* pointer, size_t size)
{
    void* newPointer;

    if (pointer == NULL) {
        return CD_malloc(size);
    }

    if (size == 0) {
        CD_free(pointer);

        return NULL;
    }

    if ((newPointer = realloc(pointer, size)) == NULL) {
      CD_abort("could not allocate memory with a realloc");
    }

    return newPointer;
}
