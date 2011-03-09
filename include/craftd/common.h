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

#ifndef CRAFTD_COMMON_H
#define CRAFTD_COMMON_H

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>
#include <limits.h>
#include <errno.h>
#include <assert.h>

#include <pthread.h>

#include <event2/event.h>
#include <event2/buffer.h>
#include <event2/bufferevent.h>
#include <event2/listener.h>
#include <event2/thread.h>

#include <craftd/config.h>
#include <craftd/memory.h>

#define CD_DO \
    for (char __cddo_tmp__ = 0; __cddo_tmp__ == 0; __cddo_tmp__++)

#if SIZEOF_FP == 4 && SIZEOF_INTPTR_T == 4
    typedef int32_t CDPointer;
#else
    typedef int64_t CDPointer;
#endif

typedef int CDError;

#define PRIVATE(data) ((data)->_private)
#define ERROR(data)   ((data)->_error)

#define CDNull (0)

#define CD_CHUNK_COORD(x) (x)
#define CD_WORLD_COORD(x) ((x) << 4)

#include <craftd/Arithmetic.h>
#include <craftd/List.h>
#include <craftd/Map.h>
#include <craftd/Hash.h>
#include <craftd/Set.h>
#include <craftd/String.h>
#include <craftd/Regexp.h>

#include <craftd/javaendian.h>
#include <craftd/minecraft.h>

#include <craftd/Buffer.h>
#include <craftd/Buffers.h>

#endif
