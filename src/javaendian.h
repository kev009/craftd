#ifndef CRAFTD_JAVAENDIAN_H
#define CRAFTD_JAVAENDIAN_H

/*
 * Copyright (c) 2010 Kevin M. Bowling, <kevin.bowling@kev009.com>, USA
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


/* Functions to get floating point numbers into Java's endian order
 * If we're on a big-endian float machine, macro away the conversions
 *
 * Uses FLOAT_WORDS_BIGENDIAN from autoconf extras
 *
 * Also define 64-bit integer routines since they don't always exist
 */

#include <config.h>

#ifdef HAVE_ENDIAN_H
#include <endian.h>
#endif

#ifdef WORDS_BIGENDIAN
/* If big-endian, macro out ntoh64/hton4 */
#define CD_ntoh64(ll) (ll)
#define CD_hton64(ll) (ll)
#else
/* Otherwise, check if BSD-style endian functions exist */
#ifdef HAVE_DECL_BE64TOH
#define CD_ntoh64(ll) be64toh(ll)
#else
// TODO Define a public CD_hton64() signature and implement it!
#endif

#ifdef HAVE_DECL_HTOBE64
#define CD_hton64(ll) htobe64(ll)
#else
// TODO Define a public CD_hton64() signature and implement it!
#endif

#endif


#ifdef FLOAT_WORDS_BIGENDIAN
/* Floating point types are big-endian, macro out conversion */
#define ntohd(d) (d)
#define htond(d) (d)
#define ntohf(f) (f)
#define htonf(f) (f)
#else
/* We need to convert native floating-point types */
double ntohd(double d);
double htond(double d);
float ntohf(float f);
float htonf(float f);
#endif

#endif
