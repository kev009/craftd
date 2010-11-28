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
#ifdef HAVE_SYS_ENDIAN_H
#include <sys/endian.h>
#endif
#ifdef HAVE_NETINTET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_ARPA_INET_H
#include <arpa/inet.h>
#endif

#ifdef WORDS_BIGENDIAN
/* If big-endian, macro out ntohll and htonll if OS doesn't have it */

#ifndef htonll
#define htonll(ll) (ll)
#endif /* htonll */
#ifndef ntohll
#define ntohll(ll) (ll)
#endif /* ntohll */

#else
/* Not WORDS_BIGENDIAN */

/* Add generic ntohll/htonll routine if needed */
#if defined(HAVE_DECL_BE64TOH) && !defined(ntohll)

#define ntohll(ll) be64toh(ll)
#ifdef HAVE_DECL_HTOBE64
#define htonll(ll) htobe64(ll)
#else
#define htonll(ll) be64toh(ll)
#endif

#else /* Don't have be64toh */

#ifndef(ntohll)
#define ntohll(x) ((((uint64_t)ntohl(x)) << 32) + ntohl(x >> 32))
#endif

#ifndef htonll
#define htonll(x) ((((uint64_t)htonl(x)) << 32) + htonl(x >> 32))
#endif

#endif /* ntohll/htonll routines */

#endif /* WORDS_BIGENDIAN */


#ifdef FLOAT_WORDS_BIGENDIAN
/* Floating point types are big-endian, macro out conversion */
#define Cswapd(d) (d)
#define Cswapf(f) (f)
#else
/* We need to convert native floating-point types */
double Cswapd(double d);
float Cswapf(float f);
#endif

#endif /* FLOAT_WORDS_BIGENDIAN */
