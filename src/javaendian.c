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

#include <arpa/inet.h>

#include "javaendian.h"

/*
 * Don't conditionally compile these functions so we get compiler feedback even
 * if they are not used.  Access control is provided by the header.
 */

#ifndef WORDS_BIGENDIAN
// TODO add function for CD_ntoh64()

// TODO Add function for CD_hton64()
#endif

/* TODO these functions are aweful and may result in undefined behavior
 * FIXME!!!
 */

#ifndef FLOAT_WORDS_BIGENDIAN
double ntohd(double d)
{
  int *overlay;
  int buffer;
  
  overlay = (int*)&d;
  buffer = overlay[0];
  
  overlay[0] = ntohl(overlay[1]);
  overlay[1] = ntohl(buffer);
  
  return *overlay;
}

double htond(double d)
{
  int *overlay;
  int buffer;
  
  overlay = (int*)&d;
  buffer = overlay[0];
  
  overlay[0] = htonl(overlay[1]);
  overlay[1] = htonl(buffer);
  
  return *overlay;
}

float ntohf(float f)
{
  int *ptr = (int*)&f;
  int nf = ntohl (*ptr);
  return *(float *)&nf;
}

float htonf(float f)
{  
  int *ptr = (int*)&f;
  int hf = htonl (*ptr);
  return *(float *)&hf;  
}
#endif
