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

#include <craftd/Regexp.h>
#include <craftd/Logger.h>

CDRegexp*
CD_CreateRegexp (char* string, int options)
{
    pcre*       pattern;
    const char* error       = NULL;
    int         errorOffset = 0;

    options |= PCRE_UTF8 | PCRE_EXTRA;

    // If the pcre compilation fails
    if ((pattern = pcre_compile(string, options, &error, &errorOffset, NULL)) == NULL) {
        ERR("%s at %d", error, errorOffset);

        return NULL;
    }

    CDRegexp* self = CD_malloc(sizeof(CDRegexp));

    assert(self);

    self->string  = string;
    self->options = options;
    self->pattern = pattern;
    self->study   = pcre_study(pattern, 0, &error);

    return self;
}

void
CD_DestroyRegexp (CDRegexp* self)
{
    if (self->study) {
        CD_free(self->study);
    }

    pcre_free(self->pattern);

    CD_free(self->string);
    CD_free(self);
}

void
CD_DestroyRegexpKeepString (CDRegexp* self)
{
    if (self->study) {
        CD_free(self->study);
    }

    pcre_free(self->pattern);

    CD_free(self);
}

CDRegexpMatches*
CD_CreateRegexpMatches (size_t length)
{
    CDRegexpMatches* self = CD_malloc(sizeof(CDRegexpMatches));

    assert(self);

    self->length = length;
    self->item   = CD_malloc(length * sizeof(CDString*));

    return self;
}

void
CD_DestroyRegexpMatches (CDRegexpMatches* self)
{
    for (size_t i = 0; i < self->length; i++) {
        if (self->item[i] != NULL) {
            CD_DestroyString(self->item[i]);
        }
    }
}

CDRegexpMatches*
CD_RegexpMatch (CDRegexp* self, CDString* string)
{
    CDRegexpMatches* matches;

    int  length = 0;
    int* substrings;

    assert(self);
    assert(string);

    pcre_fullinfo(self->pattern, self->study, PCRE_INFO_CAPTURECOUNT, &length);
    substrings = CD_malloc((length = (3 * (length + 1))) * sizeof(int));

    length = pcre_exec(self->pattern, self->study, CD_StringContent(string), CD_StringSize(string), 0, 0, substrings, length);

    if (length < 0) {
        CD_free(substrings);

        return NULL;
    }

    matches = CD_CreateRegexpMatches(length);

    for (size_t i = 0; i < matches->length; i++) {
        const char* substrStart  = CD_StringContent(string) + substrings[2 * i];
        int         substrLength = substrings[2 * i + 1] - substrings[2 * i];

        matches->item[i] = CD_CreateStringFromBufferCopy(substrStart, substrLength);
    }

    CD_free(substrings);

    return matches;
}

CDRegexpMatches*
CD_RegexpMatchString (char* regexp, int options, CDString* string)
{
    CDRegexp* self = CD_CreateRegexp(regexp, options);

    if (!self) {
        return NULL;
    }

    CDRegexpMatches* result = CD_RegexpMatch(self, string);

    CD_DestroyRegexpKeepString(self);

    return result;
}

CDRegexpMatches*
CD_RegexpMatchCString (char* regexp, int options, char* string)
{
    CDRegexp* self = CD_CreateRegexp(regexp, options);

    if (!self) {
        return NULL;
    }

    CDString*        str     = CD_CreateStringFromCString(string);
    CDRegexpMatches* matches = CD_RegexpMatch(self, str);

    CD_DestroyRegexpKeepString(self);
    CD_DestroyString(str);

    return matches;
}

bool
CD_RegexpTest (CDRegexp* self, CDString* string)
{
    size_t length = 0;
    int*   substrings;
    int    result;

    assert(self);
    assert(string);

    pcre_fullinfo(self->pattern, self->study, PCRE_INFO_CAPTURECOUNT, &length);
    substrings = CD_malloc((length * 3) * sizeof(int));

    result = pcre_exec(self->pattern, self->study, CD_StringContent(string), CD_StringSize(string), 0, 0, substrings, length);

    CD_free(substrings);

    return result >= 0;
}
