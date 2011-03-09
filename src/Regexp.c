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

CDRegexp*
CD_CreateRegexp (char* string, int options)
{
    CDRegexp* result;

    pcre*       pattern;
    const char* error       = NULL;
    int         errorOffset = 0;

    options |= PCRE_UTF8 | PCRE_EXTRA;

    // If the pcre compilation fails
    if ((pattern = pcre_compile(string, options, &error, &errorOffset, NULL)) == NULL) {
        return NULL;
    }

    result          = malloc(sizeof(Regexp));
    result->string  = string;
    result->options = options;
    result->pattern = pattern;
    result->study   = pcre_study(pattern, 0, &error);

    return result;
}

void
CD_DestroyRegexp (CDRegexp* regexp)
{
    if (regexp->study) {
        free(regexp->study);
    }

    pcre_free(regexp->pattern);

    free(regexp->string);

    free(regexp);
}

void
CD_DestroyRegexpKeepString (CDRegexp* regexp)
{
    if (regexp->study) {
        free(regexp->study);
    }

    pcre_free(regexp->pattern);

    free(regexp);
}

RegexpMatches*
CD_CreateRegexpMatches (size_t length)
{
    RegexpMatches* result;
    
    result         = malloc(sizeof(RegexpMatches));
    result->length = length;
    result->item   = malloc(length * sizeof(String*));

    return result;
}

void
CD_DestroyRegexpMatches (RegexpMatches* object)
{
    size_t i;
    for (i = 0; i < object->length; i++) {
        if (object->item[i] != NULL) {
            CD_DestroyString(object->item[i]);
        }
    }
}

RegexpMatches*
CD_MatchRegexp (CDRegexp* regexp, String* string)
{
    RegexpMatches* matches;

    int  length = 0;
    int* substrings;

    char* substr_start;
    int   substr_length;
    char* tmp;

    size_t i;

    pcre_fullinfo(regexp->pattern, regexp->study, PCRE_INFO_CAPTURECOUNT, &length);
    substrings = malloc((length = 3 * (length + 1)) * sizeof(int));

    length = pcre_exec(regexp->pattern, regexp->study, string->data, string->rawLength, 0, 0, substrings, length);

    if (length < 0) {
        return NULL;
    }

    matches = CD_CreateRegexpMatches(length);

    for (i = 0; i < matches->length; i++) {
        substr_start  = string->data + substrings[2 * i];
        substr_length = substrings[2 * i + 1] - substrings[2 * i];

        tmp = malloc((substr_length + 1) * sizeof(char));
        strncpy(tmp, substr_start, substr_length);

        matches->item[i] = CD_CreateString(tmp);
    }

    free(substrings);

    return matches;
}

RegexpMatches*
CD_MatchRegexpString (char* regexp, int options, String* string)
{
    CDRegexp*        re     = CD_CreateRegexp(regexp, options);
    RegexpMatches* result = CD_MatchRegexp(re, string);

    CD_DestroyRegexpKeepString(re);

    return result;
}

RegexpMatches*
CD_MatchRegexpString2 (char* regexp, int options, char* string)
{
    CDRegexp*        re     = CD_CreateRegexp(regexp, options);
    String*        str    = CD_CreateString(string);

    RegexpMatches* result = CD_MatchRegexp(re, str);

    CD_DestroyRegexpKeepString(re);
    CD_DestroyStringKeepData(str);

    return result;
}

bool
CD_TestRegexp (CDRegexp* regexp, String* string)
{
    size_t length = 0;
    int*   substrings;
    int    result;

    pcre_fullinfo(regexp->pattern, regexp->study, PCRE_INFO_CAPTURECOUNT, &length);
    substrings = malloc((length * 3) * sizeof(int));

    result = pcre_exec(regexp->pattern, regexp->study, string->data, string->rawLength, 0, 0, substrings, length);

    free(substrings);

    return result >= 0;
}
