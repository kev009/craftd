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

#include <craftd/Server.h>
#include <craftd/Plugin.h>
#include <craftd/Player.h>

#include "tinytest/tinytest.h"
#include "tinytest/tinytest_macros.h"

void
cdtest_String (void* data)
{
    // String tests

    CDString* string    = CD_CreateStringFromCString("æßðđ¼½¬²³æðđ]}»”¢“}¹²³þæßł@»ł”##æðþŋŋŋ§2ŋŋŋł€¶®ÐJª§&<©>‘ŁØ&ØΩ§3");
    CDString* sanitized = CD_StringSanitizeForMinecraft(string);

    tt_assert(strcmp(CD_StringContent(sanitized), "æ???¼½¬??æ??]}»???}????æ??@»??##æ?????§2??????®?Jª§&<?>??Ø&Ø?") == 0);

    end: {
        CD_DestroyString(string);
        CD_DestroyString(sanitized);
    }
}

struct testcase_t cd_utils_tests[] = {
    { "String", cdtest_String, },
    END_OF_TESTCASES
};

struct testgroup_t cd_groups[] = {
    /* Every group has a 'prefix', and an array of tests. That's it. */
    { "utils/", cd_utils_tests },
    END_OF_GROUPS
};

bool
CD_PluginInitialize (CDPlugin* self)
{
    puts("");
    tinytest_main(0, NULL, cd_groups);
    puts("");

    return true;
}
