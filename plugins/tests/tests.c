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
cdtest_String_UTF8_length (void* data)
{
    CDString* test = CD_CreateStringFromCString("Æ§Ð");

    tt_int_op(CD_StringLength(test), ==, 3);
    tt_int_op(CD_StringSize(test), ==, 6);

    end: {
        CD_DestroyString(test);
    }
}

void
cdtest_String_UTF8_charAt (void* data)
{
    CDString* test = CD_CreateStringFromCString("Æ§Ð");
    CDString* ch   = CD_CharAt(test, 1);

    tt_int_op(strcmp(CD_StringContent(ch), "§"), ==, 0);

    end: {
        CD_DestroyString(test);
        CD_DestroyString(ch);
    }
}

struct testcase_t cd_utils_String_UTF8_tests[] = {
    { "length", cdtest_String_UTF8_length, },
    { "charAt", cdtest_String_UTF8_charAt, },

    END_OF_TESTCASES
};

void
cdtest_String_Minecraft_sanitize (void* data)
{
    CDString* string    = CD_CreateStringFromCString("æßðđ¼½¬²³æðđ]}»”¢“}¹²³þæßł@»ł”##æðþŋŋŋ§2ŋŋŋł€¶®ÐJª§&<©>‘ŁØ&ØΩ§3");
    CDString* sanitized = CD_StringSanitizeForMinecraft(string);

    tt_int_op(strcmp(CD_StringContent(sanitized),
        "æ???¼½¬??æ??]}»???}????æ??@»??##æ?????§2??????®?Jª§&<?>??Ø&Ø?"), ==, 0);

    end: {
        CD_DestroyString(string);
        CD_DestroyString(sanitized);
    }
}

void
cdtest_String_Minecraft_valid (void* data)
{
    CDString* invalid = CD_CreateStringFromCString("æßðđ¼½¬²³æðđ]}»”¢“}¹²³þæßł@»ł”##æðþŋŋŋ§2ŋŋŋł€¶®ÐJª§&<©>‘ŁØ&ØΩ§3");
    CDString* valid   = CD_CreateStringFromCString("æ???¼½¬??æ??]}»???}????æ??@»??##æ?????§2??????®?Jª§&<?>??Ø&Ø?");

    tt_assert(CD_StringIsValidForMinecraft(invalid) == false);
    tt_assert(CD_StringIsValidForMinecraft(valid) == true);

    end: {
        CD_DestroyString(invalid);
        CD_DestroyString(valid);
    }
}

struct testcase_t cd_utils_String_Minecraft_tests[] = {
    { "sanitize", cdtest_String_Minecraft_sanitize, },
    { "valid",    cdtest_String_Minecraft_valid, },

    END_OF_TESTCASES
};

struct testgroup_t cd_groups[] = {
    { "utils/String/UTF8/", cd_utils_String_UTF8_tests },
    { "utils/String/Minecraft/", cd_utils_String_Minecraft_tests },

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
