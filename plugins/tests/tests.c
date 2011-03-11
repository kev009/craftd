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
cdtest_String_fromBuffer (void* data)
{
    const char* test   = "lol wut";
    CDString*   string = CD_CreateStringFromBuffer(test, 3);

    tt_int_op(CD_StringLength(string), ==, 3);

    end: {
        CD_DestroyString(string);
    }
}

struct testcase_t cd_utils_String_tests[] = {
    { "fromBuffer", cdtest_String_fromBuffer, },

    END_OF_TESTCASES
};


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

    tt_assert(CD_StringIsEqual(ch, "§"));

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
    CDString* sanitized = MC_StringSanitize(string);

    tt_assert(CD_StringIsEqual(sanitized, "æ???¼½¬??æ??]}»???}????æ??@»??##æ?????§2??????®?Jª§&<?>??Ø&Ø?"));

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

    tt_assert(MC_StringIsValid(invalid) == false);
    tt_assert(MC_StringIsValid(valid) == true);

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

void
cdtest_Hash_put (void* data)
{
    CDHash* hash = CD_CreateHash();

    CD_HashPut(hash, "lol", 2);

    tt_int_op((int) CD_HashGet(hash, "lol"), ==, 2);

    end: {
        CD_DestroyHash(hash);
    }
}

void
cdtest_Hash_foreach (void* data)
{
    CDHash* hash = CD_CreateHash();

    CD_HashPut(hash, "lol", 1);
    CD_HashPut(hash, "omg", 2);
    CD_HashPut(hash, "wat", 3);
    CD_HashPut(hash, "win", 4);

    CD_HASH_FOREACH(hash, it) {
        if (CD_CStringIsEqual("lol", CD_HashIteratorKey(it))) {
            tt_int_op((int) CD_HashIteratorValue(it), ==, 1);
        }
        else if (CD_CStringIsEqual("omg", CD_HashIteratorKey(it))) {
            tt_int_op((int) CD_HashIteratorValue(it), ==, 2);
        }
        else if (CD_CStringIsEqual("wat", CD_HashIteratorKey(it))) {
            tt_int_op((int) CD_HashIteratorValue(it), ==, 3);
        }
        else if (CD_CStringIsEqual("win", CD_HashIteratorKey(it))) {
            tt_int_op((int) CD_HashIteratorValue(it), ==, 4);
        }
        else {
            tt_abort_msg("Unknown hash key");
        }
    }

    end: {
        CD_DestroyHash(hash);
    }
}

struct testcase_t cd_utils_Hash_tests[] = {
    { "put", cdtest_Hash_put, },
    { "foreach", cdtest_Hash_foreach, },

    END_OF_TESTCASES
};

void
cdtest_Map_put (void* data)
{
    CDMap* map = CD_CreateMap();

    CD_MapPut(map, 9001, 2);

    tt_int_op((int) CD_MapGet(map, 9001), ==, 2);

    end: {
        CD_DestroyMap(map);
    }
}

void
cdtest_Map_foreach (void* data)
{
    CDMap* map = CD_CreateMap();

    CD_MapPut(map, 23, 1);
    CD_MapPut(map, 42, 2);
    CD_MapPut(map, 9001, 3);
    CD_MapPut(map, 911, 4);

    CD_MAP_FOREACH(map, it) {
        if (CD_MapIteratorKey(it) == 23) {
            tt_int_op((int) CD_MapIteratorValue(it), ==, 1);
        }
        else if (CD_MapIteratorKey(it) == 42) {
            tt_int_op((int) CD_MapIteratorValue(it), ==, 2);
        }
        else if (CD_MapIteratorKey(it) == 9001) {
            tt_int_op((int) CD_MapIteratorValue(it), ==, 3);
        }
        else if (CD_MapIteratorKey(it) == 911) {
            tt_int_op((int) CD_MapIteratorValue(it), ==, 4);
        }
        else {
            tt_abort_msg("Unknown map key");
        }
    }

    end: {
        CD_DestroyMap(map);
    }
}

struct testcase_t cd_utils_Map_tests[] = {
    { "put", cdtest_Map_put, },
    { "foreach", cdtest_Map_foreach, },

    END_OF_TESTCASES
};

void
cdtest_List_push (void* data)
{
    CDList* list = CD_CreateList();

    CD_ListPush(list, 42);

    tt_int_op(CD_ListShift(list), ==, 42);

    end: {
        CD_DestroyList(list);
    }
}

void
cdtest_List_foreach (void* data)
{
    CDList* list  = CD_CreateList();
    int     total = 0;

    CD_ListPush(list, 23);
    CD_ListPush(list, 42);
    CD_ListPush(list, 9001);
    CD_ListPush(list, 911);

    CD_LIST_FOREACH(list, it) {
        switch (CD_ListIteratorValue(it)) {
            case 23: case 42: case 9001: case 911: total += CD_ListIteratorValue(it); break;

            default: {
                tt_abort_msg("Unknown value in list");
            }
        }
    }

    tt_int_op(total, ==, 23 + 42 + 9001 + 911);

    end: {
        CD_DestroyList(list);
    }
}

void
cdtest_List_clear (void* data)
{
    CDList* list = CD_CreateList();

    CD_ListPush(list, 23);
    CD_ListPush(list, 42);
    CD_ListPush(list, 9001);
    CD_ListPush(list, 911);

    CD_free(CD_ListClear(list));

    tt_int_op(CD_ListLength(list), ==, 0);

    end: {
        CD_DestroyList(list);
    }
}

static
int8_t
cdtest_ListCompare (CDPointer a, CDPointer b)
{
    if (a < b) {
        return -1;
    }
    else if (a > b) {
        return 1;
    }
    else {
        return 0;
    }
}

void
cdtest_List_sort (void* data)
{
    CDList* list = CD_CreateList();

    CD_ListPush(list, 30);
    CD_ListPush(list, 40);
    CD_ListPush(list, 20);
    CD_ListPush(list, 10);

    CD_ListSort(list, CDSortInsert, cdtest_ListCompare);

    tt_int_op(CD_ListShift(list), ==, 10);
    tt_int_op(CD_ListShift(list), ==, 20);
    tt_int_op(CD_ListShift(list), ==, 30);
    tt_int_op(CD_ListShift(list), ==, 40);

    end: {
        CD_DestroyList(list);
    }
}

void
cdtest_List_insertSorted (void *data)
{
    CDList* list = CD_CreateList();

    CD_ListSortedPush(list, 30, cdtest_ListCompare);
    CD_ListSortedPush(list, 10, cdtest_ListCompare);
    CD_ListSortedPush(list, 40, cdtest_ListCompare);
    CD_ListSortedPush(list, 20, cdtest_ListCompare);

    tt_int_op(CD_ListShift(list), ==, 10);
    tt_int_op(CD_ListShift(list), ==, 20);
    tt_int_op(CD_ListShift(list), ==, 30);
    tt_int_op(CD_ListShift(list), ==, 40);

    end: {
        CD_DestroyList(list);
    }
}

struct testcase_t cd_utils_List_tests[] = {
    { "push", cdtest_List_push, },
    { "foreach", cdtest_List_foreach, },
    { "clear", cdtest_List_clear, },
    { "sort", cdtest_List_sort, },
    { "insert sorted", cdtest_List_insertSorted, },


    END_OF_TESTCASES
};

void
cdtest_Set_put (void* data)
{
    CDSet* set = CD_CreateSet(10, NULL, NULL);

    CD_SetPut(set, 1);
    CD_SetPut(set, 9001);

    tt_assert(CD_SetHas(set, 1));
    tt_assert(CD_SetHas(set, 9001));

    end: {
        CD_DestroySet(set);
    }
}

void
cdtest_Set_delete (void* data)
{
    CDSet* set = CD_CreateSet(10, NULL, NULL);

    CD_SetPut(set, 1);
    CD_SetPut(set, 2);

    tt_int_op(CD_SetDelete(set, 2), ==, 2);
    tt_assert(!CD_SetHas(set, 2));

    tt_int_op(CD_SetLength(set), ==, 1);

    end: {
        CD_DestroySet(set);
    }
}

void
cdtest_Set_length (void* data)
{
    CDSet* set = CD_CreateSet(10, NULL, NULL);

    CD_SetPut(set, 1);
    CD_SetPut(set, 3);
    CD_SetPut(set, 3); // Redundant element should not get added
    CD_SetPut(set, 2);

    tt_int_op(CD_SetLength(set), ==, 3);

    end: {
        CD_DestroySet(set);
    }
}

struct testcase_t cd_utils_Set_tests[] = {
    { "put",    cdtest_Set_put, },
    { "delete", cdtest_Set_delete, },
    { "length", cdtest_Set_length, },

    END_OF_TESTCASES
};

void
cdtest_Regexp_match (void* data)
{
    CDRegexpMatches* matches = CD_RegexpMatchCString("(\\w+) (\\d+) (\\w+)(?: (.+?))?$", 0, "lol 23 omg");

    tt_int_op(matches->length, ==, 5);
    tt_int_op(matches->matched, ==, 3);
    tt_int_op(matches->item[4], ==, NULL);

    tt_assert(CD_StringIsEqual(matches->item[1], "lol"));
    tt_assert(CD_StringIsEqual(matches->item[2], "23"));
    tt_assert(CD_StringIsEqual(matches->item[3], "omg"));

    end: {
        CD_DestroyRegexpMatches(matches);
    }
}

void
cdtest_Regexp_test (void* data)
{
    CDRegexp* regexp = CD_CreateRegexp("^\\d+$", CDRegexpNone);
    CDString* string = CD_CreateStringFromCString("23");

    tt_assert(CD_RegexpTest(regexp, string));

    end: {
        CD_DestroyRegexpKeepString(regexp);
        CD_DestroyString(string);
    }
}

struct testcase_t cd_utils_Regexp_tests[] = {
    { "match", cdtest_Regexp_match, },
    { "test",  cdtest_Regexp_test, },

    END_OF_TESTCASES
};

struct testgroup_t cd_groups[] = {
    { "utils/String/",           cd_utils_String_tests },
    { "utils/String/UTF8/",      cd_utils_String_UTF8_tests },
    { "utils/String/Minecraft/", cd_utils_String_Minecraft_tests },
    { "utils/Hash/",             cd_utils_Hash_tests },
    { "utils/Map/",              cd_utils_Map_tests },
    { "utils/List/",             cd_utils_List_tests },
    { "utils/Set/",              cd_utils_Set_tests },
    { "utils/Regexp/",           cd_utils_Regexp_tests },

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
