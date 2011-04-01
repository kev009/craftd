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

#include <craftd/common.h>

int
CD_mkdir (const char* path, mode_t mode)
{
    size_t length = strlen(path);
    char   tmp[length + 1];

    for (size_t i = 0; i < length; i++) {
        if (path[i] == '/' || path[i] == '\\') {
            tmp[i] = '\0';

            if (mkdir(tmp, mode) < 0) {
                if (errno != EEXIST) {
                    return -1;
                }
            }
        }

        tmp[i] = path[i];
    }

    return 0;
}

size_t
CD_FileSize (const char* path)
{
    struct stat s;

    if (stat(path, &s) == 0) {
        return s.st_size;
    }
    
    return 0;
}

bool
CD_IsFile (const char* path)
{
    struct stat s;

    if (stat(path, &s) == 0) {
        if (S_ISREG(s.st_mode)) {
            return true;
        }
    }

    return false;
}

bool
CD_IsDirectory (const char* path)
{
    struct stat s;

    if (stat(path, &s) == 0) {
        if (S_ISDIR(s.st_mode)) {
            return true;
        }
    }

    return false;
}

bool
CD_IsSymlink (const char* path)
{
    struct stat s;

    if (stat(path, &s) == 0) {
        if (S_ISLNK(s.st_mode)) {
            return true;
        }
    }

    return false;
}

bool
CD_PathExists (const char* path)
{
    struct stat s;

    if (stat(path, &s) < 0) {
        return false;
    }
    else {
        return true;
    }
}

bool
CD_IsReadable (const char* path)
{
    struct stat s;

    if (stat(path, &s) < 0) {
        return false;
    }
    else {
        DO {
            uid_t uid = geteuid();

            if (s.st_uid == uid && s.st_mode & S_IRUSR) {
                return true;
            }
        }

        DO {
            gid_t gid = getegid();

            if (s.st_gid == gid && s.st_mode & S_IRGRP) {
                return true;
            }
        }

        if (s.st_mode & S_IROTH) {
            return true;
        }
    }

    return false;
}

bool
CD_IsWriteable (const char* path)
{
    struct stat s;

    if (stat(path, &s) < 0) {
        CDString* tmp = CD_CreateStringFromCString(path);
        CDString* dir = CD_StringDirname(tmp);

        if (CD_IsDirectory(CD_StringContent(dir)) && CD_IsReadable(CD_StringContent(dir)) && CD_IsExecutable(CD_StringContent(dir))) {
            return true;
        }

        return false;
    }
    else {
        DO {
            uid_t uid = geteuid();

            if (s.st_uid == uid && s.st_mode & S_IWUSR) {
                return true;
            }
        }

        DO {
            gid_t gid = getegid();

            if (s.st_gid == gid && s.st_mode & S_IWGRP) {
                return true;
            }
        }

        if (s.st_mode & S_IWOTH) {
            return true;
        }
    }

    return false;
}

bool
CD_IsExecutable (const char* path)
{
    struct stat s;

    if (stat(path, &s) < 0) {
        return false;
    }
    else {
        DO {
            uid_t uid = geteuid();

            if (s.st_uid == uid && s.st_mode & S_IXUSR) {
                return true;
            }
        }

        DO {
            gid_t gid = getegid();

            if (s.st_gid == gid && s.st_mode & S_IXGRP) {
                return true;
            }
        }

        if (s.st_mode & S_IXOTH) {
            return true;
        }
    }

    return false;
}
