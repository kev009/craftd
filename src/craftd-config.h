#ifndef CRAFTD_CONFIG_H
#define CRAFTD_CONFIG_H

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

#include <config.h>

#include <stdbool.h>
#include <stdint.h>

#include "bstrlib.h"

/* Networking knobs */
const int MAX_LISTENBACKLOG;
const int MAX_BUF;

/* Public methods */
void craftd_config_setdefaults();
void craftd_config_parse(const char *file);
void craftd_config_readmotd(char *file);

/* Public data */
#define MOTD_LINES (20) // Max MOTD line count
extern bstring Config_motd[MOTD_LINES];
int Config_motdsz;

#ifdef USE_CDPROXY
typedef struct _Server
{
  // Server Definitions
  char *host;
  int port;
  char *name;
} Server;
#endif

#ifdef USE_CDGAME
typedef struct _Spawnpos
{
  // Default spawn coordinates
  int x;
  int y;
  int z;
} Spawnpos;
#endif
struct
{
  // Server settings
  bool daemonize;
  int game_port;
  int max_listenbacklog;
  int mcstring_max;
  int workpool_size;
  char *motd_file;
#ifdef USE_CDGAME
  char *world_dir;
  int dayrate;
  int sunsetrate;
  int nightrate;
  int sunriserate;
  Spawnpos spawn;
#endif

#ifdef USE_CDPROXY
  char *proxy_default_server;
  Server **proxy_servers;
#endif
  // httpd settings
  bool httpd_enabled; 
  int httpd_port;
  char *docroot;
} Config;

#endif
