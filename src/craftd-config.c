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
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <jansson.h>

#include "craftd-config.h"
#include "craftd.h"
#include "util.h"
#include "mapchunk.h"

/* Networking knobs */
const int MAX_LISTENBACKLOG = 16;
const int MAX_BUF = 8096;

/* MOTD */
bstring Config_motd[MOTD_LINES];
int Config_motdsz = 0;

/* Search path for the main config file, in order */
static const char *config_searchpath[] = {
  "/.craftd/craftd.conf", // $HOME is appended to this in _setdefaults()!
  "/etc/craftd/craftd.conf",
  "/usr/local/etc/craftd/craftd.conf",
  "craftd.conf", // Current working directory (for devs)
  NULL
};

/**
 * This function defines and initializes defaults for the craftd settings
 * structure.
 */
void
craftd_config_setdefaults()
{
  // Add $HOME to the first searchpath entry
  if (getenv("HOME") != NULL)
  {
    char *homeconfig = Malloc(1 + strlen(getenv("HOME")) +
        strlen(config_searchpath[0]));
    homeconfig[0] = '\0';
    strcat(homeconfig, getenv("HOME"));
    strcat(homeconfig, config_searchpath[0]);
    config_searchpath[0] = homeconfig;
  }

  // Game settings
  Config.daemonize = true;
  Config.game_port = 25565;
  Config.max_listenbacklog = 16;
  Config.mcstring_max = 100;
  Config.workpool_size = 2;
  char *motddefault = "motd.conf";
  Config.motd_file = motddefault;
#ifdef USE_CDGAME
  char *worlddefault = "world/";
  Config.world_dir = worlddefault;
  Config.dayrate = 20;
  Config.sunsetrate = 20;
  Config.nightrate = 20;
  Config.sunriserate = 20;
  Config.spawn.x = 0;
  Config.spawn.y = 0;
  Config.spawn.z = 0;
#endif

#ifdef USE_CDPROXY
  // Proxy Settings
  char *defaultservername = "Default Server";
  Config.proxy_default_server = defaultservername;
  Server **defaultproxyservers = Malloc(sizeof(Server *)*2);
  defaultproxyservers[0] = Malloc(sizeof(Server));
  defaultproxyservers[0]->host = strdup("127.0.0.1");
  defaultproxyservers[0]->name = strdup("Default Server");
  defaultproxyservers[0]->port = 25565;
  defaultproxyservers[1] = NULL;
  Config.proxy_servers = defaultproxyservers;
#endif

  // httpd settings
  Config.httpd_enabled = true;
  Config.httpd_port = 25566;
  char *docrootdefault = "htdocs/";
  Config.docroot = docrootdefault;
}

void
craftd_config_readmotd(char *file)
{
  //struct bstrList *motd = bstrListCreate();
  FILE *fp;

  LOG(LOG_DEBUG, "Reading MOTD file %s", file);
  
  /* Open the MOTD or exit */
  if ((fp = fopen(file, "r")) == NULL)
    PERR("Error reading MOTD file"); // Read errno and quit
  struct bStream *bs = bsopen((bNread)fread, fp);

  bstring line;
  int i;
  for (i = 0; (line = bgets((bNgetc)fgetc, fp, '\n')) != NULL; ++i)
  {
    if (i > MOTD_LINES)
      ERR("MOTD File is too long, max lines is %d", MOTD_LINES);

    LOG(LOG_DEBUG, "MOTD: %s", line->data);
    Config_motd[i] = line;
  }

  Config_motdsz = i;

  bsclose(bs);
  fclose(fp);

  return;
}

/**
 * Read a boolean value from a JSON object and store it as a C99 boolean in the
 * specified storage location.
 */
void
parseJBool(bool *storage, const json_t *obj, const char *key)
{
  json_t *boolobj = json_object_get(obj, key);

  /* Check if the key is defined in the config file (nonfatal) */
  if (!boolobj)
  {
    LOG(LOG_DEBUG, "Config: key \"%s\" is undefined.  Using default.", key);
    return;
  }

  /* Check if the value is a boolean (fatal) */
  if (!json_is_boolean(boolobj))
    ERR("Error processing key: \"%s\".  Value is not boolean!", key);

  /* Set the storage location to the C99 boolean type */
  if (json_is_true(boolobj))
  {
    LOG(LOG_DEBUG, "Got bool value: 'true' for key: \"%s\"", key);
    *storage = true;
  }
  else
  {
    LOG(LOG_DEBUG, "Got bool value: 'false' for key: \"%s\"", key);
    *storage = false;
  }
}

/**
 * Read an integer value from a JSON object and store it in the specified
 * location.  
 * 
 * @remarks 0 is not currently treated as a valid integer
 * 
 * @param storage a location to place the parsed integer value
 * @param obj json object to parse
 * @param key json key to read
 */
void
parseJInt(int *storage, const json_t *obj, const char *key)
{
  int32_t ival;
  json_t *intobj = json_object_get(obj, key);

  /* Check if the key is defined in the config file (nonfatal) */
  if (!intobj)
  {
    LOG(LOG_DEBUG, "Config: key \"%s\" is undefined.  Using default.", key);
    return;
  }
  
  /* Check if the value is a number (fatal) */
  if (!json_is_number(intobj))
    ERR("Error processing key: \"%s\".  Value is not an integer!", key); 
  
  /* Cast and make sure the value is set (fatal) */
  ival = (int32_t)json_integer_value(intobj);
  if (ival == 0)
    ERR("Error processing key: \"%s\"", key);

  LOG(LOG_DEBUG, "Got int value: %d for key: \"%s\"", ival, key);
  
  /* Set the storage location to the new value */
  *storage = ival;
}

/**
 * Read a string value from a JSON object and store it in the specified
 * location.
 * 
 * @param storage a character array pointer to place the parsed string
 * @param obj json object to parse
 * @param key key to read
 */
char *
parseJString(const json_t *obj, const char *key, char *defaultvalue)
{
  const char *strval;
  json_t *strobj = json_object_get(obj, key);
  
  /* Check if the key is defined in the config file (nonfatal) */
  if (!strobj)
  {
    LOG(LOG_DEBUG, "Config: key \"%s\" is undefined.  Using default.", key);
    return defaultvalue;
  }
  
  /* Check if the value is a string (fatal) */
  if (!json_is_string(strobj))
    ERR("Error processing key: \"%s\".  Value is not a string!", key);
  
  strval = json_string_value(strobj);
  LOG(LOG_DEBUG, "Got string value: \"%s\" for key: \"%s\"", strval, key);
  
  /* Set the storage location to the new value */
  //storage = (char *)Malloc(strlen(strval));
  return strdup(strval);
}

/**
 * Parse the file name for craftd configuration information.
 *
 * @param file the JSON file name to parse
 */
void
craftd_config_parse(const char *file)
{
  json_t *json, *jsonhttp;
  json_error_t error;

  /* If we didn't get a config file passed as an argument, check search path */
  if (file == NULL)
  {
    int i;
    for (i = 0; config_searchpath[i] != NULL &&
        access(config_searchpath[i], R_OK) != 0; ++i);
  
    file = config_searchpath[i];

    if (file == NULL)
      ERR("Config: craftd.conf not found!");
  }
  /* Check that the file argument exists */
  else
  {
    if (access(file, R_OK) != 0)
      ERR("Config: file %s not readable!");
  }

  LOG(LOG_INFO, "Config: Using file: %s", file);

  json = json_load_file(file, 0, &error);
  if (!json)
  {
    LOG(LOG_ERR, "Config (line: %d, col: %d):\nConfig:%s", error.line,
	error.column, error.text);
    ERR("Cannot parse config file!");
  }
  
  /* Get the general game server configuration */
  if(MODE==GAME)
  {
    json_t *jsongame = json_object_get(json, "server");
    if (json_is_object(jsongame))
    {
      parseJBool(&Config.daemonize, jsongame, "daemonize");
      parseJInt(&Config.game_port, jsongame, "game-port");
      parseJInt(&Config.mcstring_max, jsongame, "minecraft-stringmax");
      parseJInt(&Config.workpool_size, jsongame, "worker-pool-size");
      Config.motd_file = parseJString(jsongame, "motd-file",Config.motd_file);
      Config.world_dir = parseJString(jsongame, "world-dir",Config.world_dir);
      parseJInt(&Config.dayrate,jsongame,"day-rate");
      parseJInt(&Config.sunsetrate,jsongame,"sunset-rate");
      parseJInt(&Config.nightrate,jsongame,"night-rate");
      parseJInt(&Config.sunriserate,jsongame,"sunrise-rate");
    }
    else
    {
      LOG(LOG_INFO, "Config: no server section, skipping.");
    }
  }
  if(MODE==PROXY)
  {
    json_t *jsonproxy = json_object_get(json, "proxy");
    if(json_is_object(jsonproxy))
    {
      parseJBool(&Config.daemonize, jsonproxy, "daemonize");
      parseJInt(&Config.game_port, jsonproxy, "game-port");
      parseJInt(&Config.mcstring_max, jsonproxy, "minecraft-stringmax");
      parseJInt(&Config.workpool_size, jsonproxy, "worker-pool-size");
      Config.motd_file = parseJString(jsonproxy, "motd-file",Config.motd_file);
      Config.proxy_default_server = parseJString(jsonproxy,"default-server",Config.proxy_default_server);
      
      json_t *jsonpservers = json_object_get(jsonproxy,"servers");
      
      if(json_is_array(jsonpservers))
      {
	Server **proxyservers = (Server**)Malloc((json_array_size(jsonpservers) + 1) 
			      * sizeof(Server*));
	int aIndex = 0;
	for(; aIndex < json_array_size(jsonpservers); aIndex++)
	{
	  json_t *serverelement = json_array_get(jsonpservers, aIndex);
	  proxyservers[aIndex] = (Server *) Malloc(sizeof(Server));	
	  /* Remember to free()/realloc() proxyservers when a server context is deleted
	  * or added during runtime */
	  bzero(proxyservers[aIndex],sizeof(Server));
	  proxyservers[aIndex]->host = parseJString(serverelement,"host","127.0.0.1");
	  proxyservers[aIndex]->name = parseJString(serverelement,"name","undefined");
	  parseJInt(&proxyservers[aIndex]->port,serverelement,"port");
	}
	proxyservers[aIndex] = NULL; // Null terminate the list
	Config.proxy_servers = proxyservers; // Save changes
      }
      else
      {
	LOG(LOG_INFO, "Config: no proxy-servers section, skipping");
      }
    }
    else
    {
      LOG(LOG_INFO, "Config: no proxy section, skipping.");
    }
  }

  /* Get the httpd server configuration */
  jsonhttp = json_object_get(json, "httpd");
  if(json_is_object(jsonhttp))
  {
    parseJBool(&Config.httpd_enabled, jsonhttp, "enabled");
    parseJInt(&Config.httpd_port, jsonhttp, "httpd-port");
    Config.docroot = parseJString(jsonhttp, "static-docroot",Config.docroot);
  }
  else
  {
    LOG(LOG_INFO, "Config: no httpd section, skipping.");
  }
  
  /* Release the file reader */
  json_decref(json);
}
