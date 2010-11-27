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

#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <dirent.h>

// For sys info, maybe getrusage()
//#include <sys/resource.h>

#include <event2/event.h>
#include <event2/http.h>
#include <event2/buffer.h>
#include <event2/util.h>

#include "craftd.h"
#include "craftd-config.h"
#include "httpd.h"
#include "util.h"

/* This HTTP server borrows heavily from Nick Mathewson's example in libevent */

// TODO pull docroot from runtime config
const char *docroot = "/home/kev009/craftd/htdocs";

char uri_root[512];

/* MIME types for common file extensions */
static const struct table_entry{
  const char *extension;
  const char *content_type;
} content_type_table[] = {
  { "txt", "text/plain" },
  { "c", "text/plain" },
  { "h", "text/plain" },
  { "js", "text/javascript" },
  { "html", "text/html" },
  { "htm", "text/html" },
  { "css", "text/css" },
  { "gif", "image/gif" },
  { "jpg", "image/jpeg"},
  { "jpeg", "image/jpeg" },
  { "png", "image/png" },
  { "pdf", "application/pdf" },
  { "ps", "application/postsript" },
  { NULL, NULL },
};

/* Try to guess a good MIME content-type for 'path' */
static const char *
guess_content_type(const char *path)
{
	const char *last_period, *extension;
	const struct table_entry *ent;
	last_period = strrchr(path, '.');
	if (!last_period || strchr(last_period, '/'))
		goto not_found; /* no exension */
	extension = last_period + 1;
	for (ent = &content_type_table[0]; ent->extension; ++ent) {
		if (!evutil_ascii_strcasecmp(ent->extension, extension))
			return ent->content_type;
	}

not_found:
	return "application/misc";
}

/**
 * JSON test buffer
 */
static void
json_request_cb(struct evhttp_request *req, void *arg)
{
  struct evbuffer *bout;

  bout = evbuffer_new();

  evbuffer_add_printf(bout, "<html>\n <head>\n  <title>JSON</title>\n   <body>");

  struct PL_entry *player;
  pthread_rwlock_rdlock(&PL_rwlock);
  evbuffer_add_printf(bout, "\n    <h2>%d users online</h2>\n", PL_count);
  SLIST_FOREACH(player, &PL_head, PL_entries)
  {
    evbuffer_add_printf(bout, "    %s - %s<br>\n", player->username.str, 
        player->ip);
  }
  pthread_rwlock_unlock(&PL_rwlock);

  evbuffer_add_printf(bout, "\n   </body></html>");
  evhttp_add_header(evhttp_request_get_output_headers(req),
      "Content-Type", "text/html");


  evhttp_send_reply(req, HTTP_OK, "OK", bout);
  evbuffer_free(bout);
  return;
}

/**
 * This is the default static document callback
 */
static void
send_staticdoc_cb(struct evhttp_request *req, void *arg)
{
  struct evbuffer *evb;
  const char *uri = evhttp_request_get_uri(req);
  struct evhttp_uri *decoded = NULL;
  const char *path;
  char *decoded_path;
  char *whole_path = NULL;
  size_t len;
  int fd = -1;
  struct stat st;

  // TODO EVHTTP_REQ_HEAD also acceptable here
  if (evhttp_request_get_command(req) != EVHTTP_REQ_GET)
  {
    evhttp_send_error(req, 405, "Invalid request method");
    return;
  }

  printf("Got a GET request for <%s>\n", uri); // LOG

  /* Decode the URI */
  decoded = evhttp_uri_parse(uri);
  if (!decoded)
  {
    printf("bad request\n"); // LOG
    return;
  }

  /* Check the requested path */
  path = evhttp_uri_get_path(decoded);
  if (!path)
    path = "/index.html";
  else if ( (strlen(path)==1) && (strncmp(path, "/", 1) == 0) )
    path = "/index.html";

  /* Decode the path and find out what the request really is */
  decoded_path = evhttp_uridecode(path, 0, NULL);

  /* Throw out .. paths.  It may be better to implement chroot() or strict
   * basepath instead.
   */
  if (strstr(decoded_path, ".."))
    goto err;

  len = strlen(decoded_path)+strlen(docroot)+2;
  whole_path = Malloc(len);

  evutil_snprintf(whole_path, len, "%s/%s", docroot, decoded_path);

  if (stat(whole_path, &st) < 0)
    goto err;

  /* Buffer for outbound content */
  evb = evbuffer_new();

  if (S_ISDIR(st.st_mode))
  {
    /* If it's a directory, do an auto index */
    DIR *d;
    struct dirent *ent;
    const char *trailing_slash = "";

    if(!strlen(path) || path[strlen(path)-1] != '/')
      trailing_slash = "/";

    /* TODO opendir is POSIX 2008.  Add some autoconf and compat/ magic */
    if (!(d = opendir(whole_path)))
      goto err;
    close(fd);  // TODO was this intentional in the example?

    evbuffer_add_printf(evb, "<html>\n <head>\n"
        "  <title>%s</title>\n"
        "  <base href=\"%s%s%s\">\n"
        "  <body>\n"
        "   <h1>%s</h1>\n"
        "   <ul>\n",
        decoded_path, /* TODO HTML escape this */
        uri_root, /* escape? */
        path, /* escape? */
        trailing_slash,
        decoded_path); /* escape */

    while ((ent = readdir(d)))
    {
      evbuffer_add_printf(evb, "    <li><a href=\"%s\">%s</a>\n",
          ent->d_name, ent->d_name); /* TODO escape */
    }

    evbuffer_add_printf(evb, "</ul></body></html>\n");
    closedir(d);
    evhttp_add_header(evhttp_request_get_output_headers(req),
        "Content-Type", "text/html");
  }
  /* Otherwise, we're a file */
  else
  {
    const char *type = guess_content_type(decoded_path);
    if ((fd = open(whole_path, O_RDONLY)) < 0)
    {
      puts("can'tt open file"); // LOG, log errno?
      goto err;
    }

    if (fstat(fd, &st) < 0)
    {
      /* Check that the length still matches */
      puts("fstat different"); // log errno?
      goto err;
    }

    evhttp_add_header(evhttp_request_get_output_headers(req),
        "Content-Type", type);
    evbuffer_add_file(evb, fd, 0, st.st_size);
  }

  /* All went well for an index or file req */
  evhttp_send_reply(req, HTTP_OK, "OK", evb);
  evbuffer_free(evb);
  return;  /* TODO check docs: does evbuffer free the fd automagically? */

//TODO Valgrind and make sure shit isn't left open here

err:
  evhttp_send_error(req, HTTP_NOTFOUND, "Document not found");
  if (fd >= 0)
    close(fd);
  if (decoded)
    evhttp_uri_free(decoded);
  if (decoded_path)
    free(decoded_path);
  if (whole_path)
    free(whole_path);
}

/* Public */
void *run_httpd(void *arg)
{
    struct event_base* htbase;
    struct evhttp* httpd;
    struct evhttp_bound_socket *hthandle;
    
    htbase = event_base_new();
    if(!htbase)
    {
      puts("httpd event base cannot start"); // LOG
      exit(1);
    }

    httpd = evhttp_new(htbase);
    if(!htbase)
    {
      puts("httpd evhttp module cannot start"); // LOG
      exit(1);
    }

    /* Set dynamic callbacks */
    evhttp_set_cb(httpd, "/json", json_request_cb, NULL);
    //evhttp_set_cb(httpd, "/admin", admin_request_cb, NULL);

    /* Default static content handler */
    evhttp_set_gencb(httpd, send_staticdoc_cb, NULL);
    
    
    hthandle = evhttp_bind_socket_with_handle(httpd , INADDR_ANY, HTTPD_PORT);
    if(!hthandle)
    {
      puts("cannot bind httpd!"); // LOG
      exit(1);
    }

    puts("httpd started!"); // LOG

    event_base_dispatch(htbase);

    return NULL;
}

