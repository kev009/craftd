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

#include "../include/HTTPd.h"

#ifdef HAVE_JSON
#   include <jansson.h>
#endif

static
const char*
cd_GuessContentType (const char* path)
{
	const char*          lastPeriod = strrchr(path, '.');
    const char*          extension;
	const CDContentType* type;

	if (lastPeriod == NULL || strchr(lastPeriod, '/')) {
		goto end;
    }

	extension = lastPeriod + 1;
	for (type = CDContentTypes; type->extension; type++) {
		if (!evutil_ascii_strcasecmp(type->extension, extension)) {
			return type->mime;
        }
	}

    end: {
    	return "application/octet-stream";
    }
}

#ifdef HAVE_JSON
static
void
cd_JSONRequest (struct evhttp_request* request, CDHTTPd* self)
{
    struct evbuffer* buffer = evhttp_request_get_input_buffer(request);
    char*            text   = CD_alloc(evbuffer_get_length(buffer) + 1);
    
    evbuffer_remove(buffer, text, evbuffer_get_length(buffer));

    json_error_t error;
    json_t*      input  = json_loads(text, 0, &error);
    json_t*      output = json_object();

    printf("%s\n", text);

    if (evhttp_request_get_command(request) != EVHTTP_REQ_POST) {
        goto error;
    }

    if (input == NULL) {
        SERR(self->server, "RPC.JSON: error on line %d: %s", error.line, error.text);
        
        goto error;
    }

    CD_EventDispatch(self->server, "RPC.JSON", input, output);

    done: {
        char*            outString = json_dumps(output, JSON_INDENT(2));
        struct evbuffer* outBuffer = evbuffer_new();

        evbuffer_add_printf(outBuffer, "%s", outString);

        evhttp_send_reply(request, HTTP_OK, "OK", outBuffer);

        evbuffer_free(outBuffer);
        free(outString);
        json_delete(output);
        json_delete(input);
        CD_free(text);

        return;
    }

    error: {
        evhttp_send_error(request, HTTP_INTERNAL, "Internal server error");

        CD_free(text);

        if (input) {
            json_delete(input);
        }

        if (output) {
            json_delete(output);
        }

        return;
    }     
}
#endif

static
void
cd_StaticRequest (struct evhttp_request* request, CDHTTPd* self)
{
    int         error   = HTTP_OK;
    const char* message = "OK";

    const char*        uri     = evhttp_request_get_uri(request);
    struct evhttp_uri* decoded = evhttp_uri_parse(uri);

    if (evhttp_request_get_command(request) != EVHTTP_REQ_GET) {
        error   = HTTP_BADMETHOD;
        message = "Invalid request method";

        goto end;
    }

    if (!decoded || strstr(evhttp_uri_get_path(decoded), "..")) {
        error   = HTTP_BADREQUEST;
        message = "Bad request";

        goto end;
    }

    DO {
        CDString* path = CD_CreateStringFromFormat("%s/%s", self->config.root,
            evhttp_uri_get_path(decoded) ? evhttp_uri_get_path(decoded) : "index.html");

        if (CD_IsDirectory(CD_StringContent(path))) {
            CD_AppendCString(path, "/index.html");
        }

        if (!CD_IsReadable(CD_StringContent(path))) {
            error   = HTTP_NOTFOUND;
            message = "File not found";

            CD_DestroyString(path);

            goto end;
        }

        struct evbuffer* buffer = evbuffer_new();
        int              fd     = open(CD_StringContent(path), O_RDONLY);

        evhttp_add_header(evhttp_request_get_output_headers(request),
            "Content-Type", cd_GuessContentType(CD_StringContent(path)));

        evbuffer_add_file(buffer, fd, 0, CD_FileSize(CD_StringContent(path)));

        evhttp_send_reply(request, HTTP_OK, "OK", buffer);

        evbuffer_free(buffer);
        CD_DestroyString(path);
    }

    end: {
        if (decoded) {
            evhttp_uri_free(decoded);
        }

        if (error != HTTP_OK) {
            evhttp_send_error(request, error, message);
        }
    }
}

CDHTTPd*
CD_CreateHTTPd (CDPlugin* plugin)
{
    CDHTTPd* self = CD_malloc(sizeof(CDHTTPd));

    if (pthread_attr_init(&self->attributes) != 0) {
        CD_abort("pthread attribute failed to initialize");
    }

    if (pthread_attr_setdetachstate(&self->attributes, PTHREAD_CREATE_DETACHED) != 0) {
        CD_abort("pthread attribute failed to set in detach state");
    }

    self->server      = plugin->server;
    self->event.base  = event_base_new();
    self->event.httpd = evhttp_new(self->event.base);
    
    #ifdef HAVE_JSON
    CD_EventProvides(self->server, "RPC.JSON", CD_CreateEventParameters("json_t", "json_t", NULL));
    evhttp_set_cb(self->event.httpd, "/rpc/json", (void (*)(struct evhttp_request*, void*)) cd_JSONRequest, self);
    #endif

    evhttp_set_gencb(self->event.httpd, (void (*)(struct evhttp_request*, void*)) cd_StaticRequest, self);

    DO {
        self->config.connection.bind.ipv4 = "127.0.0.1";
        self->config.connection.bind.ipv6 = "::1";
        self->config.connection.port      = 25566;
        self->config.root                 = "/usr/share/craftd/htdocs";

        C_SAVE(C_PATH(plugin->config, "root"), C_STRING, self->config.root);

        C_IN(connection, C_ROOT(plugin->config), "connection") {
            C_SAVE(C_GET(connection, "port"), C_INT, self->config.connection.port);

            C_IN(bind, connection, "bind") {
                C_SAVE(C_GET(bind, "ipv4"), C_STRING, self->config.connection.bind.ipv4);
                C_SAVE(C_GET(bind, "ipv6"), C_STRING, self->config.connection.bind.ipv6);
            }
        }

    }

    return self;
}

void
CD_DestroyHTTPd (CDHTTPd* self)
{
    assert(self);

    CD_StopHTTPd(self);

    if (self->event.base) {
        event_base_free(self->event.base);
        self->event.base = NULL;
    }

    if (self->event.httpd) {
        evhttp_free(self->event.httpd);
        self->event.httpd = NULL;
    }

    CD_free(self);
}

void*
CD_RunHTTPd (CDHTTPd* self)
{
    self->event.handle = evhttp_bind_socket_with_handle(self->event.httpd,
        self->config.connection.bind.ipv4,
        self->config.connection.port);

    SLOG(self->server, LOG_NOTICE, "Started HTTPd at http://%s:%d",
        self->config.connection.bind.ipv4,
        self->config.connection.port);

    CD_EventDispatch(self->server, "HTTPd.start!", self);

    event_base_loop(self->event.base, 0);

    CD_EventDispatch(self->server, "HTTPd.stopped", self);

    return NULL;
}

bool
CD_StopHTTPd (CDHTTPd* self)
{
    struct timeval interval = { 0, 0 };

    CD_EventDispatch(self->server, "HTTPd.stop!", self);

    return event_base_loopexit(self->event.base, &interval);
}
