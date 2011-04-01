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

#include <craftd/HTTPd.h>
#include <craftd/Server.h>

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


static
void
cd_JSONRequest (struct evhttp_request* request, CDServer* server)
{
    json_error_t error;
    json_t*      input  = json_loads((const char*) evbuffer_pullup(evhttp_request_get_input_buffer(request), -1), 0, &error);
    json_t*      output = json_object();

    if (evhttp_request_get_command(request) != EVHTTP_REQ_POST || input == NULL) {
        goto error;
    }

    CD_EventDispatch(server, "RPC.JSON", input, output);

    done: {
        json_delete(input);

        char*           outString = json_dumps(output, JSON_INDENT(2));
        struct evbuffer* outBuffer = evbuffer_new();

        evbuffer_add_printf(outBuffer, "%s", outString);

        evhttp_send_reply(request, HTTP_OK, "OK", outBuffer);

        evbuffer_free(outBuffer);
        free(outString);
        json_delete(output);

        return;
    }

    error: {
        evhttp_send_error(request, HTTP_INTERNAL, "Internal server error");

        if (input) {
            json_delete(input);
        }

        if (output) {
            json_delete(output);
        }

        return;
    }     
}

static
void
cd_StaticRequest (struct evhttp_request* request, CDServer* server)
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
        CDString* path = CD_CreateStringFromFormat("%s/%s", server->config->cache.httpd.root,
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
CD_CreateHTTPd (CDServer* server)
{
    CDHTTPd* self = CD_malloc(sizeof(CDHTTPd));

    if (pthread_attr_init(&self->attributes) != 0) {
        CD_abort("pthread attribute failed to initialize");
    }

    if (pthread_attr_setdetachstate(&self->attributes, PTHREAD_CREATE_DETACHED) != 0) {
        CD_abort("pthread attribute failed to set in detach state");
    }

    self->server      = server;
    self->event.base  = event_base_new();
    self->event.httpd = evhttp_new(self->event.base);
    
    evhttp_set_cb(self->event.httpd, "/rpc/json", (void (*)(struct evhttp_request*, void*)) cd_JSONRequest, server);
    evhttp_set_gencb(self->event.httpd, (void (*)(struct evhttp_request*, void*)) cd_StaticRequest, server);

    return self;
}

void*
CD_RunHTTPd (CDHTTPd* self)
{
    self->event.handle = evhttp_bind_socket_with_handle(self->event.httpd,
        self->server->config->cache.httpd.connection.bind.ipv4,
        self->server->config->cache.httpd.connection.port);

    SLOG(self->server, LOG_NOTICE, "Started HTTPd on %s:%d",
        self->server->config->cache.httpd.connection.bind.ipv4,
        self->server->config->cache.httpd.connection.port);

    event_base_dispatch(self->event.base);

    return NULL;
}
