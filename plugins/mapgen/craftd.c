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
 
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <syslog.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/queue.h>
#include <pthread.h>

#include <event2/event.h>
#include <event2/buffer.h>
#include <event2/bufferevent.h>
#include <event2/listener.h>
#include <event2/thread.h>

#include "bstrlib.h"
#include "bstraux.h"

#include "craftd-config.h"

#include "util.h"
#include "mapchunk.h"
#include "network/network.h"
#include "timeloop.h"
#include "httpd.h"
#include "mapgen/mapgen.h"

/**
 * Try and perform cleanup with an atexit call
 */
void
exit_handler(void)
{
  LOG(LOG_INFO, "Exiting.");
  closelog();
}

/**
 * Temporary entity ID creation
 */
int
newEid()
{
  static int eid = 0;
  eid++;
  return eid;
}

void
readcb(struct bufferevent *bev, void *ctx)
{
  struct WQ_entry *workitem;
  struct PL_entry *player = ctx;  
  
  /* Allocate and construct new work item */
  workitem = Malloc(sizeof(struct WQ_entry));
  workitem->bev = bev;
  workitem->player = player;
  workitem->worktype = WQ_GAME_INPUT;
  workitem->workdata = NULL;
  workitem->lock = &player->inlock;
  
  /* Add item to work queue */
  pthread_mutex_lock(&worker_cvmutex);
  STAILQ_INSERT_TAIL(&WQ_head, workitem, WQ_entries);
  
  /* Dispatch to an open worker */
  pthread_cond_signal(&worker_cv);
  pthread_mutex_unlock(&worker_cvmutex);

  return; // Good read
}

void
errorcb(struct bufferevent *bev, short error, void *ctx)
{
    int finished = 0;
    
    // Get player context from linked list 
    struct PL_entry *player = ctx;
    
    if (error & BEV_EVENT_EOF)
    {
        /* Connection closed */
        finished = 1;
    }
    else if (error & BEV_EVENT_ERROR)
    {
        /* Some other kind of error, handle it here by checking errno */
        LOG(LOG_INFO, "libevent: ip %s - %s", player->ip,
            evutil_socket_error_to_string(EVUTIL_SOCKET_ERROR()));
        finished = 1;
    }
    else if (error & BEV_EVENT_TIMEOUT)
    {
        /* Handle timeout somehow */
        LOG(LOG_ERR, "A buf event timeout?");
	finished = 1;
    }

    if (finished)
    {
	/* If the client disconnects, remove any pending WP buffer events */
	struct WQ_entry *workitem, *workitemtmp;
	pthread_mutex_lock(&worker_cvmutex);
        STAILQ_FOREACH_SAFE(workitem, &WQ_head, WQ_entries, workitemtmp)
	{
	  if(workitem->bev == bev)
	  {
	    STAILQ_REMOVE(&WQ_head, workitem, WQ_entry, WQ_entries);
	    LOG(LOG_DEBUG, "bev removed from workerpool by errorcb");

            free(workitem);
	  }
	}

        if (player->username != NULL)
        {
          /* In-band disconnect message */
          bstring dconmsg = bformat("Player %s has left the game.", 
              player->username->data);
          send_syschat(dconmsg);
          bstrFree(dconmsg);

          /* System log message */
          LOG(LOG_INFO, "Connection closed for: %s", player->username->data);

          /* Free the username */
          bstrFree(player->username);
        }
        else
        {
          /* The user hasn't gotten far enough to register a username */
          LOG(LOG_INFO, "Connection closed ip: %s", player->ip);
        }

        //TODO: Add mutual exclusion so a worker doesn't get a null ptr
        //TODO: Convert this to a SLIST_FOREACH
        //XXXX Grab a rdlock until player is found, wrlock delete, free
        pthread_rwlock_wrlock(&PL_rwlock);
	SLIST_REMOVE(&PL_head, ctx, PL_entry, PL_entries);
        --PL_count;
	pthread_rwlock_unlock(&PL_rwlock);
		
        if (ctx)
	  free(ctx);
	if (bev)
	  bufferevent_free(bev);

        pthread_mutex_unlock(&worker_cvmutex);
    }
}

void
do_accept(evutil_socket_t listener, short event, void *arg)
{
  struct event_base *base = arg;
  struct PL_entry *player;

  struct sockaddr_storage ss;
  socklen_t slen = sizeof(ss);
  int fd = accept(listener, (struct sockaddr*)&ss, &slen);
  if (fd < 0)
  {
    ERR("accept error");
  }
  else if (fd > FD_SETSIZE)
  {
    LOG(LOG_CRIT, "too many clients");
    close(fd);
  }
  else
  {
    struct bufferevent *bev;

    /* Allocate space for a new player */
    player = Malloc(sizeof(struct PL_entry));
	
    /* Get the IPv4 or IPv6 address and store it */
    if (getpeername(fd, (struct sockaddr *)&ss, &slen))
    {
      LOG(LOG_ERR, "Couldn't get peer IP");
      close(fd);
      free(player);
      return;
    }

    void *inaddr;
    if (ss.ss_family == AF_INET)
    {
      inaddr = &((struct sockaddr_in*)&ss)->sin_addr;
    }
    else if (ss.ss_family == AF_INET6)
    {
      inaddr = &((struct sockaddr_in6*)&ss)->sin6_addr;
    }
    else
    {
      LOG(LOG_ERR, "weird address family");
      close(fd);
      free(player);
      return;
    }

    /* Get the socket and buffer event into nonblocking mode, register
     * callbacks, set watermarks
     */
    evutil_make_socket_nonblocking(fd);
    bev = bufferevent_socket_new(base, fd, 
                                 BEV_OPT_CLOSE_ON_FREE|BEV_OPT_THREADSAFE);
    bufferevent_setcb(bev, readcb, NULL, errorcb, player);
    //bufferevent_setwatermark(bev, EV_READ, 0, MAX_BUF);
    bufferevent_enable(bev, EV_READ|EV_WRITE);

    /* Get most of the player set up before lock&insert */
    player->fd = fd;
    player->bev = bev;
    player->username = NULL;
    player->eid = newEid();
    player->loadedchunks = Set_new(0, chunkcoordcmp, chunkcoordhash);
    

    evutil_inet_ntop(ss.ss_family, inaddr, player->ip, sizeof(player->ip));

    /* Initialize the player's internal rwlocks */
    pthread_rwlock_init(&player->rwlock, NULL);
    pthread_rwlock_init(&player->position.rwlock, NULL);
    
    /* Initialize the player's IO blocking */
    pthread_rwlock_init(&player->inlock,NULL);
    pthread_rwlock_init(&player->outlock,NULL);

    /* Lock for the list ptr update and add them to the Player List */
    pthread_rwlock_wrlock(&PL_rwlock);
    SLIST_INSERT_HEAD(&PL_head, player, PL_entries);
    ++PL_count;
    pthread_rwlock_unlock(&PL_rwlock);
  }
}

void
run_server(void)
{
    evutil_socket_t listener;
    struct event *listener_event;

    base = event_base_new();
    
    if(MODE == PROXY)
      dns_base = evdns_base_new(base,1);
    
    if (!base)
    {
        LOG(LOG_CRIT, "Could not create MC libevent base!");
        exit(EXIT_FAILURE);
    }

    if ((listener = socket(PF_INET, SOCK_STREAM, 0)) < 0)
    {
        PERR("cannot create socket");
        return;
    }

    evutil_make_socket_nonblocking(listener);

#ifndef WIN32
    {
        int one = 1;
        setsockopt(listener, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
    }
#endif

    if (bind(listener, (struct sockaddr*)&Config.game_bind4, 
      sizeof(Config.game_bind4)) < 0)
    {
        PERR("cannot bind");
        return;
    }

    if (listen(listener, MAX_LISTENBACKLOG) < 0)
    {
        PERR("listen error");
        return;
    }

    listener_event = event_new(base, listener, EV_READ|EV_PERSIST, 
        do_accept, (void*)base);

    event_add(listener_event, NULL);

    event_base_dispatch(base);
}

int
main(int argc, char *argv[])
{
  pthread_t httpd_thread_id;
  pthread_t mg_thread_id;
  pthread_attr_t httpd_thread_attr;
  pthread_t timeloop_thread_id;
  pthread_attr_t timeloop_thread_attr;
  int status = 0;
  // Setup game server specifics
  base = NULL; //initialize eventbase to null
  worker_handler = workergame;
  MODE = GAME;

  openlog(PACKAGE_TARNAME, LOG_PID, LOG_DAEMON);
  atexit(exit_handler);
  //setvbuf(stdout, NULL, _IONBF, 0); // set nonblocking stdout

  /* We initialize with stdout logging until config is loaded and the process
   * daemonizes.
   */
  LOG = &log_console;
  LOG_setlogmask = &log_console_setlogmask;

  /* Print version info */
  craftd_version(argv[0]);

  /* Get command line arguments */
  int opt;
  int dontfork = 0;
  int debugging = 0;
  char *argconfigfile = NULL;
  while((opt = getopt(argc, argv, "c:dhnv")) != -1)
  {
    switch(opt)
    {
      case 'd':  // debugging mode
        debugging = 1;
        break;
      case 'v': // print version
        exit(EXIT_SUCCESS); // Version header already printed
      case 'n': // don't fork or daemonize, use stdout for logging
        dontfork = 1;
        break;
      case 'c': // use the specified config file
        argconfigfile = optarg;
        break;
      case 'h': // print help message
      default:
        fprintf(stderr, "\nUsage: %s [OPTION]...\n"
            "-c <conf file>\tspecify a conf file location\n"
            "-d\t\tenable verbose debugging messages\n"
            "-h\t\tdisplay this help and exit\n"
            "-n\t\tdon't fork/daemonize (overrides config file)\n"
            "-v\t\toutput version information and exit\n"
            "\nFor complete documentation, visit the wiki.\n\n", argv[0]);
        exit(EXIT_FAILURE);
    }
  }

  /* By default, mask debugging messages */
  if (!debugging)
  {
    log_console_setlogmask(LOG_MASK(LOG_DEBUG));
    setlogmask(LOG_MASK(LOG_DEBUG));
  }

  /* Print startup message */
  LOG(LOG_INFO, "Server starting!  Max FDs: %d", sysconf(_SC_OPEN_MAX));

  /* Initialize the configuration */
  craftd_config_setdefaults();
  craftd_config_parse(argconfigfile);
  loadLevelDat();
  craftd_config_readmotd(Config.motd_file);

  /* Tell libevent to use our logging functions */
  event_set_log_callback(ev_log_callback);
  
  /* Declare the worker pool after reading config values */
  pthread_attr_t WP_thread_attr;
  pthread_t WP_thread_id[Config.workpool_size];
  int WP_id[Config.workpool_size];
  
  /* Player List singly-linked list setup */
  // hsearch w/direct ptr hashtable for name lookup if we need faster direct
  // access (keep two ADTs and entries for each player)
  pthread_rwlock_init(&PL_rwlock, NULL);
  SLIST_INIT(&PL_head);
  PL_count = 0;

  /* Work Queue is a singly-linked tail queue for player work requests */
  STAILQ_INIT(&WQ_head);

  if (!dontfork && Config.daemonize == true)
  {
    LOG(LOG_INFO, "Daemonizing.");

    /* Swap over to syslog */
    LOG = &syslog;
    LOG_setlogmask = &setlogmask;
    
    CRAFTD_daemonize(0, 0); // chdir, and close FDs
  }

#ifdef WIN32
  status = evthread_use_windows_threads();
#else
  status = evthread_use_pthreads();
#endif
 
  if(debugging)
    evthread_enable_lock_debuging();

  if(status != 0)
    ERR("Cannot initialize libevent threading");

  /* Start timeloop */
  pthread_attr_init(&timeloop_thread_attr);
  pthread_attr_setdetachstate(&timeloop_thread_attr, PTHREAD_CREATE_DETACHED);
  status = pthread_create(&timeloop_thread_id, &timeloop_thread_attr, 
      run_timeloop, NULL);
  if(status != 0)
    ERR("Cannot start timeloop");
  
  /* Start httpd if it is enabled */
  if (Config.httpd_enabled)
  {
    pthread_attr_init(&httpd_thread_attr);
    pthread_attr_setdetachstate(&httpd_thread_attr, PTHREAD_CREATE_DETACHED);
    status = pthread_create(&httpd_thread_id, &httpd_thread_attr, 
        run_httpd, NULL);
  
    if(status != 0)
      ERR("Cannot start httpd");
  }

  /* Start map generator worker */
  pthread_create(&mg_thread_id, NULL, &mg_run_worker, NULL);

  /* Start packet handler pool */
  pthread_attr_init(&WP_thread_attr);
  pthread_attr_setdetachstate(&WP_thread_attr, PTHREAD_CREATE_DETACHED);
  
  pthread_mutex_init(&worker_cvmutex, NULL);
  status = pthread_cond_init(&worker_cv, NULL);
  if(status !=0)
    ERR("Worker condition var init failed!");
  
  for (int i = 0; i < Config.workpool_size; ++i)
  {
    WP_id[i] = i;
    status = pthread_create(&WP_thread_id[i], &WP_thread_attr,
        run_worker, (void *) &WP_id[i]);
    if(status != 0)
      ERR("Worker pool startup failed!");
  }
  
  worker_init();

  /* Start inbound game server*/
  sleep(1);
  run_server();
  
  return 0;
}
