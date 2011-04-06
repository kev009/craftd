AM_LIBS = $(libevent2_pthreads_LIBS) $(jansson_LIBS) $(PCRE_LIBS) $(PTHREAD_LIBS) $(LIBS)
AM_CFLAGS = $(PTHREAD_CFLAGS)
AM_CPPFLAGS = $(libevent2_pthreads_CFLAGS) $(jansson_CFLAGS)
AM_CPPFLAGS += $(PCRE_CFLAGS)
AM_CPPFLAGS += -I$(top_srcdir)/include -I$(top_builddir)/include
#AM_CPPFLAGS += -I$(top_srcdir)/third-party/bstring
#AM_CPPFLAGS += -I$(top_srcdir)/third-party/klib

bstringdir = $(top_srcdir)/third-party/bstring

bin_PROGRAMS = craftd

plugindir = $(top_srcdir)/plugins
craftd_SOURCES = $(bstringdir)/bstrlib.c $(bstringdir)/bstrlib.h
craftd_SOURCES += $(bstringdir)/bstraux.c $(bstringdir)/bstraux.h

# Add in lexicographic order:
#
# craftd_SOURCES += 
# ls *.c | awk '{ print $1" \\" }' | sort
# truncate last \
#

craftd_SOURCES += Buffer.c \
		  Buffers.c \
		  Client.c \
		  Config.c \
		  Console.c \
		  ConsoleLogger.c \
		  craftd.c \
		  Dynamic.c \
		  Error.c \
		  Event.c \
		  Hash.c \
		  HTTPd.c \
		  Job.c \
		  List.c \
		  Logger.c \
		  Map.c \
		  Plugin.c \
		  Plugins.c \
		  Regexp.c \
		  ScriptingEngine.c \
		  ScriptingEngines.c \
		  Server.c \
		  Set.c \
		  String.c \
		  SystemLogger.c \
		  TimeLoop.c \
		  utils.c \
		  Worker.c \
		  Workers.c

craftd_LDFLAGS = $(libevent2_pthreads_LIBS) $(jansson_LIBS) -export-dynamic
craftd_LDADD = $(PTHREAD_LIBS)
