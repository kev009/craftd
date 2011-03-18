AM_LIBS = $(libevent2_pthreads_LIBS) $(jansson_LIBS) $(PCRE_LIBS) $(PTHREAD_LIBS) $(LIBS)
AM_CFLAGS = $(PTHREAD_CFLAGS)
AM_CPPFLAGS = $(libevent2_pthreads_CFLAGS) $(jansson_CFLAGS)
AM_CPPFLAGS += $(PCRE_CFLAGS)
AM_CPPFLAGS += -I$(top_srcdir)/include -I$(top_builddir)/include
#AM_CPPFLAGS += -I$(top_srcdir)/third-party/bstring
#AM_CPPFLAGS += -I$(top_srcdir)/third-party/klib

bstringdir = $(top_srcdir)/third-party/bstring

bin_PROGRAMS = 

if CDGAME
  bin_PROGRAMS += craftd
endif

#if CDPROXY
#  bin_PROGRAMS += craftd-proxy
#endif

plugindir = $(top_srcdir)/plugins
craftd_SOURCES = $(bstringdir)/bstrlib.c $(bstringdir)/bstrlib.h
craftd_SOURCES += $(bstringdir)/bstraux.c $(bstringdir)/bstraux.h
craftd_SOURCES += craftd.c Buffer.c Buffers.c Config.c Console.c ConsoleLogger.c Event.c Hash.c Job.c List.c Logger.c Map.c Packet.c PacketLength.c Player.c Plugins.c Plugin.c Regexp.c Set.c Server.c SystemLogger.c String.c TimeLoop.c Worker.c Workers.c minecraft.c _memory.c
craftd_LDFLAGS = $(libevent2_pthreads_LIBS) $(jansson_LIBS) -export-dynamic
craftd_LDADD = $(PTHREAD_LIBS)
#-dlopen $(plugindir)/libcdbase.la

#craftd_proxy_SOURCES = $(bstringdir)/bstrlib.c $(bstringdir)/bstrlib.h
#craftd_proxy_SOURCES += $(bstringdir)/bstraux.c $(bstringdir)/bstraux.h
#craftd_proxy_SOURCES += craftd-proxy.c
#craftd_proxy_LDFLAGS = $(libevent2_pthreads_LIBS)
#craftd_proxy_LDADD = $(PTHREAD_LIBS)
