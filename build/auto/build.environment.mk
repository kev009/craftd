AM_LIBS = $(libevent2_pthreads_LIBS) $(jansson_LIBS) $(PCRE_LIBS) $(PTHREAD_LIBS) $(LIBS)
AM_CFLAGS = $(PTHREAD_CFLAGS)
AM_CPPFLAGS = $(libevent2_pthreads_CFLAGS) $(jansson_CFLAGS)
AM_CPPFLAGS += $(PCRE_CFLAGS)
AM_CPPFLAGS += -I$(top_srcdir)/include -I$(top_builddir)/include

bstringdir = $(top_srcdir)/third-party/bstring
plugindir = $(top_srcdir)/plugins

