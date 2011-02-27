AM_LIBS = $(libevent2_pthreads_LIBS) $(PCRE_LIBS) $(PTHREAD_LIBS) $(LIBS)
AM_CFLAGS = $(PTHREAD_CFLAGS)
AM_CPPFLAGS = $(libevent2_pthreads_CFLAGS)
AM_CPPFLAGS += -I$(PCRE_CFLAGS)
AM_CPPFLAGS += -I$(top_srcdir)/third-party/jansson/src
AM_CPPFLAGS += -I$(top_srcdir)/third-party/bstring
AM_CPPFLAGS += -I$(top_srcdir)/third-party/klib
AM_CPPFLAGS += -I$(top_srcdir)/compat/
AM_CPPFLAGS += -I$(top_srcdir)/include

lib_LTLIBRARIES = libcdbase.la

# The base plugin handles default game rules
libcdbase_la_SOURCES = base.c
libcdbase_la_LDFLAGS = -version-info 0:1:0
