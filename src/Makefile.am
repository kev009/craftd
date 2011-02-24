AM_LIBS = $(libevent2_pthreads_LIBS) $(PTHREAD_LIBS) $(LIBS)
AM_CFLAGS = $(PTHREAD_CFLAGS)
AM_CPPFLAGS = $(libevent2_pthreads_CFLAGS)
AM_CPPFLAGS += -I$(top_srcdir)/third-party/jansson/src
AM_CPPFLAGS += -I$(top_srcdir)/third-party/bstring
AM_CPPFLAGS += -I$(top_srcdir)/third-party/klib
AM_CPPFLAGS += -I$(top_srcdir)/compat/
AM_CPPFLAGS += -I$(top_srcdir)/include

extra_DIST = algos/LICENSE-MIT

# Custom build shim for jansson
jsondir = $(top_srcdir)/third-party/jansson/src
noinst_LTLIBRARIES = libjansson.la libcdnbt.la
libjansson_la_includedir = $(jsondir)/
libjansson_la_SOURCES = \
        $(jsondir)/dump.c \
        $(jsondir)/error.c \
        $(jsondir)/hashtable.c \
        $(jsondir)/hashtable.h \
        $(jsondir)/jansson_private.h \
	$(jsondir)/jansson_config.h \
        $(jsondir)/jansson.h \
        $(jsondir)/load.c \
        $(jsondir)/strbuffer.c \
        $(jsondir)/strbuffer.h \
        $(jsondir)/utf.c \
        $(jsondir)/utf.h \
        $(jsondir)/value.c \
        $(jsondir)/pack_unpack.c
libjansson_la_LDFLAGS = \
        -export-symbols-regex '^json_'

cdnbtdir = nbt
libcdnbt_la_includedir = $(cdnbtdir)/
libcdnbt_la_SOURCES = $(cdnbtdir)/nbt.c $(cdnbtdir)/nbt.h

bstringdir = $(top_srcdir)/third-party/bstring

bin_PROGRAMS = 

if CDGAME
  bin_PROGRAMS += craftd
endif

if CDPROXY
  bin_PROGRAMS += craftd-proxy
endif

craftd_SOURCES = $(bstringdir)/bstrlib.c $(bstringdir)/bstrlib.h
craftd_SOURCES += $(bstringdir)/bstraux.c $(bstringdir)/bstraux.h
craftd_SOURCES += craftd.c Error.c Packet.c Player.c Server.c Worker.c Workers.c minecraft.c
craftd_LDFLAGS = $(libevent2_pthreads_LIBS)
craftd_LDADD = $(PTHREAD_LIBS) libjansson.la libcdnbt.la

craftd_proxy_SOURCES = $(bstringdir)/bstrlib.c $(bstringdir)/bstrlib.h
craftd_proxy_SOURCES += $(bstringdir)/bstraux.c $(bstringdir)/bstraux.h
craftd_proxy_SOURCES += craftd-proxy.c
craftd_proxy_LDFLAGS = $(libevent2_pthreads_LIBS)
craftd_proxy_LDADD = $(PTHREAD_LIBS) libjansson.la libcdnbt.la
