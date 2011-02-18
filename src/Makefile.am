AM_LIBS = $(libevent2_pthreads_LIBS) $(PTHREAD_LIBS) $(LIBS)
AM_CFLAGS = $(PTHREAD_CFLAGS)
AM_CPPFLAGS = $(libevent2_pthreads_CFLAGS)
AM_CPPFLAGS += -I$(top_srcdir)/third-party/jansson/src
AM_CPPFLAGS += -I$(top_srcdir)/third-party/bstring
AM_CPPFLAGS += -I$(top_srcdir)/third-party/klib
AM_CPPFLAGS += -I$(top_srcdir)/compat/

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
        $(jsondir)/variadic.c
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
craftd_SOURCES += craftd.c craftd.h
craftd_SOURCES += craftd-config.c craftd-config.h
craftd_SOURCES += algos/set.h algos/set.c algos/arith.h
craftd_SOURCES += network/network.h network/network-private.h network/packets.h
craftd_SOURCES += network/worker.c network/decoder.c
craftd_SOURCES += network/send-common.c network/send-game.c
craftd_SOURCES += network/process-common.c network/process-game.c
craftd_SOURCES += network/length.c network/worker-game.c
craftd_SOURCES += util.c util.h version.c javaendian.h
craftd_SOURCES += timeloop.c timeloop.h
craftd_SOURCES += mapchunk.c mapchunk.h
craftd_SOURCES += httpd.c httpd.h
craftd_SOURCES += mapgen/mapgen.c
craftd_LDFLAGS = $(libevent2_pthreads_LIBS)
craftd_LDADD = $(PTHREAD_LIBS) libjansson.la libcdnbt.la

craftd_proxy_SOURCES = $(bstringdir)/bstrlib.c $(bstringdir)/bstrlib.h
craftd_proxy_SOURCES += $(bstringdir)/bstraux.c $(bstringdir)/bstraux.h
craftd_proxy_SOURCES += craftd-proxy.c craftd.h
craftd_proxy_SOURCES += craftd-config.c craftd-config.h
craftd_proxy_SOURCES += algos/set.h algos/set.c algos/arith.h
craftd_proxy_SOURCES += network/network.h network/network-private.h network/packets.h
craftd_proxy_SOURCES += network/worker.c network/decoder.c
craftd_proxy_SOURCES += network/send-common.c network/send-proxy.c
craftd_proxy_SOURCES += network/process-common.c network/process-proxy.c
craftd_proxy_SOURCES += network/length.c network/worker-proxy.c
craftd_proxy_SOURCES += proxy.c proxy.h
craftd_proxy_SOURCES += util.c util.h version.c javaendian.h
craftd_proxy_SOURCES += timeloop.c timeloop.h
craftd_proxy_SOURCES += httpd.c httpd.h
craftd_proxy_LDFLAGS = $(libevent2_pthreads_LIBS)
craftd_proxy_LDADD = $(PTHREAD_LIBS) libjansson.la libcdnbt.la

lib_LTLIBRARIES = libmg_trivial.la libmg_classic.la
libmg_trivial_la_LDFLAGS = $(libevent2_pthreads_LIBS)
libmg_trivial_la_SOURCES = mapgen/trivial.c util.c
libmg_trivial_la_CFLAGS = -D_FIX_UTIL
libmg_trivial_la_LIBADD = libcdnbt.la

libmg_classic_la_LDFLAGS = $(libevent2_pthreads_LIBS) -lm
libmg_classic_la_SOURCES = mapgen/classic.c util.c
libmg_classic_la_CFLAGS = -D_FIX_UTIL
libmg_classic_la_LIBADD = libcdnbt.la noise/libnoise_simplex.la

SUBDIRS=noise
