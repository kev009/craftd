AM_LIBS = $(libevent2_pthreads_LIBS) $(PCRE_LIBS) $(PTHREAD_LIBS) $(LIBS)
AM_CFLAGS = $(PTHREAD_CFLAGS)
AM_CPPFLAGS = $(libevent2_pthreads_CFLAGS)
AM_CPPFLAGS += $(PCRE_CFLAGS)
AM_CPPFLAGS += -I$(top_srcdir)/include
AM_CPPFLAGS += -I$(top_srcdir)/include/craftd/jansson # Temprorary for jansson

SUBDIRS = mapgen

# The list of core libraries that are always built
pkglib_LTLIBRARIES = libcdbase.la libcdnbt.la libcdtests.la

# The base plugin handles default game rules
libcdbase_la_SOURCES = base/base.c
libcdbase_la_LDFLAGS = -version-info 0:0:0

# cdnbt loads Notch's NBT region file format
libcdnbt_la_SOURCES = nbt/cdnbt.c nbt/nbt.c nbt/nbt.h nbt/itoa.c nbt/itoa.h
libcdnbt_la_LDFLAGS = -version-info 0:0:0

# The base plugin handles default game rules
libcdtests_la_SOURCES = tests/tests.c tests/tinytest/tinytest.c tests/tinytest/tinytest.h tests/tinytest/tinytest_macros.h
libcdtests_la_CFLAGS  = -Itests/tinytest
libcdtests_la_LDFLAGS = -version-info 0:0:0
