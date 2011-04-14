SUBDIRS = survival/mapgen/noise

pkglib_LTLIBRARIES = libsurvival.tests.la libsurvival.base.la libsurvival.persistence.nbt.la libsurvival.mapgen.classic.la libsurvival.mapgen.trivial.la libhttpd.la
# BROKEN: libsvcmdadmin.la

libsurvival_tests_la_SOURCES = survival/tests/main.c survival/tests/tinytest/tinytest.c survival/tests/tinytest/tinytest.h survival/tests/tinytest/tinytest_macros.h
libsurvival_tests_la_CPPFLAGS = $(AM_CPPFLAGS) -Isurvival/tests
libsurvival_tests_la_LDFLAGS = -version-info=0:0:0

libsurvival_base_la_SOURCES = survival/base/main.c
libsurvival_base_la_LDFLAGS = -version-info=0:0:0
libsurvival_base_la_LIBS = $(AM_LIBS) $(jansson_LIBS)
EXTRA_DIST = survival/base/callbacks.c

# admin mod code is currently broken
#libsvcmdadmin_la_SOURCES = survival/commands/admin/main.c 
#survival/commands/admin/src/auth.c survival/commands/admin/src/ticket.c survival/commands/admin/src/player.c  survival/commands/admin/src/workers.c
#libsvcmdadmin_la_LDFLAGS = -version-info=0:0:0
#EXTRA_DIST += survival/commands/admin/src

libsurvival_persistence_nbt_la_SOURCES = survival/persistence/nbt/main.c survival/persistence/nbt/src/itoa.c survival/persistence/nbt/include/itoa.h survival/persistence/nbt/include/nbt.h survival/persistence/nbt/cNBT/nbt_loading.c survival/persistence/nbt/cNBT/nbt.h
libsurvival_persistence_nbt_la_CPPFLAGS = $(AM_CPPFLAGS) -Isurvival/persistence/nbt/include -Isurvival/persistence/nbt

# Classic map generator
libsurvival_mapgen_classic_la_SOURCES = survival/mapgen/classic/main.c
libsurvival_mapgen_classic_la_CPPFLAGS = $(AM_CPPFLAGS) -Isurvival/mapgen
libsurvival_mapgen_classic_la_LIBADD = survival/mapgen/noise/libnoise_simplex.la -lm
libsurvival_mapgen_classic_la_LDFLAGS = -version-info 0:0:0

# Trivial map generator
libsurvival_mapgen_trivial_la_SOURCES = survival/mapgen/trivial/main.c
libsurvival_mapgen_trivial_la_CPPFLAGS = $(AM_CPPFLAGS) -Isurvival/mapgen
libsurvival_mapgen_trivial_la_LIBADD = survival/mapgen/noise/libnoise_simplex.la -lm
libsurvival_mapgen_trivial_la_LDFLAGS = -version-info 0:0:0

libhttpd_la_SOURCES = httpd/main.c httpd/src/HTTPd.c httpd/include/HTTPd.h
libhttpd_la_CPPFLAGS = $(AM_CPPFLAGS) -Ihttpd
libhttpd_la_LDFLAGS = -version-info 0:0:0

# Global build envs
include $(top_srcdir)/build/auto/build.mk
