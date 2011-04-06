pkglib_LTLIBRARIES = libsvtests.la libsvbase.la #libsvcmdadmin.la

libsvtests_la_SOURCES = survival/tests/main.c survival/tests/tinytest/tinytest.c survival/tests/tinytest/tinytest.h survival/tests/tinytest/tinytest_macros.h
libsvtests_la_CPPFLAGS = -Isurvival/tests -I$(top_srcdir)/include -I$(top_builddir)/include
libsvtests_la_LDFLAGS = -version-info=0:0:0

libsvbase_la_SOURCES = survival/base/main.c
libsvbase_la_LDFLAGS = -version-info=0:0:0
EXTRA_DIST = survival/base/callbacks.c

#libsvcmdadmin_la_SOURCES = survival/commands/admin/main.c survival/commands/admin/src/auth.c survival/commands/admin/src/ticket.c survival/commands/admin/src/player.c  survival/commands/admin/src/workers.c
#libsvcmdadmin_la_LDFLAGS = -version-info=0:0:0

# Global build envs
include $(top_srcdir)/build/auto/build.mk
