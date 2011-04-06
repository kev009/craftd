pkglib_LTLIBRARIES = libcdtests.la

libcdtests_la_SOURCES = main.c tinytest/tinytest.c tinytest/tinytest.h tinytest/tinytest_macros.h
libcdtests_la_CFLAGS  = -Itests/tinytest
libcdtests_la_LDFLAGS = -version-info 0:0:0


include $(top_srcdir)/build/auto/build.mk
include $(top_srcdir)/build/auto/plugins.mk
