AM_LIBS = $(libevent2_pthreads_LIBS) $(jansson_LIBS) $(PCRE_LIBS) $(PTHREAD_LIBS) $(LIBS)
AM_CFLAGS = $(PTHREAD_CFLAGS)
AM_CPPFLAGS = $(libevent2_pthreads_CFLAGS) $(jansson_CFLAGS)
AM_CPPFLAGS += $(PCRE_CFLAGS)
AM_CPPFLAGS += -I$(top_srcdir)/include -I$(top_builddir)/include

SUBDIRS = mapgen

# The list of core libraries that are always built
pkglib_LTLIBRARIES = libcdbeta.la libcdnbt.la libcdadmin.la libcdtests.la

# The beta plugin handles default game rules
libcdbeta_la_SOURCES = beta/main.c beta/Buffer.c beta/Packet.c beta/PacketLength.c beta/Player.c beta/World.c beta/minecraft.c
libcdbeta_la_LDFLAGS = -version-info 0:0:0

# cdnbt loads Notch's NBT region file format
#libcdnbt_la_SOURCES = nbt/cdnbt.c nbt/nbt.c nbt/nbt.h nbt/itoa.c nbt/itoa.h
#libcdnbt_la_LDFLAGS = -version-info 0:0:0

# The admin plugin handles server administration stuff
#libcdadmin_la_SOURCES = admin/admin.c
#libcdadmin_la_LDFLAGS = -version-info 0:0:0

# The test plugin makes various tests on craftd classes/functions
libcdtests_la_SOURCES = tests/tests.c tests/tinytest/tinytest.c tests/tinytest/tinytest.h tests/tinytest/tinytest_macros.h
libcdtests_la_CFLAGS  = -Itests/tinytest
libcdtests_la_LDFLAGS = -version-info 0:0:0
