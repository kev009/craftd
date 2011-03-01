AM_LIBS = $(libevent2_pthreads_LIBS) $(PCRE_LIBS) $(PTHREAD_LIBS) $(LIBS)
AM_CFLAGS = $(PTHREAD_CFLAGS)
AM_CPPFLAGS = $(libevent2_pthreads_CFLAGS)
AM_CPPFLAGS += $(PCRE_CFLAGS)
AM_CPPFLAGS += -I$(top_srcdir)/compat/
AM_CPPFLAGS += -I$(top_srcdir)/include

# The list of core libraries that are always built
pkglib_LTLIBRARIES = libcdbase.la libcdnbt.la

# The base plugin handles default game rules
libcdbase_la_SOURCES = base.c
libcdbase_la_LDFLAGS = -version-info 0:0:0

# cdnbt loads Notch's NBT region file format
libcdnbt_la_SOURCES = cdnbt/cdnbt.c cdnbt/nbt.c cdnbt/nbt.h cdnbt/itoa.c cdnbt/itoa.h
libcdnbt_la_LDFLAGS = -version-info 0:0:0
