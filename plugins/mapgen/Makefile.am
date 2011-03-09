AM_LIBS = $(libevent2_pthreads_LIBS) $(PCRE_LIBS) $(PTHREAD_LIBS) $(LIBS)
AM_CFLAGS = $(PTHREAD_CFLAGS)
AM_CPPFLAGS = $(libevent2_pthreads_CFLAGS)
AM_CPPFLAGS += $(PCRE_CFLAGS)
AM_CPPFLAGS += -I$(top_srcdir)/include
AM_CPPFLAGS += -I$(top_srcdir)/include/craftd/jansson # Temprorary for jansson
pkglib_LTLIBRARIES = libcdmapgen.classic.la libcdmapgen.trivial.la

# Static convenience libraries for noise lib
noinst_LTLIBRARIES = libnoise_cell.la libnoise_std.la libnoise_sd.la libnoise_simplex.la libnoise_srd.la
libnoise_cell_la_SOURCES = noise/cellular.c noise/cellular.h
libnoise_std_la_SOURCES = noise/noise1234.c noise/noise1234.h
libnoise_sd_la_SOURCES = noise/sdnoise1234.c noise/sdnoise1234.h
libnoise_simplex_la_SOURCES = noise/simplexnoise1234.c noise/simplexnoise1234.h
libnoise_srd_la_SOURCES = noise/srdnoise23.c noise/srdnoise23.h

# Classic map generator
libcdmapgen_classic_la_SOURCES = mapgen.classic.c
libcdmapgen_classic_la_LIBADD = libnoise_simplex.la -lm
libcdmapgen_classic_la_LDFLAGS = -version-info 0:0:0

# Classic map generator
libcdmapgen_trivial_la_SOURCES = mapgen.trivial.c
libcdmapgen_trivial_la_LIBADD = libnoise_simplex.la -lm
libcdmapgen_trivial_la_LDFLAGS = -version-info 0:0:0
