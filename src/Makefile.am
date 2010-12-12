AM_LIBS = $(PTHREAD_LIBS) $(LIBS)
AM_CFLAGS = $(PTHREAD_CFLAGS)
AM_CPPFLAGS = $(libevent2_pthreads_CFLAGS)

# Include jansson headers
AM_CPPFLAGS += -I$(srcdir)/../third-party/jansson/src

bin_PROGRAMS = craftd
craftd_SOURCES = craftd.c craftd.h
craftd_SOURCES += craftd-config.c craftd-config.h
craftd_SOURCES += network/network.h network/network-private.h network/packets.h
craftd_SOURCES += network/worker.c network/decoder.c network/send.c
craftd_SOURCES += network/length.c
craftd_SOURCES += util.c util.h version.c javaendian.h
craftd_SOURCES += timeloop.c timeloop.h
craftd_SOURCES += mapchunk.c mapchunk.h
craftd_SOURCES += httpd.c httpd.h
craftd_LDADD = $(libevent2_pthreads_LIBS) $(PTHREAD_LIBS) \
  $(srcdir)/../third-party/jansson/src/.libs/libjansson.a
