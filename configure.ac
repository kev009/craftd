# craftd configure.ac
AC_PREREQ([2.64])

AC_INIT([craftd], [0.1a], [kevin.bowling@kev009.com])

AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([foreign -Wall -Werror dist-bzip2])
AC_CONFIG_MACRO_DIR([m4])

# Build bundled libjansson
#AC_CONFIG_SUBDIRS([third-party/jansson])

# Don't look for C++ or Fortran compilers
m4_undefine([AC_PROG_CXX])
m4_defun([AC_PROG_CXX],[])
m4_undefine([AC_PROG_F77])
m4_defun([AC_PROG_F77],[])

AC_USE_SYSTEM_EXTENSIONS
AC_PROG_CC_C99
AC_PROG_LIBTOOL
AC_SUBST(LIBTOOL_DEPS)
#AC_LTDL_DLLIB

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile src/Makefile htdocs/Makefile \
                 third-party/jansson/src/jansson_config.h \
		 src/noise/Makefile])
AC_CONFIG_SRCDIR([src/mapchunk.h])

# Checks for libraries.
AC_CHECK_LIB([ltdl], [lt_dlopen])
AX_LIB_SOCKET_NSL
AX_CHECK_ZLIB
AX_PTHREAD([], AC_MSG_ERROR([pthreads is required]))

PKG_CHECK_MODULES([libevent2_pthreads], [libevent_pthreads >= 2.0.9],
, ,
AC_MSG_ERROR([System libevent2_pthreads not found.  Install it and specify the PKG_CONFIG_PATH. ]))

# Establish default CFLAGS and pthread portability
AX_CFLAGS_WARN_ALL
CC="$PTHREAD_CC"
PTHREAD_LIBS="$PTHREAD_LIBS"
PTHREAD_CFLAGS="$PTHREAD_CFLAGS"
# Force pthread for everything?
#LIBS="$PTHREAD_LIBS $LIBS"
#CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
#CC="$PTHREAD_CC"

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h netdb.h netinet/in.h stdlib.h string.h \
                  sys/socket.h unistd.h endian.h sys/endian.h])

# Define libjansson stuff
AC_TYPE_LONG_LONG_INT
case $ac_cv_type_long_long_int in
     yes) json_have_long_long=1;;
     *) json_have_long_long=0;;
esac
AC_SUBST([json_have_long_long])

AC_C_INLINE
case $ac_cv_c_inline in
    yes) json_inline=inline;;
    no) json_inline=;;
    *) json_inline=$ac_cv_c_inline;;
esac
AC_SUBST([json_inline])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_TYPE_INT16_T
AC_TYPE_INT32_T
AC_TYPE_INT64_T
AC_TYPE_INT8_T
AC_TYPE_UINT64_T
AC_TYPE_UINT32_T
AC_TYPE_UINT16_T
AC_TYPE_UINT8_T
AC_TYPE_SIZE_T

AC_C_BIGENDIAN
AC_CHECK_DECLS([be64toh,htobe64], [], [],
[#ifdef HAVE_ENDIAN_H
#include <endian.h>
#endif
#ifdef HAVE_SYS_ENDIAN_H
#include <sys/endian.h>
#endif
])

AC_CHECK_MEMBERS([struct sigaction.sa_handler], [], [],
[#include <signal.h>
])

# Check if we need to reorder float and double types
AX_C_FLOAT_WORDS_BIGENDIAN

# Checks for library functions.
AC_CHECK_FUNCS([socket])

AC_ARG_ENABLE(gameserver, AC_HELP_STRING([--enable-gameserver],
              [enable the craftd minecraft server (default is yes)]),
              ac_enable_gameserver=$enableval,
              ac_enable_gameserver=yes)
if test "$ac_enable_gameserver" = yes; then
  AC_DEFINE(USE_CDGAME, 1, [use gameserver])
fi
AM_CONDITIONAL(CDGAME, test $ac_enable_gameserver = yes)

AC_ARG_ENABLE(proxy, AC_HELP_STRING([--enable-proxy],
              [enable the craftd minecraft proxy (default is yes)]),
              ac_enable_proxy=$enableval,
              ac_enable_proxy=yes)
if test "$ac_enable_proxy" = yes; then
  AC_DEFINE(USE_CDPROXY, 1, [use proxy])
fi
AM_CONDITIONAL(CDPROXY, test $ac_enable_proxy = yes)

AC_OUTPUT
