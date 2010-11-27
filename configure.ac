# craftd configure.ac
AC_PREREQ([2.64])

AC_INIT([craftd], [0.1a], [kevin.bowling@kev009.com])

AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([foreign -Wall -Werror dist-bzip2])
AC_CONFIG_MACRO_DIR([m4])

# Don't look for C++ or Fortran compilers
m4_undefine([AC_PROG_CXX])
m4_defun([AC_PROG_CXX],[])
m4_undefine([AC_PROG_F77])
m4_defun([AC_PROG_F77],[])

AC_PROG_CC_C99

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([Makefile src/Makefile])
AC_CONFIG_SRCDIR([src/mapchunk.h])

# Checks for libraries.
AX_LIB_SOCKET_NSL
AX_CHECK_ZLIB
AX_PTHREAD([], AC_MSG_ERROR([pthreads is required]))

PKG_CHECK_MODULES([libevent2_pthreads], [libevent_pthreads >= 2.0.0],
, ,
AC_MSG_ERROR([System libevent2_pthreads not found.  Install it and specify the PKG_CONFIG_PATH. ]))

# Establish default CFLAGS and pthread portability
CFLAGS="$CFLAGS -Wall"
CC="$PTHREAD_CC"
PTHREAD_LIBS="$PTHREAD_LIBS"
PTHREAD_CFLAGS="$PTHREAD_CFLAGS"
# Force pthread for everything?
#LIBS="$PTHREAD_LIBS $LIBS"
#CFLAGS="$CFLAGS $PTHREAD_CFLAGS -Wall"
#CC="$PTHREAD_CC"

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h netdb.h netinet/in.h stdlib.h string.h sys/socket.h unistd.h endian.h])

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

AC_C_BIGENDIAN
AC_CHECK_DECLS([be64toh, htobe64], [], [],[#include <endian.h>])
# Check if we need to reorder float and double types
AX_C_FLOAT_WORDS_BIGENDIAN

# Checks for library functions.
AC_CHECK_FUNCS([socket])

AC_OUTPUT
