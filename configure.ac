# craftd configure.ac
AC_PREREQ([2.64])

AC_INIT([craftd], [0.2-pre], [kevin.bowling@kev009.com])

AC_CONFIG_AUX_DIR([build/auto/configaux])
AM_INIT_AUTOMAKE([foreign -Wall -Werror subdir-objects dist-bzip2])
AC_CONFIG_MACRO_DIR([build/auto/m4])

# Don't look for C++ or Fortran compilers
m4_undefine([AC_PROG_CXX])
m4_defun([AC_PROG_CXX],[])
m4_undefine([AC_PROG_F77])
m4_defun([AC_PROG_F77],[])

AC_PROG_CC_C99
AM_PROG_CC_C_O
AC_USE_SYSTEM_EXTENSIONS
LT_PREREQ([2.2])
AM_PROG_LIBTOOL
AC_LTDL_DLLIB
AC_SUBST(LIBTOOL_DEPS)
LT_INIT([dlopen])

AC_CONFIG_HEADERS([include/craftd/config.h])
AC_CONFIG_FILES([
                 Makefile
                 src/Makefile 
                 include/Makefile
                 third-party/Makefile
                 plugins/Makefile
                 plugins/survival/mapgen/noise/Makefile
                 ])

AC_CONFIG_SRCDIR([src/craftd.c])

# Checks for libraries.
AC_CHECK_LIB([ltdl], [lt_dlopen])
AX_LIB_SOCKET_NSL
AX_CHECK_ZLIB
AX_PTHREAD([], AC_MSG_ERROR([pthreads is required]))
AX_PATH_LIB_PCRE

PKG_CHECK_MODULES([libevent2_pthreads], [libevent_pthreads >= 2.0.9],
, ,
AC_MSG_ERROR([System libevent2_pthreads not found.  Install it and check the
              PKG_CONFIG_PATH env var. ]))

PKG_CHECK_MODULES([libconfig], [libconfig >= 1.3],
, ,
AC_MSG_ERROR([System libconfig not found.  Install it and check the
              PKG_CONFIG_PATH env var.]))

# Jansson removed from core.  Add --with-json logic later.
# PKG_CHECK_MODULES([jansson], [jansson >= 2.0],
# , ,
# AC_MSG_ERROR([System jansson not found. Install it and check the PKG_CONFIG_PATH
#              env var. ]))


# Establish default CFLAGS and pthread portability
AX_CFLAGS_WARN_ALL
CC="$PTHREAD_CC"
PTHREAD_LIBS="$PTHREAD_LIBS"
PTHREAD_CFLAGS="$PTHREAD_CFLAGS"
# Force pthread for everything?
#LIBS="$PTHREAD_LIBS $LIBS"
CFLAGS="$CFLAGS -Wno-unused-label"
#CC="$PTHREAD_CC"

# Checks for header files.
AC_CHECK_HEADERS([arpa/inet.h netdb.h netinet/in.h stdlib.h string.h \
                  sys/socket.h unistd.h endian.h sys/endian.h ltdl.h])

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
AC_TYPE_INTPTR_T
AC_CHECK_SIZEOF([intptr_t])
AC_CHECK_SIZEOF([fp], [], [typedef void (*fp)();])

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

AC_OUTPUT
