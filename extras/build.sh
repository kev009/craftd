#!/bin/sh

CRAFTDDIR=$(pwd)/craftd
SRCDIR=$CRAFTDDIR/src
LIBDIR=$CRAFTDDIR/lib
DATADIR=$CRAFTDDIR/data
EXAMPLESDIR=$CRAFTDDIR/examples
LIBEVENT2VER=libevent-2.0.10-stable
LIBEVENT2DIR=$SRCDIR/$LIBEVENT2VER
JANSSONVER=jansson-2.0
JANSSONDIR=$SRCDIR/$JANSSONVER

echo "Creating directories..."
mkdir -p $SRCDIR $LIBDIR $DATADIR $EXAMPLESDIR || exit 1

echo "Downloading $LIBEVENT2VER..."
cd $SRCDIR
wget -q http://monkey.org/~provos/$LIBEVENT2VER.tar.gz || exit 1
tar xzf $LIBEVENT2VER.tar.gz || exit 1

echo "Preparing $LIBEVENT2VER..."
cd $LIBEVENT2DIR
./configure -q || exit 1

echo "Building $LIBEVENT2VER..."
make >/dev/null 2>&1 || exit 1
cp .libs/*.so* $LIBDIR/ || exit 1

cd $SRCDIR
echo "Downloading $JANSSONVER..."
wget -q http://www.digip.org/jansson/releases/$JANSSONVER.tar.gz || exit 1
tar zxf $JANSSONVER.tar.gz || exit 1

echo "Preparing $JANSSONVER..."
cd $JANSSONVER
./configure -q || exit 1

echo "Building $JANSSONVER..."
make >/dev/null 2>&1 || exit 1
cp src/.libs/*.so* $LIBDIR/ || exit 1

echo "Fetching latest craftd from git repository..."
cd $SRCDIR
git clone -q git://github.com/kev009/craftd.git || exit 1

echo "Building craftd..."
cd $SRCDIR/craftd
./autogen.sh >/dev/null 2>&1 || exit 1
export libevent2_pthreads_LIBS="-levent -levent_pthreads -L$LIBDIR"
export libevent2_pthreads_CFLAGS="-I$LIBEVENT2DIR/include"
export jansson_LIBS="-ljansson -L$LIBDIR"
export jansson_CFLAGS="-I$JANSSONDIR/src"

./configure -q --prefix=$CRAFTDDIR --bindir=$CRAFTDDIR --sysconfdir=$EXAMPLESDIR --datarootdir=$DATADIR || exit 1
make >/dev/null 2>&1 || exit 1
make install >/dev/null 2>&1 || exit 1

echo "Creating config..."
cp $EXAMPLESDIR/craftd/craftd.conf.dist $CRAFTDDIR/craftd.conf
cd $CRAFTDDIR

echo "Done"
echo
echo "--------------------------------------------------"
echo "Start craftd with: LD_LIBRARY_PATH=lib ./craftd"
echo "--------------------------------------------------"
