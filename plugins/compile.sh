rm -f base.o base.so
${CC:-gcc} -std=gnu99 -O0 -ggdb3 -fPIC -I.. -I../include -I../third-party/klib -I../third-party/bstring -I../third-party/jansson/src -o base.o -c base.c
${CC:-gcc} -shared -Wl,-soname,base.so -o base.so base.o
