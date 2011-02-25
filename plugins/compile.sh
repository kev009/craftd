${CC:-gcc} -O0 -ggdb3 -fPIC -I../include -I../third-party/klib -I../third-party/bstring -I../third-party/jansson/src -o base.o -c base.c
${CC:-gcc} -shared -Wl,-soname,base.so -o base.so base.o
