CFLAGS = -Wall
INCLUDES = -I/project/Phidgetlinux/x86-version/include/ -I../runtime/
LIBS = -L/project/Phidgetlinux/x86-version/lib/

all: ffi.o phidgets_ffi.o

ffi.o : ffi.c
	gcc $(CFLAGS) $(INCLUDES) $(LIBS) -c ffi.c

phidgets_ffi.o : phidgets_ffi.c
	gcc $(CFLAGS) $(INCLUDES) $(LIBS) -c phidgets_ffi.c

clean :
	rm -f *.o

