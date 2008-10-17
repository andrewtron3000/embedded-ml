CFLAGS = -Wall
INCLUDES = -I/project/Phidgetlinux/x86-version/include/ -I../runtime/
LIBS = -L/project/Phidgetlinux/x86-version/lib/

all: ffi.o socket_ffi.o descriptorio_ffi.o clock_ffi.o
	ar rS libffi.a $^
	ranlib libffi.a

ffi.o : ffi.c
	gcc $(CFLAGS) $(INCLUDES) $(LIBS) -c ffi.c

phidgets_ffi.o : phidgets_ffi.c
	gcc $(CFLAGS) $(INCLUDES) $(LIBS) -c phidgets_ffi.c

socket_ffi.o : socket_ffi.c
	gcc $(CFLAGS) $(INCLUDES) $(LIBS) -c socket_ffi.c

descriptorio_ffi.o : descriptorio_ffi.c
	gcc $(CFLAGS) $(INCLUDES) $(LIBS) -c descriptorio_ffi.c

clock_ffi.o : clock_ffi.c
	gcc $(CFLAGS) $(INCLUDES) $(LIBS) -c clock_ffi.c

clean :
	rm -f *.o *.a

