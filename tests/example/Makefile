sandmark: main.c
	../../mlc.exe -cbackend sandmark.uml
	gcc -m32 -Wall runtime-c.c sandmark.c main.c

clean:
	rm -f a.out *.~ *.o sandmark.c sandmark.h *.cps *.cpsa runtime-c.c runtime-c.h
