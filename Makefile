DASM=LuaJIT/dynasm

all: bench.bin ungz.bin

clean:
	rm pigz_s.h
	rm pigz_o.s
	rm *.o
	rm *.bin

pigz_s.h: pigz.s
	luajit $(DASM)/dynasm.lua -o pigz_s.h -F pigz.s

test.o: test.c test.h pigz.h
	gcc -c -g -o test.o test.c

assembler.bin: assembler.c pigz_s.h test.o
	gcc -o assembler.bin -g -I $(DASM) assembler.c test.o

pigz.o: assembler.bin
	./assembler.bin >/dev/null
	gcc -c -o pigz.o pigz_o.s

ungz.bin: pigz.o pigz.h ungz.c
	gcc -o ungz.bin -O2 ungz.c pigz.o

bench.bin: pigz.o pigz.h bench.c
	gcc -o bench.bin -O2 bench.c pigz.o
