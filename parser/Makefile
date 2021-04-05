ALL: packcc main
	./main example.zz



main: main.c
	$(CC) -g $< -o $@


main.c: main.peg
	../packcc/build/gcc/release/bin/packcc main.peg

packcc: ../packcc/build/gcc/release/bin/packcc
