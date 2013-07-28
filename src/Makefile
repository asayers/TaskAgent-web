CC=ghc
CFLAGS=-O2

compile: Main.hs
	$(CC) -o server $< -O2

compile-dist: Main.hs
	$(CC) -o server $< -O2
	strip -s server
