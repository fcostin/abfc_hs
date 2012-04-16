all:	build/main.out
.IGNORE: all

clean:
	rm -rf build/*.out
	rm	-rf *.hi
	rm	-rf *.o
	rm	-rf Abfc/*.hi
	rm	-rf Abfc/*.o
.IGNORE: clean

build/%.out:	%.hs
	ghc -o $@ $^
