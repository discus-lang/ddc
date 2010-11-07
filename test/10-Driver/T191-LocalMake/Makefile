
top = ../../../

DDC = $(top)bin/ddc -basedir $(top)

all : Main.bin

clean :
	rm -f *.o *.ddc.* *.di *.bin *.stdout

check : Main.sea.bin
	./Main.sea.bin > Main.sea.stdout
	cmp Main.sea.stdout Main.stdout.check

Main.sea.bin: Main.ds
	$(DDC) --make $< -o $@

