
top = ../../../../

DDC = $(top)bin/ddc -basedir $(top)

export DYLD_LIBRARY_PATH = $(top)runtime

all : Main.bin

clean :
	echo buildDir = ${BUILDDIR}
	rm -f ${BUILDDIR}/*

check : ${BUILDDIR}/Main.sea.bin
	${BUILDDIR}/Main.sea.bin > ${BUILDDIR}/Main.sea.stdout
	cmp ${BUILDDIR}/Main.sea.stdout Main.stdout.check

${BUILDDIR}/Main.sea.bin: Main.ds
	$(DDC) --make $< -o $@ -outputdir ${BUILDDIR}

