# Base libraries.

# -- Find Source Files --------------------------------------------------------
libs_ds	=  $(shell find library -name "*.ds" -follow)

# -- Targets ------------------------------------------------------------------
.PHONY	: libs
libs	: library/Graphics.di

library/Prelude.di library/Graphics.di : bin/ddc $(libs_ds)
	@echo "* Building base library"
	@bin/ddc $(config_ddc_flags) -O -build library/Prelude.ds
	@touch library/Prelude.di

	@echo "* Building graphics library"
	@bin/ddc $(config_ddc_flags) -O -build library/Graphics.ds
	@touch library/Graphics.di
	@echo


# -- clean objects in the runtime system
.PHONY : cleanLibrary
cleanLibrary :
	@echo "* Cleaning library"
	@find library \
		    	-name "*.o" \
		-o	-name "*.dep" \
		-o	-name "*.so" \
		-o  -name "*.dylib" \
		-o	-name "*.a" \
		-o	-name "*.di" \
		-o	-name "*.di-new" \
		-o	-name "*.ddc.c" \
		-o	-name "*.ddc.h" \
		-o	-name "*.ddc.ll" \
		-o	-name "*.ddc.s" \
		-o	-name "*.dump-*"  \
		-follow | xargs -n 1 rm -f
