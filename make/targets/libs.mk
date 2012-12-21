# Base libraries.

# -- Find Source Files --------------------------------------------------------
libs_ds	=  $(shell find packages/ddc-alpha/library -name "*.ds" -follow)

# -- Targets ------------------------------------------------------------------
.PHONY	: libs
libs	: packages/ddc-alpha/library/Graphics.di

packages/ddc-alpha/library/Prelude.di packages/ddc-alpha/library/Graphics.di : bin/ddc-alpha $(libs_ds)
	@echo "* Building base library"
	@bin/ddc-alpha $(config_ddc_flags) -O -build packages/ddc-alpha/library/Prelude.ds
	@touch packages/ddc-alpha/library/Prelude.di

	@echo
	@echo "* Building graphics library"
	@bin/ddc-alpha $(config_ddc_flags) -O -build packages/ddc-alpha/library/Graphics.ds
	@touch packages/ddc-alpha/library/Graphics.di


# -- clean objects in the runtime system
.PHONY : cleanLibrary
cleanLibrary :
	@echo "* Cleaning library"
	@find packages/ddc-code/build \
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
