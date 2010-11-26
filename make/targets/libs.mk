# Base libraries.

.PHONY	: libs
libs	: library/Graphics.di

library/Prelude.di library/Graphics.di : bin/ddc
	@echo "* Building base libraries ----------------------------------------------------------"
	@bin/ddc -O -build library/Prelude.ds
	@touch library/Prelude.di

	@echo
	@bin/ddc -O -build library/Graphics.ds
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
		-o	-name "*.dump-*"  \
		-follow | xargs -n 1 rm -f
