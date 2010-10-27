# External libraries

.PHONY	: external
external : external/TinyPTC-X11-0.7.3/xshm.o

external/TinyPTC-X11-0.7.3/xshm.o :
	@echo "* Building external libraries ------------------------------------------------------"
	cd external/TinyPTC-X11-0.7.3; $(MAKE) CFLAGS="$(GCC_FLAGS)"
	@echo
