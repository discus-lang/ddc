# Runtime system

# -----------------------------------------------------------------------------
# Runtime for alpha compiler
# Find source files for the runtime system.
runtime_c = \
	$(shell ls runtime/*.c) \
	$(shell find runtime/Foreign/C -name "*.c") \
	$(shell find runtime/Prim -name "*.c") \
	$(shell find runtime/Storage -name "*.c") \
	$(shell find runtime/Debug -name "*.c")

runtime_dep	= $(patsubst %.c,%.dep,$(runtime_c))
runtime_o	= $(patsubst %.c,%.o,$(runtime_c))


# Link runtime libraries
runtime/libddc-runtime.a  : $(runtime_o)
	@echo
	@echo "* Linking $@"
	@ar r $@ $^
	@echo

runtime/libddc-runtime.$(SHARED_SUFFIX) : $(runtime_o)
	@echo
	@echo "* Linking $@"
	@$(GCC_LINK_SHARED) -o $@ $^
	@echo


# -----------------------------------------------------------------------------
# Runtime for new compiler
sea-runtime_dce = $(shell find packages/ddc-core-sea/runtime/src -name "*.dce")
sea-runtime_c   = $(shell find packages/ddc-core-sea/runtime/src -name "*.c")

sea-runtime_o   = $(patsubst %.dce,%.o,$(sea-runtime_dce)) \
		  $(patsubst %.c,%.o,$(sea-runtime_c))

packages/ddc-core-sea/runtime/libddc-runtime.a : $(sea-runtime_o)
	@echo
	@echo "* Linking $@"
	@ar r $@ $^
	@echo

packages/ddc-core-sea/runtime/libddc-runtime.$(SHARED_SUFFIX) : $(sea-runtime_o)
	@echo
	@echo "* Linking $@"
	@$(GCC_LINK_SHARED) -m32 -o $@ $^
	@echo


# -----------------------------------------------------------------------------
# Build runtime system.
#   The shared runtime is only built if SHARED_SUFFIX is defined.
.PHONY  : runtime
runtime : $(runtime_dep) \
		runtime/libddc-runtime.a \
		$(if $(SHARED_SUFFIX),runtime/libddc-runtime.$(SHARED_SUFFIX),) \
		packages/ddc-core-sea/runtime/libddc-runtime.a \
		$(if $(SHARED_SUFFIX),packages/ddc-core-sea/runtime/libddc-runtime.$(SHARED_SUFFIX),)


# Clean objects in the runtime system
.PHONY : cleanRuntime
cleanRuntime :
	@echo "* Cleaning runtime"
	@find runtime \
		    	-name "*.o" \
		-o	-name "*.dep" \
		-o	-name "*.so" \
		-o	-name "*.dylib" \
		-o	-name "*.a" \
		-o	-name "*~" \
		-follow | xargs -n 1 rm -f

	@find packages/ddc-core-sea/runtime \
		        -name "*.o" \
		-o      -name "*.dep" \
		-o      -name "*.so" \
		-o      -name "*.dylib" \
		-o      -name "*.a" \
		-o      -name "*~" \
		-follow | xargs -n 1 rm -f
