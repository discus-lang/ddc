# Runtime system

# -----------------------------------------------------------------------------
# Runtime for alpha compiler
# Find source files for the runtime system.
runtime_c = \
	$(shell ls   packages/ddc-alpha/runtime/*.c) \
	$(shell find packages/ddc-alpha/runtime/Foreign/C -name "*.c") \
	$(shell find packages/ddc-alpha/runtime/Prim -name "*.c") \
	$(shell find packages/ddc-alpha/runtime/Storage -name "*.c") \
	$(shell find packages/ddc-alpha/runtime/Debug -name "*.c")

runtime_dep	= $(patsubst %.c,%.dep,$(runtime_c))
runtime_o	= $(patsubst %.c,%.o,$(runtime_c))


# Link runtime libraries
packages/ddc-alpha/runtime/libddc-runtime.a  : $(runtime_o)
	@echo "* Linking $@"
	@ar r $@ $^

packages/ddc-alpha/runtime/libddc-runtime.$(SHARED_SUFFIX) : $(runtime_o)
	@echo "* Linking $@"
	@$(GCC_LINK_SHARED) -o $@ $^


# -----------------------------------------------------------------------------
# Runtime for new compiler
salt-runtime_dcl = \
        $(shell find packages/ddc-code/lite/base               -name "*.dcl")

salt-runtime_dcs = \
	$(shell find packages/ddc-code/salt/runtime${BITS}     -name "*.dcs")

salt-runtime_c   = \
        $(shell find packages/ddc-code/sea/primitive           -name "*.c")

salt-runtime_o   = \
        $(patsubst %.dcl,%.o,$(salt-runtime_dcl)) \
        $(patsubst %.dcs,%.o,$(salt-runtime_dcs)) \
        $(patsubst %.c,%.o,$(salt-runtime_c))

packages/ddc-code/build/libddc-runtime.a : $(salt-runtime_o)
	@echo "* Linking $@"
	@ar r $@ $^

packages/ddc-code/build/libddc-runtime.$(SHARED_SUFFIX) : $(salt-runtime_o)
	@echo "* Linking $@"
	@$(GCC_LINK_SHARED) -o $@ $^


# -----------------------------------------------------------------------------
# Build runtime system.
#   The shared runtime is only built if SHARED_SUFFIX is defined.
.PHONY  : runtime
runtime : $(runtime_dep) \
		packages/ddc-alpha/runtime/libddc-runtime.a \
		$(if $(SHARED_SUFFIX),packages/ddc-alpha/runtime/libddc-runtime.$(SHARED_SUFFIX),) \
		packages/ddc-code/build/libddc-runtime.a \
		$(if $(SHARED_SUFFIX),packages/ddc-code/build/libddc-runtime.$(SHARED_SUFFIX),)

.PHONY  : runtime-new
runtime-new : $(runtime_dep) \
                packages/ddc-code/build/libddc-runtime.a \
                $(if $(SHARED_SUFFIX),packages/ddc-code/build/libddc-runtime.$(SHARED_SUFFIX),)


# Clean objects in the runtime system
.PHONY : cleanRuntime
cleanRuntime :
	@echo "* Cleaning runtime"
	@find packages/ddc-alpha/runtime \
		    	-name "*.o" \
		-o	-name "*.dep" \
		-o	-name "*.so" \
		-o	-name "*.dylib" \
		-o	-name "*.a" \
		-o	-name "*~" \
		-follow | xargs -n 1 rm -f
