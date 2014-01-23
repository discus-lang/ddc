# Runtime system

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
                packages/ddc-code/build/libddc-runtime.a \
                $(if $(SHARED_SUFFIX),packages/ddc-code/build/libddc-runtime.$(SHARED_SUFFIX),)


# Clean objects in the runtime system
.PHONY : cleanRuntime
cleanRuntime :
	@echo "* Cleaning runtime"
