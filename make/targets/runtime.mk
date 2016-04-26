# Runtime system

# -----------------------------------------------------------------------------
# Runtime Core Salt sources.
salt-runtime_dcs = \
	$(shell find packages/ddc-code/salt/runtime${BITS}     -name "*.dcs")


# Runtime C sources.
salt-runtime_c   = \
        $(shell find packages/ddc-code/sea/primitive           -name "*.c")


# Runtime object file outputs.
salt-runtime_o   = \
        $(patsubst %.dcs,%.o,$(salt-runtime_dcs)) \
        $(patsubst %.c,%.o,$(salt-runtime_c))


# Link the static runtime.
packages/ddc-code/build/libddc-runtime.a : $(salt-runtime_o)
	@echo "* Linking $@"
	@ar r $@ $^


# Link the dynamic runtime.
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
.PHONY : clean-runtime
clean-runtime :
	@echo "* Cleaning runtime"
	@rm packages/ddc-code/build/*

