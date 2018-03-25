# Runtime system.

# Runtime Core Salt sources.
salt-runtime_dcs = \
	$(shell find src/s2/ddc-runtime/salt/runtime        -name "*.dcs") \
	$(shell find src/s2/ddc-runtime/salt/runtime${BITS} -name "*.dcs")


# Runtime C sources.
salt-runtime_c   = \
        $(shell find src/s2/ddc-runtime/sea/runtime         -name "*.c")


# Runtime object file outputs.
salt-runtime_o   = \
        $(patsubst %.dcs,%.o,$(salt-runtime_dcs)) \
        $(patsubst %.c,%.o,$(salt-runtime_c))


# Call ddc to build its own runtime.
#  The ddc executable itself knows what source files it needs to build
#  to create the runtime, as it uses this during the cabal-install process.
#  We place a dependency on all the source files so it gets rebuilt
#  if we touch any of them. The list of source files needs to match the
#  ones backed into ddc.
src/s2/ddc-runtime/build/libddc-runtime.a : \
 bin/ddc $(salt-runtime_c) $(salt-runtime_dcs)
	@echo "* Building $@"
	@bin/ddc -basebuild


# Dummy rule for the shared runtime.
#  The ddc -basebuild command also creates this when generating the static
#  runtime above, but we print the file path to console anyway.
src/s2/ddc-runtime/build/libddc-runtime.$(SHARED_SUFFIX) : \
 src/s2/ddc-runtime/build/libddc-runtime.a
	@echo "* Building $@"


# Build runtime system.
#   The shared runtime is only built if SHARED_SUFFIX is defined.
.PHONY  : runtime
runtime : $(runtime_dep) \
                src/s2/ddc-runtime/build/libddc-runtime.a \
                $(if $(SHARED_SUFFIX),src/s2/ddc-runtime/build/libddc-runtime.$(SHARED_SUFFIX),)


# Clean objects in the runtime system
.PHONY : clean-runtime
clean-runtime :
	@echo "* Cleaning runtime"
	@rm -f src/s2/ddc-runtime/build/*

