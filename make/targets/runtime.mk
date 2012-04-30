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
	@echo "* Linking $@"
	@ar r $@ $^

runtime/libddc-runtime.$(SHARED_SUFFIX) : $(runtime_o)
	@echo "* Linking $@"
	@$(GCC_LINK_SHARED) -o $@ $^


# -----------------------------------------------------------------------------
# Runtime for new compiler
salt-runtime_dce = \
	packages/ddc-core-salt/runtime/src/Storage/Object${BITS}.dce \
	$(shell find packages/ddc-core-salt/runtime/src/Primitive -name "*.dce")

salt-runtime_c   = $(shell find packages/ddc-core-salt/runtime/src -name "*.c")

salt-runtime_o   = $(patsubst %.dce,%.o,$(salt-runtime_dce)) \
	  	   $(patsubst %.c,%.o,$(salt-runtime_c))

packages/ddc-core-salt/runtime/libddc-runtime.a : $(salt-runtime_o)
	@echo "* Linking $@"
	@ar r $@ $^

packages/ddc-core-salt/runtime/libddc-runtime.$(SHARED_SUFFIX) : $(salt-runtime_o)
	@echo "* Linking $@"
	@$(GCC_LINK_SHARED) -o $@ $^


# -----------------------------------------------------------------------------
# Build runtime system.
#   The shared runtime is only built if SHARED_SUFFIX is defined.
.PHONY  : runtime
runtime : $(runtime_dep) \
		runtime/libddc-runtime.a \
		$(if $(SHARED_SUFFIX),runtime/libddc-runtime.$(SHARED_SUFFIX),) \
		packages/ddc-core-salt/runtime/libddc-runtime.a \
		$(if $(SHARED_SUFFIX),packages/ddc-core-salt/runtime/libddc-runtime.$(SHARED_SUFFIX),)


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

	@find packages/ddc-core-salt/runtime \
		        -name "*.o" \
		-o      -name "*.dep" \
		-o      -name "*.so" \
		-o      -name "*.dylib" \
		-o      -name "*.a" \
		-o      -name "*~" \
		-follow | xargs -n 1 rm -f
