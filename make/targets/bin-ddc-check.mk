# Make rules for ddc-check.

# Find source files for ddc-check.
ddc-check_packages = \
	src/s1/ddc-core/DDC \
	src/s1/ddc-core-babel/DDC \
	src/s1/ddc-core-simpl/DDC \
	src/s1/ddc-core-salt/DDC \
	src/s1/ddc-core-llvm/DDC \
	src/s1/ddc-core-flow/DDC \
	src/s1/ddc-core-machine/DDC \
	src/s1/ddc-core-tetra/DDC \
	src/s1/ddc-core-smr/DDC \
        src/s1/ddc-source-tetra/DDC \
	src/s1/ddc-build/DDC

ddc-check_src_hs_all = \
	$(shell find $(ddc-check_packages)          -name "*.hs" -follow) \
	$(shell find src/s1/ddc-tools/src/ddc-check -name "*.hs" -follow)


# Make dependencies.
make/deps/Makefile-ddc-check.deps : $(ddc-check_src_hs_all)
	@echo "* Building dependencies (ddc-check)"
	@$(GHC) $(GHC_LANGUAGE) $(GHC_FLAGS) \
                $(patsubst %,-i%,$(ddc-check_packages)) \
		-M $^ -dep-makefile make/deps/Makefile-ddc-check.deps \
                -dep-suffix "" $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-ddc-check.deps.bak
	@cp make/deps/Makefile-ddc-check.deps make/deps/Makefile-ddc-check.deps.inc


# Build object files.
src/s1/ddc-tools/src/ddc-check/%.o : src/s1/ddc-tools/src/ddc-check/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -isrc/s1/ddc-base \
		      -isrc/s1/ddc-core \
		      -isrc/s1/ddc-core-simpl \
		      -isrc/s1/ddc-core-salt \
		      -isrc/s1/ddc-core-llvm \
		      -isrc/s1/ddc-core-flow \
		      -isrc/s1/ddc-core-machine \
		      -isrc/s1/ddc-core-tetra \
		      -isrc/s1/ddc-core-smr \
		      -isrc/s1/ddc-core-babel \
                      -isrc/s1/ddc-source-tetra \
		      -isrc/s1/ddc-build \
		      -isrc/s1/ddc-tools/src/ddc-check


# Link ddc-check executable.
ddc-check_obj = $(patsubst %.hs,%.o,$(ddc-check_src_hs_all))

bin/ddc-check : $(ddc-check_obj)
	@echo "* Linking ddc-check"
	@$(GHC) -o bin/ddc-check $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddc-check_obj)
