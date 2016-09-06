# Make rules for ddc-check.

# Find source files for ddc-check.
ddc-check_packages = \
	packages/ddc-core/DDC \
	packages/ddc-core-babel/DDC \
	packages/ddc-core-simpl/DDC \
	packages/ddc-core-salt/DDC \
	packages/ddc-core-llvm/DDC \
	packages/ddc-core-flow/DDC \
	packages/ddc-core-tetra/DDC \
        packages/ddc-source-tetra/DDC \
	packages/ddc-build/DDC

ddc-check_src_hs_all = \
	$(shell find $(ddc-check_packages)            -name "*.hs" -follow) \
	$(shell find packages/ddc-tools/src/ddc-check -name "*.hs" -follow)


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
packages/ddc-tools/src/ddc-check/%.o : packages/ddc-tools/src/ddc-check/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-llvm \
		      -ipackages/ddc-core-flow \
		      -ipackages/ddc-core-tetra \
		      -ipackages/ddc-core-babel \
                      -ipackages/ddc-source-tetra \
		      -ipackages/ddc-build \
		      -ipackages/ddc-tools/src/ddc-check


# Link ddc-check executable.
ddc-check_obj = $(patsubst %.hs,%.o,$(ddc-check_src_hs_all))

bin/ddc-check : $(ddc-check_obj)
	@echo "* Linking ddc-check"
	@$(GHC) -o bin/ddc-check $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddc-check_obj)
