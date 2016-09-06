# Make rules for ddc main executable.

# Find source files for ddc-main.
ddc-main_packages = \
        packages/ddc-core/DDC \
	packages/ddc-core-babel/DDC \
	packages/ddc-core-simpl/DDC \
	packages/ddc-core-salt/DDC \
	packages/ddc-core-llvm/DDC \
	packages/ddc-core-flow/DDC \
	packages/ddc-core-tetra/DDC \
        packages/ddc-source-tetra/DDC \
	packages/ddc-build/DDC \
	packages/ddc-code/DDC \
	packages/ddc-driver/DDC

ddc-main_src_hs_all = \
	$(shell find $(ddc-main_packages)            -name "*.hs" -follow) \
	$(shell find packages/ddc-tools/src/ddc-main -name "*.hs" -follow)


# Make dependencies.
make/deps/Makefile-ddc-main.deps : $(ddc-main_src_hs_all)
	@echo "* Building dependencies (ddc-main)"
	@$(GHC) $(GHC_LANGUAGE) $(GHC_FLAGS) \
                $(patsubst %,-i%,$(ddc-main_packages)) \
		-M $^ -dep-makefile make/deps/Makefile-ddc-main.deps \
                -dep-suffix "" $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-ddc-main.deps.bak
	@cp make/deps/Makefile-ddc-main.deps make/deps/Makefile-ddc-main.deps.inc


# Build object files.
packages/ddc-tools/src/ddc-main/%.o : packages/ddc-tools/src/ddc-main/%.hs
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
		      -ipackages/ddc-driver \
		      -ipackages/ddc-code \
		      -ipackages/ddc-tools/src/ddc-main

# Link ddc execurable.
ddc-main_obj = $(patsubst %.hs,%.o,$(ddc-main_src_hs_all))

bin/ddc : $(ddc-main_obj)
	@echo "* Linking ddc"
	@$(GHC) -o bin/ddc $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddc-main_obj)
