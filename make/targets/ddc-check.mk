
# -- Find Source Files --------------------------------------------------------
# -- all .hs files in the src dir, including ones we need to preprocess.
ddc-check_packages = \
	packages/ddc-base/DDC \
	packages/ddc-core/DDC \
	packages/ddc-core-simpl/DDC \
	packages/ddc-core-eval/DDC \
	packages/ddc-core-salt/DDC \
	packages/ddc-core-llvm/DDC \
	packages/ddc-core-flow/DDC \
	packages/ddc-core-tetra/DDC \
        packages/ddc-source-tetra/DDC \
	packages/ddc-build/DDC \
        packages/ddc-interface/DDC


ddc-check_src_hs_all = \
	$(shell find $(ddc-check_packages)            -name "*.hs" -follow) \
	$(shell find packages/ddc-tools/src/ddc-check -name "*.hs" -follow)

# -- Dependencies -------------------------------------------------------------
make/deps/Makefile-ddc-check.deps : $(ddc-check_src_hs_all)
	@echo "* Building dependencies (ddc-check)"
	@$(GHC) $(patsubst %,-i%,$(ddc-check_packages)) \
		-M $^ -dep-makefile -optdepmake/deps/Makefile-ddc-check.deps \
                -dep-suffix "" $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-ddc-check.deps.bak
	@cp make/deps/Makefile-ddc-check.deps make/deps/Makefile-ddc-check.deps.inc


# -- Link ddci-core -----------------------------------------------------------
ddc-check_obj = $(patsubst %.hs,%.o,$(ddc-check_src_hs_all))

bin/ddc-check : $(ddc-check_obj)
	@echo "* Linking ddc-check"
	@$(GHC) -o bin/ddc-check $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddc-check_obj)
