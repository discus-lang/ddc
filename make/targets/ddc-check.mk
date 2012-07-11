
# -- Find Source Files --------------------------------------------------------
# -- all .hs files in the src dir, including ones we need to preprocess.
ddc-check_packages = \
	packages/ddc-base/DDC \
	packages/ddc-type/DDC \
	packages/ddc-core/DDC \
	packages/ddc-core-simpl/DDC \
	packages/ddc-core-eval/DDC \
	packages/ddc-core-salt/DDC \
 	packages/ddc-llvm/DDC \
 	packages/ddc-core-llvm/DDC \
	packages/ddc-build/DDC

ddc-check_src_hs_all = \
	$(shell find $(ddc-check_packages)            -name "*.hs" -follow) \
        $(shell find packages/ddc-tools/src/ddc-check -name "*.hs" -follow)

# -- Dependencies -------------------------------------------------------------
make/deps/Makefile-ddc-check.deps : $(ddc-check_src_hs_all)
	@echo "* Building dependencies (ddc-check)"
	@$(GHC) $(patsubst %,-i%,$(ddc-check_packages)) \
		-M $^ -dep-makefile -optdepmake/deps/Makefile-ddc-check.deps $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-ddc-check.deps.bak
	@cp make/deps/Makefile-ddc-check.deps make/deps/Makefile-ddc-check.deps.inc


# -- Link ddci-core -----------------------------------------------------------
ddc-check_obj = $(patsubst %.hs,%.o,$(ddc-check_src_hs_all))

bin/ddc-check : make/deps/Makefile-main.deps $(ddc-check_obj)
	@echo "* Linking ddc-check"
	@$(GHC) -o bin/ddc-check $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddc-check_obj)
