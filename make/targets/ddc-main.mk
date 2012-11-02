
# -- Find Source Files --------------------------------------------------------
# -- all .hs files in the src dir, including ones we need to preprocess.
ddc-main_packages = \
        packages/ddc-base/DDC \
        packages/ddc-core/DDC \
        packages/ddc-core-simpl/DDC \
        packages/ddc-core-eval/DDC \
        packages/ddc-core-salt/DDC \
        packages/ddc-llvm/DDC \
        packages/ddc-core-llvm/DDC \
        packages/ddc-build/DDC \
        packages/ddc-driver/DDC \

ddc-main_src_hs_all = \
	$(shell find $(ddc-main_packages)            -name "*.hs" -follow) \
	$(shell find packages/ddc-tools/src/ddc-main -name "*.hs" -follow)

# -- Dependencies -------------------------------------------------------------
make/deps/Makefile-ddc-main.deps : $(ddc-main_src_hs_all)
	@echo "* Building dependencies (ddc-main)"
	@$(GHC) $(patsubst %,-i%,$(ddc-main_packages)) \
		-M $^ -dep-makefile -optdepmake/deps/Makefile-ddc-main.deps $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-ddc-main.deps.bak
	@cp make/deps/Makefile-ddc-main.deps make/deps/Makefile-ddc-main.deps.inc


# -- Link ddc -----------------------------------------------------------------
ddc-main_obj = $(patsubst %.hs,%.o,$(ddc-main_src_hs_all))

bin/ddc : $(ddc-main_obj)
	@echo "* Linking ddc"
	@$(GHC) -o bin/ddc $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddc-main_obj)
