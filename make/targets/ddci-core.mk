
# -- Find Source Files --------------------------------------------------------
# -- all .hs files in the src dir, including ones we need to preprocess.
ddci-core_packages = \
        packages/ddc-base/DDC \
        packages/ddc-type/DDC \
        packages/ddc-core/DDC \
        packages/ddc-core-simpl/DDC \
        packages/ddc-core-eval/DDC \
        packages/ddc-core-salt/DDC \
        packages/ddc-llvm/DDC \
        packages/ddc-core-llvm/DDC \
        packages/ddci-core/DDCI 


ddci-core_src_hs_all = \
        $(shell find $(ddci-core_packages) -name "*.hs" -follow) \
        packages/ddci-core/Main.hs

# -- Dependencies -------------------------------------------------------------
make/deps/Makefile-ddci-core.deps : $(ddci-core_src_hs_all)
	@echo "* Building dependencies (ddci-core)"
	@$(GHC) $(patsubst %,-i%,$(ddci-core_packages)) \
	        -M $^ -dep-makefile -optdepmake/deps/Makefile-ddci-core.deps $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-ddci-core.deps.bak
	@cp make/deps/Makefile-ddci-core.deps make/deps/Makefile-ddci-core.deps.inc


# -- Link ddci-core -----------------------------------------------------------
ddci-core_obj = $(patsubst %.hs,%.o,$(ddci-core_src_hs_all))

bin/ddci-core : make/deps/Makefile-main.deps $(ddci-core_obj)
	@echo "* Linking ddci-core"
	@$(GHC) -o bin/ddci-core $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddci-core_obj)
