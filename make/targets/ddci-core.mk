
# -- Find Source Files --------------------------------------------------------
# -- all .hs files in the src dir, including ones we need to preprocess.
ddci-core_packages = \
        packages/ddc-base/DDC \
        packages/ddc-core/DDC \
        packages/ddc-core-simpl/DDC \
        packages/ddc-core-eval/DDC \
        packages/ddc-core-salt/DDC \
        packages/ddc-core-llvm/DDC \
        packages/ddc-core-flow/DDC \
        packages/ddc-build/DDC \
        packages/ddc-code/DDC \
        packages/ddc-driver/DDC \
        packages/ddc-tools/src/ddci-core/DDCI 

# -- packages without /DDC etc at end, so we can load them in ghci
ddci-core_packages_root = \
        $(patsubst %/DDC,%,$(patsubst %/DDCI,%,$(ddci-core_packages)))

ddci-core_src_hs_all = \
        $(shell find $(ddci-core_packages) -name "*.hs" -follow) \
        packages/ddc-tools/src/ddci-core/Main.hs

# -- Dependencies -------------------------------------------------------------
make/deps/Makefile-ddci-core.deps : $(ddci-core_src_hs_all)
	@echo "* Building dependencies (ddci-core)"
	@$(GHC) $(patsubst %,-i%,$(ddci-core_packages)) \
	        -M $^ -dep-makefile -optdepmake/deps/Makefile-ddci-core.deps $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-ddci-core.deps.bak
	@cp make/deps/Makefile-ddci-core.deps make/deps/Makefile-ddci-core.deps.inc


# -- Link ddci-core -----------------------------------------------------------
ddci-core_obj = $(patsubst %.hs,%.o,$(ddci-core_src_hs_all))

bin/ddci-core : $(ddci-core_obj)
	@echo "* Linking ddci-core"
	@$(GHC) -o bin/ddci-core $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddci-core_obj)


# -- Helper for getting into interactive mode. Disable -O2
ddci-core-ghci :
	$(GHCI) $(patsubst -O2,,$(GHC_FLAGS)) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) \
                $(patsubst %,-i%,$(ddci-core_packages_root)) packages/ddc-tools/src/ddci-core/Main.hs

# -- Generate tags (identifier/location map for editors)
ddci-core-tags :
	@echo "* Generating tags"
	@$(GHC) $(patsubst -O2,,$(GHC_FLAGS)) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) \
                $(patsubst %,-i%,$(ddci-core_packages_root)) packages/ddc-tools/src/ddci-core/Main.hs -e ':ctags'
	@echo "* Copying into packages/"
	@sed "s/packages\///" tags > packages/tags
