
# -- Find Source Files --------------------------------------------------------
# -- all .hs files in the src dir, including ones we need to preprocess.
ddci-tetra_packages = \
	packages/ddc-base/DDC \
	packages/ddc-core/DDC \
        packages/ddc-core-eval/DDC \
	packages/ddc-core-simpl/DDC \
	packages/ddc-core-salt/DDC \
	packages/ddc-core-llvm/DDC \
	packages/ddc-core-tetra/DDC \
        packages/ddc-core-flow/DDC \
	packages/ddc-source-tetra/DDC \
	packages/ddc-build/DDC \
	packages/ddc-code/DDC \
	packages/ddc-driver/DDC \
	packages/ddc-interface/DDC \
	packages/ddc-tools/src/ddci-tetra/DDCI 

# -- packages without /DDC etc at end, so we can load them in ghci
ddci-tetra_packages_root = \
	$(patsubst %/DDC,%,$(patsubst %/DDCI,%,$(ddci-tetra_packages)))

ddci-tetra_src_hs_all = \
	$(shell find $(ddci-tetra_packages) -name "*.hs" -follow) \
	packages/ddc-tools/src/ddci-tetra/Main.hs

# -- Dependencies -------------------------------------------------------------
make/deps/Makefile-ddci-tetra.deps : $(ddci-tetra_src_hs_all)
	@echo "* Building dependencies (ddci-tetra)"
	@$(GHC) $(patsubst %,-i%,$(ddci-tetra_packages)) \
		-M $^ -dep-makefile -optdepmake/deps/Makefile-ddci-tetra.deps $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-ddci-tetra.deps.bak
	@cp make/deps/Makefile-ddci-tetra.deps make/deps/Makefile-ddci-tetra.deps.inc


# -- Link ddci-core -----------------------------------------------------------
ddci-tetra_obj = $(patsubst %.hs,%.o,$(ddci-tetra_src_hs_all))

bin/ddci-tetra : $(ddci-tetra_obj)
	@echo "* Linking ddci-tetra"
	@$(GHC) -o bin/ddci-tetra $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddci-tetra_obj)


# -- Helper for getting into interactive mode. Disable -O2
ddci-tetra-ghci :
	$(GHCI) $(patsubst -O2,,$(GHC_FLAGS)) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) \
		$(patsubst %,-i%,$(ddci-tetra_packages_root)) packages/ddc-tools/src/ddci-tetra/Main.hs

# -- Generate tags (identifier/location map for editors)
ddci-tetra-tags :
	@echo "* Generating tags"
	@$(GHC) $(patsubst -O2,,$(GHC_FLAGS)) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) \
		$(patsubst %,-i%,$(ddci-tetra_packages_root)) packages/ddc-tools/src/ddci-tetra/Main.hs -e ':ctags'
	@echo "* Copying into packages/"
	@sed "s/packages\///" tags > packages/tags
