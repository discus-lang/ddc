# Make rules for ddci-tetra executable.

# Find source files for ddci-tetra.
ddci-tetra_packages = \
	packages/ddc-core/DDC \
	packages/ddc-core-babel/DDC \
	packages/ddc-core-simpl/DDC \
	packages/ddc-core-salt/DDC \
	packages/ddc-core-llvm/DDC \
	packages/ddc-core-tetra/DDC \
	packages/ddc-core-smr/DDC \
        packages/ddc-core-flow/DDC \
        packages/ddc-core-machine/DDC \
	packages/ddc-source-tetra/DDC \
	packages/ddc-build/DDC \
	packages/ddc-code/DDC \
	packages/ddc-driver/DDC \
	packages/ddc-tools/src/ddci-tetra/DDCI 

ddci-tetra_packages_root = \
	$(patsubst %/DDC,%,$(patsubst %/DDCI,%,$(ddci-tetra_packages)))

ddci-tetra_src_hs_all = \
	$(shell find $(ddci-tetra_packages) -name "*.hs" -follow) \
	packages/ddc-tools/src/ddci-tetra/Main.hs


# Make dependencies.
make/deps/Makefile-ddci-tetra.deps : $(ddci-tetra_src_hs_all)
	@echo "* Building dependencies (ddci-tetra)"
	@$(GHC) $(GHC_LANGUAGE) $(GHC_FLAGS) \
                $(patsubst %,-i%,$(ddci-tetra_packages)) \
		-M $^ -dep-makefile make/deps/Makefile-ddci-tetra.deps \
                -dep-suffix "" $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-ddci-tetra.deps.bak
	@cp make/deps/Makefile-ddci-tetra.deps make/deps/Makefile-ddci-tetra.deps.inc


# Build object files.
packages/ddc-tools/src/ddci-tetra/%.o : packages/ddc-tools/src/ddci-tetra/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-llvm \
		      -ipackages/ddc-core-flow \
		      -ipackages/ddc-core-machine \
		      -ipackages/ddc-core-tetra \
		      -ipackages/ddc-core-smr \
		      -ipackages/ddc-core-babel \
		      -ipackages/ddc-source-tetra \
		      -ipackages/ddc-build \
		      -ipackages/ddc-driver \
		      -ipackages/ddc-code \
		      -ipackages/ddc-tools/src/ddci-tetra


# Link ddci-tetra executable.
ddci-tetra_obj = $(patsubst %.hs,%.o,$(ddci-tetra_src_hs_all))

bin/ddci-tetra : $(ddci-tetra_obj)
	@echo "* Linking ddci-tetra"
	@$(GHC) -o bin/ddci-tetra $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddci-tetra_obj)


# Helper for getting into interactive mode. Disable -O2
ddci-tetra-ghci :
	$(GHCI) $(patsubst -O2,,$(GHC_FLAGS)) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) \
		$(patsubst %,-i%,$(ddci-tetra_packages_root)) \
		packages/ddc-tools/src/ddci-tetra/Main.hs


# Generate tags (identifier/location map for editors)
ddci-tetra-tags :
	@echo "* Generating tags"
	@$(GHC) $(patsubst -O2,,$(GHC_FLAGS)) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) \
		$(patsubst %,-i%,$(ddci-tetra_packages_root)) \
		packages/ddc-tools/src/ddci-tetra/Main.hs -e ':ctags'
	@echo "* Copying into packages/"
	@sed "s/packages\///" tags > packages/tags

