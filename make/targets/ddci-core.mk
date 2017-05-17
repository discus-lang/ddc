# Make rules for ddci-core executable.

# Find source files for ddci-core.
ddci-core_packages = \
	packages/ddc-core/DDC \
	packages/ddc-core-babel/DDC \
	packages/ddc-core-simpl/DDC \
	packages/ddc-core-salt/DDC \
	packages/ddc-core-llvm/DDC \
	packages/ddc-core-flow/DDC \
	packages/ddc-core-machine/DDC \
	packages/ddc-core-tetra/DDC \
	packages/ddc-core-smr/DDC \
        packages/ddc-source-tetra/DDC \
	packages/ddc-build/DDC \
	packages/ddc-code/DDC \
	packages/ddc-driver/DDC \
	packages/ddc-tools/src/ddci-core/DDCI 

ddci-core_src_hs_all = \
	$(shell find $(ddci-core_packages) -name "*.hs" -follow) \
	packages/ddc-tools/src/ddci-core/Main.hs

# Package names without /DDC etc at end, so we can load them in ghci
ddci-core_packages_root = \
	$(patsubst %/DDC,%,$(patsubst %/DDCI,%,$(ddci-core_packages)))


# Make dependencies.
make/deps/Makefile-ddci-core.deps : $(ddci-core_src_hs_all)
	@echo "* Building dependencies (ddci-core)"
	@$(GHC) $(GHC_LANGUAGE) $(GHC_FLAGS) \
                $(patsubst %,-i%,$(ddci-core_packages)) \
		-M $^ -dep-makefile make/deps/Makefile-ddci-core.deps \
                -dep-suffix "" $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-ddci-core.deps.bak
	@cp make/deps/Makefile-ddci-core.deps make/deps/Makefile-ddci-core.deps.inc


# Build object files.
packages/ddc-tools/src/ddci-core/%.o : packages/ddc-tools/src/ddci-core/%.hs
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
		      -ipackages/ddc-tools/src/ddci-core


# Link ddci-core.
ddci-core_obj = $(patsubst %.hs,%.o,$(ddci-core_src_hs_all))

bin/ddci-core : $(ddci-core_obj)
	@echo "* Linking ddci-core"
	@$(GHC) -o bin/ddci-core $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddci-core_obj)


# Helper for getting into interactive mode. Disable -O2
ddci-core-ghci :
	$(GHCI) $(patsubst -O2,,$(GHC_FLAGS)) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) \
		$(patsubst %,-i%,$(ddci-core_packages_root)) \
		packages/ddc-tools/src/ddci-core/Main.hs


# Generate tags (identifier/location map for editors)
ddci-core-tags :
	@echo "* Generating tags"
	@$(GHC) $(patsubst -O2,,$(GHC_FLAGS)) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) \
		$(patsubst %,-i%,$(ddci-core_packages_root)) \
		-Wwarn packages/ddc-tools/src/ddci-core/Main.hs -e ':ctags'
	@echo "* Copying into packages/"
	@sed "s/packages\///" tags > packages/tags

