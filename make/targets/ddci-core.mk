# Make rules for ddci-core executable.

# Find source files for ddci-core.
ddci-core_packages = \
	src/s1/ddc-core/DDC \
	src/s1/ddc-core-babel/DDC \
	src/s1/ddc-core-simpl/DDC \
	src/s1/ddc-core-salt/DDC \
	src/s1/ddc-core-llvm/DDC \
	src/s1/ddc-core-flow/DDC \
	src/s1/ddc-core-machine/DDC \
	src/s1/ddc-core-tetra/DDC \
	src/s1/ddc-core-smr/DDC \
        src/s1/ddc-source-tetra/DDC \
	src/s1/ddc-build/DDC \
	packages/ddc-code/DDC \
	src/s1/ddc-driver/DDC \
	src/s1/ddc-tools/src/ddci-core/DDCI

ddci-core_src_hs_all = \
	$(shell find $(ddci-core_packages) -name "*.hs" -follow) \
	src/s1/ddc-tools/src/ddci-core/Main.hs

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
src/s1/ddc-tools/src/ddci-core/%.o : src/s1/ddc-tools/src/ddci-core/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -isrc/s1/ddc-base \
		      -isrc/s1/ddc-core \
		      -isrc/s1/ddc-core-simpl \
		      -isrc/s1/ddc-core-salt \
		      -isrc/s1/ddc-core-llvm \
		      -isrc/s1/ddc-core-flow \
		      -isrc/s1/ddc-core-machine \
		      -isrc/s1/ddc-core-tetra \
		      -isrc/s1/ddc-core-smr \
		      -isrc/s1/ddc-core-babel \
                      -isrc/s1/ddc-source-tetra \
		      -isrc/s1/ddc-build \
		      -isrc/s1/ddc-driver \
		      -ipackages/ddc-code \
		      -isrc/s1/ddc-tools/src/ddci-core


# Link ddci-core.
ddci-core_obj = $(patsubst %.hs,%.o,$(ddci-core_src_hs_all))

bin/ddci-core : $(ddci-core_obj)
	@echo "* Linking ddci-core"
	@$(GHC) -o bin/ddci-core $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddci-core_obj)


# Helper for getting into interactive mode. Disable -O2
ddci-core-ghci :
	$(GHCI) $(patsubst -O2,,$(GHC_FLAGS)) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) \
		$(patsubst %,-i%,$(ddci-core_packages_root)) \
		src/s1/ddc-tools/src/ddci-core/Main.hs


# Generate tags (identifier/location map for editors)
ddci-core-tags :
	@echo "* Generating tags"
	@$(GHC) $(patsubst -O2,,$(GHC_FLAGS)) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) \
		$(patsubst %,-i%,$(ddci-core_packages_root)) \
		-Wwarn src/s1/ddc-tools/src/ddci-core/Main.hs -e ':ctags'
	@echo "* Copying into src/s1/"
	@sed "s/src\/s1\///" tags > src/s1/tags

