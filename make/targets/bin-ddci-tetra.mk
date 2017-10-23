# Make rules for ddci-tetra executable.

# Find source files for ddci-tetra.
ddci-tetra_packages = \
	src/s1/ddc-core/DDC \
	src/s1/ddc-core-babel/DDC \
	src/s1/ddc-core-simpl/DDC \
	src/s1/ddc-core-salt/DDC \
	src/s1/ddc-core-llvm/DDC \
	src/s1/ddc-core-tetra/DDC \
        src/s1/ddc-core-flow/DDC \
        src/s1/ddc-core-machine/DDC \
	src/s1/ddc-source-tetra/DDC \
	src/s1/ddc-build/DDC \
	src/s1/ddc-code/DDC \
	src/s1/ddc-driver/DDC \
	src/s1/ddc-tools/src/ddci-tetra/DDCI

ddci-tetra_packages_root = \
	$(patsubst %/DDC,%,$(patsubst %/DDCI,%,$(ddci-tetra_packages)))

ddci-tetra_src_hs_all = \
	$(shell find $(ddci-tetra_packages) -name "*.hs" -follow) \
	src/s1/ddc-tools/src/ddci-tetra/Main.hs


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
src/s1/ddc-tools/src/ddci-tetra/%.o : src/s1/ddc-tools/src/ddci-tetra/%.hs
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
		      -isrc/s1/ddc-core-babel \
		      -isrc/s1/ddc-source-tetra \
		      -isrc/s1/ddc-build \
		      -isrc/s1/ddc-driver \
		      -isrc/s1/ddc-code \
		      -isrc/s1/ddc-tools/src/ddci-tetra


# Link ddci-tetra executable.
ddci-tetra_obj = $(patsubst %.hs,%.o,$(ddci-tetra_src_hs_all))

bin/ddci-tetra : $(ddci-tetra_obj)
	@echo "* Linking ddci-tetra"
	@$(GHC) -o bin/ddci-tetra $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddci-tetra_obj)


# Helper for getting into interactive mode. Disable -O2
ddci-tetra-ghci :
	$(GHCI) $(patsubst -O2,,$(GHC_FLAGS)) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) \
		$(patsubst %,-i%,$(ddci-tetra_packages_root)) \
		src/s1/ddc-tools/src/ddci-tetra/Main.hs


# Generate tags (identifier/location map for editors)
ddci-tetra-tags :
	@echo "* Generating tags"
	@$(GHC) $(patsubst -O2,,$(GHC_FLAGS)) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) \
		$(patsubst %,-i%,$(ddci-tetra_packages_root)) \
		src/s1/ddc-tools/src/ddci-tetra/Main.hs -e ':ctags'
	@echo "* Copying into src/s1/"
	@sed "s/src\/s1\///" tags > src/s1/tags

