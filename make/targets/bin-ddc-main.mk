# Make rules for ddc main executable.

# Find source files for ddc-main.
ddc-main_packages = \
        src/s1/ddc-core/DDC \
	src/s1/ddc-core-babel/DDC \
	src/s1/ddc-core-salt/DDC \
	src/s1/ddc-core-llvm/DDC \
	src/s1/ddc-core-flow/DDC \
	src/s1/ddc-core-machine/DDC \
	src/s1/ddc-core-discus/DDC \
        src/s1/ddc-source-discus/DDC \
	src/s1/ddc-build/DDC \
	src/s1/ddc-driver/DDC

ddc-main_src_hs_all = \
	$(shell find $(ddc-main_packages)          -name "*.hs" -follow) \
	$(shell find src/s1/ddc-tools/src/ddc-main -name "*.hs" -follow)


# Make dependencies.
make/deps/Makefile-ddc-main.deps : $(ddc-main_src_hs_all)
	@echo "* Building dependencies (ddc-main)"
	@$(GHC) $(GHC_LANGUAGE) $(GHC_FLAGS) \
                $(patsubst %,-i%,$(ddc-main_packages)) \
		-M $^ -dep-makefile make/deps/Makefile-ddc-main.deps \
                -dep-suffix "" $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-ddc-main.deps.bak
	@cp make/deps/Makefile-ddc-main.deps make/deps/Makefile-ddc-main.deps.inc


# Build object files.
src/s1/ddc-tools/src/ddc-main/%.o : src/s1/ddc-tools/src/ddc-main/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -isrc/s1/ddc-base \
		      -isrc/s1/ddc-core \
		      -isrc/s1/ddc-core-salt \
		      -isrc/s1/ddc-core-llvm \
		      -isrc/s1/ddc-core-flow \
		      -isrc/s1/ddc-core-machine \
		      -isrc/s1/ddc-core-discus \
		      -isrc/s1/ddc-core-babel \
                      -isrc/s1/ddc-source-discus \
		      -isrc/s1/ddc-build \
		      -isrc/s1/ddc-driver \
		      -isrc/s1/ddc-tools/src/ddc-main

# Link ddc execurable.
ddc-main_obj = $(patsubst %.hs,%.o,$(ddc-main_src_hs_all))

bin/ddc : $(ddc-main_obj)
	@echo "* Linking ddc"
	@$(GHC) -o bin/ddc $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddc-main_obj)
