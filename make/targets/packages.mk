
# -----------------------------------------------------------------------------
# All the Cabal packages in dependency order.
PACKAGES = \
	ddc-core \
	ddc-core-salt \
	ddc-core-llvm \
	ddc-core-discus \
	ddc-core-flow \
	ddc-core-machine \
        ddc-source-discus \
	ddc-build \
	ddc-driver


# Build and install all the Cabal packages.
.PHONY : packages
packages :
	@for package in ${PACKAGES} ddc-tools; do \
		if [ `ghc-pkg list $${package} --simple-output | wc -l` -gt 0 ]; then \
		    echo "* Skipping $${package}; already installed"; \
		    continue; \
		fi; \
		cd src/s1/$${package}; \
		echo "\n* Building $${package}\n"; \
		 cabal clean && \
		 cabal configure && \
		 cabal build && \
		 cabal install --enable-documentation --force-reinstalls;\
		if [ $$? -ne 0 ]; then \
		    echo "* Error in $${package}!"; \
		    break; \
		fi; \
		cd ../..; \
	done;


# Unregister all the Cabal packages.
.PHONY : packages-unregister
packages-unregister :
	for package in ${PACKAGES}; do \
		ghc-pkg unregister $${package} --force; \
	done;


# Make source distributions for all the Cabal packages.
.PHONY : packages-sdist
packages-sdist :
	for package in ${PACKAGES}; do \
		cd src/s1/$${package}; \
		 cabal sdist; \
		cd ../..; \
	done;

# -----------------------------------------------------------------------------
# Some of the module names are reused between ddc-main and ddc-core,
#   so we need to write these rules specific to the package.
#   Writing specific rules for each package also means that we can control
#   inter-package dependencies.
src/s1/ddc-core/%.o : src/s1/ddc-core/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -isrc/s1/ddc-base -isrc/s1/ddc-core

src/s1/ddc-core-salt/%.o : src/s1/ddc-core-salt/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -isrc/s1/ddc-core \
		      -isrc/s1/ddc-core-salt

src/s1/ddc-core-llvm/%.o : src/s1/ddc-core-llvm/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -isrc/s1/ddc-core \
		      -isrc/s1/ddc-core-salt \
		      -isrc/s1/ddc-core-llvm

src/s1/ddc-core-flow/%.o : src/s1/ddc-core-flow/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -isrc/s1/ddc-core \
		      -isrc/s1/ddc-core-salt \
		      -isrc/s1/ddc-core-flow \
		      -isrc/s1/ddc-core-discus

src/s1/ddc-core-machine/%.o : src/s1/ddc-core-machine/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -isrc/s1/ddc-core \
		      -isrc/s1/ddc-core-machine

src/s1/ddc-core-discus/%.o : src/s1/ddc-core-discus/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -isrc/s1/ddc-core \
		      -isrc/s1/ddc-core-salt \
		      -isrc/s1/ddc-core-discus

src/s1/ddc-source-discus/%.o : src/s1/ddc-source-discus/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -isrc/s1/ddc-core \
                      -isrc/s1/ddc-core-salt \
		      -isrc/s1/ddc-core-discus \
		      -isrc/s1/ddc-source-discus

src/s1/ddc-build/%.o : src/s1/ddc-build/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -isrc/s1/ddc-core \
		      -isrc/s1/ddc-core-salt \
		      -isrc/s1/ddc-core-llvm \
		      -isrc/s1/ddc-core-flow \
		      -isrc/s1/ddc-core-machine \
		      -isrc/s1/ddc-core-discus \
		      -isrc/s1/ddc-core-babel \
		      -isrc/s1/ddc-source-discus \
		      -isrc/s1/ddc-build

src/s1/ddc-driver/%.o : src/s1/ddc-driver/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -isrc/s1/ddc-core \
		      -isrc/s1/ddc-core-salt \
		      -isrc/s1/ddc-core-llvm \
		      -isrc/s1/ddc-core-flow \
		      -isrc/s1/ddc-core-machine \
		      -isrc/s1/ddc-core-discus \
		      -isrc/s1/ddc-source-discus \
		      -isrc/s1/ddc-build \
		      -isrc/s1/ddc-driver

src/s1/ddc-tools/src/ddc-war/%.o : src/s1/ddc-tools/src/ddc-war/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) -O2 -threaded $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-isrc/s1/ddc-tools/src/ddc-war -c $<

