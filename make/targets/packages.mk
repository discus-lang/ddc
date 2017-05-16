
# -----------------------------------------------------------------------------
# All the Cabal packages in dependency order.
PACKAGES = \
	ddc-core \
	ddc-core-simpl \
	ddc-core-salt \
	ddc-core-llvm \
	ddc-core-tetra \
	ddc-core-smr \
	ddc-core-flow \
	ddc-core-machine \
	ddc-core-babel \
        ddc-source-tetra \
	ddc-build \
	ddc-driver \
	ddc-code


# Build and install all the Cabal packages.
.PHONY : packages
packages :
	@for package in ${PACKAGES} ddc-tools; do \
		if [ `ghc-pkg list $${package} --simple-output | wc -l` -gt 0 ]; then \
		    echo "* Skipping $${package}; already installed"; \
		    continue; \
		fi; \
		cd packages/$${package}; \
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
		cd packages/$${package}; \
		 cabal sdist; \
		cd ../..; \
	done;

# -----------------------------------------------------------------------------
# Some of the module names are reused between ddc-main and ddc-core, 
#   so we need to write these rules specific to the package.
#   Writing specific rules for each package also means that we can control
#   inter-package dependencies.
packages/ddc-core/%.o : packages/ddc-core/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base -ipackages/ddc-core

packages/ddc-core-simpl/%.o : packages/ddc-core-simpl/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl

packages/ddc-core-salt/%.o : packages/ddc-core-salt/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt

packages/ddc-core-llvm/%.o : packages/ddc-core-llvm/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-llvm

packages/ddc-core-flow/%.o : packages/ddc-core-flow/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-flow \
		      -ipackages/ddc-core-tetra

packages/ddc-core-machine/%.o : packages/ddc-core-machine/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-machine

packages/ddc-core-babel/%.o : packages/ddc-core-babel/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt  \
		      -ipackages/ddc-core-tetra

packages/ddc-core-tetra/%.o : packages/ddc-core-tetra/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-tetra

packages/ddc-core-smr/%.o : packages/ddc-core-smr/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-tetra \
		      -ipackages/ddc-core-smr

packages/ddc-source-tetra/%.o : packages/ddc-source-tetra/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-core \
                      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-tetra \
		      -ipackages/ddc-source-tetra
		       
packages/ddc-build/%.o : packages/ddc-build/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-core \
		      -ipackages/ddc-core-babel \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-llvm \
		      -ipackages/ddc-core-flow \
		      -ipackages/ddc-core-machine \
		      -ipackages/ddc-core-tetra \
		      -ipackages/ddc-core-babel \
                      -ipackages/ddc-source-tetra \
		      -ipackages/ddc-build

packages/ddc-driver/%.o : packages/ddc-driver/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-llvm \
		      -ipackages/ddc-core-flow \
		      -ipackages/ddc-core-machine \
		      -ipackages/ddc-core-tetra \
		      -ipackages/ddc-core-babel \
                      -ipackages/ddc-source-tetra \
		      -ipackages/ddc-build \
		      -ipackages/ddc-driver

packages/ddc-code/%.o : packages/ddc-code/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $<


packages/ddc-war/%.o : packages/ddc-war/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) -O2 -threaded $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-war

