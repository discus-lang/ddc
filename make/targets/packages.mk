
# All the Cabal packages in dependency order.
PACKAGES = \
	ddc-base \
	ddc-core \
	ddc-core-simpl \
	ddc-core-eval \
	ddc-core-salt \
	ddc-core-llvm \
	ddc-core-flow \
	ddc-core-tetra \
        ddc-source-tetra \
	ddc-build \
        ddc-interface \
	ddc-driver \
	ddc-code \


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
