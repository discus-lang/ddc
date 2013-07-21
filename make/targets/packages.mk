
# All the Cabal packages in dependency order.
PACKAGES = \
	ddc-base \
	ddc-core \
	ddc-core-simpl \
	ddc-core-eval \
	ddc-core-salt \
	ddc-core-llvm \
	ddc-core-flow \
	ddc-core-blue \
        ddc-source-tetra \
	ddc-build \
	ddc-driver \
	ddc-code \


# Build and install all the Cabal packages.
.PHONY : packages
packages :
	for package in ${PACKAGES} ddc-tools; do \
		cd packages/$${package}; \
		 cabal clean; \
		 cabal configure; \
		 cabal build; \
		 cabal install --enable-documentation --force-reinstalls; \
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
