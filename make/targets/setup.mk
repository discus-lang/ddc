
# Print the packages needed to build DDC.
.PHONY  : show-pkgs
show-pkgs :
	@echo $(DDC_PACKAGES) \
         | sed 's/-hide-all-packages //;s/-package //g;s/base //g;s/directory //;s/array //;s/containers //'


# Install prerequisite cabal packages.
.PHONY 	: setup
setup :
	@echo "* Installing prerequisite cabal packages..."
	@$(DEPS_INSTALLER) v1-update
	@$(DEPS_INSTALLER) v1-install \
		text mtl stm json \
		parsec-3.1.13.0 \
		inchworm-1.1.1.2 \
		shimmer-0.1.3.4 \
		buildbox-2.2.1.2 \
		cryptohash-sha256-0.11.101.0

