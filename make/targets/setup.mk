
# Print the packages needed to build DDC.
.PHONY  : show-pkgs
show-pkgs :
	@echo $(DDC_PACKAGES) \
         | sed 's/-hide-all-packages //;s/-package //g;s/base //g;s/directory //;s/array //;s/containers //'


# Install prerequisite cabal packages.
.PHONY 	: setup
setup :
	@echo "* Installing prerequisite cabal packages..."
	@$(DEPS_INSTALLER) update
	@$(DEPS_INSTALLER) install mtl parsec random stm text buildbox inchworm shimmer

