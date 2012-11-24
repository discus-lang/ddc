# Print the packages needed to build DDC.

.PHONY  : show-pkgs
show-pkgs :
	@echo -n "Packages required to compile DDC: "
	@echo $(DDC_PACKAGES) | sed -r 's/(-hide-all-packages|-package|base|array|containers) //g'
