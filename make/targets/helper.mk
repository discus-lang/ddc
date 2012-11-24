# Print the packages needed to build DDC.

.PHONY  : show-pkgs
show-pkgs :
	@echo -n "Packages required to compile DDC: "
	@echo $(DDC_PACKAGES) | sed 's/-hide-all-packages //;s/-package //g;s/base //;s/array //;s/containers //'
