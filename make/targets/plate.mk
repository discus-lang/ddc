# The boilerplate generator and generated boilerplate.

# Build the boiler plate generator
bin/plate : tools/plate/Main.hs src/Config/Config.hs
	@echo "* Building boilerplate generator ---------------------------------------------------"
	$(GHC) $(GHC_FLAGS) $(DDC_PACKAGES) -isrc -itools/plate -o bin/plate --make $^


# Generate boilerplate
src/Source/Plate/Trans.hs : bin/plate src/Source/Plate/Trans.hs-stub src/Source/Exp.hs
	@echo
	@echo "* Generating boilerplate for $@"
	bin/plate src/Source/Exp.hs src/Source/Plate/Trans.hs-stub src/Source/Plate/Trans.hs
	@echo
