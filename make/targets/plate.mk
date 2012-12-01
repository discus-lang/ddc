# The boilerplate generator and generated boilerplate.

# Build the boiler plate generator
bin/plate : packages/ddc-plate/Main.hs
	@echo "* Building boilerplate generator"
	@$(GHC) $(GHC_FLAGS) $(DDC_PACKAGES) \
                -ipackages/ddc-plate/src \
                -o bin/plate --make $^


# Generate boilerplate
packages/ddc-alpha/src/Source/Plate/Trans.hs : \
  bin/plate packages/ddc-alpha/src/Source/Plate/Trans.hs-stub packages/ddc-alpha/src/Source/Exp.hs
	@echo "* Generating boilerplate for $@"
	@bin/plate \
                packages/ddc-alpha/src/Source/Exp.hs \
                packages/ddc-alpha/src/Source/Plate/Trans.hs-stub \
                packages/ddc-alpha/src/Source/Plate/Trans.hs
