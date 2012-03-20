# Default make rules.
include make/build.mk
include make/config/target.mk

%.hs : %.x
	@echo "* Preprocessing $<"
	@alex -g $<


%.hi : %.o
	@true


%.o : %.c
	@echo "* Compiling $<"
	@gcc $(GCC_FLAGS) -c $< -o $@ 


%.dep : %.c
	@echo "* Building Deps $<"
	@gcc $(GCC_FLAGS) -MM $< -MT $(patsubst %.dep,%.o,$@) -o $@


# Some of the module names are reused between ddc-main and ddc-core, 
#   so we need to write these rules specific to the package.
#   Writing specific rules for each package also means that we can control
#   inter-package dependencies.
packages/ddc-main/%.o : packages/ddc-main/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(DDC_PACKAGES) $(GHC_INCDIRS) -c $< -ipackages/ddc-main 

packages/ddc-main/%.hi-boot : packages/ddc-main/%.hs-boot packages/ddc-main/%.o-boot
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(DDC_PACKAGES) $(GHC_INCDIRS) -c $< -ipackages/ddc-main 


packages/ddc-base/%.o : packages/ddc-base/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base

packages/ddc-core/%.o : packages/ddc-core/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base -ipackages/ddc-core

packages/ddc-core-simpl/%.o : packages/ddc-core-simpl/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base -ipackages/ddc-core -ipackages/ddc-core-simpl

packages/ddc-core-eval/%.o : packages/ddc-core-eval/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base -ipackages/ddc-core -ipackages/ddc-core-eval

packages/ddc-core-sea/%.o : packages/ddc-core-sea/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base -ipackages/ddc-core -ipackages/ddc-core-simpl \
                      -ipackages/ddc-core-sea

packages/ddc-llvm/%.o : packages/ddc-llvm/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base -ipackages/ddc-llvm

packages/ddc-core-llvm/%.o : packages/ddc-core-llvm/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base -ipackages/ddc-core -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-sea -ipackages/ddc-llvm -ipackages/ddc-core-llvm

packages/ddci-core/%.o : packages/ddci-core/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base -ipackages/ddc-core -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-eval \
                      -ipackages/ddc-core-sea \
                      -ipackages/ddc-core-llvm -ipackages/ddc-llvm \
                      -ipackages/ddci-core

