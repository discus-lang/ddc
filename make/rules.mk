# Default make rules.
include make/build.mk
include make/config/target.mk

# -----------------------------------------------------------------------------
# Runtime system 
packages/ddc-code/sea/%.o : packages/ddc-code/sea/%.c
	@echo "* Compiling $<"
	@gcc $(GCC_FLAGS) -Ipackages/ddc-code/sea/runtime -c $< -o $@ 


# -----------------------------------------------------------------------------
# Some of the module names are reused between ddc-main and ddc-core, 
#   so we need to write these rules specific to the package.
#   Writing specific rules for each package also means that we can control
#   inter-package dependencies.
packages/ddc-alpha/%.o : packages/ddc-alpha/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< \
		-ipackages/ddc-alpha/src

packages/ddc-alpha/%.hi-boot : packages/ddc-alpha/%.hs-boot packages/ddc-alpha/%.o-boot
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< \
		-ipackages/ddc-alpha/src

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
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl

packages/ddc-core-eval/%.o : packages/ddc-core-eval/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-eval

packages/ddc-core-salt/%.o : packages/ddc-core-salt/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt

packages/ddc-core-llvm/%.o : packages/ddc-core-llvm/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-llvm

packages/ddc-core-flow/%.o : packages/ddc-core-flow/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-flow

packages/ddc-core-tetra/%.o : packages/ddc-core-tetra/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-tetra

packages/ddc-source-tetra/%.o : packages/ddc-source-tetra/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-tetra \
		      -ipackages/ddc-source-tetra
		       
packages/ddc-build/%.o : packages/ddc-build/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-eval \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-llvm \
		      -ipackages/ddc-core-flow \
		      -ipackages/ddc-core-tetra \
		      -ipackages/ddc-build

packages/ddc-driver/%.o : packages/ddc-driver/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-eval \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-llvm \
		      -ipackages/ddc-core-flow \
		      -ipackages/ddc-core-tetra \
		      -ipackages/ddc-build \
		      -ipackages/ddc-driver

packages/ddc-code/%.o : packages/ddc-code/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $<

packages/ddc-interface/%.o : packages/ddc-interface/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base 
		
packages/ddc-tools/src/ddc-check/%.o : packages/ddc-tools/src/ddc-check/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-eval \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-llvm \
		      -ipackages/ddc-core-flow \
		      -ipackages/ddc-core-tetra \
		      -ipackages/ddc-build \
		      -ipackages/ddc-tools/src/ddc-check

packages/ddc-tools/src/ddci-core/%.o : packages/ddc-tools/src/ddci-core/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-eval \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-llvm \
		      -ipackages/ddc-core-flow \
		      -ipackages/ddc-core-tetra \
		      -ipackages/ddc-build \
		      -ipackages/ddc-driver \
		      -ipackages/ddc-code \
		      -ipackages/ddc-interface \
		      -ipackages/ddc-tools/src/ddci-core

packages/ddc-tools/src/ddc-main/%.o : packages/ddc-tools/src/ddc-main/%.hs
	@echo "* Compiling $<"
	@$(GHC) $(GHC_FLAGS) $(GHC_WARNINGS2) $(DDC_PACKAGES) $(GHC_INCDIRS) \
		-c $< -ipackages/ddc-base \
		      -ipackages/ddc-core \
		      -ipackages/ddc-core-simpl \
		      -ipackages/ddc-core-eval \
		      -ipackages/ddc-core-salt \
		      -ipackages/ddc-core-llvm \
		      -ipackages/ddc-core-flow \
		      -ipackages/ddc-core-tetra \
		      -ipackages/ddc-build \
		      -ipackages/ddc-driver \
		      -ipackages/ddc-code \
		      -ipackages/ddc-interface \
		      -ipackages/ddc-tools/src/ddc-main


# -- Generic Rules ------------------------------------------------------------
%.hs : %.x
	@echo "* Preprocessing $<"
	@alex -g $<


%.hi : %.o
	@true


%.dep : %.c
	@echo "* Dependencies for $<"
	@gcc $(GCC_FLAGS) -MM $< -MT $(patsubst %.dep,%.o,$@) -o $@


%.o : %.c
	@echo "* Compiling $<"
	@gcc $(GCC_FLAGS) -c $< -o $@ 


%.o : %.dcl bin/ddc
	@echo "* Compiling $<"
	@bin/ddc -c $<


%.o : %.dcs bin/ddc
	@echo "* Compiling $<"
	@bin/ddc -c $<

