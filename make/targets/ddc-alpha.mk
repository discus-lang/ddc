
# -- Find Source Files --------------------------------------------------------
# -- files needing to be processed via alex
ddc-alpha_src_alex_x            =  $(shell find packages/ddc-alpha/src -name "*.x" -follow)
ddc-alpha_src_alex_hs           =  $(patsubst %.x,%.hs,$(src_alex_x))

# -- files that are ready to compile
ddc-alpha_src_hs_existing       =  $(shell find packages/ddc-alpha/src -name "*.hs" -follow)

# -- files that will be generated
ddc-alpha_src_hs_generated = \
        packages/ddc-alpha/src/Config/Config.hs \
        packages/ddc-alpha/src/Source/Plate/Trans.hs \
        $(src_alex_hs)

# -- all .hs files in the src dir, including ones we need to preprocess.
ddc-alpha_src_hs_all    += $(filter-out $(ddc-alpha_src_alex_hs),$(ddc-alpha_src_hs_existing))
ddc-alpha_src_hs_all	+= $(ddc-alpha_src_alex_hs)


# -- Configuration ------------------------------------------------------------
# -- use the $(Target) from make/config.mk to decide which ddc config file to use
packages/ddc-alpha/src/Config/Config.hs : packages/ddc-alpha/src/Config/Config.hs.$(Target)
	@echo "* Using configuration" $^
	@cp $^ $@


# -- Dependencies -------------------------------------------------------------
make/deps/Makefile-ddc-alpha.deps : $(ddc-alpha_src_hs_existing) $(ddc-alpha_src_hs_generated)
	@echo "* Building dependencies (ddc-alpha)"
	@$(GHC) -ipackages/ddc-alpha/src -M $^ -dep-makefile \
                -optdepmake/deps/Makefile-ddc-alpha.deps $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-ddc-alpha.deps.bak
	@cp make/deps/Makefile-ddc-alpha.deps make/deps/Makefile-ddc-alpha.deps.inc


# -- Link DDC -----------------------------------------------------------------
# -- all the resulting .o files we'll get after compiling the .hs files
ddc-alpha_src_obj =  $(patsubst %.hs,%.o,$(ddc-alpha_src_hs_existing))

bin/ddc-alpha : make/deps/Makefile-ddc-alpha.deps $(ddc-alpha_src_obj)
	@echo "* Linking ddc-alpha"
	@$(GHC) -o bin/ddc-alpha $(GHC_FLAGS) $(GHC_VERSION_FLAGS) \
                $(DDC_PACKAGES) \
                $(ddc-alpha_src_obj)
