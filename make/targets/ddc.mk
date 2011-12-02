
# -- Find Source Files --------------------------------------------------------
# -- files needing to be processed via alex
ddc-main_src_alex_x	        =  $(shell find packages/ddc-main -name "*.x" -follow)
ddc-main_src_alex_hs	        =  $(patsubst %.x,%.hs,$(src_alex_x))

# -- files that are ready to compile
ddc-main_src_hs_existing	=  $(shell find packages/ddc-main -name "*.hs" -follow)

# -- files that will be generated
ddc-main_src_hs_generated = \
        packages/ddc-main/Config/Config.hs \
        packages/ddc-main/Source/Plate/Trans.hs \
        $(src_alex_hs)

# -- all .hs files in the src dir, including ones we need to preprocess.
ddc-main_src_hs_all     += $(filter-out $(ddc-main_src_alex_hs),$(ddc-main_src_hs_existing))
ddc-main_src_hs_all	+= $(ddc-main_src_alex_hs)


# -- Configuration ------------------------------------------------------------
# -- use the $(Target) from make/config.mk to decide which ddc config file to use
packages/ddc-main/Config/Config.hs : packages/ddc-main/Config/Config.hs.$(Target)
	@echo "* Using configuration" $^
	@cp $^ $@


# -- Dependencies -------------------------------------------------------------
make/deps/Makefile-main.deps : $(ddc-main_src_hs_existing) $(ddc-main_src_hs_generated)
	@echo "* Building dependencies (ddc)"
	@$(GHC) -ipackages/ddc-main -M $^ -dep-makefile -optdepmake/deps/Makefile-main.deps $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-main.deps.bak
	@cp make/deps/Makefile-main.deps make/deps/Makefile-main.deps.inc


# -- Link DDC -----------------------------------------------------------------
# -- all the resulting .o files we'll get after compiling the .hs files
ddc-main_src_obj =  $(patsubst %.hs,%.o,$(ddc-main_src_hs_existing))

bin/ddc	: make/deps/Makefile-main.deps $(ddc-main_src_obj)
	@echo "* Linking ddc"
	@$(GHC) -o bin/ddc $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(ddc-main_src_obj)

