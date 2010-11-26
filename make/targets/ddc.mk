
# -- Find Source Files --------------------------------------------------------
# -- files needing to be processed via alex
src_alex_x	=  $(shell find src -name "*.x" -follow)
src_alex_hs	=  $(patsubst %.x,%.hs,$(src_alex_x))

# -- files that are ready to compile
src_hs_existing	=  $(shell find src -name "*.hs" -follow)

# -- files that will be generated
src_hs_generated = src/Config/Config.hs src/Source/Plate/Trans.hs

# -- all .hs files in the src dir, including ones we need to preprocess.
src_hs_all	+= $(src_hs_existing)
src_hs_all	+= $(src_alex_hs)


# -- Configuration ------------------------------------------------------------
# -- use the $(Target) from make/config.mk to decide which ddc config file to use
src/Config/Config.hs : src/Config/Config.hs.$(Target)
	@echo "* Using configuration" $^
	@cp $^ $@
	@echo

# -- Dependencies -------------------------------------------------------------
.PHONY	: deps
deps	: make/Makefile.deps

make/Makefile.deps : $(src_hs_existing) $(src_hs_generated)
	@echo "* Building dependencies"
	@$(GHC) -isrc -M $^ -dep-makefile -optdepmake/Makefile.deps $(GHC_INCDIRS)
	@rm -f make/Makefile.deps.bak
	@cp make/Makefile.deps make/Makefile.deps.inc
	@echo

# -- Link DDC -----------------------------------------------------------------
# -- all the resulting .o files we'll get after compiling the .hs files
src_obj		=  $(patsubst %.hs,%.o,$(src_hs_existing))

bin/ddc	: make/Makefile.deps $(src_obj)
	@echo
	@echo "* Linking ddc"
	@$(GHC) -o bin/ddc $(GHC_FLAGS) $(GHC_VERSION_FLAGS) $(DDC_PACKAGES) $(src_obj)
	@echo
	
	

