
# targets:
#       all             -- build everything
#       deps            -- build dependencies
#	runtime		-- build the runtime system
#	external	-- build external libraries
#	libs		-- build base libraries
#	doc		-- build Haddock docks
#
#	test		-- run quick check and regression tests
#
#       clean           -- clean everything
#       cleanWar        -- clean libraries and tests, but leave the compiler build alone
#       cleanRuntime    -- clean the runtime system
#       cleanLibrary    -- clean out the libraries
#
#       bin/ddc         -- build the compiler binary
#       bin/war         -- build the test driver
#       bin/plate       -- build the boilerplate generator
#

.PHONY	: all
all 	: bin/ddc bin/war runtime external libs

include make/build.mk

# -- Find Source Files ----------------------------------------------------------------------------
# -- files needing to be processed via alex
src_alex_x	=  $(shell find src -name "*.x" -follow)
src_alex_hs	=  $(patsubst %.x,%.hs,$(src_alex_x))

# -- files that are ready to compile
src_hs_existing	=  $(shell find src -name "*.hs" -follow)

# -- all .hs files in the src dir, including ones we need to preprocess.
src_hs_all	+= $(src_hs_existing)
src_hs_all	+= $(src_alex_hs)


# -- Configuration ---------------------------------------------------------------------------------
# -- use the $(Target) from make/config.mk to decide which ddc config file to use
src/Config/Config.hs : src/Config/Config.hs.$(Target)
	@echo "* Using configuration" $^
	cp $^ $@
	@echo


# -- Dependencies ----------------------------------------------------------------------------------
.PHONY	: deps
deps	: make/Makefile.deps

make/Makefile.deps : src/Config/Config.hs $(src_hs_existing) 
	@echo "* Building dependencies"
	@$(GHC) -isrc -M $^ -dep-makefile -optdepmake/Makefile.deps $(GHC_INCDIRS)
	@rm -f make/Makefile.deps.bak
	@cp make/Makefile.deps make/Makefile.deps.inc
	@echo


# -- Boilerplate ----------------------------------------------------------------------------------
# -- build the boiler plate generator
bin/plate : tools/plate/Main.hs src/Config/Config.hs
	@echo "* Building boilerplate generator -------------------------------"
	$(GHC) $(GHC_FLAGS) -isrc -itools/plate -o bin/plate --make $^ 

# -- generate boilerplate
src/Source/Plate/Trans.hs : bin/plate src/Source/Plate/Trans.hs-stub src/Source/Exp.hs 
	@echo "* Generating boilerplate for $@"
	bin/plate src/Source/Exp.hs src/Source/Plate/Trans.hs-stub src/Source/Plate/Trans.hs
	@echo


# -- Link DDC -------------------------------------------------------------------------------------
# -- all the resulting .o files we'll get after compiling the .hs files
src_obj		=  $(patsubst %.hs,%.o,$(src_hs_existing))

bin/ddc	: make/Makefile.deps $(src_obj)
	@echo "* Linking $@"
	$(GHC) -o bin/ddc $(GHC_FLAGS) $(DDC_PACKAGES) $(src_obj)
	@echo	


# -- External libraries ---------------------------------------------------------------------------
.PHONY	: external
external : external/TinyPTC-X11-0.7.3/xshm.o

external/TinyPTC-X11-0.7.3/xshm.o : 
	@echo "* Building external libraries ----------------------------------"
	cd external/TinyPTC-X11-0.7.3; $(MAKE) CFLAGS="$(GCC_FLAGS)"
	@echo


# -- Runtime system -------------------------------------------------------------------------------
runtime_c = \
	$(shell ls runtime/*.c) \
	$(shell find runtime/Prim -name "*.c")

runtime_dep	= $(patsubst %.c,%.dep,$(runtime_c))
runtime_o	= $(patsubst %.c,%.o,$(runtime_c))

runtime/libddc-runtime.$(SHARED_SUFFIX) : $(runtime_o)
	@echo "* Linking $@"
	$(BUILD_SHARED) -o $@ $^
	@echo

runtime/libddc-runtime.a  : $(runtime_o)
	@echo "* Building $@"
	ar r $@ $^
	@echo

.PHONY  : runtime
runtime : $(runtime_dep) runtime/libddc-runtime.$(SHARED_SUFFIX) runtime/libddc-runtime.a


# -- Base Libraries --------------------------------------------------------------------------------
.PHONY	: libs
libs	: library/Prelude.di

library/Prelude.di : bin/ddc
	@echo "* Building base libraries --------------------------------------"
	bin/ddc -O -build library/Prelude.ds
	@touch library/Prelude.di
	@echo


# -- Test Driver -----------------------------------------------------------------------------------
war_hs	= $(shell find tools/war -name "*.hs") $(shell find src/Util -name "*.hs")

bin/war : $(war_hs)
	$(GHC) $(GHC_FLAGS) -O2 -fglasgow-exts -threaded -fglasgow-exts \
		-isrc -itools/war --make tools/war/Main.hs -o bin/war


# -- Run all avaliable tests -----------------------------------------------------------------------
.PHONY	: test
test	: bin/ddc bin/war library/Prelude.di
	@echo "* Running tests ------------------------------------------------"
	bin/ddc --test
	@echo
	bin/war test
	@echo


# -- Haddock docs ----------------------------------------------------------------------------------
# -- build haddoc docs
nodoc	= \
	src/Source/Lexer.hs \
	src/Util/Tunnel.hs \
	src/Source/Type/SlurpA.hs \
	src/Source/Type/SlurpX.hs

.PHONY	: doc
doc	:
	@echo "* Building documentation"
	@haddock -h -o doc/haddock --optghc=-isrc --ignore-all-exports \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
		$(filter-out $(nodoc),$(src_hs_all))

# -- HLint -----------------------------------------------------------------------------------------
.PHONY	: hlint
hlint	: 
	@echo "* Running HLint"
	hlint 	\
		-i "Use camelCase" 	\
		-i "Use ."		\
		-i "Use :"		\
		-i "Use >>="		\
		-i "Eta reduce"		\
		src


# -- Cleaning --------------------------------------------------------------------------------------
# -- clean objects in the runtime system
.PHONY : cleanRuntime
cleanRuntime :
	@echo "* Cleaning runtime"
	@find runtime \
		    	-name "*.o" \
		-o	-name "*.dep" \
		-o	-name "*.so" \
		-o  -name "*.dylib" \
		-o	-name "*.a" \
		-o	-name "*~" \
		-follow | xargs -n 1 rm -f


# -- clean objects in the runtime system
.PHONY : cleanLibrary
cleanLibrary :
	@echo "* Cleaning library"
	@find library \
		    	-name "*.o" \
		-o	-name "*.dep" \
		-o	-name "*.so" \
		-o  -name "*.dylib" \
		-o	-name "*.a" \
		-o	-name "*.di" \
		-follow | xargs -n 1 rm -f


# -- clean up libraries and tests, but leave the compiler build alone
.PHONY  : cleanWar
cleanWar :
	@echo "* Cleaning war"
	@find test \
			-name "*.dump-*.*"  \
		-o	-name "*.graph-*.dot" \
		-o	-name "*.hi"    \
		-o	-name "build.mk" \
		-o	-name "*.di"    \
		-o	-name "*.gdl"   \
		-o	-name "*.o"     \
		-o	-name "*.ddc.c" \
		-o	-name "*.ddc.h" \
		-o	-type f -name "*.bin" \
		-o	-name "*.out"   \
		-o 	-name "*.diff"  \
		-o	-name "*.tix"   \
		-o	-name "*.compile.stderr" \
		-o	-name "*.compile.stdout" \
		-o	-name "*.stdout" \
		-o	-name "*.stderr" \
		-follow | xargs -n 1 rm -f


# -- clean up everything
.PHONY : clean
clean  : cleanWar cleanRuntime cleanLibrary
	@echo "* Cleaning leftovers"
	@find . \
			-name "*.o" \
		-o	-name "*.so" \
		-o  -name "*.dylib" \
		-o	-name "*.hi" \
		-o	-name "*.hi-boot" \
		-o	-name "*.hcr" \
		-o	-name "*.td" \
		-o	-name "*.ti" \
		-o	-name "Makefile.deps" \
		-follow | xargs -n 1 rm -f
		
	@rm -f doc/haddock/*
	@rm -f src/Config/Config.hs
	@rm -f make/Makefile.deps.inc
	@rm -f 	bin/* \
		make/Makefile.deps.bak 


include make/plate.mk
-include runtime/*.dep


# We include Makefile.deps.inc here instead of Makefile.deps directly.
#	Stupid GNU make treats missing files as dependencies, and if they are 
#	missing it tries to build them. This causes dependencies to be build
#	even when we do a "make clean"
#
# 	This behavior is different to the documentation which says
#	that missing -included files should be ignored.
#
-include make/Makefile.deps.inc
