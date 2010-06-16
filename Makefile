
#   (Build targets)
#       all             -- build the compiler and libs (default)
#
#       total           -- build the compiler, libs, docs and run all the tests in all ways (slow) 
#       cleantotal      -- same as above, but do a full clean first (slowest) 
#
#       deps            -- build dependencies (should be automatic with the 'all' target)
#       bin/ddc         -- build the compiler binary
#       bin/war         -- build the test driver
#       bin/plate       -- build the boilerplate generator
#	runtime		-- build the runtime system
#	external	-- build external libraries
#	libs		-- build base libraries
#	docs		-- build Haddock docks
#
#   (Running just the quickcheck and regression tests)
#	war		-- run the minimal testing required before pushing patches (interactive)
#       logwar          -- same as above, logging failures to war.failed  (non-interative)
#       totalwar        -- run tests in all possible ways                 (slow, interactive)
#       totallogwar     -- same as above, logging failures to war.failed  (slow, non-interative)
#
#   (Running code quality tools)
#	hlint		-- run hlint
#
#   (Cleaning up)
#       clean           -- clean everything
#       cleanWar        -- clean libraries and tests, but leave the compiler build alone
#       cleanRuntime    -- clean the runtime system
#       cleanLibrary    -- clean out the libraries
#
#

# -- Build the compiler and libs
.PHONY	: all
all 	:
	@$(MAKE) allWithConfig

include make/build.mk

.PHONY	: allWithConfig
allWithConfig :
	@$(MAKE) deps
	@$(MAKE) bin/ddc bin/war runtime external libs -j $(THREADS)


# -- Build the compiler, libs, docs, and run all the tests in all ways (slow)
.PHONY  : total
total	: 
	@$(MAKE) allWithConfig
	@$(MAKE) docs
	@$(MAKE) totallogwar 


# -- Same as 'total', but do a full clean first
.PHONY  : cleantotal
cleantotal : 
	@$(MAKE) clean
	@$(MAKE) total	


# -- What to do during the nightly builds
.PHONY  : nightly
nightly :
	@date
	@echo
	@echo "                              DDC Nightly build"
	@echo "------------------------------------------------------------------------------------"
	@$(MAKE) --version
	@echo
	@gcc --version
	@echo
	@$(GHC) --version
	@echo
	@ghc-pkg list | grep "QuickCheck\|regex-base\|regex-posix\|regex-compat\|haskell-src\|parsec"
	@echo
	@echo "------------------------------------------------------------------------------------"
	@$(MAKE) cleantotal
		

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
	@echo "* Building boilerplate generator ---------------------------------------------------"
	$(GHC) $(GHC_FLAGS) -isrc -itools/plate -o bin/plate --make $^ 

# -- generate boilerplate
src/Source/Plate/Trans.hs : bin/plate src/Source/Plate/Trans.hs-stub src/Source/Exp.hs 
	@echo
	@echo "* Generating boilerplate for $@"
	bin/plate src/Source/Exp.hs src/Source/Plate/Trans.hs-stub src/Source/Plate/Trans.hs
	@echo


# -- Link DDC -------------------------------------------------------------------------------------
# -- all the resulting .o files we'll get after compiling the .hs files
src_obj		=  $(patsubst %.hs,%.o,$(src_hs_existing))

bin/ddc	: make/Makefile.deps $(src_obj)
	@echo
	@echo "* Linking $@"
	$(GHC) -o bin/ddc $(GHC_FLAGS) $(DDC_PACKAGES) $(src_obj)
	@echo


# -- External libraries ---------------------------------------------------------------------------
.PHONY	: external
external : external/TinyPTC-X11-0.7.3/xshm.o

external/TinyPTC-X11-0.7.3/xshm.o : 
	@echo "* Building external libraries ------------------------------------------------------"
	cd external/TinyPTC-X11-0.7.3; $(MAKE) CFLAGS="$(GCC_FLAGS)"
	@echo


# -- Runtime system -------------------------------------------------------------------------------
runtime_c = \
	$(shell ls runtime/*.c) \
	$(shell find runtime/Prim -name "*.c")

runtime_dep	= $(patsubst %.c,%.dep,$(runtime_c))
runtime_o	= $(patsubst %.c,%.o,$(runtime_c))

runtime/libddc-runtime.$(SHARED_SUFFIX) : $(runtime_o)
	@echo
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
libs	: library/Graphics.di

library/Prelude.di library/Graphics.di : bin/ddc
	@echo "* Building base libraries ----------------------------------------------------------"
	bin/ddc -O -build library/Prelude.ds
	@touch library/Prelude.di
	
	@echo
	bin/ddc -O -build library/Graphics.ds
	@touch library/Graphics.di
	@echo


# -- Test Driver -----------------------------------------------------------------------------------
war_hs	= $(shell find tools/war -name "*.hs") $(shell find src/Util -name "*.hs")

bin/war : $(war_hs)
	$(GHC) $(GHC_FLAGS) -O2 -fglasgow-exts -threaded -fglasgow-exts \
		-isrc -itools/war --make tools/war/Main.hs -o bin/war


# -- Haddock docs ----------------------------------------------------------------------------------
# -- build haddoc docs
nodoc	= \
	src/Source/Lexer.hs \
	src/Util/Tunnel.hs \
	src/Source/Type/SlurpA.hs \
	src/Source/Type/SlurpX.hs

.PHONY	: docs
docs	:
	@echo "* Building haddock documentation"
	@haddock -w -h -o doc/haddock --optghc=-isrc \
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


# -- War ------------------------------------------------------------------------------------------
# Running the testsuite, for the war against bugs

# Run the testsuite interactively
.PHONY 	: war
war : bin/ddc bin/war library/Prelude.di
	@echo "* Running tests --------------------------------------------------------------------"
	bin/war test -j $(THREADS)
	@echo

# Run tests in all ways interactively
.PHONY  : totalwar
totalwar : bin/ddc bin/war library/Prelude.di
	@echo "* Running tests --------------------------------------------------------------------"
	bin/war test -j $(THREADS) +compway normal +compway opt -O
	@echo

# Run the tests,  logging failures to war.failed
.PHONY : logwar
logwar : bin/ddc bin/war library/Prelude.di
	@echo "* Running tests --------------------------------------------------------------------"
	bin/war test -j $(THREADS) -batch -logFailed "war.failed" 
	@echo

# Run tests in all ways interactively, logging failures to war.failed
.PHONY  : totallogwar
totallogwar : bin/ddc bin/war library/Prelude.di
	@echo "* Running tests --------------------------------------------------------------------"
	bin/war test -j $(THREADS) -batch -logFailed "war.failed" +compway normal +compway opt -O 
	@echo

# Alias for war
.PHONY	: test
test	: war

		
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
		-o	-name "*.di-new" \
		-o	-name "*.ddc.c" \
		-o	-name "*.ddc.h" \
		-o	-name "*.dump-*.*"  \
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
		-o	-name "*.di-new" \
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


# -- Tarball ---------------------------------------------------------------------------------------
# -- make a tarball for distribution

srcdir := $(shell pwd)
datestamp := $(shell date "+%Y%m%d")
dateseconds := $(shell date "+%s")
tmpdir = ddc-head-$(dateseconds)
tarname = $(srcdir)/ddc-head-$(datestamp).tgz

.PHONY  : tarball
tarball :
	@echo "* Creating current tarball"
	@mkdir /tmp/$(tmpdir)
	@cd /tmp/$(tmpdir) && darcs get $(srcdir) ddc-head && tar zcf $(tarname) ddc-head
	@rm -rf /tmp/$(tmpdir)
	@chmod g+w,a+r $(tarname)
	@echo "* Tarball is :" $(tarname)

# -- Include magic ---------------------------------------------------------------------------------

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
