
# targets:
#       all             -- build everything
#       deps            -- build dependencies
#
#       bin/ddc         -- build the compiler binary
#       bin/war         -- build the test driver
#       bin/churn       -- build the test churner
#       bin/plate       -- build the boilerplate generator
#
#       clean           -- clean everything
#       cleanWar        -- clean libraries and tests, but leave the compiler build alone
#       cleanRuntime    -- clean the runtime system
#
#       war             -- run the tests
#       churn           -- run the churner for a while

# -- build everything
all	: src/Config/Config.hs bin/ddc bin/war bin/churn runtime external

include make/build.mk
include make/plate.mk
-include make/Makefile.deps
-include runtime/*.dep


# -- Configuration ---------------------------------------------------------------------------------
# -- use the $(Target) from make/config.mk to decide which ddc config file to use

src/Config/Config.hs : src/Config/Config.hs.$(Target)
	@echo "* Using configuration" $^
	cp $^ $@
	@echo


# -- Dependencies ----------------------------------------------------------------------------------
.PHONY : deps
deps : make/Makefile.deps $(runtime_dep)
	@echo

make/Makefile.deps : $(src_hs) src/Config/Config.hs
	@echo "* Building dependencies"
	@$(GHC) -isrc -M $^ -optdep-f -optdepmake/Makefile.deps $(GHC_INCDIRS)
	@rm -f make/Makefile.deps.bak
	@echo


# -- Building --------------------------------------------------------------------------------------

# -- build the boiler plate generator
bin/plate : tools/plate/Main.hs src/Config/Config.hs
	$(GHC) $(GHC_FLAGS) -isrc -itools/plate -o bin/plate --make $^ 

# -- generate boilerplate
src/Source/Plate/Trans.hs : src/Source/Plate/Trans.hs-stub src/Source/Exp.hs bin/plate
	@echo "* Generating boilerplate for $@"
	bin/plate src/Source/Exp.hs src/Source/Plate/Trans.hs-stub src/Source/Plate/Trans.hs
	@echo

# -- build the main compiler
bin/ddc	: $(obj) $(GHC_INCOBJS)
	@echo "* Linking $@"
	$(GHC) $(GHC_FLAGS) -o bin/ddc \
		-package unix -package mtl -package containers -package parsec -package regex-compat \
		$^ 
	@echo	
	

# -- build the test driver
bin/war : tools/war/War.hs tools/war/Diff.hs tools/war/Interface.hs tools/war/Order.hs tools/war/Bits.hs tools/war/TestSource.hs
	$(GHC) $(GHC_FLAGS) -fglasgow-exts -isrc -itools/war --make tools/war/War.hs -o bin/war

# -- build the churner
bin/churn : tools/churn/Main.hs tools/churn/Bits.hs tools/churn/Base.hs tools/churn/Exp.hs
	$(GHC) $(GHC_FLAGS) -fglasgow-exts -isrc -itools/churn --make tools/churn/Main.hs -o bin/churn

# -- build the runtime system
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
runtime : runtime/libddc-runtime.$(SHARED_SUFFIX) runtime/libddc-runtime.a

# -- build external libraries
.PHONY	: external
external :
	@echo "* Building external libraries"
	cd external/TinyPTC-X11-0.7.3; make CFLAGS="$(GCC_FLAGS)"

# -- build haddoc docs
nodoc	= \
	src/Source/Lexer.hs \
	src/Util/Tunnel.hs \
	src/Source/Type/SlurpA.hs \
	src/Source/Type/SlurpX.hs

.PHONY	: doc
doc	: $(filter-out $(nodoc),$(src_hs))
	@echo "* Building documentation"
	@haddock -h -o doc/haddock --ignore-all-exports $^ 


# -- Testing ---------------------------------------------------------------------------------------

# -- run all the tests
.PHONY : war
war : 
	@echo "* Building tests"
	@bin/war

# -- run the churner for a while
.PHONY : churn
churn : 
	@echo "* Churning compiler"
	@bin/churn

# -- mark up hpc (this is broken)
.PHONY : hpcmarkup
hpcmarkup :
	@echo "* Marking up HPC output"
	@hpc markup --destdir=doc/hpc ddc.tix

	
# -- Cleaning --------------------------------------------------------------------------------------

# -- clean objects in the runtime system
.PHONY : cleanRuntime
cleanRuntime :
	@echo "* Cleaning up runtime"
	@find runtime \
		    	-name "*.o" \
		-o	-name "*.dep" \
		-o	-name "*.so" \
		-o  -name "*.dylib" \
		-o	-name "*.a" \
		-follow | xargs -n 1 rm -f
	@echo		

# -- clean up libraries and tests, but leave the compiler build alone
.PHONY  : cleanWar
cleanWar :
	@echo "* Cleaning up war"
	@find library test demo \
			-name "*.dump-*.*"  \
		-o	-name "*.graph-*.dot" \
		-o	-name "*.di"    \
		-o	-name "*.gdl"   \
		-o	-name "*.o"     \
		-o	-name "*.ddc.c" \
		-o	-name "*.ddc.h" \
		-o	-name "*.bin"   \
		-o	-name "*.out"   \
		-o 	-name "*.diff"  \
		-o	-name "*.tix"   \
		-follow | xargs -n 1 rm -f

	@rm -f churn/scratch/*
	
	@echo


# -- clean up everything
.PHONY : clean
clean  : cleanWar cleanRuntime
	@echo "* Cleaning up leftovers"
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
		
	@rm -f 	bin/* \
		make/Makefile.deps.bak 

	@echo

