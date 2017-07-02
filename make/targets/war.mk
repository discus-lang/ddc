
# -- Find Source Files ----------------------------------------------------------------------------
war_src_hs_all  = $(shell find src/s1/ddc-war -name "*.hs" -follow)


# -- Dependencies ---------------------------------------------------------------------------------
make/deps/Makefile-war.deps : $(war_src_hs_all)
	@echo "* Building dependencies (war)"
	@$(GHC) -M $^ -dep-makefile make/deps/Makefile-war.deps \
                -dep-suffix "" $(GHC_INCDIRS)
	@rm -f make/deps/Makefile-war.deps.bak
	@cp make/deps/Makefile-war.deps make/deps/Makefile-war.deps.inc


# -- Link war -------------------------------------------------------------------------------------
war_obj         = $(patsubst %.hs,%.o,$(war_src_hs_all))

bin/war : $(war_obj)
	@echo "* Linking war"
	@$(GHC) -o bin/war $(GHC_FLAGS) -O2 -threaded $(DDC_PACKAGES) $(war_obj)


# -- Running tests --------------------------------------------------------------------------------
# .. for the war against bugs.

# Run the testsuite in just the standard way.
.PHONY 	: war
war : allWithConfig
	@echo "* Running tests -------------------------------------------------------------"
	@bin/ddc -build src/s2/base/base.build
	@bin/war test/ddc-demo test/ddc-spec test/ddc-regress \
                -j $(THREADS) \
                -results        war.results \
                -results-failed war.failed \
		+compway std \
                +compway opt -O
	@echo


# Alias for war
.PHONY  : test
test    : war


# Run the testsuite with the LLVM backend.
.PHONY 	: llvmwar
llvmwar : allWithConfig
	@echo "* Running tests -------------------------------------------------------------"
	@bin/ddc -build src/s2/base/base.build
	@bin/war test/ddc-demo test/ddc-regress \
                -j $(THREADS) \
                -results        war.results \
                -results-failed war.failed \
                +compway llvm -fvia-llvm
	@echo


# Run tests in all ways.
.PHONY  : totalwar
totalwar : allWithConfig
	@echo "* Running tests -------------------------------------------------------------"
	@bin/ddc -build src/s2/base/base.build
	@bin/war test/ddc-demo test/ddc-regress \
                -j $(THREADS) \
                -results        war.results  \
                -results-failed war.failed   \
		+compway llvm  -fvia-llvm    \
		+compway llvmo -fvia-llvm -O
	@echo


# Run tests in all ways in batch mode.
# This is used by the nightly build
.PHONY  : batchwar
batchwar : allWithConfig
	@echo "* Running tests -------------------------------------------------------------"
	@bin/ddc -build src/s2/base/base.build
	@bin/war test/ddc-demo test/ddc-regress \
                -batch \
                -j $(THREADS) \
                -results        war.results  \
                -results-failed war.failed   \
		+compway llvm  -fvia-llvm    \
		+compway llvmo -fvia-llvm -O
	@echo


# -- Clean junk dropped while testing -------------------------------------------------------------
.PHONY  : clean-war
clean-war :
	@echo "* Cleaning war"
	@find test \
			-name "*.dump-*" \
                -o      -name "dump.*" \
		-o	-name "*.hi"    \
		-o	-name "*.di"    \
		-o	-name "*.gdl"   \
		-o	-name "*.o"     \
		-o	-name "*.ddc.c" \
		-o	-name "*.ddc.h" \
		-o	-name "*.ddc.ll" \
		-o	-name "*.ddc.s" \
		-o	-name "*.out"   \
		-o 	-name "*.diff"  \
		-o	-name "*.tix"   \
		-o	-name "*.compile.stderr" \
		-o	-name "*.compile.stdout" \
		-o	-name "*.stdout" \
		-o	-name "*.stderr" \
		-o      -name "build.mk" \
                -o	-name "war-*" \
		-o    -type f -name "*.bin" \
                -follow | xargs -n 1 rm -Rf

