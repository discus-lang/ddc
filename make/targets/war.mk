
# -- The driver program ---------------------------------------------------------------------------
war_hs	= $(shell find packages/ddc-war -name "*.hs") \
          $(shell find packages/ddc-alpha/src/Util -name "*.hs")

bin/war : $(war_hs)
	@$(GHC) $(GHC_FLAGS) $(DDC_PACKAGES) -Wall -fno-warn-unused-do-bind -O2 -threaded  \
		-ipackages/ddc-war --make packages/ddc-war/Main.hs -o bin/war


# -- Running tests --------------------------------------------------------------------------------
# .. for the war against bugs.

# Run the testsuite in just the standard way.
.PHONY 	: war
war : newWithConfig
	@echo "* Running tests -------------------------------------------------------------"
	@bin/war test/ddc-main \
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
	@bin/war test \
                -j $(THREADS) \
                -results        war.results \
                -results-failed war.failed \
                +compway llvm -fvia-llvm
	@echo


# Run tests in all ways.
.PHONY  : totalwar
totalwar : allWithConfig
	@echo "* Running tests -------------------------------------------------------------"
	@bin/war test \
                -j $(THREADS) \
                -results        war.results  \
                -results-failed war.failed   \
		+compway viac  -fvia-c       \
                +compway viaco -fvia-c    -O \
		+compway llvm  -fvia-llvm    \
		+compway llvmo -fvia-llvm -O
	@echo


# Run tests in all ways in batch mode.
# This is used by the nightly build
.PHONY  : batchwar
batchwar : allWithConfig
	@echo "* Running tests -------------------------------------------------------------"
	@bin/war test \
                -batch \
                -j $(THREADS) \
                -results        war.results  \
                -results-failed war.failed   \
		+compway viac  -fvia-c       \
                +compway viaco -fvia-c    -O \
		+compway llvm  -fvia-llvm    \
		+compway llvmo -fvia-llvm -O
	@echo


# -- Clean junk dropped while testing -------------------------------------------------------------
.PHONY  : cleanWar
cleanWar :
	@echo "* Cleaning war"
	@find test \
			-name "*.dump-*" \
                -o      -name "dump.*" \
		-o	-name "*.graph-*.dot" \
		-o	-name "*.hi"    \
		-o	-name "build.mk" \
		-o	-name "*.di"    \
		-o	-name "*.di-new" \
		-o	-name "*.gdl"   \
		-o	-name "*.o"     \
		-o	-name "*.ddc.c" \
		-o	-name "*.ddc.h" \
		-o	-name "*.ddc.ll" \
		-o	-name "*.ddc.s" \
		-o	-type f -name "*.bin" \
		-o	-name "*.out"   \
		-o 	-name "*.diff"  \
		-o	-name "*.tix"   \
		-o	-name "*.compile.stderr" \
		-o	-name "*.compile.stdout" \
		-o	-name "*.stdout" \
		-o	-name "*.stderr" \
		-o	-name "war-*" \
		-follow | xargs -n 1 rm -Rf

