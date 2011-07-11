
# -- The driver program ---------------------------------------------------------------------------
war_hs	= $(shell find tools/war -name "*.hs") $(shell find src/Util -name "*.hs")

bin/war : $(war_hs)
	@echo "* Building war test driver ---------------------------------------------------------"
	@$(GHC) $(GHC_FLAGS) $(DDC_PACKAGES) -O2 -threaded  \
		-isrc -itools/war --make tools/war/Main.hs -o bin/war

# -- Running tests --------------------------------------------------------------------------------
# .. for the war against bugs.

# Run the testsuite interactively
.PHONY 	: war
war : bin/ddc runtime bin/war library/Prelude.di
	@echo "* Running tests --------------------------------------------------------------------"
	bin/war test -j $(THREADS)
	@echo

# Run tests in all ways interactively
.PHONY  : totalwar
totalwar : bin/ddc bin/war library/Prelude.di
	@echo "* Running tests --------------------------------------------------------------------"
	bin/war test -j $(THREADS) \
		+compway normal \
		+compway opt  -O \
		+compway llvm -O -fvia-llvm
	@echo

# Run the tests,  logging failures to war.failed
.PHONY : logwar
logwar : bin/ddc bin/war library/Prelude.di
	@echo "* Running tests --------------------------------------------------------------------"
	bin/war test -j $(THREADS) \
		-batch -logFailed "war.failed"
	@echo

# Run tests in all ways interactively, logging failures to war.failed
.PHONY  : totallogwar
totallogwar : bin/ddc bin/war library/Prelude.di
	@echo "* Running tests --------------------------------------------------------------------"
	bin/war test -j $(THREADS) \
		-batch -logFailed "war.failed" \
		+compway normal \
		+compway opt -O \
		+compway llvm -O -fvia-llvm
	@echo

# Alias for war
.PHONY	: test
test	: war


# -- Clean junk dropped while testing -------------------------------------------------------------
.PHONY  : cleanWar
cleanWar :
	@echo "* Cleaning war"
	@find test \
			-name "*.dump-*"  \
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
