
# -- The driver program ---------------------------------------------------------------------------
war_hs	= $(shell find tools/war -name "*.hs") $(shell find src/Util -name "*.hs")

bin/war : $(war_hs)
	$(GHC) $(GHC_FLAGS) $(DDC_PACKAGES) -O2 -fglasgow-exts -threaded -fglasgow-exts \
		-isrc -itools/war --make tools/war/Main.hs -o bin/war

# -- The new driver program (in development)
war3_hs	= $(shell find tools/war3 -name "*.hs") $(shell find src/Util -name "*.hs")
bin/war3 : $(war3_hs)
	$(GHC) $(GHC_FLAGS) $(DDC_PACKAGES) -O2 -fglasgow-exts -threaded -fglasgow-exts \
		-isrc -itools/war3 --make tools/war3/Main.hs -o bin/war3


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


# -- Clean junk dropped while testing -------------------------------------------------------------
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
