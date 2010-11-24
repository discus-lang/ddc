# The DDC build system.

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

# -- Meta Targets -------------------------------------------------------------
#    These may recursively invoke make to do several things.
#    These are the ONLY instances of recursive makes in the system.

# -- Build the compiler and libs
.PHONY	: all
all 	:
	@$(MAKE) allWithConfig

include make/build.mk

.PHONY	: allWithConfig
allWithConfig :
	@$(MAKE) src/Source/Lexer.hs
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
	@alex --version
	@echo
	@ghc-pkg list | grep "QuickCheck\|regex-base\|regex-posix\|regex-compat\|haskell-src\|parsec\|buildbox\|text"
	@echo
	@sh -c 'llc --version || exit 0'
	@echo
	@echo "------------------------------------------------------------------------------------"
	@$(MAKE) cleantotal


# -- Real Targets -------------------------------------------------------------
#    These don't recursively invoke make.
#
include make/targets/plate.mk
include make/targets/external.mk
include make/targets/runtime.mk
include make/targets/libs.mk
include make/targets/docs.mk
include make/targets/war.mk
include make/targets/lint.mk
include make/targets/tarball.mk
include make/targets/clean.mk
include make/targets/ddc.mk


# -- Include magic ------------------------------------------------------------
include make/rules.mk
-include runtime/*.dep

# We include Makefile.deps.inc here instead of Makefile.deps directly.
#   Stupid GNU make treats missing files as dependencies, and if they are
#   missing it tries to build them. This causes dependencies to be built
#   even when we do a "make clean"
#
#   This behavior is different to the documentation which says
#   that missing -included files should be ignored.
#
-include make/Makefile.deps.inc
