# Build haddock docs

# We leave these out to break import loops.
nodoc	= \
	packages/ddc-main/src/Source/Lexer.hs \
	packages/ddc-main/src/Util/Tunnel.hs \
	packages/ddc-main/src/Source/Type/SlurpA.hs \
	packages/ddc-main/src/Source/Type/SlurpX.hs

.PHONY	: docs


# Building docs is split between the main compiler and ddci-core because some of the 
# names are shared. When the ddc-core package is ready we can make the main compiler
# use it and eliminate the duplication.
docs	:
	@echo "* Building haddock documentation ---------------------------------------------------"
	@$(MAKE) docs-main
	@$(MAKE) docs-core

docs-main :
	@haddock -w -h -o doc/haddock --optghc=-ipackages/ddc-main \
                $(patsubst %,--optghc=%,$(DDC_PACKAGES)) \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
		$(filter-out $(nodoc),$(ddc-main_src_hs_all))

docs-core :
	@haddock -w -h -o doc/haddock-core \
	        --optghc=-ipackages/ddc-base \
	        --optghc=-ipackages/ddc-core \
	        --optghc=-ipackages/ddc-core-interpreter \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
		$(ddci-core_src_hs_all)
