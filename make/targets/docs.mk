# Build haddock docs

# We leave these out to break import loops.
nodoc	= \
	packages/ddc-main/src/Source/Lexer.hs \
	packages/ddc-main/src/Util/Tunnel.hs \
	packages/ddc-main/src/Source/Type/SlurpA.hs \
	packages/ddc-main/src/Source/Type/SlurpX.hs

.PHONY	: docs
docs	:
	@echo "* Building haddock documentation ---------------------------------------------------"
	@haddock -w -h -o doc/haddock --optghc=-ipackages/ddc-main \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
		$(filter-out $(nodoc),$(src_hs_all))
	@echo
