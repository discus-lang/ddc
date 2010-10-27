# Build haddock docs

# We leave these out to break import loops.
nodoc	= \
	src/Source/Lexer.hs \
	src/Util/Tunnel.hs \
	src/Source/Type/SlurpA.hs \
	src/Source/Type/SlurpX.hs

.PHONY	: docs
docs	:
	@echo "* Building haddock documentation ---------------------------------------------------"
	@haddock -w -h -o doc/haddock --optghc=-isrc \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
		$(filter-out $(nodoc),$(src_hs_all))
	@echo
