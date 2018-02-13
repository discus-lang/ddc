# Build haddock docs

.PHONY	: docs

# Building docs is split between the main compiler and ddci-core because some of the
# names are shared. When the ddc-core package is ready we can make the main compiler
# use it and eliminate the duplication.
docs	:
	@echo "* Building haddock documentation ---------------------------------------------------"
	@$(MAKE) docs-core

docs-core :
	@$(HADDOCK) -w -h -o doc/haddock-core \
			--optghc=-isrc/s1/ddc-build \
			--optghc=-isrc/s1/ddc-core \
			--optghc=-isrc/s1/ddc-core-llvm \
			--optghc=-isrc/s1/ddc-core-salt \
			--optghc=-isrc/s1/ddc-core-simpl \
			--optghc=-isrc/s1/ddc-core-flow \
			--optghc=-isrc/s1/ddc-core-machine \
			--optghc=-isrc/s1/ddc-core-discus \
                        --optghc=-isrc/s1/ddc-source-discus \
			--optghc=-isrc/s1/ddc-driver \
			--optghc=-isrc/s1/ddci-core \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
                $(patsubst %,--optghc=%,$(GHC_WARNINGS)) \
		$(ddci-core_src_hs_all)

# Build hoogle docs
docs-hoogle	:
	@echo "* Building hoogle documentation ---------------------------------------------------"
	@$(MAKE) docs-hoogle-core

docs-hoogle-core :
	@$(HADDOCK) --hoogle -w -o doc/hoogle-core \
			--optghc=-isrc/s1/ddc-build \
			--optghc=-isrc/s1/ddc-core \
			--optghc=-isrc/s1/ddc-core-llvm \
			--optghc=-isrc/s1/ddc-core-salt \
			--optghc=-isrc/s1/ddc-core-simpl \
			--optghc=-isrc/s1/ddc-core-flow \
			--optghc=-isrc/s1/ddc-core-machine \
			--optghc=-isrc/s1/ddc-core-discus \
                        --optghc=-isrc/s1/ddc-source-discus \
			--optghc=-isrc/s1/ddc-driver \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
                $(patsubst %,--optghc=%,$(GHC_WARNINGS)) \
		$(ddci-core_src_hs_all)
