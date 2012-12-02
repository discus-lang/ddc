# Build haddock docs

# We leave these out to break import loops.
nodoc	= \
	packages/ddc-alpha/src/Source/Lexer.hs \
	packages/ddc-alpha/src/Util/Tunnel.hs \
	packages/ddc-alpha/src/Source/Type/SlurpA.hs \
	packages/ddc-alpha/src/Source/Type/SlurpX.hs

.PHONY	: docs


# Building docs is split between the main compiler and ddci-core because some of the 
# names are shared. When the ddc-core package is ready we can make the main compiler
# use it and eliminate the duplication.
docs	:
	@echo "* Building haddock documentation ---------------------------------------------------"
	@$(MAKE) docs-alpha
	@$(MAKE) docs-core

docs-alpha :
	@haddock -w -h -o doc/haddock --optghc=-ipackages/ddc-alpha/src \
                $(patsubst %,--optghc=%,$(DDC_PACKAGES)) \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
		$(filter-out $(nodoc),$(ddc-alpha_src_hs_all))

docs-core :
	@haddock -w -h -o doc/haddock-core \
                        --optghc=-ipackages/ddc-base \
                        --optghc=-ipackages/ddc-build \
                        --optghc=-ipackages/ddc-core \
			--optghc=-ipackages/ddc-core-eval \
			--optghc=-ipackages/ddc-core-llvm \
			--optghc=-ipackages/ddc-core-salt \
			--optghc=-ipackages/ddc-core-simpl \
                        --optghc=-ipackages/ddc-driver \
                        --optghc=-ipackages/ddci-core \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
		$(ddci-core_src_hs_all)

# Build hoogle docs
docs-hoogle	:
	@echo "* Building hoogle documentation ---------------------------------------------------"
	@$(MAKE) docs-hoogle-alpha
	@$(MAKE) docs-hoogle-core

docs-hoogle-alpha :
	@haddock --hoogle -w -o doc/hoogle 
                        --optghc=-ipackages/ddc-alpha/src \
			$(patsubst %,--optghc=%,$(DDC_PACKAGES)) \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
		$(filter-out $(nodoc),$(ddc-main_src_hs_all))

docs-hoogle-core :
	@haddock --hoogle -w -o doc/hoogle-core \
                        --optghc=-ipackages/ddc-base \
                        --optghc=-ipackages/ddc-build \
                        --optghc=-ipackages/ddc-core \
			--optghc=-ipackages/ddc-core-eval \
			--optghc=-ipackages/ddc-core-llvm \
			--optghc=-ipackages/ddc-core-salt \
			--optghc=-ipackages/ddc-core-simpl \
                        --optghc=-ipackages/ddc-driver \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
		$(ddci-core_src_hs_all)
