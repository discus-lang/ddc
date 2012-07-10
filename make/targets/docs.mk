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
								--optghc=-ipackages/ddc-build \
					--optghc=-ipackages/ddc-core \
								--optghc=-ipackages/ddc-core-eval \
								--optghc=-ipackages/ddc-core-llvm \
								--optghc=-ipackages/ddc-core-salt \
								--optghc=-ipackages/ddc-core-simpl \
								--optghc=-ipackages/ddc-core-llvm \
								--optghc=-ipackages/ddc-type \
					--optghc=-ipackages/ddci-core \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
		$(ddci-core_src_hs_all)

# Build hoogle docs
docs-hoogle	:
	@echo "* Building hoogle documentation ---------------------------------------------------"
	@$(MAKE) docs-hoogle-main
	@$(MAKE) docs-hoogle-core

docs-hoogle-main :
	@haddock --hoogle -w -o doc/hoogle --optghc=-ipackages/ddc-main \
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
								--optghc=-ipackages/ddc-core-llvm \
								--optghc=-ipackages/ddc-type \
					--optghc=-ipackages/ddci-core \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
		$(ddci-core_src_hs_all)
