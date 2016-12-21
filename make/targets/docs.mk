# Build haddock docs

.PHONY	: docs

# Building docs is split between the main compiler and ddci-core because some of the 
# names are shared. When the ddc-core package is ready we can make the main compiler
# use it and eliminate the duplication.
docs	:
	@echo "* Building haddock documentation ---------------------------------------------------"
	@$(MAKE) docs-core

docs-core :
	@haddock -w -h -o doc/haddock-core \
			--optghc=-ipackages/ddc-build \
			--optghc=-ipackages/ddc-core \
			--optghc=-ipackages/ddc-core-llvm \
			--optghc=-ipackages/ddc-core-salt \
			--optghc=-ipackages/ddc-core-simpl \
			--optghc=-ipackages/ddc-core-flow \
			--optghc=-ipackages/ddc-core-machine \
			--optghc=-ipackages/ddc-core-tetra \
                        --optghc=-ipackages/ddc-source-tetra \
			--optghc=-ipackages/ddc-driver \
			--optghc=-ipackages/ddci-core \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
                $(patsubst %,--optghc=%,$(GHC_WARNINGS)) \
		$(ddci-core_src_hs_all)

# Build hoogle docs
docs-hoogle	:
	@echo "* Building hoogle documentation ---------------------------------------------------"
	@$(MAKE) docs-hoogle-core

docs-hoogle-core :
	@haddock --hoogle -w -o doc/hoogle-core \
			--optghc=-ipackages/ddc-build \
			--optghc=-ipackages/ddc-core \
			--optghc=-ipackages/ddc-core-llvm \
			--optghc=-ipackages/ddc-core-salt \
			--optghc=-ipackages/ddc-core-simpl \
			--optghc=-ipackages/ddc-core-flow \
			--optghc=-ipackages/ddc-core-machine \
			--optghc=-ipackages/ddc-core-tetra \
                        --optghc=-ipackages/ddc-source-tetra \
			--optghc=-ipackages/ddc-driver \
		$(patsubst %,--optghc=%,$(GHC_LANGUAGE)) \
                $(patsubst %,--optghc=%,$(GHC_WARNINGS)) \
		$(ddci-core_src_hs_all)
