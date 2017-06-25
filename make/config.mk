# Default build configuration.
#
#   This file is under version control.
#   If you want to override these options then create a file make/config-override.mk
#   and assign the appropdiate variables there.
#

# Set the optimisations to enable when compiling the compiler.
#     distro     - fastest at runtime.
#     devel      - development compile (faster compiler build).
#     devel_prof - development compile with profiling.
BUILDFLAVOUR	= distro

# Number of jobs to use during make.
THREADS		= 3


# GHC Config ------------------------------------------------------------------
# GHC binary to use when building.
GHC		= ghc
GHCI		= ghci
# with a cabal sandbox
# GHC    = cabal exec ghc --
# GHCI    = cabal exec ghci --

GHC_VERSION	= $(shell $(GHC) --version | sed -e "s/.* //g" -e "s/\..*//")
GHC_VERSION_FLAGS = -rtsopts


# Haddock Config
HADDOCK = haddock
# with a cabal sandbox
# HADDOCK = cabal exec haddock --


# Use the linear solver with ddc-core-flow.
#  This requires the limp and limp-cbc packages to be installed.
DDC_FLOW_USE_LINEAR_SOLVER = 0
# If you modify this, you should make recompile affected files:
# touch packages/ddc-core-flow/DDC/Core/Flow/Transform/Rates/Clusters.hs
# touch packages/ddc-core-flow/DDC/Core/Flow/Transform/Rates/Clusters/Linear.hs


# Override default config with local config.
-include make/config-override.mk



