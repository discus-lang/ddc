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
THREADS		= 1


# GHC Config ------------------------------------------------------------------
# GHC binary to use when building.
GHC		= ghc
GHCI		= ghci
# with a cabal sandbox
# GHC    = cabal exec ghc --
# GHCI    = cabal exec ghci --
GHC_VERSION	= $(shell $(GHC) --version | sed -e "s/.* //g" -e "s/\..*//")
GHC_VERSION_FLAGS = -rtsopts

GHC_DEPS_HI_TO_O = sed 's/\.hi$$/.o/'

# Linear solver time
DDC_FLOW_USE_LINEAR_SOLVER = 1
# If you modify this, you should make recompile affected files:
# touch packages/ddc-core-flow/DDC/Core/Flow/Transform/Rates/Clusters.hs 
# touch packages/ddc-core-flow/DDC/Core/Flow/Transform/Rates/Clusters/Linear.hs 

# Override default config with local config.
-include make/config-override.mk



