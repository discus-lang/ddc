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

GHC_VERSION	= $(shell $(GHC) --version | sed -e "s/.* //g" -e "s/\..*//")

ifeq "$(GHC_VERSION)" "7"
GHC_VERSION_FLAGS = -rtsopts
endif


# Coq Config ------------------------------------------------------------------
# Coq binaries to use when building
COQDEP		= coqdep
COQC		= coqc


# Override the above config.
-include make/config-override.mk



