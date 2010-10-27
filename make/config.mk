# Default build configuration.
# 
#   This file is under version control.
#   If you want to override these options then create a file make/config-override.deps 
#   and assign the appropdiate variables there.
#

# Set the optimisations to enable when compiling the compiler.
#     distro     - fastest at runtime.
#     devel      - development compile (faster compiler build).
#     devel_prof - development compile with profiling.
BUILDFLAVOUR	= distro

# The name of the GHC to use when building.
GHC		= ghc

# Number of jobs to use during make.
THREADS		= 2

# Override the above config.
-include make/config-override.deps



