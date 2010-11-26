# Setting 
include make/config.mk
include make/config/options.mk

BuildFlavour    := $(strip $(BUILDFLAVOUR))

# -- Distribution compile (fastest at runtime)
ifeq "$(BuildFlavour)" "distro"
GHC_FLAGS	:= -rtsopts -O2 $(GHC_WARNINGS) $(GHC_LANGUAGE)
GCC_FLAGS	:= -std=c99 -O3 -Wundef

# -- Development Compile (fastest compile)
else ifeq "$(BuildFlavour)" "devel"
GHC_FLAGS	:= -rtsopts -O0 $(GHC_WARNINGS) $(GHC_LANGUAGE)
GCC_FLAGS	:= -std=c99 -O3 -Wall -Wextra

# -- Debug compile
else ifeq "$(BuildFlavour)" "devel_debug"
GHC_FLAGS	:= -rtsopts -O0 $(GHC_WARNINGS) $(GHC_LANGUAGE)
GCC_FLAGS	:= -std=c99 -Wundef -g

# -- Profiling compile
else ifeq "$(BuildFlavour)" "devel_prof"
GHC_FLAGS	:= -rtsopts -O2 -prof -auto-all $(GHC_WARNINGS) $(GHC_LANGUAGE)
GCC_FLAGS	:= -std=c99 -Wundef -g -pg

# -- For Haskell Program Coverage
else ifeq "$(BuildFlavour)" "devel_hpc"
GHC_FLAGS	:= -rtsopts -fhpc $(GHC_WARNINGS) $(GHC_LANGUAGE)
GCC_FLAGS	:= -std=c99 -O3 -Wundef

else 
all : $(error "Unknown BuildFlavour '$(BuildFlavour)'. Set this in make/config.mk")
endif
