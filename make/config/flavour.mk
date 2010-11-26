# Setting 
include make/config.mk
include make/config/options.mk

BuildFlavour    := $(strip $(BUILDFLAVOUR))

# -- Distribution compile (fastest at runtime)
ifeq "$(BuildFlavour)" "distro"
GHC_FLAGS	:= -O2 $(GHC_VERSION_FLAGS) $(GHC_WARNINGS) $(GHC_LANGUAGE)
GCC_FLAGS	:= -std=c99 -O3 -Wundef

# -- Development Compile (fastest compile)
else ifeq "$(BuildFlavour)" "devel"
GHC_FLAGS	:= -O0 $(GHC_VERSION_FLAGS) $(GHC_WARNINGS) $(GHC_LANGUAGE)
GCC_FLAGS	:= -std=c99 -O3 -Wall -Wextra -Werror

# -- Debug compile
else ifeq "$(BuildFlavour)" "devel_debug"
GHC_FLAGS	:= -O0 $(GHC_VERSION_FLAGS) $(GHC_WARNINGS) $(GHC_LANGUAGE)
GCC_FLAGS	:= -std=c99 -Wundef -g

# -- Profiling compile
else ifeq "$(BuildFlavour)" "devel_prof"
GHC_FLAGS	:= -O2 -prof -auto-all $(GHC_VERSION_FLAGS) $(GHC_WARNINGS) $(GHC_LANGUAGE)
GCC_FLAGS	:= -std=c99 -Wundef -g -pg

# -- For Haskell Program Coverage
else ifeq "$(BuildFlavour)" "devel_hpc"
GHC_FLAGS	:= -fhpc $(GHC_VERSION_FLAGS) $(GHC_WARNINGS) $(GHC_LANGUAGE)
GCC_FLAGS	:= -std=c99 -O3 -Wundef

else 
all : $(error "Unknown BuildFlavour '$(BuildFlavour)'. Set this in make/config.mk")
endif
