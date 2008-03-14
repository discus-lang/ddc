
include make/config.mk

# -- Language features -------------------------------------------------------------------
# -- 	Enable these individually so we can keep an eye on what's being used.
		
GHC_LANGUAGE	:= \
	-XPatternGuards \
	-XImplicitParams \
	-XUnboxedTuples \
	-XParallelListComp \
	-XPatternSignatures \
	-XMultiParamTypeClasses \
	-XFlexibleInstances \
	-XFlexibleContexts \
	-XFunctionalDependencies \
	-XScopedTypeVariables \
	-XKindSignatures \
	-XUndecidableInstances \
	-XTypeSynonymInstances


# -- Warnings ----------------------------------------------------------------------------
# -- 	GHC warnings we compile with.
# --	There's no point turning warnings on without -Werror.

GHC_WARNINGS	:= \
	-Werror \
	-fwarn-deprecations \
	-fwarn-duplicate-exports \
	-fwarn-hi-shadowing \
	-fwarn-missing-fields \
	-fwarn-missing-methods \
	-fwarn-overlapping-patterns \
	-fwarn-type-defaults \
	-fwarn-monomorphism-restriction \
	-fwarn-unused-binds

#	-fwarn-unused-matches			# we should probably enable this one
#	-fwarn-name-shadowing 			# this generates lots of warnings about shadowing
						# a binding called 'exp'. Is this real, or broken?

#	-fwarn-incomplete-patterns 		# incomplete pattern warnings are buggy in GHC
#	-fwarn-incomplete-record-updates 	# this isn't smart enough to be useful
#	-fwarn-orphans  			# not sure if this is a problem
#	-fwarn-unused-imports 			# unused imports warnings are buggy in GHC

#	-fwarn-missing-signatures		# I use this all the time.
#	-fwarn-simple-patterns			# I use this all the time.
#	-fwarn-tabs				# heresy!


# -- Build Flavours --------------------------------------------------------------------

# -- Distribution compile (fastest at runtime)
ifeq "$(BuildFlavour)" "distro"
GHC_FLAGS	:= $(GHC_LANGUAGE) -tmpdir /tmp -O2
GCC_FLAGS	:= -std=c99 -O3 -Wundef

# -- Development Compile (fastest compile)
else ifeq "$(BuildFlavour)" "devel"
GHC_FLAGS	:= $(GHC_WARNINGS) $(GHC_LANGUAGE) -tmpdir /tmp -Onot
GCC_FLAGS	:= -std=c99 -O3

# -- Profiling compile
else ifeq "$(BuildFlavour)" "devel_prof"
GHC_FLAGS	:= $(GHC_LANGUAGE) -tmpdir /tmp -O2 -prof -auto-all
GCC_FLAGS	:= -std=c99 -Werror -Wundef -g -pg

# -- For Haskell Program Coverage
else ifeq "$(BuildFlavour)" "devel_hpc"
GHC_FLAGS	:= $(GHC_LANGUAGE) -fhpc -tmpdir /tmp 
GCC_FLAGS	:= -std=c99 -O3 -Wundef

else 
all : $(error "Unknown BuildFlavour '$(BuildFlavour)'. Set this in make/config.mk")
endif


# -- Target setup -----------------------------------------------------------------------

# -- Linux 
ifeq "$(Target)" "linux-x86"
GCC_FLAGS           += -fPIC
BUILD_SHARED        := gcc -shared
SHARED_SUFFIX       := so

# -- Darwin
else ifeq "$(Target)" "darwin-x86"
GCC_FLAGS           += -fPIC 
BUILD_SHARED        := gcc -dynamiclib -undefined dynamic_lookup
SHARED_SUFFIX       := dylib

else
all : $(error "Unknown Target '$(Target)'. Set this in make/config.mk")

endif
