
include make/config.mk

# -- Language features -------------------------------------------------------------------
# -- 	Enable these individually so we can keep an eye on what's being used.
		
GHC_LANGUAGE	:= \
	-XPatternGuards \
	-XImplicitParams \
	-XUnboxedTuples \
	-XParallelListComp \
	-XScopedTypeVariables \
	-XMultiParamTypeClasses \
	-XFlexibleInstances \
	-XFlexibleContexts \
	-XFunctionalDependencies \
	-XScopedTypeVariables \
	-XKindSignatures \
	-XUndecidableInstances \
	-XTypeSynonymInstances \
	-XNamedFieldPuns \
	-XExistentialQuantification

DDC_PACKAGES	:= \
	-package unix \
	-package mtl \
	-package containers \
	-package parsec \
	-package regex-compat \
	-package QuickCheck

# -- Warnings ----------------------------------------------------------------------------
# -- 	GHC warnings we compile with.
# --	There's no point turning warnings on without -Werror.

GHC_WARNINGS	:= \
	-Werror \
	-fwarn-deprecations \
	-fwarn-duplicate-exports \
	-fwarn-hi-shadowing \
	-fwarn-missing-fields \
	-fwarn-overlapping-patterns \
	-fwarn-type-defaults \
	-fwarn-monomorphism-restriction \
	-fwarn-unused-binds \
	-fno-warn-missing-methods \
	-fwarn-unused-imports

# 	-fwarn-missing-methods			# we don't usually define coarbitrary in the Arbitrary class
#	-fwarn-unused-matches			# we should probably enable this one
#	-fwarn-name-shadowing 			# this generates lots of warnings about shadowing
						# a binding called 'exp'. Is this real, or broken?

#	-fwarn-incomplete-patterns 		# incomplete pattern warnings are buggy in GHC
#	-fwarn-incomplete-record-updates 	# this isn't smart enough to be useful
#	-fwarn-orphans  			# not sure if this is a problem

#	-fwarn-missing-signatures		# I use this all the time.
#	-fwarn-simple-patterns			# I use this all the time.
#	-fwarn-tabs				# heresy!


# -- Build Flavours --------------------------------------------------------------------
Target          := $(strip $(TARGET))
BuildFlavour    := $(strip $(BUILDFLAVOUR))


# -- Distribution compile (fastest at runtime)
ifeq "$(BuildFlavour)" "distro"
GHC_FLAGS	:= $(GHC_LANGUAGE) -tmpdir /tmp -O2
GCC_FLAGS	:= -std=c99 -O3 -Wundef

# -- Development Compile (fastest compile)
else ifeq "$(BuildFlavour)" "devel"
GHC_FLAGS	:= $(GHC_WARNINGS) $(GHC_LANGUAGE) -tmpdir /tmp -O0
GCC_FLAGS	:= -std=c99 -O3 -Wall -Wextra

# -- Debug compile
else ifeq "$(BuildFlavour)" "devel_debug"
GHC_FLAGS	:= $(GHC_LANGUAGE) -tmpdir /tmp -O0
GCC_FLAGS	:= -std=c99 -Wundef -g

# -- Profiling compile
else ifeq "$(BuildFlavour)" "devel_prof"
GHC_FLAGS	:= $(GHC_LANGUAGE) -tmpdir /tmp -O2 -prof -auto-all
GCC_FLAGS	:= -std=c99 -Wundef -g -pg

# -- For Haskell Program Coverage
else ifeq "$(BuildFlavour)" "devel_hpc"
GHC_FLAGS	:= $(GHC_LANGUAGE) -fhpc -tmpdir /tmp 
GCC_FLAGS	:= -std=c99 -O3 -Wundef

else 
all : $(error "Unknown BuildFlavour '$(BuildFlavour)'. Set this in make/config.mk")
endif


# -- Target setup -----------------------------------------------------------------------

# -- Linux on x86
ifeq "$(Target)" "linux-x86"
GCC_FLAGS           += -fPIC -D BITS=32
BUILD_SHARED        := gcc -shared
SHARED_SUFFIX       := so

# -- Linux on x86_64
else ifeq "$(Target)" "linux-x86_64"
GCC_FLAGS           += -fPIC -D BITS=64 -m64
BUILD_SHARED        := gcc -shared
SHARED_SUFFIX       := so

# -- Linux on ppc
else ifeq "$(Target)" "linux-ppc"
GCC_FLAGS           += -fPIC -D BITS=32 -m32
BUILD_SHARED        := gcc -shared
SHARED_SUFFIX       := so

# -- FreeBSD on x86
else ifeq "$(Target)" "freebsd-x86"
GCC_FLAGS           += -fPIC -D BITS=32
BUILD_SHARED        := gcc -shared
SHARED_SUFFIX       := so 

# -- Darwin on x86
else ifeq "$(Target)" "darwin-x86"
GCC_FLAGS           += -fPIC -D BITS=32 -m32
BUILD_SHARED        := gcc -m32 -dynamiclib -undefined dynamic_lookup 
SHARED_SUFFIX       := dylib

# -- Darwin on x86_64
else ifeq "$(Target)" "darwin-x86_64"
GCC_FLAGS           += -fPIC -D BITS=64 -m64
BUILD_SHARED        := gcc -m64 -dynamiclib -undefined dynamic_lookup
SHARED_SUFFIX       := dylib

else
all : $(error "Unknown Target '$(Target)'. Set this in make/config.mk")

endif
