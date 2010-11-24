# Language features, warning options and packages used by DDC.
#
#   We use the same options and package settings for DDC proper as well as its tools
#   This is so we can keep an eye on what is being used by the whole project.
# 

# -- Language features --------------------------------------------------------
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
	-XExistentialQuantification \
	-XBangPatterns


# -- Warnings -----------------------------------------------------------------
# -- There's no point turning warnings on without -Werror.
GHC_WARNINGS	:= \
	-Werror \
	-fwarn-deprecations \
	-fwarn-duplicate-exports \
	-fwarn-hi-shadowing \
	-fwarn-missing-fields \
	-fwarn-overlapping-patterns \
	-fwarn-type-defaults \
	-fwarn-unused-binds \
	-fno-warn-missing-methods \
	-fwarn-unused-imports

ifeq "$(GHC_VERSION)" "7"
GHC_WARNINGS	:= \
	$(GHC_WARNINGS) \
	-fwarn-monomorphism-restriction
endif

# Warnings we should probably add.
#	-fwarn-unused-matches			# we should probably enable this one0

# Warnings I'm not sure about.
#	-fwarn-orphans  			# not sure if this is a problem

# Warnings we would add if they had a better implementation.
#	-fwarn-incomplete-patterns		# incomplete pattern warnings are buggy in GHC
#	-fwarn-incomplete-record-updates 	# this isn't smart enough to be useful

# Warnings that enforce programming styles that we don't use.
# 	-fwarn-missing-methods			# we don't usually define coarbitrary in the Arbitrary class
#	-fwarn-name-shadowing 			# too many good names are stolen by the prelude: first, last, max etc.
#	-fwarn-missing-signatures		# I use this all the time.
#	-fwarn-simple-patterns			# I use this all the time.
#	-fwarn-tabs				# heresy!


# -- Packages -----------------------------------------------------------------
# 
#    Don't add unix or posix packages.
#    They're not supported natively by the Haskell platform on Windows, and when
#    I tried to cabal install the unix package under cygwin I got a boatload of
#    problems that looked like Cabal bugs. This might change in the future though.
#    -- BL 2010/10
#
DDC_PACKAGES	:= \
	-hide-all-packages \
	-package base \
	-package array \
	-package containers \
	-package mtl \
	-package directory \
	-package filepath \
	-package process \
	-package old-time \
	-package time \
	-package haskell-src \
	-package parsec \
	-package QuickCheck \
	-package buildbox \
	-package text
