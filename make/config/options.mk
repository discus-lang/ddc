# Language features, warning options and packages used by DDC.
#   We use the same options and package settings for DDC proper as well as its tools
#   This is so we can keep an eye on what is being used by the whole project.

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
	-fwarn-unused-imports \
	-fno-warn-missing-methods


# -- Warnings for GHC 7.0.1
#    -fspec-constr-count is to try and shut up internal compiler warnings in 7.0.1,
#       which doesn't seem to work. This should be fixed in 7.0.2, then we can disable it.
#    -fno-warn-deprecations is because the alex generated files includes -fglagsow-exts
#
ifeq "$(GHC_VERSION)" "7"
GHC_WARNINGS	:= \
	$(GHC_WARNINGS) \
	-fwarn-monomorphism-restriction \
	-fwarn-unrecognised-pragmas \
	-fspec-constr-count=10
endif


# Warnings that are enabled manually in the DDC tree
#	-fwarn-unused-matches
#	-fwarn-incomplete-patterns
#	-fwarn-name-shadowing 

# Warnings we should probably enable
#	-fwarn-unused-do-bind
#	-fwarn-wrong-do-bind

# Warnings I'm not sure about.
#	-fwarn-orphans  			# not sure if this is a problem

# Warnings we would add if they had a better implementation.
#	-fwarn-incomplete-record-updates 	# this isn't smart enough to be useful

# Warnings that enforce programming styles that we don't use.
# 	-fwarn-missing-methods			# we don't usually define coarbitrary in the Arbitrary class
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
