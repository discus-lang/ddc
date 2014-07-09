# Language features, warning options and packages used by DDC.
#   We use the same options and package settings for DDC proper as well as its tools
#   This is so we can keep an eye on what is being used by the whole project.

# -- Language features --------------------------------------------------------
GHC_LANGUAGE	:= \
	-XBangPatterns \
        -XPatternGuards \
        -XParallelListComp \
        -XKindSignatures \
        -XScopedTypeVariables \
        -XFlexibleInstances \
	-XFlexibleContexts \
	-XMultiParamTypeClasses \
        -XFunctionalDependencies \
	-XTypeSynonymInstances \
	-XExistentialQuantification \
	-XNoMonomorphismRestriction \
        -XRankNTypes \
	-XStandaloneDeriving \
        -XDeriveDataTypeable \
        -XViewPatterns \
        -XTupleSections


# -- Warnings -----------------------------------------------------------------
# -- There's no point turning warnings on without -Werror.
GHC_WARNINGS	:= \
	-Werror \
	-fwarn-unrecognised-pragmas \
        -fwarn-deprecations \
	-fwarn-duplicate-exports \
	-fwarn-hi-shadowing \
        -fwarn-identities \
	-fwarn-incomplete-patterns \
        -fwarn-missing-fields \
        -fwarn-name-shadowing \
	-fwarn-overlapping-patterns \
	-fwarn-type-defaults \
	-fwarn-monomorphism-restriction \
        -fwarn-name-shadowing \
        -fwarn-unused-binds \
	-fwarn-unused-imports \
        -fwarn-unused-matches \
        -fwarn-wrong-do-bind \
        -fno-warn-missing-methods \
        -fno-warn-missing-signatures \
        -fno-warn-missing-local-sigs \
        -fno-warn-orphans

# Warnings that enforce programming styles that we don't use.
# 	-fwarn-missing-methods
#	-fwarn-missing-signatures
#       -fwarn-missing-local-sigs
#       -fwarn-incomplete-uni-patterns
#       -fwarn-unused-do-bind 
#       -fwarn-incomplete-record-updates 

# Warning flags that are deprecated in GHC 7.6.
#       -fwarn-lazy-unlifted-bindings


# -- Packages -----------------------------------------------------------------
#
#    Don't add unix or posix packages, as there can be problems building 
#    them under windows.
#
DDC_PACKAGES	:= \
	-hide-all-packages \
        -package base \
        -package stm \
        -package mtl \
        -package array \
        -package random \
        -package deepseq \
        -package process \
        -package filepath \
        -package haskeline \
        -package directory \
        -package containers \
        -package glpk-hs \
        -package transformers \
        -package text \
        -package parsec \
        -package buildbox \
        -package wl-pprint \
        
