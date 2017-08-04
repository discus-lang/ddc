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
        -XTupleSections \
        -XInstanceSigs \
        -XPatternSynonyms \
        -XConstraintKinds \
        -XDuplicateRecordFields


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
        -fno-warn-missing-local-signatures \
        -fno-warn-orphans \
        -fno-warn-simplifiable-class-constraints

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
        -package array \
        -package compact \
        -package bytestring \
        -package containers \
        -package deepseq \
        -package directory \
        -package filepath \
        -package process \
        -package time \
        -package transformers \
        -package mtl \
        -package parsec \
        -package random \
        -package stm \
        -package text \
        -package wl-pprint \
        -package buildbox \
        -package haskeline \
        -package inchworm

ifeq "$(DDC_FLOW_USE_LINEAR_SOLVER)" "1"
DDC_PACKAGES	:= $(DDC_PACKAGES) \
        -package limp \
        -package limp-cbc
endif

