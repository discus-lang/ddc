{-# OPTIONS -O2 #-}

module Shared.VarSpace
	( NameSpace(..)
	, spaceName 
	, namePrefix )
where

import Shared.Pretty
import Util

-----
-- NameSpace
--	Ord is derived so we can use NameSpace as a Map key, 
--		the actual ordering doesn't matter.
--	
data NameSpace
	= NameNothing

	| NameValue		-- value   variables.
	| NameType		-- value type variables.
	| NameRegion		-- region  variables.
	| NameEffect		-- effect  variables.
	| NameClosure		-- closure variables.
	| NameClass		-- class   variables.
	| NameModule		-- module names.
	| NameField		-- field names.
	| NameLabel		-- labels, for jumping to in Sea code.
	deriving (Show, Eq,  Ord)


-----
instance Pretty NameSpace PMode where
 ppr x		= ppr $ show x
 
-- | return an english name for this namespace.
spaceName :: NameSpace -> String
spaceName space
  = case space of
 	NameNothing	-> "nothing"

	NameValue	-> "value"
	NameType	-> "type"
	NameRegion	-> "region"
	NameEffect	-> "effect"
	NameClosure	-> "closure"
	NameClass	-> "class"
	NameModule	-> "module"
	NameField	-> "field"
	NameLabel	-> "label"


-- | A common prefix to use for variable names of a certain NameSpace
namePrefix :: NameSpace -> String
namePrefix space
 = case space of
 	NameNothing	-> "z"
	NameValue	-> "x"
	NameType	-> "t"
	NameRegion	-> "r"
	NameEffect	-> "e"
	NameClosure	-> "c"
	NameClass	-> "w"
	NameModule	-> "m"
	NameField	-> "f"
	NameLabel	-> "l"
