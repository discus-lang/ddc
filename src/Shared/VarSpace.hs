
module Shared.VarSpace
	( NameSpace(..)
	, spaceName 
	, namePrefix )
where

import Util

-----
-- NameSpace
--	Ord is derived so we can use NameSpace as a Map key, 
--		the actual ordering doesn't matter.
--	
data NameSpace
	= NameNothing

	| NameValue		-- value   variables.
	| NameType		-- type    variables.
	| NameRegion		-- region  variables.
	| NameEffect		-- effect  variables.
	| NameClosure		-- closure variables.
	| NameClass		-- class   variables.

	| NameModule		-- module names.
	| NameField		-- field names.
	| NameAttr		-- object attribute names.
	| NameLabel		-- labels, for jumping to.

	deriving (Show, Eq,  Ord)


-----
instance Pretty NameSpace where
 ppr x		= ppr $ show x
 
spaceName space
  = case space of
 	NameNothing	-> "Nothing"

	NameValue	-> "Value"
	NameType	-> "Type"
	NameRegion	-> "Region"
	NameEffect	-> "Effect"
	NameClosure	-> "Closure"
	NameClass	-> "Class"

	NameModule	-> "Module"
	NameField	-> "Field"
	NameAttr	-> "Attr"
	NameLabel	-> "Label"


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
	NameAttr	-> "a"
	NameLabel	-> "l"
