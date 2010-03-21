{-# OPTIONS -O2 #-}

-- | Variable Namespaces
module DDC.Var.NameSpace
	( NameSpace(..)
	, shortNameOfSpace
	, charPrefixOfSpace )
where

-- | Possible variable namespaces.
data NameSpace
	= NameNothing
	| NameValue		-- ^ value   variables.
	| NameType		-- ^ value   type variables.
	| NameRegion		-- ^ region  type variables.
	| NameEffect		-- ^ effect  type variables.
	| NameClosure		-- ^ closure type variables.
	| NameClass		-- ^ type class / witness kind variables.
	| NameModule		-- ^ module names.
	| NameField		-- ^ field names.
	| NameLabel		-- ^ Sea level labels.
	deriving (Show, Eq,  Ord)


-- | Return an english name for this namespace.
shortNameOfSpace :: NameSpace -> String
shortNameOfSpace space
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


-- | Get a unique charater to use for this namespace.
--	Used as a prefix for variable names in this namespace.
charPrefixOfSpace :: NameSpace -> Char
charPrefixOfSpace space
 = case space of
 	NameNothing	-> 'z'
	NameValue	-> 'x'
	NameType	-> 't'
	NameRegion	-> 'r'
	NameEffect	-> 'e'
	NameClosure	-> 'c'
	NameClass	-> 'w'
	NameModule	-> 'm'
	NameField	-> 'f'
	NameLabel	-> 'l'
