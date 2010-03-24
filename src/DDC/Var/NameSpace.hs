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
	| NameValue		-- ^ Value   variables.
	| NameType		-- ^ Value   type variables.
	| NameRegion		-- ^ Region  type variables.
	| NameEffect		-- ^ Effect  type variables.
	| NameClosure		-- ^ Closure type variables.
	| NameClass		-- ^ Type class \/ witness kind variables.
	| NameField		-- ^ Field names.
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
	NameField	-> 'f'
	NameLabel	-> 'l'
