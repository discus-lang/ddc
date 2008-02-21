
module Shared.Exp
	(DataField (..))

where

import Shared.Var
import Shared.Pretty
import Util

data DataField x t
	= DataField
	{ dPrimary	:: Bool
	, dLabel	:: Maybe Var
	, dType		:: t
	, dInit		:: Maybe x }
	deriving (Show, Eq)



-- Pretty instances of shared expressions ----------------------------------------------------------

instance (Pretty x PMode, Pretty t PMode) 
 => 	 Pretty (DataField x t) PMode where

 ppr DataField
 	{ dPrimary	= True
	, dLabel	= Nothing
	, dType		= t
	, dInit		= Nothing }
	= t % ";"

 ppr DataField
 	{ dPrimary	= True
	, dLabel	= mLabel
	, dType		= t
	, dInit		= mInit }
	
	= fromMaybe (ppr " ") (liftM ppr mLabel)
	%> (" :: "
		% t
		% case mInit of 
		   Nothing	-> ppr " "
		   Just i	-> "\n = " % i) % ";"

 ppr DataField
 	{ dPrimary	= False
	, dLabel	= mLabel
	, dType		= t
	, dInit		= mInit }
	
	= "." % fromMaybe (ppr " ") (liftM ppr mLabel)
	%> (" :: "
		% t
		% case mInit of 
		   Nothing	-> ppr " "
		   Just i	-> "\n = " % i) % ";"

