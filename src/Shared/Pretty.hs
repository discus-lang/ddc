module Shared.Pretty
	()
where

import Util
import Shared.Exp

----
instance (Pretty x, Pretty t) 
 => 	 Pretty (DataField x t) where

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

