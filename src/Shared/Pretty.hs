module Shared.Pretty
(
	annotMisc, scrubAnnotMisc,
	annotVar,  scrubAnnotVar,
	annotType, scrubAnnotType,
	scrubAnnots
)

where

import Util
import Shared.Exp
import Shared.Bits


----
instance (Pretty x, Pretty t) 
 => 	 Pretty (DataField x t) where

 prettyp DataField
 	{ dPrimary	= True
	, dLabel	= Nothing
	, dType		= t
	, dInit		= Nothing }
	= t % ";"

 prettyp DataField
 	{ dPrimary	= True
	, dLabel	= mLabel
	, dType		= t
	, dInit		= mInit }
	
	= fromMaybe " " (liftM pretty mLabel)
	%> (" :: "
		% t
		% case mInit of 
		   Nothing	-> prettyp " "
		   Just i	-> "\n = " % i) % ";"

 prettyp DataField
 	{ dPrimary	= False
	, dLabel	= mLabel
	, dType		= t
	, dInit		= mInit }
	
	= "." % fromMaybe " " (liftM pretty mLabel)
	%> (" :: "
		% t
		% case mInit of 
		   Nothing	-> prettyp " "
		   Just i	-> "\n = " % i) % ";"

