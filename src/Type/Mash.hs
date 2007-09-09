
module Type.Mash
-- (
--	mashClassT
-- )
	()

where

-----
import qualified Data.Map	as Map
import Data.Map			(Map)

import Util
import Shared.Error

import Type.Exp
import Type.Util
import Type.Plate
import Type.State
import Type.Class


-----
debug		= False
trace ss 	= when debug $ traceM ss
stage		= "Type.Squid.Mash"

{-
-----
mashClassT :: 	ClassId	-> SquidM ()
mashClassT	cid
 = do	Just c		<- lookupClass cid
 	let kind	= classKind c
	
	case kind of
	 KRegion	-> mashClassR cid
	 KEffect	-> mashClassE cid
	 _		-> return ()
 	

mashClassE :: ClassId -> SquidM ()
mashClassE    cidE
 = do
 	Just c	<- lookupClass cidE
	let vs	= [v | (TEffect (EVar v), _)	<- classNodes c]

	(case vs of
	   []		-> return ()
	   (v:vs)	-> addVarSubs v vs)
		
	return ()


mashClassR ::	ClassId -> SquidM ()
mashClassR	cidR
 = do
 	Just c	<- lookupClass cidR

	let vs	= [v | (TRegion (RVar v), _)	<- classNodes c]
	
	(case vs  of
	   []		-> return ()
	   (v:vs)	-> addVarSubs v vs)
	   
	return ()
-}

