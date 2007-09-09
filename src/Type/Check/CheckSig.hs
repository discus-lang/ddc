
-----------------------
-- This isn't a full check
--
--	Check that corresponding vars are quantified the same
--

module Type.Check.CheckSig
(
--	checkSig
)

where

import qualified Debug.Trace	as Debug

import Util
import Type.Exp
import Type.Util
import Type.Error

-----
-- checkSig
--	Check an inferred scheme against a type signature.
--	Although sigs are added as constraints to the type graph - and constructor
--	mismatches will be caught there, we still need to check that the sig is not
--	more general than the inferred scheme.
--
{-
checkSig 
	::  Var -> Type 	-- sig    var / type
	->  Var -> Type 	-- scheme var / type
	-> [Error]

checkSig gVar gType sVar sType
 = case gType of
 	TSig t		-> []
	TSigExact t	-> checkSig2 gVar t sVar sType


checkSig2 gVar gType sVar sType
 = let	bits		= unifyT (chopToShape gType) (chopToShape sType)

	-- Check rigid vars in sig are only unified
	--	with other vars in scheme.
	errsRigid	= mapMaybe (checkBitRigid gVar gType sVar sType) bits

	-- Check vars are quantified the same
	gForallVars	= stripForallVars gType
	sForallVars	= stripForallVars sType
	varMap		= mapMaybe stripVarMap1 bits
	
	errsQuantSig	= mapMaybe (checkBitQuant	
					gVar gType 
					sVar sType
					varMap 
					sForallVars)
			$ gForallVars
				
	varMap'		= map (\(a, b) -> (b, a)) varMap
	
	errsQuantScheme	= mapMaybe (checkBitQuant
					gVar gType
					sVar sType
					varMap'
					gForallVars)
			$ sForallVars
	
	--
	errs		=  errsRigid 
			++ errsQuantSig
			++ errsQuantScheme

   in	   
{-   	Debug.trace 	(  "sig         = " ++ show gType	++ "\n"
			++ "scheme      = " ++ show sType	++ "\n"
			++ "bits        = " ++ show bits 	++ "\n"
			++ "gForallVars = " ++ show gForallVars	++ "\n"
			++ "sForallVars = " ++ show sForallVars	++ "\n"
			++ "varMap      = " ++ show varMap	++ "\n"
			++ "errs        = " ++ show errs)
	$ 
-}	
	 errs

	
-----
checkBitRigid
	:: Var -> Type
	-> Var -> Type
	-> (Type, Type) 
	-> Maybe Error
	
checkBitRigid gVar gType sVar sType (g, s)
 = case (g, s) of

 	(TVar{}, TVar{})	
	 -> Nothing

	(TVar{}, _)	
	 -> Just $ ErrorSigScheme
	 	{ eSig 		= (gVar, gType)
		, eScheme	= (sVar, sType)
		, eClashSig	= g
		, eClashScheme	= s }
		
	_		-> Nothing

-----
checkBitQuant
	:: Var -> Type		-- sig    var / type
	-> Var -> Type		-- scheme var / type
	-> [(Var, Var)]		-- var map sigVar -> schemeVar
	-> [Var]		-- 
	-> Var
	-> Maybe Error

checkBitQuant 
	gVar gType 
	sVar sType
	varMap
	vsQuant
	v
	
 =	case lookup v varMap of
 	 Just v'	
	  | elem v' vsQuant	-> Nothing
	  | otherwise	
	  ->	Just $ ErrorSigForall 
	  	{ eSig		= (gVar, gType)
		, eScheme	= (sVar, sType) 
		, eVar		= v }

	 Nothing	
	  -> 	Just $ ErrorSigForall 
	  	{ eSig		= (gVar, gType)
		, eScheme	= (sVar, sType) 
		, eVar		= v }



-----
stripForallVars
	:: Type -> [Var]
	
stripForallVars t
 = case t of
 	TForall vks t	-> map fst vks
	_		-> []
 	
	
-----
stripVarMap1 
	:: (Type, Type)
	-> Maybe (Var, Var)
	
stripVarMap1 xx
 = case xx of
 	(TVar v1, 		TVar v2)		-> Just (v1, v2)
	(TRegion (RVar v1),	TRegion (RVar v2))	-> Just (v1, v2)
	(TEffect (EVar v1),	TEffect (EVar v2))	-> Just (v1, v2)
	_						-> Nothing
 	






{-	

-----
checkEs ::	(?boundSig	:: [(Var, Kind)])
	->	(?boundSch	:: [(Var, Kind)])
	-> 	[Effect] -> [Effect] -> [Error]

checkEs esSig esSch
 = let
 	err1	= catMap (checkEs1 esSig esSch) esSch
	err2	= catMap (checkEs2 esSig esSch) esSig
   in
   	err1 ++ err2


-----
-- this isn't the best
--	does not count for diff var names
--	if higher order effect vars don't ever get use they'll be scrubbed by the summariser,
--	and we'll get a non-useful error message saying some higher order var doesn't exist.
--
--	does not match diff uniquebinds between sig and scheme



-----
-- checkEs1
--	Check that an effect in the scheme is present in the signature.
--		
checkEs1 esSig esSch eSch

	| elemF effMatch eSch esSig
	= []
	
	| otherwise
	= [ErrorSigEffects
		{ eSig		= poison
		, eScheme	= poison
		, eEffSig	= esSig
		, eEffScheme	= esSch }]
		
-----
-- checkEs2
--	Check that an effect in the sig is present in the scheme.
--
checkEs2 esSig esSch eSig

	| elemF effMatch eSig esSch
	= []
	
	| otherwise
	= [ErrorSigEffects
		{ eSig		= poison
		, eScheme	= poison
		, eEffSig	= esSig
		, eEffScheme	= esSch }]


effMatch ::	Effect -> Effect -> Bool
effMatch	e1 e2

	| ECon v1 _	<- e1
	, ECon v2 _	<- e2
	, v1 == v2
	= True
	
	| EVar v1	<- e1
	, EVar v2	<- e2
	= True

	| otherwise
	= False
	
-}
-}
