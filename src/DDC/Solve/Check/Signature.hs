{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Checking inferred type schemes against signatures.
module DDC.Solve.Check.Signature
	(checkSchemeAgainstSig)
where
import Type.ToCore
import DDC.Type
import DDC.Type.SigMode
import DDC.Solve.Error
import DDC.Solve.State
import DDC.Solve.Interface.Problem
import DDC.Main.Pretty
import DDC.Var
import Data.Maybe
import qualified Data.Foldable		as Foldable
import qualified Data.Map		as Map
import Util

debug	 	= True
trace s	 	= when debug $ traceM s


-- | Check a generalised type scheme against a provided type signature.
--
--   TODO: Check more-than bounds on quantified vars
--	   Check more-than bounds on monomorphic vars
--         Check contexts
--         Check sets of quantified variables.
--
checkSchemeAgainstSig :: Type -> ProbSig -> SquidM ()
checkSchemeAgainstSig tScheme prob@(ProbSig _ _ mode tSig)
 = do	
	-- The signature and inferred scheme will use different names, 
	-- 	so we unify their bodies to recover the renaming.
	-- 	TODO: Handle unification failure.
	let tSchBody	= stripToBodyT tScheme
	let tSigBody	= stripToBodyT tSig
	let Just csBits	= unifyTT tSchBody tSigBody
	
	let vvsRenames	= catMaybes
	 		$ map (uncurry slurpRename)
			$ Foldable.toList csBits

	-- Rename the inferred scheme so it (hopefully) matches the signature.
	let mpRenames	= Map.fromList vvsRenames
	let tScheme'	= subVV_everywhere mpRenames tScheme
	
	-- Our subsumption operator only works on types in core form..
	let (_bksSigQuant',  ksSigContext', tSigBody') = stripForallContextT $ toCoreT tSig
	let (_bksSchQuant',  ksSchContext', tSchBody') = stripForallContextT $ toCoreT tScheme'
	
	-- Compare effects and closures in the body of the type
	let bodySubsLow		= subsumesTT tSchBody' tSigBody'
	let bodySubsHigh	= subsumesTT tSigBody' tSchBody'

	-- Compare contexts
	let extraContextSch	= nub ksSchContext' \\ nub ksSigContext'
	let extraContextSig	= nub ksSigContext' \\ nub ksSchContext'

	trace	$ vcat
		[ "    --  Checking signature " % show mode
		, "    sig\n"			%> prettyTypeSplit tSig,		blank
		, "    sig core\n"		%> prettyTypeSplit (toCoreT tSig),	blank
		, blank
		, "    scheme\n"		%> prettyTypeSplit tScheme,		blank
		, "    scheme renamed\n"	%> prettyTypeSplit tScheme',		blank
		, "    scheme core'\n"		%> prettyTypeSplit (toCoreT tScheme'),	blank
		, blank
		, "    bodySubsLow      = "	% isSubsumes bodySubsLow,		blank
		, "    bodySubsHigh     = "	% isSubsumes bodySubsHigh,		blank 
		, "    extraContextSch  = "	% extraContextSch,			blank
		, "    extraContextSig  = "	% extraContextSig,			blank ]

	let mErrs	= checkSchemeDiagnose 
				prob tScheme' 
				bodySubsLow bodySubsHigh
				extraContextSch extraContextSig
				
	addErrors $ maybeToList mErrs
	
	return ()
		

-- | Diagnose errors with an inferred type scheme not matching its signature.
--   The signature is associated with a mode, which determines how it is supposed
--   to be related to the inferred scheme,
checkSchemeDiagnose
	:: ProbSig	-- ^ Type signature from the probem definition.
	-> Type		-- ^ The inferred scheme.
	-> Subsumes	-- ^ Whether the body of the signature subsumes the body of the scheme.
	-> Subsumes	-- ^ Whether the body of the scheme    subsumes the body of the signature.
	-> [Kind]	-- ^ Extra contexts in the scheme wrt the sig
	-> [Kind]	-- ^ Extra contexts in the sig    wrt the scheme
	-> Maybe Error	-- ^ Maybe an error if the signature doesn't match
	
checkSchemeDiagnose (ProbSig v sp mode tSig) tScheme 
	subsLow         subsHigh
	extraContextSch extraContextSig

 = let	errMismatch
	 = ErrorSigMismatch
		{ eScheme		= (v, tScheme)
		, eSigMode		= mode
		, eSigPos		= sp
		, eSigType		= tSig 
		, eSigBadType		= Nothing 
		, eSigBadContext	= Nothing }

   in	case mode of
	 -- Signature must match inferred.
	 -- TODO: Eliminate this mode during desugaring.
    	 SigModeMatch
	  -> Nothing

	 -- Signature must be less-then inferred scheme.
    	 SigModeLess
	  | NoSubsumes t1 t2	<- subsLow
	  -> Just $ errMismatch { eSigBadType = Just (t2, t1) }

	  | not $ null extraContextSch
	  -> Just $ errMismatch { eSigBadContext = Just extraContextSch }

	  | otherwise		-> Nothing
    	 
	 -- Signature must be more-than inferred scheme.
	 SigModeMore
	  | NoSubsumes t1 t2	<- subsHigh
	  -> Just $ errMismatch { eSigBadType = Just (t2, t1) }

	  | not $ null extraContextSig
	  -> Just $ errMismatch { eSigBadContext = Just extraContextSig }

	  | otherwise		-> Nothing

	 -- Signature must match inferred scheme exactly.
    	 SigModeExact
	  | NoSubsumes t1 t2	<- subsLow
	  -> Just $ errMismatch { eSigBadType = Just (t2, t1) }
	
	  | NoSubsumes t1 t2 	<- subsHigh
	  -> Just $ errMismatch { eSigBadType = Just (t2, t1) }

	  | not $ null extraContextSig
	  -> Just $ errMismatch { eSigBadContext = Just extraContextSig }

	  | not $ null extraContextSch
	  -> Just $ errMismatch { eSigBadContext = Just extraContextSch }

	  | otherwise		-> Nothing


-- | If these two types are both vars then return them both, else Nothing.
slurpRename :: Type -> Type -> Maybe (Var, Var)
slurpRename t1 t2
 	| TVar _ u1	<- t1
	, TVar _ u2	<- t2
	, Just v1	<- takeVarOfBound u1
	, Just v2	<- takeVarOfBound u2
	= Just (v1, v2)
	
	| otherwise
	= Nothing

