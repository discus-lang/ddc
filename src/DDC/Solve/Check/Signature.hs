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
--   TODO: This won't work for type sigs that have constraints on monomorphic effect
--         or closure variables. This could happen if we have mutable data at top 
--         level which contains effectful functions, so the effect vars aren't generalised.
--         It would also happen if we had sigs on local bindings.
--
checkSchemeAgainstSig :: Type -> ProbSig -> SquidM ()
checkSchemeAgainstSig tScheme prob@(ProbSig _ _ mode tSig)
 = do	
	-- Break both types down into their component parts
	-- This doesn't reveal more-than constraints on skolem vars.
	let (bksSchemeQuant, ksSchemeContext, tSchemeBody)	
		= stripForallContextT $ toCoreT tScheme

	let (bksSigQuant,    ksSigContext,    tSigBody)	
		= stripForallContextT $ toCoreT tSig

	-- Try to unify the bodies of both types
	-- They always have to unify, but we allow the constraints to differ depending
	-- on what mode the signature has.
	-- TODO: Handle unification failure
	let Just csBits	= unifyTT    tSchemeBody tSigBody
	
	-- The inferred scheme will use different variable names than the signature.
	-- We recover the name mapping from the constraints given to us by the unifier.
	-- TODO: Check for "not sufficiently polymorphic" problems, make sure
	--       different names in the sig are also different in the inferred scheme.
	let vvsRenames	= catMaybes
	 		$ map (uncurry slurpRename)
			$ Foldable.toList csBits

	-- Rename the inferred scheme to it (hopefully) matches the signature.
	let tSchemeBody'	= subVV_everywhere (Map.fromList vvsRenames) tSchemeBody
	
	-- 
	let subsLow		= subsumesTT tSchemeBody' tSigBody
	let subsHigh		= subsumesTT tSigBody     tSchemeBody'

	-- TODO: Compare sets of quantified variables.
	-- TODO: Compare constraints.

	trace	$ vcat
		[ "    schemeQuant\n"		%> bksSchemeQuant, 			blank
		, "    schemeContext\n"		%> ksSchemeContext,		 	blank
		, "    schemeBody\n"		%> prettyTypeSplit tSchemeBody, 	blank
		, "    schemeBody'\n"		%> prettyTypeSplit tSchemeBody',	blank
		, blank
		, "    sigQuant\n"		%> bksSigQuant, 			blank
		, "    sigContext\n"		%>  ksSigContext,		 	blank
		, "    sigBody\n"		%> prettyTypeSplit tSigBody, 		blank
		, blank
		, "    mode       = "		% show mode
		, "    csBits     = "		% csBits
		, "    vvsRenames = "		% vvsRenames
		, "    subsLow    = "		% isSubsumes subsLow
		, "    subsHigh   = "		% isSubsumes subsHigh ,		blank ]

	let mErrs	= checkSchemeDiagnose prob tScheme subsLow subsHigh
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
	-> Maybe Error	-- ^ Maybe an error if the signature doesn't match
	
checkSchemeDiagnose (ProbSig v sp mode tSig) tScheme subsLow subsHigh
 = let	errMismatch
	 = ErrorSigMismatch
		{ eScheme	= (v, tScheme)
		, eSigMode	= mode
		, eSigPos	= sp
		, eSigType	= tSig 
		, eSigOffending	= Nothing }

   in	case mode of
	 -- Signature must match inferred.
	 -- TODO: Eliminate this mode during desugaring.
    	 SigModeMatch
	  -> Nothing

	 -- Signature must be less-then inferred scheme.
    	 SigModeLess
	  | NoSubsumes t1 t2	<- subsLow
	  -> Just $ errMismatch { eSigOffending = Just (t2, t1) }

	  | otherwise		-> Nothing
    	 
	 -- Signature must be more-than inferred scheme.
	 SigModeMore
	  | NoSubsumes t1 t2	<- subsHigh
	  -> Just $ errMismatch { eSigOffending = Just (t2, t1) }

	  | otherwise		-> Nothing

	 -- Signature must match inferred scheme exactly.
    	 SigModeExact
	  | NoSubsumes t1 t2	<- subsLow
	  -> Just $ errMismatch { eSigOffending = Just (t2, t1) }
	
	  | NoSubsumes t1 t2 	<- subsHigh
	  -> Just $ errMismatch { eSigOffending = Just (t2, t1) }

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

