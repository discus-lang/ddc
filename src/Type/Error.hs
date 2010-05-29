
-- | Representation and display of type error messages.
module Type.Error
	( Error(..) )
where
import Type.Location
import Type.Pretty
import Util
import DDC.Base.SourcePos
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type
import DDC.Var
import qualified Shared.VarUtil	as Var

stage	= "Type.Error"

-----
data Error
	-- Used for debugging
	= ErrorBadness
		{ eMessage	:: String }

	-- Constructor airity mismatch.
	| ErrorCtorAirity				-- Wrong number of arguments to constructor
		{ eCtorVar		:: Var		--	in pattern match.
		, eCtorAirity		:: Int
		, ePatternAirity 	:: Int }

	-- Constructor mismatch.
	| ErrorUnifyCtorMismatch			-- Type constructors don't match.
		{ eCtor1	:: Type
		, eTypeSource1	:: TypeSource

		, eCtor2	:: Type 
		, eTypeSource2	:: TypeSource }
		
	-- Kind mismatch
	| ErrorUnifyKindMismatch
		{ eKind1	:: Kind
		, eTypeSource1	:: TypeSource
		, eKind2	:: Kind
		, eTypeSource2	:: TypeSource }

	-- Infinite types.
	| ErrorInfiniteType				-- Cannot construct infinite type during packing.
		{ eVar		:: Var			-- 	The var we were extracting from the graph
		, eLoops	:: [(Type, Type)] }	-- 	The recurrances found.
		

	-- Signature mismatch.
{-
	| ErrorSigScheme				-- An inferred type scheme does not match a type signature
		{ eSig		:: (Var, Type)		-- 	typeVar, type
		, eScheme	:: (Var, Type)		-- 	
		, eClashSig	:: Type			--	Offending type from signature.
		, eClashScheme	:: Type }		-- 	Offending type from scheme.

	| ErrorSigEffects
		{ eSig		:: (Var, Type)		-- Some function in the signature is more/less effectful than
		, eScheme	:: (Var, Type)		--	the corresponding function in the scheme
		, eEffSig	:: [Effect]
		, eEffScheme	:: [Effect] }
		
	| ErrorSigForall				-- Inferred type is is not quantified the same a signature.
		{ eSig		:: (Var, Type)		--	
		, eScheme	:: (Var, Type) 		--
		, eErrVar	:: Var }		--	Offending variable.
-}
	-- Type class problems
	| ErrorNoInstance				-- There is no instance for the class at these arguments.
		{ eClassVar	:: Var
		, eTypeArgs	:: [Type] 
		, eFetterMaybeSrc :: Maybe TypeSource }
		
	-- Field projection problems.
	| ErrorNoProjections				-- This type has no projections defined for it.
		{ eProj		:: TProj
		, eConstructor	:: Type }		

	| ErrorFieldNotPresent				-- Some field is not a member of this type.
		{ eProj		:: TProj		-- 	Requested projection.
		, eConstructor	:: Type 		-- 	Offending type.
		, eFields	:: [Var] }		--	Possible fields

	| ErrorAmbiguousProjection			-- Tried to project a field from a type variable.
		{ eProj		:: TProj }		-- 	

	-- Purity problems
	| ErrorCannotPurify				-- Cannot purify top-level or write effects.
		{ eEffect	:: Effect		--	The effect we can't purify.
		, eEffectSource	:: TypeSource
		, eFetter	:: Fetter		--	The Pure fetter we were trying to satisfy.
		, eFetterSource	:: TypeSource }

	-- Region constraint conflicts
	| ErrorRegionConstraint
		{ eFetter1	:: Fetter
		, eFetterSource1 :: TypeSource
		
		, eFetter2	:: Fetter
		, eFetterSource2 :: TypeSource }
		
	-- Update soundness problems.
	| ErrorUpdateSoundness				-- Update soundness problem 
		{ eErrVar	:: Var
		, eType		:: Type			--	The offending scheme.
		, eTypeDanger	:: [Type] }		--	The dangerous var(s) which are causing the problem.

	-- Inference screw-ups.
	| ErrorLateConstraint				-- Found out about a mutability constraint too late.
		{ eScheme		:: (Var, Type)	-- 	The scheme we've been using.
		, eMutableRegion	:: Type		-- 	The region that is mutable.
		, eMutableFetter	:: Fetter	--	The fetter holding the region mutable.
		, eMutableSrc		:: TypeSource	--	The source of the mutability constraint.
		, eDangerVar		:: Var }	--	The type variable that is dangerous and polymorphic.
		
	-- Main function has wrong type
	| ErrorWrongMainType	
		{ eScheme	:: (Var, Type) }	-- the type that was inferred for main		

	-- A CAF has effects that can't be masked.
	| ErrorEffectfulCAF
		{ eScheme	:: (Var, Type)		-- the CAF name and type.
		, eEffect	:: Effect }		-- the offending effect.
		
	deriving (Show)

-----	
instance Pretty Error PMode where

 -- Random badness.
 ppr err@(ErrorBadness
 		{ eMessage	= s })
		
	= "    Badness: " % s % "\n"

 -- Constructor Airity.
 ppr err@(ErrorCtorAirity{})
 	= Var.prettyPos (eCtorVar err) % "\n"
	% "    Wrong number of arguments for constructor match.\n"
	% "         constructor: " % eCtorVar err 				% "\n"
	% "          defined at: " % Var.prettyPosBound (eCtorVar err)		% "\n"
	% "                 has: " % eCtorAirity err % " arguments, but has been used here with " % ePatternAirity err % ".\n"


 -- Constructor mismatch.
 ppr err@(ErrorUnifyCtorMismatch 
 		{ eCtor1 	= t1
		, eTypeSource1	= ts1
		, eCtor2 	= t2 
		, eTypeSource2	= ts2})
 	= (dispSourcePos ts1)						% "\n"
	% "    Type mismatch during unification.\n"
	% "          cannot match: " % t1				% "\n"
	% "                  with: " % t2				% "\n"
	% "\n"
	%> dispTypeSource t1 ts1
	% "\n"
	% "        conflicts with, " 					% "\n"
	%> dispTypeSource t2 ts2

 -- Kind Mismatch
 ppr err@(ErrorUnifyKindMismatch
		{ eKind1	= k1
		, eTypeSource1	= ts1
		
		, eKind2	= k2
		, eTypeSource2	= ts2 })
	= (dispSourcePos ts1)						% "\n"
	% "    Kind mismatch during unification.\n"
	% "          cannot match: " % k1				% "\n"
	% "                  with: " % k2				% "\n"
{-	% "\n"
	%> dispKindSource k1 ts1
	% "\n"
	% "       conflicts with, "					% "\n"
	%> dispKindSource k2 ts2
-}
 -- Infinite types.
 ppr err@(ErrorInfiniteType
 		{ eVar		= var 
		, eLoops	= tsLoops })
	= prettyValuePos var % "\n"
	% "    Cannot construct infinite type for '" % var % "'.\n"			
	% "    via: \n" 
	%> (punc "\n" $ map (\(t1, t2) -> t1 % " = " % t2) tsLoops)
	% "\n"

	 
 -- Signature mismatch.
{-
 ppr err@(ErrorSigScheme{})
	= prettyValuePos (fst $ eScheme err)				% "\n"
	% "    Inferred type for '" 
				% (ppr $ fst $ eScheme err) 
				% "' does not match signature." 	% "\n"
	% "\n"
	% "        in the type of: " % (fst $ eScheme err)		% "\n"
	% "          cannot match: " % eClashScheme  err		% "\n"
	% "                  with: " % eClashSig     err		% "\n"
	% "\n"
	% "         inferred type: " % prettyVTS (eScheme err)		% "\n"
	% "\n"
	% "        type signature: " % prettyVTS (eSig    err) 		% "\n"

 ppr err@(ErrorSigEffects{})
	= prettyValuePos (fst $ eScheme err)				% "\n"
	% "    Inferred type for '" 
				% (ppr $ fst $ eScheme err) 
				% "' has different effects than signature.\n"
	% "\n"
	% "        in the type of: " % (fst $ eScheme err)		% "\n"
	% "          cannot match: " % eEffScheme err			% "\n"
	% "                  with: " % eEffSig err			% "\n"
	% "\n"
	% "         inferred type: " % prettyVTS (eScheme err)		% "\n"
	% "\n"
	% "        type signature: " % prettyVTS (eSig err)		% "\n"
					
 ppr err@(ErrorSigForall{})
	= prettyValuePos (fst $ eScheme err)				% "\n"
	% "    Inferred type for '" 
				% (ppr $ fst $ eScheme err)
				% "' is not quantified the same as signature.\n"
	% "\n"
	% "       offending var: " % eErrVar err			% "\n"
	% "\n"
	% "       inferred type: " % prettyVTS (eScheme err)		% "\n"
	% "\n"
	% "      type signature: " % prettyVTS (eSig err)		% "\n"
-}
	
 -- Type class problems.
 ppr err@(ErrorNoInstance
 		{ eClassVar	= v
		, eTypeArgs	= ts
		, eFetterMaybeSrc = Just src })
	= dispSourcePos src % "\n"
	% "    No instance for " % v % " " % punc " " (map prettyTB ts) % "\n"

 ppr err@(ErrorNoInstance
 		{ eClassVar	= v
		, eTypeArgs	= ts
		, eFetterMaybeSrc = Nothing })
	= "    No instance for " % v % " " % punc " " (map prettyTB ts) % "\n"
	
 -- Field projection problems.
 ppr err@(ErrorNoProjections
 		{ eProj		= p
		, eConstructor	= t })
	= (getProjSP p)		% "\n"
	% "    Type '" % t	% "' has no projections defined for it.\n"


 ppr err@(ErrorFieldNotPresent 
 		{ eProj		= p
		, eConstructor	= tt
		, eFields	= fields })
	| Just (v, k, _)	<- takeTData tt
	= (getProjSP p)							% "\n"
	% "    Type '" % v 	% "' has no projection named '" % p	% "'\n"
--	% "      possible fields: " % punc " " fields			% "\n"

	
 ppr err@(ErrorAmbiguousProjection
 		{ eProj		= p })
	= (getProjSP p)							% "\n"
	% "    Ambiguous projection: " % p 				% "\n"
	

 -- Purity problems.
 ppr err@(ErrorCannotPurify
 		{ eEffect	= e
		, eEffectSource	= eSource
		, eFetter	= f
		, eFetterSource	= fSource })
		
	= (dispSourcePos eSource)					% "\n" 
	% "    Cannot purify effect `" % e % "'.\n"
	% "\n"
	%> dispTypeSource e eSource 
	% "\n"
	% "     conflicts with,\n"
	%> dispFetterSource f fSource


 -- Region constraint problems
 ppr err@(ErrorRegionConstraint
 		{ eFetter1	= f1
		, eFetterSource1 = fSource1
 		, eFetter2	= f2
		, eFetterSource2 = fSource2})
	= (dispSourcePos fSource1)					% "\n"
	% "    Conflicting region constraints.\n"
	%> (dispFetterSource f1 fSource1)
	% "\n"
	% "     conflicts with,\n"
	%> (dispFetterSource f2 fSource2)


 -- Update soundness problems.
 ppr err@(ErrorUpdateSoundness
 		{ eErrVar	= v
		, eType 	= t
		, eTypeDanger	= tDanger })
		
	= prettyValuePos v				% "\n"
	% "    Update soundess problem in scheme for `" % v % "'.\n"
	% prettyVTS (v, t)
	% "\n\n"
	% "        dangerous vars: " % tDanger	% "\n"


 -- Inference screw-ups.
 ppr err@(ErrorLateConstraint
 		{ eScheme		= (v, scheme)
		, eMutableRegion	= varR
		, eMutableFetter	= fMutable
		, eMutableSrc		= srcMutable
		, eDangerVar		= varDanger })
		
	= prettyValuePos v				% "\n"
	% "    The type inferencer made an incorrect assumption about the\n"
	% "        mutability of '" % v % "', (and backtracking isn't implemented).\n"
	% "\n"
	% "        we were using this type scheme:"
	% prettyVTS (v, scheme)
	% "\n\n"
	% "        but a new\n"
	%> dispFetterSource fMutable srcMutable
	% "\n"
	% "        was discovered, which means '" % varDanger % "' was not actually\n"
	% "        safe to generalise.\n"
	% "\n"
	% "    Please add a type signature for '" % v % "' which provides this\n"
	% "        mutability constraint.\n"

 -- Main function has wrong type
 ppr err@(ErrorWrongMainType
 		{ eScheme	= (v, scheme) })
		
	= prettyValuePos v				% "\n"
	% "    The type of main must be a function.\n"
	% "        but it was inferred to be:"
	% prettyVTS (v, scheme)
	% "\n\n"

 -- A CAF has effects that can't be masked.
 ppr err@(ErrorEffectfulCAF
 		{ eScheme	= (v, scheme)
		, eEffect	= eff })

	= v				% "\n"
	% "    CAF " % v % " must be pure, but effects:\n\n"
	%> eff
	% "\n\n    were found.\n\n"

 ppr err
	= panic stage
	$ "no match for " % show err

-----
prettyVTS (v, t)
 	= indentSpace 12 (
		"\n" ++ (varName v) ++ "\n  :: "
		++ (indentSpace 2 $ pprStrPlain $ prettyTypeSplit $ t))


-- | Pretty print the location of the value var that corresponds to this typevar.
prettyValuePos :: Var -> String
prettyValuePos var
	= fromMaybe "?"
	$ liftM (Var.prettyPos)
	$ liftM (\(IValueVar v) -> v)
	$ find  (=@= IValueVar{}) 
	$ varInfo var


-- | Get the source position from a variable
getVSP :: Var -> SourcePos
getVSP	 var
 = let	Just (ISourcePos pos)
		= find ((=@=) ISourcePos{})
		$ varInfo var
		
   in	pos
 	

-- | Get the source position from the variable in a TProj
getProjSP :: TProj -> SourcePos
getProjSP    p
 = case p of
 	TJField v	-> getVSP v
	TJFieldR v	-> getVSP v

