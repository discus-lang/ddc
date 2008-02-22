
module Type.Error
	( Error(..) )
where

import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Pretty
import Shared.Literal
import Shared.Error
import Shared.Base		(SourcePos(..))

import Type.Location
import Type.Exp
import Type.Pretty

import Util

-----
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
		, eTypeArgs	:: [Type] }		
		
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
		
	

	| ErrorMutablePurifiedRead			-- Mutable region is also constraint to be constant
		{ eMutableFetter :: Fetter		--	due to a purified read effect.
		, eMutableSource :: TypeSource		
		, ePureFetter	:: Fetter
		, ePureSource	:: TypeSource
		, eReadEff	:: Effect
		, eReadSource	:: TypeSource }

{-
	| ErrorConstWrite				-- Attempted write to a const region.
		{ eFetter	:: Fetter		--	Const fetter.
		, eFetterSource	:: TypeSource		-- 	Source of Const fetter.
		
		, eEffect	:: Effect		--	Write effect.
		, eEffectSource	:: TypeSource }		-- 	Source of Write effect.
	
	| ErrorPureReadWrite
		{ eReadEff	:: Effect
		, eReadSource	:: TypeSource
		, ePureFetter	:: Fetter
		, ePureSource	:: TypeSource
		, eWriteEff	:: Effect
		, eWriteSource	:: TypeSource }
-}
	-- Update soundness problems.
	| ErrorUpdateSoundness				-- Update soundness problem 
		{ eErrVar	:: Var
		, eType		:: Type			--	The offending scheme.
		, eTypeDanger	:: [Type] }		--	The dangerous var(s) which are causing the problem.

	-- Inference screw-ups.
	| ErrorLateConstraint				-- Found out about a mutability constraint too late.
		{ eScheme	:: (Var, Type)		-- 	The scheme we've been using.
		, eRegen	:: Type }		-- 	The scheme we should have used.
		
	-- Main function has wrong type
	| ErrorWrongMainType	
		{ eScheme	:: (Var, Type) }	-- the type that was inferred for main		
		
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
 	= (dispSourcePos $ selectSourceTS [ts1, ts2])			% "\n"
	% "    Type mismatch during unification.\n"
	% "          cannot match: " % t1				% "\n"
	% "                  with: " % t2				% "\n"
	% "\n"
	%> dispTypeSource t1 ts1
	% "\n"
	% "        conflicts with, " 					% "\n"
	%> dispTypeSource t2 ts2

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
		, eTypeArgs	= ts })
	= "    No instance for " % v % " " % " " %!% map prettyTS ts % "\n"
	
 -- Field projection problems.
 ppr err@(ErrorNoProjections
 		{ eProj		= p
		, eConstructor	= t })
	= (getProjSP p)		% "\n"
	% "    Type '" % t	% "' has no projections defined for it.\n"


 ppr err@(ErrorFieldNotPresent 
 		{ eProj		= p
		, eConstructor	= TData v _
		, eFields	= fields })
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

 ppr err@(ErrorMutablePurifiedRead
		{ eMutableFetter = mut
		, eMutableSource = mutSource
		, ePureFetter	 = pure
		, ePureSource	 = pureSource
		, eReadEff	 = eRead
		, eReadSource	 = readSource })
		
	= (dispSourcePos pureSource)					% "\n"
	% "    Cannot purify Read effect on Mutable region.\n"
	% "      A purity constraint on a Read effect requires the region it\n"
	% "      acts on to be Const, and it cannot be Mutable at the same time.\n"
	% "\n"
	%> dispTypeSource eRead readSource
	% "\n"
	% "        is being purified by\n"
	%> dispFetterSource pure pureSource
	% "\n"
	% "        which conflicts with\n"
	%> dispFetterSource mut mutSource

{-
 ppr err@(ErrorConstWrite 
 		{ eEffect	= e
		, eEffectSource	= eSource
		, eFetter	= f
		, eFetterSource	= fSource })
	
	= (dispSourcePos  eSource)					% "\n"
	% "    Cannot write to Const region.\n"
	% prettyETS e eSource
	% "\n"
	% "     conflicts with,\n"
	% prettyFTS f fSource
-}	

{-
 ppr err@(ErrorPureReadWrite
		{ eReadEff	= r
		, eReadSource	= rTS
		, ePureFetter	= p
		, ePureSource	= pTS
		, eWriteEff	= w
		, eWriteSource	= wTS })

	= dispSourcePos wTS						% "\n"
	% "    Cannot write to Const region.\n"
	% "      This region is being forced Const because there is a\n"
	% "      purity constraint on a Read effect which accesses it.\n"
	% "\n"
	% prettyETS w wTS
	% "\n"
	% "          conflicts with,\n"
	% prettyETS r rTS
	% "\n"
	% "          which is being purified by,\n"
	% prettyFTS p pTS
-}

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
 		{ eScheme	= (v, scheme)
		, eRegen	= regen })
		
	= prettyValuePos v				% "\n"
	% "    The type inference algorithm made an incorrect assumption\n"
	% "        about the mutability of `" % v % "'.\n"
	% "                   (and backtracking isn't implemented yet).\n"
	% "\n"
	% "        scheme we've been using:"
	% prettyVTS (v, scheme)
	% "\n\n"
	% "        scheme we should have used:"
	% prettyVTS (v, regen)
	% "\n\n"
	% "    Please add a type signature for `" % v % "' which provides\n"
	% "        the mutability constraints present in the second scheme.\n"
	% "\n\n"

 -- Main function has wrong type
 ppr err@(ErrorWrongMainType
 		{ eScheme	= (v, scheme) })
		
	= prettyValuePos v				% "\n"
	% "    Inferred type of main is not () -> ().\n"
	% "\n"
	% "        inferred type:" % prettyVTS (v, scheme) % "\n"

-----
prettyVTS (v, t)
 	= indent 12 (
		"\n" ++ (Var.name v) ++ "\n  :: "
		++ (indent 2 $ pprStrPlain $ prettyTypeSplit $ t))


-- | Pretty print the location of the value var that corresponds to this typevar.
prettyValuePos :: Var -> String
prettyValuePos var
	= fromMaybe "?"
	$ liftM (Var.prettyPos)
	$ liftM (\(Var.IValueVar v) -> v)
	$ find  (=@= Var.IValueVar{}) 
	$ Var.info var


-----
-- selectSourceTS
--	Select from a set of possible TypeSource's
--	to find a good representative to report as the
--	main cause of an error.
--
selectSourceTS :: [TypeSource] -> TypeSource
selectSourceTS []
	= panic stage 
	$ "selectSourceTS: no suitable source pos"

selectSourceTS (t : ts)
 = case t of
-- 	TSIfObj{}	-> selectSourceTS ts
	_		-> t

	 


getVSP :: Var	-> SourcePos
getVSP	 var
 = let	Just (Var.ISourcePos pos)
		= find ((=@=) Var.ISourcePos{})
		$ Var.info var
		
   in	pos
 	

getProjSP :: TProj	-> SourcePos
getProjSP    p
 = case p of
 	TJField v	-> getVSP v
	TJFieldR v	-> getVSP v


