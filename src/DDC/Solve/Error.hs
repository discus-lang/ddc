{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Representation and display of type error messages.
module DDC.Solve.Error
	( Error(..) )
where
import DDC.Solve.Location
import DDC.Base.SourcePos
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type
import DDC.Type.SigMode
import DDC.Var
import qualified Shared.VarUtil	as Var
import Util

stage	= "DDC.Solve.Error"

data Error
	-- | Used for debugging
	= ErrorBadness
		{ eMessage	:: String }

	-- | Constructor arity mismatch.
	| ErrorCtorArity
		{ eCtorVar		:: Var
		, eCtorArity		:: Int
		, ePatternArity 	:: Int }

	-- | Constructor mismatch.
	| ErrorUnifyCtorMismatch
		{ eCtor1		:: Type
		, eTypeSource1		:: TypeSource

		, eCtor2		:: Type 
		, eTypeSource2		:: TypeSource }
		
	-- | Kind mismatch.
	| ErrorUnifyKindMismatch
		{ eKind1		:: Kind
		, eTypeSource1		:: TypeSource
		, eKind2		:: Kind
		, eTypeSource2		:: TypeSource }

	-- | Cannot construct infinite type.
	| ErrorInfiniteType
		{ eVar			:: Var
		, eLoops		:: [(Type, Type)] }
		
	-- | No instance for type class.
	| ErrorNoInstance
		{ eClassVar		:: Var
		, eTypeArgs		:: [Type] 
		, eFetterSrc 		:: TypeSource }
		
	-- | Type has no projections defined for it.
	| ErrorNoProjections
		{ eProj			:: TProj
		, eConstructor		:: Type }		

	-- | Field not present in type.
	| ErrorFieldNotPresent
		{ eProj			:: TProj
		, eConstructor		:: Type 
		, eFields		:: [Var] }

	-- | Field projection is ambiguous.
	| ErrorAmbiguousProjection
		{ eProj			:: TProj }

	-- | Cannot purify some effect.
	| ErrorCannotPurify
		{ eEffect		:: Effect
		, eEffectSource		:: TypeSource
		, eFetter		:: Fetter
		, eFetterSource		:: TypeSource }

	-- | Region constraint conflict.
	| ErrorRegionConstraint
		{ eFetter1		:: Fetter
		, eFetterSource1	:: TypeSource
		
		, eFetter2		:: Fetter
		, eFetterSource2 	:: TypeSource }
		
	-- | We found out about a mutability constraint on a region too late,
	--	and have already generalised a scheme assuming it was constant.
	| ErrorLateConstraint
		{ eScheme		:: (Var, Type)
		, eMutableRegion	:: Type
		, eMutableFetter	:: Fetter
		, eMutableSrc		:: TypeSource
		, eDangerVar		:: Var }
		
	-- | Main function has wrong type
	| ErrorWrongMainType	
		{ eScheme		:: (Var, Type) }

	-- | A CAF has effects that can't be masked.
	| ErrorEffectfulCAF
		{ eScheme		:: (Var, Type)
		, eEffect		:: Effect }
		
	-- | Inferred type does not match signature.
	| ErrorSigMismatch
		{ eScheme		:: (Var, Type)
		, eSigMode		:: SigMode 
		, eSigPos		:: SourcePos
		, eSigType		:: Type

		-- Expected and offending part of signature.
		, eSigOffending		:: Maybe (Type, Type) }
		
	deriving (Show)


-- Pretty -----------------------------------------------------------------------------------------
instance Pretty Error PMode where

 -- Random badness.
 ppr (ErrorBadness
 		{ eMessage	= s })
		
	= "    Badness: " % s % "\n"

 -- Constructor Arity.
 ppr err@(ErrorCtorArity{})
 	= Var.prettyPos (eCtorVar err) % "\n"
	% "    Wrong number of arguments for constructor match.\n"
	% "         constructor: " % eCtorVar err 				% "\n"
	% "          defined at: " % Var.prettyPosBound (eCtorVar err)		% "\n"
	% "                 has: " % eCtorArity err 
				   % " arguments, but has been used here with " 
				   % ePatternArity err % ".\n"


 -- Constructor mismatch.
 ppr (ErrorUnifyCtorMismatch 
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
 ppr (ErrorUnifyKindMismatch
		{ eKind1	= k1
		, eTypeSource1	= ts1
		
		, eKind2	= k2 })
	= (dispSourcePos ts1)						% "\n"
	% "    Kind mismatch during unification.\n"
	% "          cannot match: " % k1				% "\n"
	% "                  with: " % k2				% "\n"

 -- Infinite types.
 ppr (ErrorInfiniteType
 		{ eVar		= var 
		, eLoops	= tsLoops })
	= prettyValuePos var % "\n"
	% "    Cannot construct infinite type for '" % var % "'.\n"			
	% "    via: \n" 
	%> (punc "\n" $ map (\(t1, t2) -> t1 % " = " % t2) tsLoops)
	% "\n"
	
 -- Type class problems.
 ppr (ErrorNoInstance
 		{ eClassVar	= v
		, eTypeArgs	= ts
		, eFetterSrc	= src })
	= dispSourcePos src % "\n"
	% "    No instance for " % v % " " % punc " " (map prettyTypeParens ts) % "\n"
	
 -- Field projection problems.
 ppr (ErrorNoProjections
 		{ eProj		= p
		, eConstructor	= t })
	= (getProjSP p)		% "\n"
	% "    Type '" % t	% "' has no projections defined for it.\n"


 ppr (ErrorFieldNotPresent 
 		{ eProj		= p
		, eConstructor	= tt })
	| Just (v, _, _)	<- takeTData tt
	= (getProjSP p)							% "\n"
	% "    Type '" % v 	% "' has no projection named '" % p	% "'\n"

	
 ppr (ErrorAmbiguousProjection
 		{ eProj		= p })
	= (getProjSP p)							% "\n"
	% "    Ambiguous projection: " % p 				% "\n"
	

 -- Purity problems.
 ppr (ErrorCannotPurify
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
 ppr (ErrorRegionConstraint
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

 -- Inference screw-ups.
 ppr (ErrorLateConstraint
 		{ eScheme		= (v, scheme)
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
 ppr (ErrorWrongMainType
 		{ eScheme	= (v, scheme) })
		
	= prettyValuePos v				% "\n"
	% "    The type of main must be a function.\n"
	% "        but it was inferred to be:"
	% prettyVTS (v, scheme)
	% "\n\n"

 -- A CAF has effects that can't be masked.
 ppr (ErrorEffectfulCAF
 		{ eScheme	= (v, _)
		, eEffect	= eff })

	= v				% "\n"
	% "    CAF " % v % " must be pure, but effects:\n\n"
	%> eff
	% "\n\n    were found.\n\n"

 ppr (ErrorSigMismatch
		{ eScheme	= (v, tScheme)
		, eSigMode	= mode
		, eSigPos	= sp
		, eSigType	= tSig 
		, eSigOffending	= mOffending })
	= sp % "\n"
	% "    Inferred type:"
	% prettyVTS (v, tScheme)
	% "\n\n"
	% "    " % strMode
	% prettyVTS (v, tSig)
	% "\n\n"
	% strOffending mOffending
	
	where	strMode
		 = case mode of
			SigModeMatch	-> "Does not match signature:"
			SigModeExact	-> "Is not identical to signature:"
			SigModeLess	-> "Is greater than signature:"
			SigModeMore	-> "Is less than signature:"
			

		strOffending Nothing	 = blank
		strOffending (Just (tExpected, tOffending))
		 = "    Because " 
		 %% case mode of
			SigModeLess 	-> tOffending %% " is greater than " %% tExpected
			SigModeMore	-> tOffending %% " is less than "    %% tExpected
			_		-> blank
		 %% "\n\n"

 -- We can't display this error...
 -- TODO: eliminate this. The ppr function should be total.
 ppr err
	= panic stage
	$ "no match for " % show err


-- Utils ------------------------------------------------------------------------------------------
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
	_		-> panic stage $ "getProjSP: no match"
