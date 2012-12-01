{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

module DDC.Solve.Error.Base
	(Error(..))
where
import DDC.Solve.Location
import DDC.Base.SourcePos
import DDC.Type
import DDC.Type.SigMode
import DDC.Var

-- | Errors that can be raised by the type checker.
data Error
	-- | Used for debugging only.
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
		
	-- | Main function has wrong type.
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

		-- | Expected and offending part of signature.	
		, eSigBadType		:: Maybe (Type, Type) 

		-- | Unexpected context.
		, eSigBadContext	:: Maybe [Kind]
		}	
	deriving (Show)
