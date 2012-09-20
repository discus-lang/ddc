{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Pretty printing of type checker error messages.
module DDC.Solve.Error.Pretty () 
where
import DDC.Solve.Error.Base
import DDC.Solve.Location
import DDC.Base.SourcePos
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type
import DDC.Type.SigMode
import DDC.Var
import qualified Shared.VarUtil	as Var
import Util

stage	= "DDC.Solve.Error.Pretty"

-- helpers for generating error messages
spErr  sp str
	= sp % newline 
	% indent str

tsErr  ts str
	= dispSourcePos ts % newline
	% indent str

varErr var str
	= prettyValuePos var % newline
	% indent str

projErr j str
	= getProjSP j % newline
	% indent str


-- Pretty ---------------------------------------------------------------------
instance Pretty Error PMode where

 -- A general state of of badness, used for debugging only.
 ppr (ErrorBadness
 		{ eMessage	= s })		
	= "    Badness: " % s

 -- Constructor Arity
 ppr err@(ErrorCtorArity{})
 	= Var.prettyPos (eCtorVar err)
	%! "    Wrong number of arguments for constructor match."
	%! "         constructor: " % eCtorVar err
	%! "          defined at: " % Var.prettyPosBound (eCtorVar err)
	%! "                 has: " % eCtorArity err 
				    % " arguments, but has been used here with " 
				    % ePatternArity err % "."

 -- Constructor mismatch
 ppr (ErrorUnifyCtorMismatch 
 		{ eCtor1 	= t1
		, eTypeSource1	= ts1
		, eCtor2 	= t2 
		, eTypeSource2	= ts2})
 	= tsErr ts1
	$  "Type mismatch during unification."
	%! "    cannot match: " % t1
	%! "            with: " % t2
	%! blank
	%! dispTypeSource t1 ts1
	%! blank
	%! "        conflicts with, " 
	%! dispTypeSource t2 ts2


 -- Kind Mismatch
 ppr (ErrorUnifyKindMismatch
		{ eKind1	= k1
		, eTypeSource1	= ts1
		
		, eKind2	= k2 })
	= tsErr ts1
	$  "Kind mismatch during unification."
	%! "    cannot match: " % k1
	%! "            with: " % k2


 -- Infinite types.
 ppr (ErrorInfiniteType
 		{ eVar		= var 
		, eLoops	= tsLoops })
	= varErr var 
	$  "Cannot construct infinite type for" %% quotes var % "."
	%! "    via:" 
	%! indent (vcat [ t1 %% "=" %% t2 | (t1, t2) <- tsLoops ])

	
 -- Type class problems.
 ppr (ErrorNoInstance
 		{ eClassVar	= v
		, eTypeArgs	= ts
		, eFetterSrc	= src })
	= tsErr src
	$ "No instance for " % v %% hsep (map prettyTypeParens ts)

	
 -- Field projection problems.
 ppr (ErrorNoProjections
 		{ eProj		= p
		, eConstructor	= t })
	= projErr p
	$ "Type" %% quotes t %% "has no projections defined for it."


 ppr (ErrorFieldNotPresent 
 		{ eProj		= p
		, eConstructor	= tt })
	| Just (v, _, _)	<- takeTData tt
	= projErr p
	$ "Type" %% quotes v %% "has no projection named" %% quotes p
	
 ppr (ErrorAmbiguousProjection
 		{ eProj		= p })
	= projErr p
	$ "Ambiguous projection: " % p
	

 -- Purity problems.
 ppr (ErrorCannotPurify
 		{ eEffect	= e
		, eEffectSource	= eSource
		, eFetter	= f
		, eFetterSource	= fSource })
		
	= tsErr eSource
	$  "Cannot purify effect" %% quotes e % "."
	%! blank
	%! dispTypeSource   e eSource 
	%! blank
	%! "    conflicts with,"
	%! dispFetterSource f fSource 


 -- Region constraint problems
 ppr (ErrorRegionConstraint
 		{ eFetter1	= f1
		, eFetterSource1 = fSource1
 		, eFetter2	= f2
		, eFetterSource2 = fSource2})
	= tsErr fSource1
	$  "Conflicting region constraints."
	%! blank
	%! dispFetterSource f1 fSource1
	%! blank
	%! "    conflicts with,"
	%! dispFetterSource f2 fSource2


 -- Inference screw-ups.
 ppr (ErrorLateConstraint
 		{ eScheme		= (v, scheme)
		, eMutableFetter	= fMutable
		, eMutableSrc		= srcMutable
		, eDangerVar		= varDanger })
		
	= varErr v
	$  blank
	%! "I naively assumed that" %% quotes v %% "was constant, and thus safe to generalise."
	%! "Recent statements have yielded new constraints, alas I was mistaken."
	%! blank
	%! "I was using:"
	%! prettyVT v scheme
	%! blank
	%! "But then I found the"
	%! dispFetterSource fMutable srcMutable
	%! blank
	%! "This means that" %% quotes varDanger %% "was not safe to generalise in the first place."
	%! blank
	%! "Please add a type signature for" %% quotes v %% "which provides this mutability"
	%! "constraint so I can see it earlier. Sorry."


 -- Main function has wrong type
 ppr (ErrorWrongMainType
 		{ eScheme	= (v, scheme) })
		
	= varErr v
	$  "The program's main function must be of type:"
	%! "    main :: Unit -> Unit"
	%! "but it was inferred to be:"
	%! prettyVT v scheme


 -- A CAF has effects that can't be masked.
 ppr (ErrorEffectfulCAF
 		{ eScheme	= (v, _)
		, eEffect	= eff })

	= varErr v
	$  "CAFs like " % v % " must be pure,"
	%! "but this one has the unmaskable effects: " % eff


 -- Type signatures
 ppr (ErrorSigMismatch
		{ eScheme		= (v, tScheme)
		, eSigMode		= mode
		, eSigPos		= sp
		, eSigType		= tSig 
		, eSigBadType		= mBadType 
		, eSigBadContext	= mBadContext })
	= spErr sp
	$ "Inferred type:"
	%! prettyVT v tScheme
	%! blank
	%! strMode
	%! prettyVT v tSig
	%! blank
	%! (vcat $ catMaybes
		[ strBadType    mBadType
		, strBadContext mBadContext ])
	where	strMode
		 = case mode of
			SigModeMatch	-> "Does not match signature:"
			SigModeExact	-> "Is not identical to signature:"
			SigModeLess	-> "Is bigger than signature:"
			SigModeMore	-> "Is smaller than signature:"
			

		strBadType Nothing	 = Nothing
		strBadType (Just (tExpected, tOffending))
		 = Just $ "Because " %% tOffending %% " does not match " %% tExpected

		strBadContext Nothing	= Nothing
		strBadContext (Just [kContext])
		 = Just $ "Unexpected constraint: " %% kContext

		strBadContext (Just ksContext)
		 = Just $ "Unexpected constraints: " %% punc " " ksContext

 -- We can't display this error...
 -- TODO: eliminate this. The ppr function should be total.
 ppr err
	= panic stage
	$ "no match for " % show err


-- Utils ------------------------------------------------------------------------------------------
prettyVT v t
 	= "    " % varName v % nl
	%> ("::" %% prettyTypeSplit t)


-- | Pretty print the location of the value var that corresponds to this typevar.
prettyValuePos :: Var -> String
prettyValuePos var
	= fromMaybe "?"
	$ liftM (Var.prettyPos)
	$ liftM (\info -> let IValueVar v = info in v)
	$ find  isIValueVar
	$ varInfo var


-- | Get the source position from a variable
getVSP :: Var -> SourcePos
getVSP	 var
 = let	Just (ISourcePos pos)
		= find isISourcePos
		$ varInfo var
		
   in	pos
 	

-- | Get the source position from the variable in a TProj
getProjSP :: TProj -> SourcePos
getProjSP    p
 = case p of
 	TJField v	-> getVSP v
	TJFieldR v	-> getVSP v
	_		-> panic stage $ "getProjSP: no match"

