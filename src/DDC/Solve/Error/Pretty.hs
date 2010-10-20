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
spErr  sp ls
	= sp % newline
	% (indent $ vcat ls)

tsErr  ts ls
	= dispSourcePos ts % newline
	% (indent $ vcat ls)

varErr var ls
	= prettyValuePos var % newline
	% (indent $ vcat ls)

projErr j  ls
	= getProjSP j % newline
	% (indent $ vcat ls)


-- Pretty ---------------------------------------------------------------------
instance Pretty Error PMode where

 -- Random badness, used for debugging only.
 ppr (ErrorBadness
 		{ eMessage	= s })		
	= "    Badness: " % s % "\n"

 -- Constructor Arity.
 ppr err@(ErrorCtorArity{})
 	= vcat
	[ ppr $ Var.prettyPos (eCtorVar err)
	, ppr "    Wrong number of arguments for constructor match."
	, "         constructor: " % eCtorVar err
	, "          defined at: " % Var.prettyPosBound (eCtorVar err)
	, "                 has: " % eCtorArity err 
				   % " arguments, but has been used here with " 
				   % ePatternArity err % "." ]

 -- Constructor mismatch.
 ppr (ErrorUnifyCtorMismatch 
 		{ eCtor1 	= t1
		, eTypeSource1	= ts1
		, eCtor2 	= t2 
		, eTypeSource2	= ts2})
 	= tsErr ts1
	[ ppr 	"Type mismatch during unification."
	, 	"    cannot match: " % t1
	, 	"            with: " % t2
	, blank
	, 	dispTypeSource t1 ts1
	, ppr	"        conflicts with, " 
	, 	dispTypeSource t2 ts2 ]


 -- Kind Mismatch
 ppr (ErrorUnifyKindMismatch
		{ eKind1	= k1
		, eTypeSource1	= ts1
		
		, eKind2	= k2 })
	= tsErr ts1
	[ ppr	"Kind mismatch during unification."
	, 	"    cannot match: " % k1
	,	"            with: " % k2 ]


 -- Infinite types.
 ppr (ErrorInfiniteType
 		{ eVar		= var 
		, eLoops	= tsLoops })
	= varErr var 
	[ ppr	"Cannot construct infinite type for '" % var % "'."
	, 	"    via: \n" 
		%> (punc "\n" $ map (\(t1, t2) -> t1 % " = " % t2) tsLoops)
	]

	
 -- Type class problems.
 ppr (ErrorNoInstance
 		{ eClassVar	= v
		, eTypeArgs	= ts
		, eFetterSrc	= src })
	= tsErr src
	[ "No instance for " % v % " " % punc " " (map prettyTypeParens ts)]

	
 -- Field projection problems.
 ppr (ErrorNoProjections
 		{ eProj		= p
		, eConstructor	= t })
	= projErr p
	[ "Type '" % t	% "' has no projections defined for it." ]


 ppr (ErrorFieldNotPresent 
 		{ eProj		= p
		, eConstructor	= tt })
	| Just (v, _, _)	<- takeTData tt
	= projErr p
	[ "Type '" % v 	% "' has no projection named '" % p	% "'"]

	
 ppr (ErrorAmbiguousProjection
 		{ eProj		= p })
	= projErr p
	[ "Ambiguous projection: " % p ]
	

 -- Purity problems.
 ppr (ErrorCannotPurify
 		{ eEffect	= e
		, eEffectSource	= eSource
		, eFetter	= f
		, eFetterSource	= fSource })
		
	= tsErr eSource
	[ 	"Cannot purify effect `" % e % "'."
	, blank
	, 	dispTypeSource   e eSource 
	, ppr	"    conflicts with,"
	, 	dispFetterSource f fSource ]


 -- Region constraint problems
 ppr (ErrorRegionConstraint
 		{ eFetter1	= f1
		, eFetterSource1 = fSource1
 		, eFetter2	= f2
		, eFetterSource2 = fSource2})
	= tsErr fSource1
	[ ppr	"Conflicting region constraints."
	, 	dispFetterSource f1 fSource1
	, blank
	, ppr 	"    conflicts with,"
	, 	dispFetterSource f2 fSource2 ]


 -- Inference screw-ups.
 ppr (ErrorLateConstraint
 		{ eScheme		= (v, scheme)
		, eMutableFetter	= fMutable
		, eMutableSrc		= srcMutable
		, eDangerVar		= varDanger })
		
	= varErr v
	[ blank
	,  	"I naively assumed that '" % v % "' was constant,"
	, ppr   "and thus safe to generalise."
	, ppr	"Recent statements have yielded new constraints,"
	, ppr	"alas, I was mistaken."
	, blank
	, ppr	"I was using:"
	, ppr	$ prettyVTS (v, scheme)
	, blank
	, ppr	"But then I found the"
	, 	dispFetterSource fMutable srcMutable
	, 	"This means that '" % varDanger % "' was not safe to generalise in the first place."
	, blank
	, 	"Please add a type signature for '" % v % "' which provides this mutability"
	, ppr	"constraint so I can see it earlier. Sorry." ]


 -- Main function has wrong type
 ppr (ErrorWrongMainType
 		{ eScheme	= (v, scheme) })
		
	= varErr v
	[ ppr	"The type of main must be a function,\n"
	, ppr	"but it was inferred to be:"
	, ppr	$ prettyVTS (v, scheme) ]


 -- A CAF has effects that can't be masked.
 ppr (ErrorEffectfulCAF
 		{ eScheme	= (v, _)
		, eEffect	= eff })

	= varErr v
	[ 	"CAFs like " % v % " must be pure,"
	, ppr	"but this one has the unmaskable effects: " % eff ]


 -- Type signatures
 ppr (ErrorSigMismatch
		{ eScheme		= (v, tScheme)
		, eSigMode		= mode
		, eSigPos		= sp
		, eSigType		= tSig 
		, eSigBadType		= mBadType 
		, eSigBadContext	= mBadContext })
	= spErr sp
	[ ppr "Inferred type:"
	, ppr	$ prettyVTS (v, tScheme)
	, blank
	, ppr strMode
	, ppr	$ prettyVTS (v, tSig)
	, blank
	, vcat 	$ catMaybes
		[ strBadType    mBadType
		, strBadContext mBadContext ] ]
	
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
prettyVTS (v, t)
 	= indentSpace 4
	$ "    " 
		++ varName v 
		++ "\n  :: "
		++ (indentSpace 2 $ pprStrPlain $ prettyTypeSplit $ t)


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

