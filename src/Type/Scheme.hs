{-# OPTIONS -fwarn-unused-imports #-}

module Type.Scheme
	( generaliseType 
	, checkContext )
where
import Type.Check.Danger
import Type.Plug
import Type.ToCore		
import Type.Strengthen
import Shared.VarPrim
import Util
import DDC.Solve.Error
import DDC.Solve.State
import DDC.Solve.Interface.Problem
import DDC.Type
import DDC.Type.SigMode
import DDC.Var.NameSpace
import DDC.Var
import DDC.Util.FreeVars
import qualified Shared.VarUtil		as Var
import qualified DDC.Main.Arg		as Arg
import qualified Data.Map		as Map
import qualified Data.Set		as Set
import qualified Data.Foldable		as Foldable

debug	 	= True
trace s	 	= when debug $ traceM s
tracel s 	= when debug $ traceM (s % "\n")
tracell s 	= when debug $ traceM (s % "\n\n")

-- | Generalise a type
generaliseType
	:: Var 			-- binding variable of the type being generalised
	-> Type			-- the type to generalise
	-> Set ClassId		-- the classIds which must remain fixed (non general)
	-> SquidM Type

generaliseType varT tCore envCids
 = {-# SCC "generaliseType" #-} generaliseType' varT tCore envCids 

generaliseType' varT tCore envCids
 = do
	args			<- gets stateArgs
	trace	$ vcat
		[ "*** Scheme.generaliseType "	% varT
		, "    tCore:\n"		%> prettyTypeSplit tCore
		, blank
		, "    envCids          = "	% envCids
		, blank]

	-- flatten out the scheme so its easier for staticRs.. to deal with
	let tFlat	= flattenT tCore
	tracell $ "    tFlat:\n"		%> prettyTypeSplit tFlat

	-- Work out which cids can't be generalised in this type.
	-- 	Can't generalise regions in non-functions.
	--	... some data object is in the same region every time you use it.
	--
	let staticRsData 	= [cid | TVar k (UClass cid) <- Set.toList $ staticRsDataT    tFlat ]
	let staticRsClosure 	= [cid | TVar k (UClass cid) <- Set.toList $ staticRsClosureT tFlat ]

	trace	$ vcat
		[ "    staticRsData     = " 	% staticRsData
		, "    staticRsClosure  = " 	% staticRsClosure ]

	-- Can't generalise cids which are under mutable constructors.
	-- ... if we generalise these classes then we could update an object at one 
	-- 	type and read it at another, violating soundness.
	let staticDanger	= if Set.member Arg.GenDangerousVars args
					then []
					else dangerousCidsT tCore

	tracel	$ "    staticDanger     = " 	% staticDanger

	-- These are all the cids we can't generalise
	let staticCids		
		= Set.unions
			[ envCids
			, Set.fromList staticRsData 
			, Set.fromList staticRsClosure
			, Set.fromList staticDanger]

	tracel	$ "    staticCids       = "	% staticCids


	-- Rewrite non-static cids to the var for their equivalence class.
	tPlug			<- plugClassIds staticCids tCore
	tracell	$ "    tPlug:\n"		%> prettyTypeSplit tPlug

	-- Clean empty effect and closure classes that aren't ports.
	let tsParam	=  slurpParamClassVarsT_constrainForm tPlug
	classInst	<- getsRef stateClassInst

	let tClean	= cleanType (Set.fromList tsParam) tPlug
	tracell	$ "    tClean:\n" 		%> prettyTypeSplit tClean

	let tReduce	= reduceContextT classInst tClean
	tracell	$ "    tReduce:\n"		%> prettyTypeSplit tReduce

	-- Mask effects and Const/Mutable/Local/Direct constraints on local regions.
	-- 	Do this before adding foralls so we don't end up with quantified regions that
	--	aren't present in the type scheme.
	--
	let rsVisible	= visibleRsT $ flattenT tReduce
	tracell	$ "    rsVisible        = " 	% rsVisible

	let tMskLocal	= maskLocalT rsVisible tReduce
	tracell	$ "    tMskLocal:\n"		%> prettyTypeSplit tMskLocal

	-- If we're generalising the type of a top level binding, 
	--	and if any of its free regions are unconstraind,
	--	then make them constant.
	vsBoundTop	<- getsRef stateVsBoundTopLevel

	let isTopLevel	= Set.member varT vsBoundTop
	tracell	$ "    isTopLevel       = " 	% isTopLevel

	let rsMskLocal	= Set.toList $ freeTClasses tMskLocal
	let fsMskLocal	= case tMskLocal of
				TConstrain _ crs	-> fettersOfConstraints crs
				_			-> []
	
	let fsMore
		| isTopLevel
		=  [ FConstraint primConst [tR]
			| tR@(TVar kR (UClass cid))	<- rsMskLocal
			, kR	== kRegion
			, notElem (FConstraint primMutable [tR]) fsMskLocal ]

		++ [ FConstraint primDirect [tR]
			| tR@(TVar kR (UClass cid)) <- rsMskLocal
			, kR	== kRegion
			, notElem (FConstraint primLazy [tR]) fsMskLocal ]
	
		| otherwise
		= []
		
	let tConstify	= addConstraints (constraintsOfFetters fsMore) tMskLocal
	tracell	$ "    tConstify:\n" 		%> prettyTypeSplit tConstify

	-- Check context for problems
	checkContext tConstify

	-- Quantify free variables.
	let vsFree	= filter (\v -> not $ varNameSpace v == NameValue)
			$ filter (\v -> not $ Var.isCtorName v)
			$ Var.sortForallVars
			$ Set.toList $ freeVars tConstify

	let vksFree	= map 	 (\v -> (v, let Just k = kindOfSpace $ varNameSpace v in k)) $ vsFree
	tracell	$ "    vksFree          = " 	% vksFree

	let tScheme	= quantifyVarsT vksFree tConstify
	tracell	$ "    tScheme:\n"		%> prettyTypeSplit tScheme

	-- Remember which vars are quantified
	--	we can use this information later to clean out non-port effect and closure vars
	--	once the solver is done.
	let vtsMore	= Map.fromList 
			$ [(v, t)	| FMore (TVar k (UVar v)) t
					<- slurpFetters tScheme]
	
	-- lookup :> bounds for each quantified var
	let (vkbsFree	:: [(Var, (Kind, Maybe Type))])
		= map (\(v, k) -> (v, (k, Map.lookup v vtsMore))) vksFree

	stateQuantifiedVarsKM 	`modifyRef` Map.union (Map.fromList vkbsFree)
	stateQuantifiedVars	`modifyRef` Set.union (Set.fromList vsFree) 

	-- Check generalised scheme any sigs we have for it.
	pbSigs		<- liftM (join . maybeToList . Map.lookup varT) 
			$  liftM squidEnvSigs
			$  gets  stateEnv 

	mapM_ (checkSchemeAgainstSig tScheme) pbSigs

	return	tScheme


-- | Check a generalised type scheme against a provided type signature.
--
--   TODO: This won't work for type sigs that have constraints on monomorphic effect
--         or closure variables. This could happen if we have mutable data at top 
--         level which contains effectful functions, so the effect vars aren't generalised.
--         It would also happen if we had sigs on local bindings.
--
checkSchemeAgainstSig :: Type -> ProbSig -> SquidM ()
checkSchemeAgainstSig tScheme prob@(ProbSig v sp mode tSig)
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

	let mErrs	= checkSchemeDiagnose tScheme prob subsLow subsHigh
	addErrors $ maybeToList mErrs
	
	return ()
		

checkSchemeDiagnose
	:: Type 
	-> ProbSig 
	-> Subsumes
	-> Subsumes
	-> Maybe Error
	
checkSchemeDiagnose tScheme prob@(ProbSig v sp mode tSig) subsLow subsHigh
	| SigModeMatch	<- mode
	= Nothing

	| SigModeLess	<- mode
	, Subsumes	<- subsLow
	= Nothing
	
	| SigModeMore	<- mode
	, Subsumes	<- subsHigh
	= Nothing

	| SigModeExact	<- mode
	, Subsumes	<- subsLow
	, Subsumes	<- subsHigh
	= Nothing
	
	| otherwise
	= Just 	$ ErrorSigMismatch
		{ eScheme	= (v, tScheme)
		, eSigMode	= mode
		, eSigPos	= sp
		, eSigType	= tSig }
	

slurpRename :: Type -> Type -> Maybe (Var, Var)
slurpRename t1 t2
 	| TVar _ u1	<- t1
	, TVar _ u2	<- t2
	, Just v1	<- takeVarOfBound u1
	, Just v2	<- takeVarOfBound u2
	= Just (v1, v2)
	
	| otherwise
	= Nothing
	


	


-- | Slurp the fetters from a type.
slurpFetters :: Type -> [Fetter]
slurpFetters tt
	= case tt of
		TForall b k t'	 -> slurpFetters t'
		TConstrain _ crs -> fettersOfConstraints crs
		_		 -> []


-- | Empty effect and closure eq-classes which do not appear in the environment or 
--	a contra-variant position in the type can never be anything but _|_,
--	so we can safely erase them now.
--
--   TODO:
--	We need to run the cleaner twice to handle types like this:
--		a -(!e1)> b
--		:- !e1 = !{ !e2 .. !en }
--
--	where all of !e1 .. !en are cleanable.
--	Are two passes enough?
--	
cleanType :: Set Type -> Type -> Type
cleanType tsSave tt
 = let 	vsKeep	= Set.fromList
		$ catMaybes
 		$ map (\t -> case t of
				TVar k (UVar v)
				 	| k == kEffect || k == kClosure
					-> Just v
				
				_	-> Nothing)
		$ Set.toList tsSave
		
   in	finaliseT vsKeep False tt
 

-- | After reducing the context of a type to be generalised, if certain constraints
--	remain then this is symptomatic of problems in the source program.
-- 
--	Projection constraints indicate an ambiguous projection.
--	Shape constraints indicate 
--	Type class constraints indicate that no instance for this type is available.
--
checkContext :: Type -> SquidM ()
checkContext tt
 = case tt of
 	TConstrain t crs
  	  -> mapM_ checkContextF $ fettersOfConstraints crs
	_ -> return ()
 
checkContextF ff
 = case ff of
 	FProj j vInst tDict tBind
	 -> addErrors
	 	[ ErrorAmbiguousProjection
			{ eProj		= j } ]

	FConstraint vClass ts
	 | not 	$ elem vClass
	 	[ primMutable,	primMutableT
		, primConst,	primConstT
		, primLazy, 	primDirect
		, primPure,	primEmpty
		, primLazyH ]
	 , varName vClass /= "Safe"
	 -> addErrors
	 	[ ErrorNoInstance
			{ eClassVar		= vClass
			, eTypeArgs		= ts 
			, eFetterMaybeSrc	= Nothing } ] -- TODO: find the source
		
	_ -> return ()
	
