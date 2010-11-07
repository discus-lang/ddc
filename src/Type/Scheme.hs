{-# OPTIONS -fwarn-unused-imports #-}

module Type.Scheme
	( generaliseType 
	, checkContext )
where
import Type.Check.Danger
import Type.Plug
import Type.Strengthen
import Shared.VarPrim
import Util
import DDC.Solve.Error
import DDC.Solve.State
import DDC.Solve.Check.Signature
import DDC.Solve.Location
import DDC.Type
import DDC.Var.NameSpace
import DDC.Var
import DDC.Util.FreeVars
import qualified Shared.VarUtil		as Var
import qualified DDC.Main.Arg		as Arg
import qualified Data.Map		as Map
import qualified Data.Set		as Set

debug	 	= True
trace s	 	= when debug $ traceM s
tracel s 	= when debug $ traceM (s % "\n")
tracell s 	= when debug $ traceM (s % "\n\n")

-- | Generalise a type
generaliseType
	:: TypeSource		-- source of variable definition
	-> Var 			-- binding variable of the type being generalised
	-> Type			-- the type to generalise
	-> Set ClassId		-- the classIds which must remain fixed (non general)
	-> SquidM Type

generaliseType src varT tCore envCids
 = do	args	<- gets stateArgs
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
	let staticRs 	= [cid | TVar k (UClass cid) <- Set.toList $ staticRsT tFlat]

	tracel	$ "    staticRs    = " 		% staticRs

	-- Can't generalise cids which are under mutable constructors.
	-- ... if we generalise these classes then we could update an object at one 
	-- 	type and read it at another, violating soundness.
	let staticRsDanger	= if Set.member Arg.DebugGeneraliseDangerousVars args
					then []
					else dangerousCidsT tCore

	tracel	$ "    staticRsDanger     = " 	% staticRsDanger

	-- These are all the cids we can't generalise
	let staticCids		
		= Set.unions
			[ envCids
			, Set.fromList staticRs
			, Set.fromList staticRsDanger]

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
	checkContext src tConstify

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
	
	-- Signature checking only works on finalised types,
	-- without embeded meta variables.
	vsQuant		<- getsRef stateQuantifiedVars
	tPlugged	<- plugClassIds Set.empty tScheme
	tracell	$ "    tPlugged:\n"		%> prettyTypeSplit tPlugged

	mapM_ (checkSchemeAgainstSig tPlugged) pbSigs

	return	tScheme


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
checkContext :: TypeSource -> Type -> SquidM ()
checkContext src tt
 = case tt of
 	TConstrain t crs
  	  -> mapM_ (checkContextF src) $ fettersOfConstraints crs
	_ -> return ()
 
checkContextF src ff
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
			{ eClassVar	= vClass
			, eTypeArgs	= ts 
			, eFetterSrc	= src } ]
		
	_ -> return ()
	
