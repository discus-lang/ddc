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
-- trace s	= when debug $ traceM s
tracel s 	= when debug $ traceM (s % nl)
tracell s 	= when debug $ traceM (s % nlnl)

-- | Generalise a type
generaliseType
	:: TypeSource		-- ^ Source of variable definition.
	-> Var 			-- ^ Binding variable of the type being generalised.
	-> Type			-- ^ Type to generalise.
	-> Set ClassId		-- ^ Classids in environment that must remain monomorphic.
	-> SquidM Type

generaliseType src varT tCore cidsEnv
 = traceI 
 $ do	tracel	$ "*** Scheme.generaliseType "	% varT
	generaliseType' src varT tCore cidsEnv


generaliseType' src varT tCore cidsEnv
 = do	args	<- gets stateArgs
	tracell $ "-- type"		%! prettyTypeSplit tCore

	-- Determine monomorphic vars -----------------------------------------
	-- TODO: determination of material cids is currently broken.
	let cidsMaterial
		= [cid 	| TVar k (UClass cid) <- Set.toList 	
			$ staticRsT $ flattenT tCore]

	-- Can't generalise cids which are under mutable constructors.
	let cidsDangerous
		= if Set.member Arg.DebugGeneraliseDangerousVars args
			then []
			else dangerousCidsT tCore

	-- These are all the cids we can't generalise
	let staticCids
		= Set.unions
		[ cidsEnv			-- can't generalise cids in the environment
		, Set.fromList cidsMaterial	-- can't generalise material cids.
		, Set.fromList cidsDangerous]	-- can't generalise dangerous cids.


	-- Plug ---------------------------------------------------------------
	-- Rewrite non-static cids to the var for their equivalence class.
	tPlug		<- plugClassIds staticCids tCore
	tracell	$ "-- plugged"  	%! prettyTypeSplit tPlug
	tracel	$ "   cidsEnv        = " % cidsEnv
	tracel	$ "   cidsMaterial   = " % cidsMaterial
	tracell	$ "   cidsDangerous  = " % cidsDangerous


	-- Clean --------------------------------------------------------------
	-- We tend to end up with lots of intermediate effect and closure variables
	-- that are otherwise unconstrained. Might as well default them to bottom.
	let tsParam	=  slurpParamClassVarsT tPlug
	classInst	<- liftM squidEnvClassInst $ gets stateEnv
	
	let tClean	= cleanType (Set.fromList tsParam) tPlug
	tracell	$ "-- cleaned" 		%! prettyTypeSplit tClean


	-- Mask ---------------------------------------------------------------
	-- Mask effects and constraints on non-observable regions.
	let rsVisible	= visibleRsT $ flattenT tClean
	let tMskLocal	= maskLocalT rsVisible tClean
	tracell	$ "-- masked" 		%! prettyTypeSplit tMskLocal
	tracell $ "   rsVisible = " % rsVisible

	-- Default ------------------------------------------------------------
	-- If we're generalising the type of a top level binding, then default
	-- any regions to direct and const. 
	-- TODO: pass in whether it's top level as a parameter instead of getting
	--       it from the solver state like this.
	vsBoundTop	<- getsRef stateVsBoundTopLevel

	let isTopLevel	= Set.member varT vsBoundTop

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
	tracell	$ "-- defaulted" 	%! prettyTypeSplit tConstify

	-- Reduce -------------------------------------------------------------
	-- Remove type class constraints for instanes that we know about.
	-- TODO: This is duplicated in extract. Why are we doing it again?
	let tReduced	= reduceContextT classInst tConstify
	tracell	$ "-- reduced"		%! prettyTypeSplit tReduced

	-- Check context for unknown instances or constraint conflicts.
	checkContext src tReduced

	-- Quantify -----------------------------------------------------------
	-- Add forall quantifiers for free variables that don't otherwise
	-- need to remain monomorphic.

	let vsFree	= filter (\v -> not $ varNameSpace v == NameValue)
			$ filter (\v -> not $ Var.isCtorName v)
			$ Var.sortForallVars
			$ Set.toList $ freeVars tReduced

	let vksFree	= map 	 (\v -> (v, let Just k = kindOfSpace $ varNameSpace v in k)) $ vsFree
	let tScheme	= quantifyVarsT vksFree tReduced

	tracell	$ "-- quantified"	%! prettyTypeSplit tScheme
	tracell	$ "   vksFree        = " 	% vksFree

	-- Remember which vars are quantified
	--	we can use this information later to clean out non-port effect and closure vars
	--	once the solver is done.
	let vtsMore	= Map.fromList 
			$ [(v, t)	| FMore (TVar k (UVar v)) t
					<- slurpFetters tScheme]
	
	-- lookup :> bounds for each quantified var
	let (vkbsFree	:: [(Var, (Kind, Maybe Type))])
		= map (\(v, k) -> (v, (k, Map.lookup v vtsMore))) vksFree

	-- TODO: Stashing quantified vars in the state feels very hacky.
	--       think of a cleaner way to manage this.
	stateQuantifiedVarsKM 	`modifyRef` Map.union (Map.fromList vkbsFree)
	stateQuantifiedVars	`modifyRef` Set.union (Set.fromList vsFree) 

	-- Check --------------------------------------------------------------
	-- Check the inferred type against any signatures that we have for it.

	-- See what sigs we have.
	pbSigs		<- liftM (join . maybeToList . Map.lookup varT) 
			$  liftM squidEnvSigs
			$  gets  stateEnv 
	
	-- Signature checking only works on finalised types, without embeded meta variables.
	vsQuant		<- getsRef stateQuantifiedVars
	tPlugged	<- plugClassIds Set.empty tScheme
	tracell	$ "-- pluged for sig check" %! prettyTypeSplit tPlugged

	mapM_ (checkSchemeAgainstSig tPlugged) pbSigs

	-- We used the plugged type for checking against sigs,
	-- but we're just returning the scheme type.
	return	tScheme


-- | Slurp the fetters from a type.
slurpFetters :: Type -> [Fetter]
slurpFetters tt
	= case tt of
		TForall b k t'	 -> slurpFetters t'
		TConstrain _ crs -> fettersOfConstraints crs
		_		 -> []


-- | Empty effect and closure eq-classes which do not appear in the environment or 
--   a contra-variant position in the type can never be anything but _|_,
--   so we can safely erase them now.
--
--   TODO: We need to run the cleaner twice to handle types like this:
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
	
