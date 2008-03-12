{-# OPTIONS -fwarn-unused-imports #-}

module Type.Scheme
	( generaliseType 
	, watchClass)
where

import Type.Exp
import Type.Pretty
import Type.Plate
import Type.Util
import Type.Error

import Type.Check.Danger

import Type.Effect.MaskLocal

import Type.State
import Type.Class
import Type.Plug		
import Type.Port
import Type.Context
import Type.Util

import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Var		(NameSpace(..))
import Shared.VarPrim

import qualified Main.Arg	as Arg

import qualified Data.Map	as Map

import qualified Data.Set	as Set
import Data.Set			(Set)

import Util

-----
-- stage	= "Type.Scheme"
debug	= True
trace s	= when debug $ traceM s


-- | Generalise a type
--
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
	trace	$ "*** Scheme.generaliseType " % varT % "\n"
		% "\n"
		% "    tCore\n"
		%> prettyTS tCore	% "\n\n"

		% "    envCids          = " % envCids		% "\n"
		% "\n"

	-- flatten out the scheme so its easier for staticRs.. to deal with
	let tFlat	= flattenT tCore
	trace	$ "    tFlat\n"
		%> prettyTS tFlat	% "\n\n"

	-- Work out which cids can't be generalised in this type.

	-- 	Can't generalise regions in non-functions.
	--	... some data object is in the same region every time you use it.
	--
	let staticRsData 	= Set.toList $ staticRsDataT	tFlat
	let staticRsClosure 	= Set.toList $ staticRsClosureT	tFlat

	trace	$ "    staticRsData     = " % staticRsData	% "\n"
		% "    staticRsClosure  = " % staticRsClosure	% "\n"


	--	Can't generalise cids which are under mutable constructors.
	--	... if we generalise these classes then we could update an object at one 
	--		type and read it at another, violating soundness.
	--	
	let staticDanger	= if Set.member Arg.GenDangerousVars args
					then []
					else dangerousCidsT tCore

	trace	$ "    staticDanger     = " % staticDanger	% "\n"

	-- These are all the cids we can't generalise
	let staticCids		= Set.toList envCids ++ staticRsData ++ staticRsClosure ++ staticDanger

	-- Rewrite non-static cids to the var for their equivalence class.
	tPlug			<- plugClassIds staticCids tCore

	trace	$ "    staticCids       = " % staticCids	% "\n\n"
		% "    tPlug\n"
		%> prettyTS tPlug 	% "\n\n"

	-- Clean empty effect and closure classes that aren't ports.
	let tsContra	=  slurpContraClassVarsT tPlug
	classInst	<- gets stateClassInst
	let tClean	= reduceContextT classInst 
			$ cleanType (Set.fromList tsContra) tPlug

	trace	$ "    tClean\n" 
			%> ("= " % prettyTS tClean)		% "\n\n"

	-- Check context for problems.
	checkContext tClean

	-- Mask effects and CMDL constraints on local regions.
	-- 	Do this before adding foralls so we don't end up with quantified regions which
	--	aren't present in the type scheme.
	--
	let rsVisible	= visibleRsT $ flattenT tClean
	let tMskLocal	= maskLocalT rsVisible tClean

	trace	$ "    rsVisible    = " % rsVisible		% "\n\n"
	trace	$ "    tMskLocal\n"
		%> prettyTS tMskLocal 	% "\n\n"


	-- Quantify free variables.
	let vsFree	= filter (\v -> not $ Var.nameSpace v == NameValue)
			$ filter (\v -> not $ Var.isCtorName v)
			$ Var.sortForallVars
			$ Set.toList $ freeVars tMskLocal

	let vksFree	= map 	 (\v -> (v, kindOfSpace $ Var.nameSpace v)) 
			$ vsFree

	trace	$ "    vksFree   = " % vksFree	% "\n\n"
	let tScheme	= quantifyVarsT vksFree tMskLocal

	-- Remember which vars are quantified
	--	we can use this information later to clean out non-port effect and closure vars
	--	once the solver is done.
	let vtsMore	= Map.fromList 
			$ [(v, t)	| FMore (TVar k v) t
					<- slurpFetters tScheme]
	
	-- lookup :> bounds for each quantified var
	let (vkbsFree	:: [(Var, (Kind, Maybe Type))])
		= map (\(v, k) -> (v, (k, Map.lookup v vtsMore))) vksFree

	modify $ \s -> s { stateQuantifiedVars	
				= Map.unions
					[ Map.fromList vkbsFree
					, stateQuantifiedVars s ] }
		
	
	trace	$ "    tScheme\n"
		%> prettyTS tScheme 	% "\n\n"

	return	tScheme


slurpFetters tt
	= case tt of
		TForall vks t'	-> slurpFetters t'
		TFetters fs _	-> fs
		_		-> []



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
 = let 	vsKeep	= Map.fromList
		$ catMaybes
 		$ map (\t -> case t of
				TVar k v 
				 	| k == KEffect || k == KClosure
					-> Just (v, (k, Nothing))
				
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
 	TFetters fs t	-> mapM_ checkContextF fs
	_		-> return ()
 
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
		, primPure,	primLazyH ]
	 -> addErrors
	 	[ ErrorNoInstance
			{ eClassVar		= vClass
			, eTypeArgs		= ts 
			, eFetterMaybeSrc	= Nothing } ] -- TODO: find the source
		
	_ -> return ()
	



-----
watchClass src code
 = do	mC	<- lookupClass (ClassId code)
 
 	let res
		| Just Class 
			{ classType	= mType
			, classQueue 	= queue  	
			, classNodes	= nodes		}	<- mC
			
		= trace ( "--- class " % code % "----------------------\n"
			% "--  src   = " % src			% "\n"
			% "--  type  = " % mType		% "\n"
			% "--  queue = " % queue		% "\n"
			% "--  nodes = " % (map fst nodes)	% "\n\n")

		| otherwise
		= return ()
	res

	
