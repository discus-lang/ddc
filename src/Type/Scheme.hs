
module Type.Scheme
	( extractType
	, generaliseType
	, collectCidsEnv )
where

import qualified Debug.Trace	as Debug

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Util

import Shared.Error
import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Var		(NameSpace(..))

import qualified Main.Arg	as Arg

import Type.Exp
import Type.Pretty
import Type.Plate
import Type.Util
import Type.Merge
import Type.Trace
import Type.Error

import Type.Check.CheckSig
import Type.Check.GraphicalData	(checkGraphicalDataT)
import Type.Check.Soundness	(dangerousCidsT)
import Type.Closure.Trim 	(trimClosureT)

import Type.Effect.MaskLocal	(maskLocalT)
-- import Type.Effect.MaskFresh	(maskEsFreshT)
-- import Type.Effect.MaskPure	(maskEsPureT)
import Type.Effect.Narrow

import Type.State
import Type.Class
import Type.Plug		(plugClassIds, staticRsDataT, staticRsClosureT)
import Type.Port
import Type.Induced
import Type.Context


-----
stage	= "Type.Scheme"
debug	= True
trace s	= when debug $ traceM s


-- | Extract a type from the graph and pack it into standard form.
--	BUGS: we need to add fetters from higher up which are acting on this type.

extractType :: Var -> SquidM (Maybe Type)
extractType varT
 = do	mCid	<- lookupVarToClassId varT

	case mCid of
	 Nothing	
	  -> do	graph			<- gets stateGraph
	  	let varToClassId	=  graphVarToClassId graph
	  	freakout stage
		 	("extractType: no classId defined for variable " % (varT, Var.bind varT)		% "\n"
			% " visible vars = " % (map (\v -> (v, Var.bind v)) $ Map.keys varToClassId)		% "\n")
			$ return Nothing

	 Just cid	-> extractTypeC varT cid
	 
extractTypeC varT cid
 = do 	tTrace		<- liftM sortFsT 	$ traceType cid

	trace	$ "*** Scheme.extractType " % varT % "\n"
		% "\n"
		% "    tTrace           =\n" %> prettyTS tTrace	% "\n\n"

	-- Check if the data portion of the type is graphical.
	--	If it is then it'll hang packType when it tries to construct an infinite type.
	let cidsDataLoop	= checkGraphicalDataT tTrace
	trace	$ "    cidsDataLoop     = " % cidsDataLoop % "\n\n"

	
	if (isNil cidsDataLoop)
	 -- no graphical data, ok to continue.
	 then extractTypeC2 varT cid tTrace

	 -- we've got graphical data, add an error to the solver state and bail out.
	 else do
	 	addErrors [ErrorInfiniteTypeClassId {
	 			eClassId	= head cidsDataLoop }]

		return $ Just $ TError KData [tTrace]

extractTypeC2 varT cid tTrace
 = do
	-- Cut loops through the effect and closure portions of this type
	let tCutLoops	= cutLoopsT tTrace
	trace	$ "    tCutLoops        =\n" %> prettyTS tCutLoops % "\n\n"

	-- Pack type into standard form
	let tPack	= packType tCutLoops
	trace	$ "    tPack            =\n" %> prettyTS tPack % "\n\n"

	-- Trim closures
	let tTrim	= packType $ trimClosureT tPack
	trace	$ "    tTrim            =\n" %> prettyTS tTrim % "\n\n"

	-- Reduce context
	classInst	<- gets stateClassInst
	let tReduced	= reduceContextT classInst tTrim
	trace	$ "    tReduced         =\n" %> prettyTS tReduced % "\n\n"
	

	return	$ Just tReduced
	


-- | Generalise a type
--
generaliseType
	:: Var 
	-> Type
	-> [ClassId]
	-> SquidM Type

generaliseType varT tCore envCids
 = do
	args			<- gets stateArgs
	trace	$ "*** Scheme.generaliseType " % varT % "\n"
		% "\n"
		% "    tCore\n"
		%> prettyTS tCore	% "\n\n"

		% "    envCids          = " % envCids		% "\n"
		% "\n"

	-- work out what effect and closure vars are in contra-variant branches
	let contraTs	= catMaybes
			$ map (\t -> case t of
					TClass KEffect cid	-> Just t
					TClass KClosure cid	-> Just t
					_			-> Nothing)
			$ slurpContraClassVarsT tCore
	
	let tPort	= moreifyFettersT (Set.fromList contraTs) tCore
	
	trace	$ "    contraTs = " % contraTs	% "\n"

	trace	$ "    tPort\n"
		%> prettyTS tPort	% "\n\n"


	-- Work out which cids can't be generalised in this type.

	-- 	Can't generalise regions in non-functions.
	--	... some data object is in the same region every time you use it.
	--
	let staticRsData 	= staticRsDataT		tPort
	let staticRsClosure 	= staticRsClosureT	tPort

	trace	$ "    staticRsData     = " % staticRsData	% "\n"
		% "    staticRsClosure  = " % staticRsClosure	% "\n"


	--	Can't generalise cids which are under mutable constructors.
	--	... if we generalise these classes then we could update an object at one 
	--		type and read it at another, violating soundness.
	--	
	let staticDanger	= if Set.member Arg.GenDangerousVars args
					then []
					else dangerousCidsT tPort

	trace	$ "    staticDanger    = " % staticDanger	% "\n"

	-- These are all the cids we can't generalise
	let staticCids		= envCids ++ staticRsData ++ staticRsClosure ++ staticDanger


	-- Rewrite non-static cids to the var for their equivalence class.
	tPlug			<- plugClassIds staticCids tPort

	trace	$ "    staticCids       = " % staticCids	% "\n\n"
		% "    tPlug\n"
		%> prettyTS tPlug 	% "\n\n"

	-- Clean empty effect classes that aren't ports.
	-- 	BUGS: don't clean variables in the type environment.
	--	TODO we have to do a reduceContext again to pick up (Pure TBot) 
	--	.. the TBot won't show up until we do the cleaning. Won't need this 
	--	once we can discharge these during the grind. It's duplicated in extractType above

	classInst	<- gets stateClassInst

	let tClean	= reduceContextT classInst $ cleanType Set.empty tPlug
	trace	$ "    tClean\n" 
			%> ("= " % prettyTS tClean)		% "\n\n"

	-- Mask effects and CMDL constraints on local regions
	-- 	Do this before adding foralls so we don't end up with quantified regions which
	--	aren't present in the type scheme.
	--
	let tMskLocal	= maskLocalT tClean


	trace	$ "    tMskLocal\n"
		%> prettyTS tMskLocal 	% "\n\n"


	-- Quantify free variables.
	let vksFree	= map 	 (\v -> (v, kindOfSpace $ Var.nameSpace v)) 
			$ filter (\v -> not $ Var.nameSpace v == NameValue)
			$ filter (\v -> not $ Var.isCtorName v)
			$ Var.sortForallVars
			$ freeVarsT tMskLocal

	-- Remember which vars are quantified
	--	we can use this information later to clean out non-port effect and closure vars
	--	once the solver is done.
	modify $ \s -> s { stateQuantifiedVars	
				= Set.unions
					[ Set.fromList $ map fst vksFree
					, stateQuantifiedVars s ] }
	
	let tScheme	= makeTForall vksFree tMskLocal
	
	
	trace	$ "    tScheme\n"
		%> prettyTS tScheme 	% "\n\n"

	return	tScheme


{-
	-- Mask effects on local and fresh regions
	let tMskFresh		= maskEsFreshT tMskLocal
	let tMskPure		= maskEsPureT  tMskFresh

	let tPack		= packType tMskPure


	-- Check the scheme against any available type signature.
	schemeSig	<- gets stateSchemeSig
	let mSig	= (Map.lookup varT schemeSig) :: Maybe Type
	let errsSig	= case mSig of
				Nothing		-> []
				Just sig	-> checkSig varT sig varT tPack
	addErrors errsSig
-}
	-- debug

--		% "    staticRsData     = " % staticRsData 	% "\n"
--		% "    staticRsClosure  = " % staticRsClosure	% "\n"
--		% "    staticDanger     = " % staticDanger	% "\n"





-- | Empty effect and closure eq-classes which do not appear in the environment or 
--	a contra-variant position in the type can never be anything but _|_,
--	so we can safely erase them now.
--
--   CHECK:
--	We need to run the cleaner twice to handle types like this:
--		a -(!e1)> b
--		:- !e1 = !{ !e2 .. !en }
--
--	where all of !e1 .. !en are cleanable.
--	Are two passes enough?
--	
cleanType :: Set Var -> Type -> Type
cleanType save tt
	= cleanType' save $ cleanType' save  tt

cleanType' save tt
 = let	vsFree	= freeVarsT tt

	vsPorts	
		= catMaybes
		$ map (\t -> case t of 
			TVar k v	-> Just v
			_		-> Nothing)
		$ portTypesT tt

	vsClean	= [ v 	| v <- vsFree
			, elem (Var.nameSpace v) [Var.NameEffect, Var.NameClosure]
			, not $ Var.isCtorName v 
			, not $ elem v vsPorts 
			, not $ Set.member v save]

	sub	= Map.fromList
		$ map (\v -> (v, TBot (kindOfSpace $ Var.nameSpace v)))
		$ vsClean 

	tClean	= packType 
		$ substituteVT sub tt
	
   in	tClean


-----
-- Collect up the list of cids which cannot be generalise because 
--	they are free in this environment.
collectCidsEnv
	:: Var			-- (type)  var of the scheme being generalised.
	-> [Var] 		-- (value) vars of environment.
	-> SquidM [ClassId]

collectCidsEnv vGenT vsEnv
 = do
	-- Convert environment value vars to type vars.
	vsEnvT_	<- mapM	(\v -> do
			Just t	<- lookupSigmaVar v
			return t)
		$ vsEnv

	-- The binding variable for a let-bound recursive function is present
	--	in the environment of its own generalisation. Don't try and
	--	call findType on ourselves or we'll loop forever.
	--
	let vsEnvT	= vsEnvT_ \\ [vGenT]

	-- Lookup the types of the environment vars.
--	tsEnv		<- mapM findType vsEnvT
--	let tsEnv	= []

	-- All cids in these types are monomorphic.
--	let cidsEnv	= nub $ catMap collectClassIds tsEnv

	return []

