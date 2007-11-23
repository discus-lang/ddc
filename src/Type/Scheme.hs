
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

import Type.Check.CheckSig
import Type.Check.GraphicalData	(checkGraphicalDataT)
-- import Type.Check.Soundness	(dangerousCidsT)
import Type.Closure.Trim 	(trimClosureT)

import Type.Effect.MaskLocal	(maskEsLocalT)
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
stage	= "Type.Squid.Scheme"
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
	
	-- Cut loops through the effect and closure portions of this type
	let tCutLoops	= cutLoopsT tTrace

	trace	$ "    tCutLoops        =\n" %> prettyTS tCutLoops % "\n\n"


	-- Check that the data portion of the type isn't graphical.
	--	If it is then it'll hang packType when it tries to construct an infinite type.
	let cidsDataLoop	= checkGraphicalDataT tCutLoops
	trace	$ "    cidsDataLoop     = " % cidsDataLoop % "\n\n"

	when (not $ isNil cidsDataLoop)
	 $ panic stage	$ "extractType: Cannot construct infinite type for " % varT % ".\n"
	 		% "    this type is graphical through classes " % cidsDataLoop % "\n\n"
	 		% "    " % varT % " :: \n" %> prettyTS tCutLoops % "\n"

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

	-- Force effect and closure vars in contra-variant positions to ports.
	--
	-- Eg: In a type like this:
	--	(a -(!e1)> b) -> (a -(!e1)> b) -> (a -(!e1)> b)
	--
	--	The first two occurances of !e1 are in contra-variant branches and do not represent
	--	represent restrictions placed on what effects can be passed in for the parameters.
	--	We can introduce new variables and rewrite this type to:
	--
	--	(a -(!e2)> b) -> (a -(!e3)> b) -> (a-(!e4)> b)
	--	:- !e4 = !{ !e2; !e3 }
	--

	(tPort, portTable0)
			<- forcePortsT tCore

	let tPortPacked = packType tPort

	-- The force table contains a lists of substitutions for vars from both co-variant
	-- and contra-variant positions.
	--
	-- We'll need this to rewrite the code code for the binding to use the new names
	--	from force-ports above.
	--
	--	BUGS: Also include the original variable (!e1) in the sum if its present in
	--	      the type environment of this binding.
	--	CHECK this against nested lambdas.. scoping.
	--
	portTablePlugged
			<- plugClassIds [] portTable0

	let portTable	= map (\(t1@(TVar k v1), ts)
				-> (v1, makeTSum k ts))
			$ gather 
			$ fst portTablePlugged		-- just the contra-variant substitutions
	
	-- stash the table in the solver state
	modify 	$ \s -> s { statePortTable	= Map.insert varT (Map.fromList portTable)
						$ statePortTable s }
	--
	trace	$ "    tPort\n"
		%> prettyTS tPort	% "\n\n"
		% "    tPortPacked\n"
		%> prettyTS tPortPacked % "\n\n"

		% "    portTable\n"
		%> pretty portTable		% "\n\n"



	-- Work out which cids can't be generalised in this type.

	-- 	Can't generalise regions in non-functions.
	--	... some data object is in the same region every time you use it.
	--
	let staticRsData = staticRsDataT     tPortPacked
	trace	$ "    staticRsData     = " % staticRsData	% "\n"

	-- 	Can't generalise regions free in the closure of the outermost function.
	--	... the objects in the closure are in the same region every time you use the function.
	--
	let staticRsClosure = staticRsClosureT tCore
{-
	--	Can't generalise cids which are under mutable constructors.
	--	... if we generalise these classes then we could update an object at one 
	--		type and read it at another, violating soundness.
	--	
	staticDanger		<- if Set.member Arg.GenDangerousVars args
					then return []
					else dangerousCidsT tCore

	let staticCids		= envCids ++ staticRsData ++ staticRsClosure ++ staticDanger
-}	

	-- These are all the cids we can't generalise
	let staticCids		= envCids ++ staticRsData ++ staticRsClosure


	-- Rewrite non-static cids to the var for their equivalence class.
	tPlug			<- plugClassIds staticCids tPortPacked

	trace	$ "    staticCids       = " % staticCids	% "\n\n"
		% "    tPlug\n"
		%> prettyTS tPlug 	% "\n\n"

	-- Clean empty effect classes that aren't ports.
	-- 	BUGS: don't clean variables in the type environment.
	--	TODO we have to do a reduceContext again to pick up (Pure TBot) 
	--	.. the TBot won't show up until we do the cleaning. Won't need this 
	--	once we can discharge these during the grind. It's duplicated in extractType above

	classInst	<- gets stateClassInst

	let tClean	= reduceContextT classInst $ cleanType tPlug
	trace	$ "    tClean\n" 
			%> ("= " % prettyTS tClean)		% "\n\n"



	-- Mask effects on local regions
	-- 	Do this before adding foralls so we don't end up with quantified regions which
	--	aren't present in the type scheme.
	--
	let tMskLocal	= maskEsLocalT tClean


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
	
	let tScheme	= addTForallVKs vksFree tMskLocal
	
	
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
cleanType :: Type -> Type
cleanType tt
	= cleanType' $ cleanType' tt

cleanType' tt
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
			, not $ elem v vsPorts ]

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

