
module Type.Crush.Proj
--	( crushProjClassT )
	()

where

import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

import Shared.Error
import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))
import Shared.VarPrim

import Type.Exp
import Type.Util
import Type.Error
import Type.Plate.Trans

import Type.Class
import Type.State
import Type.Scheme
import Type.Feed

-----
stage	= "Type.Squid.CrushProj"
debug	= True
trace s	= when debug $ traceM s

{-
-----
-- crushProjClassT
-- |	Crush out field projections from a type equivalence class.
--	Returns: Whether we've managed to crush all the FFieldIs's out of this class.
--	
crushProjClassT ::	ClassId	-> SquidM Bool
crushProjClassT cidT
 = do
 	Just c		<- lookupClass cidT

	
	trace	$ "*   Proj.crushProjClassT\n"
		% "    cidT        = " % cidT		% "\n"
		% "    queue       = " % classQueue c	% "\n"
		% "\n"

	fetters'	<- liftM catMaybes
			$  mapM (crushFieldIs cidT c) 
			$  map (\(TFetter f) -> f)
			$  classQueue c
			
	trace	$ "    fetters'    = " % fetters'		% "\n"
		% "\n"


	-- Re-lookup the class and update the list of fetters.
	--	It's likely to have changed because we've added 
	--	constraints from projections to it.
	--
	Just c2		<- lookupClass cidT
	updateClass cidT 
		c2 { classQueue = [TFetter f | f <- fetters'] }


	-- If the new list of fetters contains no more FFieldIs's 
	--	then remove this class from that register.
	--
	if (length [f  | f@FProj{} <- fetters'] == 0)
	 then do
	 	unregisterClass Var.FProj cidT 
		return True
	
 	 else	return False

	
-----------------------
crushFieldIs
 	:: ClassId 
	-> Class
	-> Fetter
	-> SquidM (Maybe Fetter)
	
crushFieldIs cidProj cProj
	f@(FProj proj _ (TClass cidObj) (TClass cid2) eff clo)
 = do
 	Just cObj	<- lookupClass cidObj

	trace	$ "*   Proj.crushFieldIs " 	% cidProj % "\n"
		% "    f           = " 		% f % "\n"
		% "    cObj queue  = " 		% classQueue cObj % "\n"
		% "    cObj queue  = " 		% show (classQueue cObj) % "\n"
		% "\n"

	crushFieldIs2 cidProj cProj cObj f


crushFieldIs2 cidProj cProj cObj
	f@(FProj proj _ (TClass cidObj) (TClass cid2) eff@(EClass cidE) clo@(CClass cidC))
	
	-- The class for the object has a TCon in it.
	| [tCon@(TCon v ts)]		<- classQueue cObj
	= do
		-- Lookup the list of projections for this type.
		mImpl	<- liftM (Map.lookup v)
			$  gets stateProject
			
		case mImpl of
		 Just (t, varImpl)	-> crushFieldIs3 cidProj cProj cObj f t varImpl

		 -- Couldn't find any projection for this type.
		 Nothing		
		  -> do	addErrors
		  		[ErrorNoProjections
				{ eProj		= proj
				, eConstructor	= tCon }]

			return Nothing
		 
crushFieldIs2 cid1 c1 c2 f
 = return $ Just f


crushFieldIs3 cidProj cProj cObj
	f@(FProj proj _ (TClass cidObj) (TClass cid2) eff@(EClass cidE) clo@(CClass cidC))
	t varImpl
 = do
	trace	$ "    t           = " % t 		% "\n"
		% "    varImpl     = " % [ (v, Var.nameSpace v, vI) | (v, vI) <- varImpl]	% "\n"

	-- Lookup the implementation var for the projection.
	let projName	= case proj of
				TJField  v	-> Var.name v
				TJFieldR v	-> "ref_" ++ Var.name v

	let mVarI	= find (\(v, _) -> Var.name v == projName) varImpl

	case mVarI of
	 Just (_, varI)	-> crushFieldIs4 cidProj cProj cObj f t varImpl varI
	 
	 -- Couldn't find the requested field in the projection dictionary.
	 Nothing 
	  -> do	addErrors
	  		[ErrorFieldNotPresent
			{ eProj		= proj
			, eConstructor	= t }]
			
		return Nothing


crushFieldIs4 cidProj cProj cObj
	f@(FProj proj (TVar varProjFunInst) (TClass cidObj) (TClass cid2) eff@(EClass cidE) clo@(CClass cidC))
	t varImpl varI
 = do
 	-- Lookup the scheme for the impl var.
	Just varT	<- lookupSigmaVar varI
	
	mScheme		<- lookupScheme varT
	let scheme	= case mScheme of
				Nothing		-> panic stage $ "crushFieldIs4: no scheme for '" % varT % "'"
				Just scheme	-> scheme

	-- Instantiate the scheme for the projection function.
	--	Also record how it was instantiated so we can supply the correct TREC args
	--	to the projection function once it's converted to core.
	--
	(inst, instVs)	<- instantiateT_table instVar scheme
	sInst 		<##> Map.insert varProjFunInst instVs

	-- Add the constraint from the projection function to the graph
	--
	let ?typeSource	= TSProj cidObj cid2 proj
	TClass cidT	<- feedType (Just cidProj) inst


	-- Now that the constraints for the projection funtion are in the graph,
	--	lookup the node for the top level application.
	--
	Just projC	<- lookupClass cidT 
	let [projFun@(TFun (TClass cidT1) (TClass cidT2) eff2@(EClass cidE2) clo2@(CClass cidC2))]	
			= classQueue projC

	-- Merge the classes from the projection fun with the ones the fetter has.
	--	This forces the object and result to have the types required by the projection.
	--
	--	Also merge the effect and closure classes from the projection application with the
	--	classes from the fetter. This ensures that any effect and closure information is also
	--	propagated into the function which performed the projection.
	--
	mergeClasses [cidT1, cidObj]
	mergeClasses [cidT2, cid2]
	mergeClasses [cidE,  cidE2]
	mergeClasses [cidC,  cidC2]


	trace	$ "    varI        = " % varI		% "\n"
		% "    scheme      = " % scheme		% "\n"
		% "    inst        = " % inst		% "\n"
		% "    cidT        = " % cidT		% "\n"
		% "    projFun     = " % projFun	% "\n"
		% "\n"



	return Nothing


{-
crushFieldIs' cidProj cProj c2
	f@(FProj (TClass cid1) (TClass cid2) proj)
	
 =	case classQueue c2 of

	 -- The class for the object has a TCon in it.
	 tCon@(TCon v ts@(TRegion r : _)) : _
	  -> do	
		-- Lookup the projection for this type.
		Just (t, varImpl)
			<- liftM (Map.lookup v)
			$  gets stateProject
		
			
		-- Lookup the list of fields for this data type.
		Just (vs, fields)
			<- liftM (Map.lookup v)
			$  gets stateDataFields

		-- Lookup the type for the requested projection.
		case makeProjType proj tCon fields r of
		 Just projType	
		  -> do 
			-- Instantiate the field type with the same arguments
			--	as the constructor.
			--
			let instTable	= zip vs ts
			projTypeI	<- transZM 
						transTableId 
							{ transT	= \t -> return $ instFieldT instTable t
							, transE	= \e -> return $ instFieldE instTable e
							, transC	= \c -> return $ instFieldC instTable c }
						projType

			-- Feed the new field type into the graph.
			let ?typeSource	= TSProj cid1 cid2 proj
			TClass cidP 	<- feedType Nothing projTypeI
			mergeClasses [cidP, cid1]

		  	trace	$ "    t           = " % t 		% "\n"
				% "    varImpl     = " % varImpl	% "\n"
				% "----\n"
				% "    v           = " % tCon		% "\n"
				% "    fields      = " % fields		% "\n"
				% "    projType    = " % projType	% "\n"
				% "    instTable   = " % show instTable	% "\n"
				% "    projTypeI   = " % show projTypeI	% "\n"
				% "    merge       = " % [cidP, cid1]	% "\n"

			return Nothing

		 -- No projection of this name for that constructor.
		 --	Add the error and return Nothing, so the FieldIs fetter is cleared.
		 Nothing
		  -> do	addErrors
		  		[ErrorFieldNotPresent
				{ eProj		= proj
				, eConstructor	= tCon }]

			return Nothing				


	-- The object class is still empty.
	 _ ->	return	$ Just f



 
 
makeProjType proj tCon fields r

 	| TJField  v	<- proj
	, Just t	<- lookup v fields
	= Just t
	
	| TJFieldR v	<- proj
	, Just t	<- lookup v fields
	= Just $ TCon primTRef [TRegion r, t]
	
	| otherwise
	= Nothing
	 

instFieldT table t
	| TRegion (RVar v)	<- t
	, Just t'		<- lookup v table
	= t'
	 
	| TEffect (EVar v)	<- t
	, Just t'		<- lookup v table
	= t'

	| TVar v		<- t
	, Just t'		<- lookup v table
	= t'
	
	| otherwise
	= t
	

instFieldE table ee
 	| EVar v		<- ee
	, Just (TEffect e')	<- lookup v table
	= e'
	
	| otherwise
	= ee
	

instFieldC table cc
	| CVar v		<- cc
	, Just (TClosure c')	<- lookup v table
	= c'
	
	| otherwise
	= cc
	
 
-}
-}
