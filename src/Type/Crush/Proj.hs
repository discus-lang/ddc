
-- | Resolve projection constraints
module Type.Crush.Proj
	( crushProjClassT )
where

import Type.Location
import Type.Exp
import Type.Util
import Type.Error
import Type.Plate.Trans
import Type.Class
import Type.State
import Type.Scheme
import Type.Dump
import Type.Crush.Unify

import Constraint.Exp
import Constraint.Pretty

import Shared.Error
import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))
import Shared.VarPrim

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

import Util

-----
stage	= "Type.Crush.Proj"
debug	= False
trace s	= when debug $ traceM s

-- | Crush out field projections from an equivalence class.
--   returns: Whether we've managed to crush all the FFieldIs's out of this class.
--	
crushProjClassT :: ClassId -> SquidM (Maybe [CTree])
crushProjClassT cidT
 = do	 
 	-- check for errors
 	errs	<- gets stateErrors
	when (not $ isNil errs)
	 $ panic stage	$ "crushProjClassT: state has errors\n"
	 		% errs
  
	-- lookup the fetter from the node and unpack it.
 	Just cProj@(ClassFetter 
			{ classFetter = fProj 
			, classSource	= src })
				<- lookupClass cidT

	trace	$ "*   Proj.crushProjClassT\n"
		% "    cidT        = " % cidT		% "\n"
		% "    fetter      = " % fProj		% "\n"

	let FProj _ _ (TClass _ cidObj) _	
			= fProj
	
	-- find the class containing the tycon of the object type 
	--	being projected.
	mClsObj		<- classDownLeftSpine cidObj
	
	case mClsObj of
	 Just cObj@(Class { classType = Nothing })
	  -> return Nothing

	 Just cObj@(Class { classType = Just tObj })
	  -> crushProj_withObj cidT src fProj cObj tObj
	
crushProj_withObj cidT src fProj cObj tObj
 = do
	trace	$ "    cObj type   = " % classType cObj	% "\n"

 	let FProj proj _ (TClass _ _) _	
			= fProj

	-- load the map of projection dictionaries from the state
	projectDicts	<- gets stateProject

	-- if the object type has resolved to a type constructor we should
	--	be able to get the projection dictionary for it.
	let result
		-- This isn't a type constructor,
		--	hopefully something will be unified into it later
		| TBot{}			<- tObj
		= do	return Nothing

		-- the object is a constructor, but there's no projection dictionary for it.
		| Just (vCon, _, _)	<- takeTData tObj
		, Nothing		<- Map.lookup vCon projectDicts
		= do	addErrors
			  [ErrorNoProjections
				{ eProj		= proj
				, eConstructor	= tObj }]
			return Nothing
	
		-- yay, we've got a projection dictionary
		| Just (vCon, _, _)	<- takeTData tObj
		, Just vsDict		<- Map.lookup vCon projectDicts
		= crushProj_withDict cidT src fProj cObj tObj (snd vsDict)

		-- functions don't have projections (not yet)
		| Just _		<- takeTFun tObj
		= do	addErrors
			 [ErrorNoProjections
			 	{ eProj		= proj
				, eConstructor	= tObj }]
			return Nothing

		| otherwise
		= panic stage
		$ "crushProjClassT: no match for " % tObj % "\n"
		
	result


crushProj_withDict
	:: ClassId -> TypeSource -> Fetter
	-> Class   -> Type
	-> Map Var Var 
	-> SquidM (Maybe [CTree])

crushProj_withDict
	cid src
	fProj@(FProj proj vInst tObj tBind)
	cObj tObjCon vsDict
 = do
	-- Extract the name of the projection we're looking for
 	let projName	= case proj of 
				TJField v	-> Var.name v
				TJFieldR v	-> "ref_" ++ Var.name v

	-- Try and look up the var of the implementation function.
	--	We must use field _names_, not the Var.bind for comparison because the
	--	renamer can't have known which type this field belongs to.
	let mImpl	=  liftM snd
			$  find (\(v1, _) -> Var.name v1 == projName)
			$  Map.toList vsDict

	trace	$ "    mImpl       = " % mImpl		% "\n"

	let result
		-- There might not be an entry in the projection dictionary for this field.
		| Nothing	<- mImpl
		= do	addErrors
		  		[ErrorFieldNotPresent
				{ eProj		= proj
				, eConstructor	= tObjCon
				, eFields	= Map.keys vsDict }]
				
			return Nothing
			
		-- We've got the name of the projection function
		| Just vImpl	<- mImpl
		= do	
		 	-- Lookup the type variable corresponding to it.
			Just vImplT	<- lookupSigmaVar vImpl

			-- Build the new constraints
			let qs	= 	[ CInst (TSI $ SICrushedFS cid fProj src) vInst vImplT
					, CEq   (TSI $ SICrushedFS cid fProj src) (TVar kValue vInst) tBind ]
					 
			trace $ 	"    qs : " %> "\n" %!% qs % "\n"

			-- Add an entry to the projection resolution map.
			--	This information is used in Desugar.ToCore to rewrite the projection
			--	syntax into real function calls.
			modify $ \s -> s { stateProjectResolve 
						= Map.insert vInst vImpl (stateProjectResolve s) }

			-- We can ignore this class from now on.
			delClass cid
			
			return $ Just qs					
	result


-- | Walk down the left spine of this type to find the type in the bottom 
--	left node (if there is one)
--
--	For example, if the graph holds a type like:
--	   TApp (TApp (TCon tc) t1) t2
--	
--	Then starting from the cid of the outermost TApp, we'll walk down 
--	the left spine until we find (TCon tc), and return that.
--
--	If the node at the bottom of the spine hasn't been unified, then
--	It'll be a Nothing, so return that instead.
--
classDownLeftSpine :: ClassId -> SquidM (Maybe Class)
classDownLeftSpine cid
 = do	Just cls	<- lookupClass cid
	case classType cls of
	 Just (TApp (TClass _ cid1) _)	
		-> classDownLeftSpine cid1
	 mType	-> return $ Just cls

