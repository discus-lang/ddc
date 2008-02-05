
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
stage	= "Type.Squid.CrushProj"
debug	= False
trace s	= when debug $ traceM s

-----
-- |	Crush out field projections from a type equivalence class.
--	Returns: Whether we've managed to crush all the FFieldIs's out of this class.
--	
crushProjClassT ::	ClassId	-> SquidM (Maybe [CTree])
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

	let FProj proj _ (TClass KData cidObj) _	= fProj
	
	-- lookup the node for the object
	Just cObj	<- lookupClass cidObj
	
	crushProjClass2 cidT src fProj cObj 
	

crushProjClass2 cidT src fProj cObj
	| Class { classType 	= Just tObj }<- cObj
	= crushProjClass3 cidT src fProj cObj tObj

	-- if this crush is interleaved with something that's added to this node
	--	then we'll have to wait for it to be unified
	| otherwise
	= do	return Nothing

	
crushProjClass3 cidT src fProj cObj tObj
 = do
 	let FProj proj _ (TClass KData cidObj) _	= fProj

	trace	$ "    cObj type   = " % classType cObj	% "\n"

	-- load the map of projection dictionaries from the state
	projectDicts	<- gets stateProject

	-- if the object type has resolved to a type constructor we should
	--	be able to get the projection dictionary for it.
	let res	
		| TBot{}		<- tObj
		= do	return Nothing

		-- the object is a constructor, but there's no projection dictionary for it.
		| tCon@(TData vCon _)	<- tObj
		, Nothing		<- Map.lookup vCon projectDicts
		= do	addErrors
			  [ErrorNoProjections
				{ eProj		= proj
				, eConstructor	= tCon }]
			return Nothing
	
		-- yay, we've got a projection dictionary
		| TData vCon _		<- tObj
		, Just vsDict		<- Map.lookup vCon projectDicts
		= crushProj2 cidT src fProj cObj tObj (snd vsDict)

		| otherwise
		= panic stage
		$ "crushProjClassT: no match for " % tObj % "\n"
		
	res


crushProj2 
	:: ClassId -> TypeSource -> Fetter
	-> Class   -> Type
	-> Map Var Var 
	-> SquidM (Maybe [CTree])

crushProj2 
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

	let res
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
			let qs	= 	[ CInst (TSI $ SICrushed src fProj) vInst vImplT
					, CEq   (TSI $ SICrushed src fProj) (TVar KData vInst) tBind ]
					 
			trace $ 	"    qs : " %> "\n" %!% qs % "\n"

			-- Add an entry to the projection resolution map.
			--	This information is used in Desugar.ToCore to rewrite the projection
			--	syntax into real function calls.
			modify $ \s -> s { stateProjectResolve 
						= Map.insert vInst vImpl (stateProjectResolve s) }

			-- We can ignore this class from now on.
			delClass cid
			
			return $ Just qs
						
	res

