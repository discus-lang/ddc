
module Type.Crush.Effects
	( crushEffectC )
where

import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import qualified Shared.VarBind	as Var
import Shared.VarPrim
import Shared.Var		(VarBind, NameSpace(..))

import Type.Exp
import Type.Util
import Type.State
import Type.Class
import Type.Feed
import Type.Trace
import Type.Pretty
import Type.Util.Pack

import Type.Crush.Unify

import Type.Plate.Collect	(collectClassIds)
import Type.Plate.Trans

-----
debug	= True
trace s	= when debug $ traceM s
stage	= "Type.Squid.Crush"


crushEffectC :: ClassId -> SquidM ()
crushEffectC cid
 = do	
 	Just c		<- lookupClass cid
	let tNode	=  classType c

 	trace	$ "\n"
		% "*   crushEffectC " 	% cid	% "\n"
		% "    tNode    = " 	% tNode	% "\n"

	tCrushed	<- transformTM crushEffectT tNode

	trace	$ "    tCrushed = "	% tCrushed % "\n"
	
	if tCrushed /= tNode
	 then do	
		-- update the class queue with the new effect
	 	updateClass cid
			c { classType = tCrushed }
			
		-- For the classIds in the new effect, update the backrefs to point
		--	to this class.
		let classIds	= collectClassIds tCrushed
		mapM_ (\cid' -> addBackRef cid' cid) classIds

		-- update the register
		unregisterClass Var.EReadH cid
		registerNodeT cid tCrushed

		return ()

	 else	return ()


crushEffectT :: Effect -> SquidM Effect
crushEffectT tt
	| TEffect ve [t1]	<- tt
	, Var.bind ve == Var.EReadH
	= do
		t2	<- liftM packType $ loadType t1

		trace	$ "    tArg     = " % t2	% "\n"
		case t2 of
		 TData v (tR : ts)	-> return $ TEffect primRead [tR]
		 _			-> return $ tt
	
	| otherwise
	= return $ tt
	

loadType :: Type -> SquidM Type
loadType tt	= transformTM loadType' tt

loadType' tt
 = case tt of
 	TClass k cid	-> traceType cid
	_		-> return tt
	
	 



{-
crushEffectC :: ClassId -> SquidM ()
crushEffectC	cidE
 = do	
 	Just c	<- lookupClass cidE
	
	effs'	<- liftM concat
		$  mapM crushEffect
		$  flattenEssT 
		$  classQueue c 
	
	-- updating doesn't fix the backrefs
	updateClass cidE 
		c { classQueue = [TEffect (ESum effs')] }

	-- For the classIds in the new effect, update the backrefs to point
	--	to this class.
	let classIds	= collectClassIds effs'

	mapM_ (\cid -> addBackRef cid cidE) classIds

--	updateClass cidE
--		c { classQueue = [] }
		
--	let ?typeSource = TSNil
--	mapM_ (feedEffect Nothing) effs'


	updateRegisterE cidE primReadH 
	updateRegisterE cidE primReadT
	updateRegisterE cidE primWriteT
		
	return ()




crushEffect :: Effect -> SquidM [Effect]
crushEffect e
 = do	eRefresh	<- refresh e
 	let mesNew	= reduceEffect eRefresh
	
	trace	$ "*   Crush.CrushEffects.crushEffect\n"
		% "    e        = " % e 	% "\n"
		% "    eRefresh = " % eRefresh 	% "\n"
		% "    eRefresh = " % show eRefresh 	% "\n"
		% "    mesNew   = " % mesNew    % "\n"
		% "\n"

	case mesNew of
		Nothing		-> return [e]
		Just esNew	-> return esNew
		

reduceEffect :: Effect -> Maybe [Effect]
reduceEffect e@(ECon v [t@(TCon vCon ts)])

	-- Read data head.
	--	Some types, like Unit, don't have any region annots so we can just discard the effect.

 	| Var.EReadH			<- Var.bind v
	= case ts of
		tR@(TRegion r) : tsRest	-> Just [ECon primRead [tR]]
		_			-> Just []
		
	-- Read of whole object. (deep read).
	| Var.EReadT			<- Var.bind v
	= let	bits	= catMap reduceDataRT ts
		esRegion	= [ECon primRead    [r] | r@(TRegion{})	<- bits]
		esType		= [ECon primReadT   [t] | t@(TClass{})	<- bits]
	  in	Just (esRegion ++ esType)

	-- Write to whole object. (deep write).
	| Var.EWriteT			<- Var.bind v
	= let	bits	= catMap reduceDataRT ts
		esRegion	= [ECon primWrite    [r] | r@(TRegion{})	<- bits]
		esType		= [ECon primWriteT   [t] | t@(TClass{})	<- bits]
	  in	Just (esRegion ++ esType)
	
	
	| otherwise
	= Nothing
	
reduceEffect e
	= Nothing

	
-----
-- updateRegister 
--
updateRegisterE
	:: ClassId 
	-> Var
	-> SquidM ()

updateRegisterE cidE var
 = do
 	gotCon	<- classHasECon cidE var
	when (gotCon == False)
	 $ unregisterClass (Var.bind var) cidE
 	

classHasECon ::	ClassId -> Var -> SquidM Bool
classHasECon	cidE var
 = do 	
 	Just c	<- lookupClass cidE
	return	$ or 
		$ map	(\eff -> case eff of
					ECon v _ -> v == var
					_	 -> False)
		$ flattenEssT
		$ classQueue c
		
	
-}
