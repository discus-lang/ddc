
module Core.Util.Beta
	( betaTree )
where
import Core.Exp
import Core.Crush
import Core.Plate.Trans
import Type.Exp		
import Util
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set


data BetaS 
 	= BetaS 
 	{ sBoundT	:: Map Var Type
	, sBoundX	:: Map Var Exp }
		
type BetaM = State BetaS
	
bindValue :: Var -> Type -> Exp -> BetaM ()
bindValue v k x
 	= modify (\s -> s { sBoundX = Map.insert v x (sBoundX s) })


-----
betaTree :: Tree -> Tree
betaTree tree
	= crushTree
	$ cleanTetTree
	$ evalState 
		(mapM betaP tree) 
		BetaS 	{ sBoundT	= Map.empty
			, sBoundX	= Map.empty }


-----
betaP :: Top -> BetaM Top
betaP (PBind v x)
 = do	x'	<- betaXM x
 	return	$ PBind v x'
	
betaP top
 =	return top


betaXM :: Exp -> BetaM Exp

betaXM x
 = transZM
	transTableId 
	{ transX_enter 	= betaX 
	, transT	= betaT }
	x
		


betaX :: Exp -> BetaM Exp

betaX (XAPP (XLAM b t1 x) t)
 = do	return	$ x
	
betaX (XApp (XLam v t x1 eff1 clo) x2 eff2)
 = do	bindValue v t x2
 	return	$ x1

betaX x@(XVar v t)
 = do	boundX	<- gets sBoundX
 	case Map.lookup v boundX of
	 Nothing	-> return x
	 Just x'	-> return x'
	
betaX x	
 = 	return x


betaT :: Type -> BetaM Type
betaT t
 	| TVar k v	<- t
 	= do	boundT	<- gets sBoundT
		case Map.lookup v boundT of
		 Nothing	-> return t
		 Just t'	-> return t'

	| otherwise
	= 	return t


-----
-- Hacks: clean out all Tets from the tree
--
cleanTetTree  :: Tree -> Tree
cleanTetTree tree
 = evalState
 	(transZM
 		transTableId
		{ transV_bind	= cleanTetV_bind
		, transX	= cleanTetX }
				
		tree)
	Set.empty
	
cleanTetV_bind v
 = do	modify (Set.insert v)
 	return v
	
cleanTetX x
 =	return x



