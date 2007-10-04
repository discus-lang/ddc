{-# OPTIONS -fallow-undecidable-instances #-}

module Source.Desugar.Base
	( Rewrite(..)
	, RewriteM
	, RewriteS(..)
	, Annot
	, none
	, initRewriteS
	, newVarN
	, newVarNS
	, newVarNI
	, getKind)

where

import Util
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.VarBind	as Var
import qualified Shared.Unique	as Unique
import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))
import Shared.Base
import Shared.Error
import Type.Exp
		
-----
stage	= "Source.Desugar.Base"
				
-----
type RewriteM	= State RewriteS

data RewriteS
	= RewriteS
	{ stateVarGen	:: Map NameSpace Var.VarBind
	, stateKind	:: Map Var Kind }
	
initRewriteS
	= RewriteS
	{ stateVarGen	
		= Map.insert NameValue 	 (Var.XBind ("v" ++ Unique.sourceDesugar) 0)
		$ Map.insert NameType	 (Var.XBind ("t" ++ Unique.sourceDesugar) 0)
		$ Map.insert NameRegion	 (Var.XBind ("r" ++ Unique.sourceDesugar) 0)
		$ Map.insert NameEffect	 (Var.XBind ("e" ++ Unique.sourceDesugar) 0)
		$ Map.insert NameClosure (Var.XBind ("c" ++ Unique.sourceDesugar) 0)
		$ Map.empty 
		
	, stateKind
		= Map.empty 
	}
		

-----
newVarNS :: NameSpace -> String -> RewriteM Var
newVarNS space str
 = do	varGen		<- gets stateVarGen
 	let Just gen	= Map.lookup space varGen
	let gen'	= Var.incVarBind gen
	
	let var		= (Var.new (pretty gen ++ str))
			{  Var.bind		= gen
			,  Var.nameSpace	= space }
			
	modify (\s -> s
		{ stateVarGen	= Map.insert space gen' varGen })
		
	return var


newVarN space 	= newVarNS space ""
	

-----
newVarNI 
	:: NameSpace -> [Var.VarInfo]	
	-> RewriteM Var

newVarNI space info
 = do 	var	<- newVarN space 
	return	var { Var.info = info }


-----
getKind :: Var -> RewriteM Kind
getKind	v
 = do	kindMap		<- gets stateKind
	let k		= case Map.lookup v kindMap of
				Nothing	-> panic stage $ "getKind: no kind for '" % v % "'.\n"
				Just k'	-> k'
	return k


-----
class Rewrite a b | a -> b where
 rewrite :: a -> RewriteM b
  
instance Rewrite a b => Rewrite [a] [b] where
 rewrite xx	= mapM rewrite xx
 
instance Rewrite a b => Rewrite (Maybe a) (Maybe b) where
 rewrite xx
  = case xx of
  	Nothing	
	 -> 	return	Nothing

	Just x	
	 -> do	x'	<- rewrite x
		return	$ Just x'

-----
type	Annot	= SourcePos
none		= NoSourcePos
