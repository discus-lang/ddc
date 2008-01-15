{-# OPTIONS -fallow-undecidable-instances #-}

module Source.Desugar.Base
	( Rewrite(..)
	, RewriteM
	, RewriteS(..)
	, Annot
	, none
	, newVarN
	, newVarNS
	, newVarNI
	, getKind)

where

import Util
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.VarBind		as Var
import qualified Shared.VarSpace	as Var
import qualified Shared.Unique		as Unique
import qualified Shared.Var		as Var
import Shared.Var			(Var, NameSpace(..))
import Shared.Base
import Shared.Error
import Type.Exp
		
-----
stage	= "Source.Desugar.Base"
				
-----
type RewriteM	= State RewriteS

data RewriteS
	= RewriteS
	{ stateVarGen	:: Var.VarBind
	, stateKind	:: Map Var Kind }
	
-----
newVarNS :: NameSpace -> String -> RewriteM Var
newVarNS space str
 = do	bind@(Var.XBind unique n) <- gets stateVarGen
	modify $ \s -> s { stateVarGen	= Var.XBind unique (n+1) }

	let var		= (Var.new (Var.namePrefix space ++ pprStr bind ++ str))
			{  Var.bind		= bind
			,  Var.nameSpace	= space }
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
