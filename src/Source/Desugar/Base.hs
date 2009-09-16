{-# LANGUAGE UndecidableInstances #-}

module Source.Desugar.Base
	( Rewrite(..)
	, RewriteM
	, RewriteS(..)
	, Annot
	, newVarN
	, newVarNS
	, newVarNI
--	, getKind
	, addError)

where

import Type.Exp
import Source.Error

import qualified Shared.VarBind		as Var
import qualified Shared.Var		as Var
import Shared.Var			(Var, NameSpace(..))
import Shared.Base
import Shared.Error
import Shared.Pretty

import qualified DDC.Base.NameSpace	as Var

import qualified Data.Map	as Map
import Data.Map			(Map)
import Util
		
-----
-- stage	= "Source.Desugar.Base"
				
-----
type Annot	= SourcePos
type RewriteM	= State RewriteS

data RewriteS
	= RewriteS
	{ stateVarGen	:: Var.VarBind
	, stateErrors	:: [Error] }

	
-- | Make a new variable in this namespace and name it after a string.
newVarNS :: NameSpace -> String -> RewriteM Var
newVarNS space str
 = do	bind@(Var.XBind unique n) <- gets stateVarGen
	modify $ \s -> s { stateVarGen	= Var.XBind unique (n+1) }

	let var		= (Var.new (Var.namePrefix space ++ pprStrPlain bind ++ str))
			{  Var.bind		= bind
			,  Var.nameSpace	= space }
	return var


-- | Make a new variable in this namespace.
newVarN space 	= newVarNS space ""

	
-- | Make a new variable in this namespace with some info attached to the var.
newVarNI :: NameSpace -> [Var.VarInfo]	-> RewriteM Var
newVarNI space info
 = do 	var	<- newVarN space 
	return	var { Var.info = info }


-- | Get the kind for this var from the kind table.
{-
getKind :: Var -> RewriteM Kind
getKind	v
 = do	kindMap		<- gets stateKind
	let k		= case Map.lookup v kindMap of
				Nothing	-> panic stage $ "getKind: no kind for '" % v % "'.\n"
				Just k'	-> k'
	return k
-}

-- | Add an error to the rewrite state.
addError :: Error -> RewriteM ()
addError err
 	= modify $ \s -> s { stateErrors = err : stateErrors s }


-- Simple Rewrite Instanes ------------------------------------------------------------------------
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


