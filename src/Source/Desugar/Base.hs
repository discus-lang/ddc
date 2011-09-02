{-# LANGUAGE UndecidableInstances #-}

module Source.Desugar.Base
	( Rewrite(..)
	, RewriteM
	, RewriteS(..)
	, Annot
	, newVarN
	, newVarNS
	, newVarNI
	, addError)
where
import DDC.Source.Error
import DDC.Base.SourcePos
import DDC.Main.Pretty
import DDC.Var
import Util

type Annot	= SourcePos
type RewriteM	= State RewriteS

data RewriteS
	= RewriteS
	{ stateVarGen	:: VarId
	, stateErrors	:: [Error] }


-- | Make a new variable in this namespace and name it after a string.
newVarNS :: NameSpace -> String -> RewriteM Var
newVarNS space str
 = do	bind@(VarId unique n) <- gets stateVarGen
	modify $ \s -> s { stateVarGen	= VarId unique (n+1) }

	let var		= (varWithName (charPrefixOfSpace space : pprStrPlain bind ++ str))
			{  varId	= bind
			,  varNameSpace	= space }
	return var


-- | Make a new variable in this namespace.
newVarN space 	= newVarNS space ""


-- | Make a new variable in this namespace with some info attached to the var.
newVarNI :: NameSpace -> [VarInfo]	-> RewriteM Var
newVarNI space info
 = do 	var	<- newVarN space
	return	var { varInfo = info }


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


