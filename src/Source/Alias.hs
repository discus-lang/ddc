
module Source.Alias
(
	aliasTree
)

where

import Util

import qualified Shared.Unique	as Unique
import qualified Shared.Var	as Var
import Shared.Var		(Var, VarBind, NameSpace(..))

import Shared.VarUtil

import Source.Exp
import Source.Plate.Trans


type AliasM	= State VarBind


aliasTree ::	Tree -> Tree
aliasTree	ps
 = 	evalState 
 		(aliasTreeM ps) 
		(Var.XBind Unique.sourceAliasX 0)
 


aliasTreeM :: 	Tree	-> AliasM Tree
aliasTreeM	ps
 = do
	ps2	<- mapM aliasP ps
	return	ps2


aliasP p
 = case p of
 	PClassInst v ts inh ss
	 -> do
	 	ss'	<- mapM aliasS ss
		return	$  PClassInst v ts inh ss'

	_ -> return p


aliasS s
 = case s of
{- 	SBind sp (Just v) x
	 -> do
	 	v_	<- newVarN NameValue
		let v'	= v_ 
			{ Var.name	= (Var.name v) ++ "_" ++ (Var.name v_)
			, Var.info	=  Var.info v 
					++ [Var.IAlias v] }

		x'	<- transformVM 
				(\var -> if var == v 
						then return v'
						else return var)
				x

		return	$ SBind sp (Just v') x'
-} 	
	_ -> return s



