
module Type.Check.Main
	( checkMain )

where

import Type.Extract
import Type.Exp
import Type.State
import Type.Error
import Type.Class
import Type.Base

import qualified Shared.Var	as Var
import Shared.VarPrim

import qualified Data.Map	as Map

import Util

-- | If the graph contains a function called 'main' then check 
--	that is has the appropriate type, () -> ()
checkMain :: SquidM ()
checkMain
 = do	sigmaTable	<- gets stateSigmaTable
 
	-- try and find an entry for the main variable in the sigma table
	--	this will tell us if we've seen the binding for it or not
 	let mMain	= find (\(v, t) -> isMainVar v)
			$ Map.toList sigmaTable
 
 	case mMain of
	 Nothing		-> return ()
	 Just (vMain, vMainT)	
	  -> do	Just tMain	<- extractType True vMainT
	  	checkMain' vMainT tMain tMain
	  
checkMain' vMainT tMain tt
 = case tt of
 	TFetters fs t	
	 -> checkMain' vMainT tMain t

	TFun (TData _ v1 []) (TData _ v2 []) eff clo
	 | v1 == primTUnit && v2 == primTUnit
	 -> return ()
	 
	_ -> addErrors [ErrorWrongMainType { eScheme = (vMainT, tMain) }]
			
isMainVar var
	| Var.name var		== "main"
	, Var.nameModule var	== Var.ModuleAbsolute ["Main"]
	= True
	
	| otherwise
	= False
