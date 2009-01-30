
module Churn.Bits
where

import Type.Exp
import Source.Util
import Source.Exp
import Shared.VarPrim

import qualified Shared.Var	as Var
import Shared.VarPrim
import Shared.VarSpace
import Shared.Base
import Shared.Pretty
import Util

varV s		= (Var.new s) 
			{ Var.nameSpace = NameValue 
			, Var.bind	= Var.XBind s 0 }
			 
xVar v		= XVar none (varV v)
none		= TNil

-- | Split this int into several parts proportional to the values in the list
--	eg drain 100 [1, 2, 2]  => [20, 40, 40]
--
--	The result list should sum to the input value.
--
drain :: Int -> [Int] -> [Int]
drain fuel split
 = let	total			= sum split
	parts@(p1:pRest)	= map (\p -> (p * fuel) `div` total) split
	slack			= fuel - sum parts
   in	p1 + slack : pRest
	
	
makeCall 	= unflattenApps TNil

pprProg prog
	= catInt "\n" $ map pprStrPlain prog
	
	