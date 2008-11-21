{-# OPTIONS -fwarn-incomplete-patterns #-}

module Desugar.Plate.FreeVars
	(freeVars)

where

import qualified Data.Set as Set
import Data.Set	(Set, (\\), unions, fromList, empty, singleton)

import Util	hiding ((\\))

import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))
import Shared.Error
import Shared.FreeVars

import Desugar.Exp
import Type.Util.Bits		(varOfBind)
import Type.Pretty

-----
-- stage	= "Desugar.Plate.FreeVars"

 
-- Exp ---------------------------------------------------------------------------------------------
instance FreeVars (Exp a) where
 freeVars xx
  = case xx of
	XNil		-> empty

	XVoid{}		-> empty

	XLit{}		-> empty

	_		-> empty