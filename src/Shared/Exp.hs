
module Shared.Exp
(
	DataField (..)
)

where

import Util
import Shared.Var

data DataField x t
	= DataField
	{ dPrimary	:: Bool
	, dLabel	:: Maybe Var
	, dType		:: t
	, dInit		:: Maybe x }
	deriving (Show, Eq)
