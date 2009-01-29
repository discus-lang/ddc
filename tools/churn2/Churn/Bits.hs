
module Churn.Bits
where

import Shared.Var	as Var

varV s		= (Var.new s) { Var.nameSpace = NameValue }

drain :: Int -> [Int] -> [Int]
drain fuel split
 = let	total			= sum split
	parts@(p1:pRest)	= map (\p -> (p * fuel) `div` total) split
	slack			= fuel - sum parts
   in	p1 + slack : pRest
	