
module Core.Util.Effect
--	( crushEffs
--	, sumEffs )
where


import Util
import Core.Exp

{-	
-----
-- crushEffs
--
crushEffs :: Effect -> [Effect]
crushEffs ee
 = case ee of
	TPure		-> []
	TSum KEffect es	-> catMap crushEffs es
	_		-> [ee]

sumEffs	:: [Effect]	-> Effect
sumEffs	es
 = case catMap crushEffs es of
 	[]	-> TPure
	es	-> TSum KEffect es


-}
