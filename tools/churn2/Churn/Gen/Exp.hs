
module Churn.Gen.Exp
where

import Churn.Gen.Base
import Churn.Bits
import Churn.Type

import Shared.Base
import Source.Exp
import Type.Exp


-- | Generate a literal integer.
genExpInt :: GenM (Exp Type)
genExpInt
 = do	n	<- genRandomR (0, 100)
	return	$ XLit tInt (LiteralFmt (LInt n) Boxed)

