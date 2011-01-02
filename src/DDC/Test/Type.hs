
-- | This module is NOT to be used DDC proper -- for unit testing only.
--   It uses `unsafePerformIO` under the covers to generate fresh variables.
--
--   Commonly used types.
--
module DDC.Test.Type
	( module DDC.Type
	, vInt
	, tFun )
where
import DDC.Type
import DDC.Base.DataFormat
import Shared.VarPrim

vInt	= primTInt Boxed
tFun 	= makeTFun
