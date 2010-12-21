
-- | This module is NOT to be used DDC proper -- for unit testing only.
--   It uses `unsafePerformIO` under the covers to generate fresh variables.
--
--   Commonly used types.
--
module DDC.Test.Type
	( module DDC.Type
	, vInt, tInt
	, tFun )
where
import DDC.Test.Var
import DDC.Type


vInt	= varT "Int"
tInt tR	= makeTData vInt (makeKFuns [kRegion] kValue) [tR]

tFun 	= makeTFun
