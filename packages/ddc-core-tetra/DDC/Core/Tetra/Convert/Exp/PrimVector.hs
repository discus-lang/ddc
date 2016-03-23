
module DDC.Core.Tetra.Convert.Exp.PrimVector
        (convertPrimVector)
where
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Exp
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Name      as A


convertPrimVector
        :: Show a
        => ExpContext                   -- ^ The surrounding expression context.
        -> Context a                    -- ^ Types and values in the environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> Maybe (ConvertM a (Exp a A.Name))

convertPrimVector _ectx _ctx _xx
 = Nothing -- error "convertPrimVector: not finished"
{-
   in case xx of

        XApp a _ _
         | Just ( E.NameOpVector E.OpVectorAlloc
                , [XType _ tR, XType _ tA, xLength])    <- takeXPrimApps xx
         , isU
-}