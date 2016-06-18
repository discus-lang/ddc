
module DDC.Core.Tetra.Convert.Exp.PrimError
        (convertPrimError)
where
import DDC.Core.Tetra.Convert.Exp.Base
import DDC.Core.Tetra.Convert.Error

import DDC.Core.Exp.Annot
import DDC.Core.Check                    (AnTEC(..))

import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Core.Salt.Name      as A
import qualified DDC.Core.Salt.Runtime   as A


-- | Covnert a Tetra error primop to Salt.
convertPrimError
        :: ExpContext                   -- ^ The surrounding expression context.
        -> Context a                    -- ^ Types and values in the environment.
        -> Exp (AnTEC a E.Name) E.Name  -- ^ Expression to convert.
        -> Maybe (ConvertM a (Exp a A.Name))

convertPrimError _ectx ctx xx
 = let  convertX  = contextConvertExp ctx
        downArgX  = convertX ExpArg   ctx
   in   
        case xx of
        XApp a _ _
         | Just ( E.NameOpError E.OpErrorDefault True
                , [_, xStr, xLine]) <- takeXPrimApps xx
         -> Just $ do
                xStr'   <- downArgX xStr
                xLine'  <- downArgX xLine
                return $ A.xErrorDefault (annotTail a) xStr' xLine'

        _ -> Nothing