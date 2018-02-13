
module DDC.Core.Discus.Convert.Exp.PrimError
        (convertPrimError)
where
import DDC.Core.Discus.Convert.Exp.Base
import DDC.Core.Discus.Convert.Error

import DDC.Core.Exp.Annot
import DDC.Core.Check                    (AnTEC(..))

import qualified DDC.Core.Discus.Prim     as E
import qualified DDC.Core.Salt.Name      as A
import qualified DDC.Core.Salt.Runtime   as A


-- | Covnert a Discus error primop to Salt.
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
                , [_, RTerm xStr, RTerm xLine]) <- takeXFragApps xx
         -> Just $ do
                xStr'   <- downArgX xStr
                xLine'  <- downArgX xLine
                return $ A.xErrorDefault (annotTail a) xStr' xLine'

        _ -> Nothing