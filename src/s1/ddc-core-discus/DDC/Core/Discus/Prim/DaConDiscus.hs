
module DDC.Core.Discus.Prim.DaConDiscus
        ( typeDaConDiscus
        , readDaConDiscus)
where
import DDC.Core.Discus.Prim.Base
import DDC.Core.Discus.Prim.TyConDiscus
import DDC.Type.Exp.Simple
import DDC.Data.Pretty
import Control.DeepSeq
import Data.Char
import Data.List


instance NFData DaConDiscus where
 rnf !_ = ()


instance Pretty DaConDiscus where
 ppr dc
  = case dc of
        DaConDiscusTuple n       -> text "T" <> int n <> text "#"


-- | Read the name of a baked-in data constructor.
readDaConDiscus :: String -> Maybe DaConDiscus
readDaConDiscus str
        | Just rest     <- stripPrefix "T" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ DaConDiscusTuple arity

        | otherwise
        = Nothing


-- | Yield the type of a baked-in data constructor.
typeDaConDiscus :: DaConDiscus -> Type Name
typeDaConDiscus (DaConDiscusTuple n)
        = tForalls (replicate n kData)
        $ \args -> foldr tFun (tTupleN args) args

