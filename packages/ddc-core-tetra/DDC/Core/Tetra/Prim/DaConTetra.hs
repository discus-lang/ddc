
module DDC.Core.Tetra.Prim.DaConTetra
        ( typeDaConTetra
        , readDaConTetra)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Tetra.Prim.TyConTetra
import DDC.Core.Compounds.Annot
import DDC.Core.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char
import Data.List


instance NFData DaConTetra

instance Pretty DaConTetra where
 ppr dc
  = case dc of
        DaConTetraTuple n       -> text "T" <> int n <> text "#"


-- | Read the name of a baked-in data constructor.
readDaConTetra :: String -> Maybe DaConTetra
readDaConTetra str
        | Just rest     <- stripPrefix "T" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ DaConTetraTuple arity

        | otherwise
        = Nothing


-- | Yield the type of a baked-in data constructor.
typeDaConTetra :: DaConTetra -> Type Name
typeDaConTetra (DaConTetraTuple n)
        = tForalls (replicate n kData)
        $ \args -> foldr tFun (tTupleN args) args

