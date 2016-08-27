
module DDC.Core.Tetra.Prim.DaConTetra
        ( typeDaConTetra
        , readDaConTetra)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Tetra.Prim.TyConTetra
import DDC.Type.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char
import Data.List

--        , xTuple2
--        , dcTuple2
--        , dcTupleN )


instance NFData DaConTetra where
 rnf !_ = ()
 

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

{-
-- | Construct a @Tuple2#@
xTuple2 :: Type Name  -> Type Name 
        -> Exp a Name -> Exp a Name 
        -> Exp a Name

xTuple2 t1 t2 x1 x2
        = xApps (XCon dcTuple2) 
                [XType t1, XType t2, x1, x2]


-- | Data constructor for @Tuple2#@
dcTuple2 :: DaCon Name (Type Name)
dcTuple2  = DaConPrim   (NameDaConTetra (DaConTetraTuple 2))
                        (typeDaConTetra (DaConTetraTuple 2))


-- | Data constructor for n-tuples
dcTupleN :: Int -> DaCon Name (Type Name)
dcTupleN n
          = DaConPrim   (NameDaConTetra (DaConTetraTuple n))
                        (typeDaConTetra (DaConTetraTuple n))
-}

