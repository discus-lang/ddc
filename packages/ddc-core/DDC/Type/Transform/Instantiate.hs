
module DDC.Type.Transform.Instantiate
        ( instantiateT
        , instantiateTs)
where
import DDC.Type.Exp
import DDC.Type.Transform.SubstituteT
import DDC.Base.Pretty          (Pretty)


-- | Instantiate a type with an argument.
--   The type to be instantiated must have an outer forall, else `Nothing`.
instantiateT :: (Ord n, Pretty n) => Type n -> Type n -> Maybe (Type n)
instantiateT (TForall b tBody) t2 = Just $ substituteT b t2 tBody
instantiateT _ _                  = Nothing


-- | Instantiate a type with several arguments.
--   The type to be instantiated must have at least as many outer foralls 
--   as provided type arguments, else `Nothing`.
instantiateTs :: (Ord n, Pretty n) => Type n -> [Type n] -> Maybe (Type n)
instantiateTs t []              = Just t
instantiateTs t (tArg:tsArgs)
 = case instantiateT t tArg of
        Nothing         -> Nothing
        Just t'         -> instantiateTs t' tsArgs

