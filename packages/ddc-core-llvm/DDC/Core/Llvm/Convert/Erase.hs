
module DDC.Core.Llvm.Convert.Erase
        ( eraseTypeWitArgs
        , eraseTForalls)
where
import DDC.Core.Exp
import DDC.Type.Sum           as Sum


-- | Erase type and witness arge Slurp out only the values from a list of function arguments.
eraseTypeWitArgs :: [Exp a n] -> [Exp a n]
eraseTypeWitArgs []       = []
eraseTypeWitArgs (x:xs)
 = case x of
        XType{}       -> eraseTypeWitArgs xs
        XWitness{}    -> eraseTypeWitArgs xs
        _               -> x : eraseTypeWitArgs xs


-- | Erase all TForall quantifiers from a type.
eraseTForalls :: Ord n => Type n -> Type n
eraseTForalls tt
 = case tt of
        TVar{}          -> tt
        TCon{}          -> tt
        TForall _ t     -> eraseTForalls t
        TApp t1 t2      -> TApp (eraseTForalls t1) (eraseTForalls t2)
        TSum ts         -> TSum $ Sum.fromList (Sum.kindOfSum ts) 
                                $ map eraseTForalls $ Sum.toList ts
