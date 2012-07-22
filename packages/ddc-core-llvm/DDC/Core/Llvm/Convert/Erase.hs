
module DDC.Core.Llvm.Convert.Erase
        ( eraseTypeWitArgs
        , eraseTForalls
        , eraseXLAMs)
where
import DDC.Core.Exp
import DDC.Core.Transform.TransformX
import DDC.Type.Sum                     as Sum


-- | Erase type and witness arge Slurp out only the values from a list of function arguments.
eraseTypeWitArgs :: [Exp a n] -> [Exp a n]
eraseTypeWitArgs []       = []
eraseTypeWitArgs (x:xs)
 = case x of
        XType{}       -> eraseTypeWitArgs xs
        XWitness{}    -> eraseTypeWitArgs xs
        _               -> x : eraseTypeWitArgs xs


-- | Erase all `TForall` quantifiers from a type.
eraseTForalls :: Ord n => Type n -> Type n
eraseTForalls tt
 = case tt of
        TVar{}          -> tt
        TCon{}          -> tt
        TForall _ t     -> eraseTForalls t
        TApp t1 t2      -> TApp (eraseTForalls t1) (eraseTForalls t2)
        TSum ts         -> TSum $ Sum.fromList (Sum.kindOfSum ts) 
                                $ map eraseTForalls $ Sum.toList ts


-- | Erase all `XLAM` binders from an expression.
eraseXLAMs :: Ord n => Exp a n -> Exp a n
eraseXLAMs 
        = transformUpX' 
        $ \x -> case x of
                 XLAM _ _ x'    -> x'
                 _              -> x

