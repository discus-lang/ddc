
module DDC.Core.Llvm.Convert.Erase
        ( eraseTypeWitArgs
        , eraseXLAMs)
where
import DDC.Core.Exp
import DDC.Core.Transform.TransformX


-- | Erase type and witness arge Slurp out only the values from a list of function arguments.
eraseTypeWitArgs :: [Exp a n] -> [Exp a n]
eraseTypeWitArgs []       = []
eraseTypeWitArgs (x:xs)
 = case x of
        XType{}       -> eraseTypeWitArgs xs
        XWitness{}    -> eraseTypeWitArgs xs
        _               -> x : eraseTypeWitArgs xs


-- | Erase all `XLAM` binders from an expression.
eraseXLAMs :: Ord n => Exp a n -> Exp a n
eraseXLAMs 
        = transformUpX' 
        $ \x -> case x of
                 XLAM _ _ x'    -> x'
                 _              -> x

