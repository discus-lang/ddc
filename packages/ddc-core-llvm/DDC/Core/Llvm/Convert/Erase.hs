
module DDC.Core.Llvm.Convert.Erase
        ( eraseTypeWitArgs
        , eraseXLAMs
        , eraseWitTApps 
        , eraseWitBinds )
where
import DDC.Type.Predicates
import DDC.Core.Exp
import DDC.Core.Transform.TransformUpX


-- | Erase type and witness arge Slurp out only the values from a list of
--   function arguments.
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


eraseWitTApps :: Type n -> Type n
eraseWitTApps tt
 = case tt of
        TApp (TApp (TCon (TyConWitness _)) _) t -> eraseWitTApps t
        _                                       -> tt


-- | Erase witness bindings
eraseWitBinds :: Eq n => [(Bool, Bind n)] -> [(Bool, Bind n)]
eraseWitBinds
 = let isBindWit (_, b) 
          = case b of
                 BName _ t | isWitnessType t -> True
                 _                           -> False
   in  filter (not . isBindWit)
