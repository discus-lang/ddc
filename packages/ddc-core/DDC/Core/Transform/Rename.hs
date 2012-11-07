
module DDC.Core.Transform.Rename
        (Rename (..))
where
import DDC.Core.Exp
import DDC.Type.Transform.Rename


instance Rename LetMode where
 renameWith sub lm
  = case lm of
        LetStrict        -> lm
        LetLazy (Just t) -> LetLazy (Just $ renameWith sub t) 
        LetLazy Nothing  -> LetLazy Nothing


instance Rename Witness where
 renameWith sub ww
  = let down    = renameWith
    in case ww of
        WVar u          -> WVar  (use0 sub u)
        WCon{}          -> ww
        WApp  w1 w2     -> WApp  (down sub w1) (down sub w2)
        WJoin w1 w2     -> WJoin (down sub w1) (down sub w2)
        WType t         -> WType (down sub t)

