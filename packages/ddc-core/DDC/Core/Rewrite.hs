
module DDC.Core.Rewrite
        (Rewrite (..))
where
import DDC.Core.Exp
import DDC.Type.Rewrite


instance Rewrite LetMode where
 rewriteWith sub lm
  = case lm of
        LetStrict        -> lm
        LetLazy (Just t) -> LetLazy (Just $ rewriteWith sub t) 
        LetLazy Nothing  -> LetLazy Nothing


--instance Rewrite (Cast a) where
-- rewriteWith sub cc
--  = let down    = rewriteWith sub 
--    in case cc of
--        CastWeakenEffect  eff   -> CastWeakenEffect  (down eff)
--        CastWeakenClosure xs    -> CastWeakenClosure (map down xs)
--        CastPurify w            -> CastPurify (down w)
--        CastForget w            -> CastForget (down w)



instance Rewrite Witness where
 rewriteWith sub ww
  = let down    = rewriteWith 
    in case ww of
        WVar u          -> WVar  (use0 sub u)
        WCon{}          -> ww
        WApp  w1 w2     -> WApp  (down sub w1) (down sub w2)
        WJoin w1 w2     -> WJoin (down sub w1) (down sub w2)
        WType t         -> WType (down sub t)

