
module DDC.Core.Transform.Rename
        ( Rename(..)

        -- * Substitution states
        , Sub(..)

        -- * Binding stacks
        , BindStack(..)
        , pushBind
        , pushBinds
        , substBound

        -- * Rewriting binding occurences
        , bind1, bind1s, bind0, bind0s

        -- * Rewriting bound occurences
        , use1,  use0)
where
import DDC.Core.Exp
import DDC.Type.Transform.Rename


instance Rename (LetMode a) where
 renameWith sub lm
  = case lm of
        LetStrict        -> lm
        LetLazy (Just t) -> LetLazy (Just $ renameWith sub t) 
        LetLazy Nothing  -> LetLazy Nothing


instance Rename (Witness a) where
 renameWith sub ww
  = let down x   = renameWith x
    in case ww of
        WVar  a u       -> WVar  a (use0 sub u)
        WCon  a c       -> WCon  a c
        WApp  a w1 w2   -> WApp  a (down sub w1) (down sub w2)
        WJoin a w1 w2   -> WJoin a (down sub w1) (down sub w2)
        WType a t       -> WType a (down sub t)

