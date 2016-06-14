{-# LANGUAGE TypeFamilies #-}
module DDC.Type.Exp.Generic.Binding 
        ( Binding       (..)
        , Anon          (..))
where
import DDC.Type.Exp.Generic.Exp


-- Binding --------------------------------------------------------------------
-- | Class of languages that include name binding.
class Binding l where

 -- | Get the bound occurrence that matches the given binding occurrence.
 boundOfBind      :: GTBindVar l -> GTBoundVar l

 -- | Check if the given bound occurence matches a binding occurrence.
 boundMatchesBind :: GTBindVar l -> GTBoundVar l -> Bool


-- Anon -----------------------------------------------------------------------
-- | Class of languages that support anonymous binding.
class Anon l where
 
 -- | Evaluate a function given a new anonymous binding and matching
 --   bound occurrence.
 withBinding  :: l -> (GTBindVar l -> GTBoundVar l -> a) -> a
 withBinding l f   =  withBindings l 1 (\[b] [u] -> f b u)

 withBindings :: l -> Int -> ([GTBindVar l] -> [GTBoundVar l] -> a) -> a