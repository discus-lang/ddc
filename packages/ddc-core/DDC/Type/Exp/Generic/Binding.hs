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
 boundOfBind      :: GBindVar l -> GBoundVar l

 -- | Check if the given bound occurence matches a binding occurrence.
 boundMatchesBind :: GBindVar l -> GBoundVar l -> Bool


-- Anon -----------------------------------------------------------------------
-- | Class of languages that support anonymous binding.
class Anon l where
 
 -- | Evaluate a function given a new anonymous binding and matching
 --   bound occurrence.
 withBinding :: l -> (GBindVar l -> GBoundVar l -> a) -> a

