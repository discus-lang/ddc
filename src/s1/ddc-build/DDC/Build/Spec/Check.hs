{-# OPTIONS_HADDOCK hide #-}
module DDC.Build.Spec.Check
        (Error(..))
where

-- | Errors that can appear in a Build Spec file.
data Error
        = ErrorUnrecognisedField
        { errorField    :: String
        , errorValue    :: String }
        deriving Show
