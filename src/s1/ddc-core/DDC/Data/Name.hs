
module DDC.Data.Name
        ( StringName   (..)
        , CompoundName (..))
where

class StringName n where
        -- | Produce a flat string from a name.
        --   The resulting string should be re-lexable as a bindable name.
        stringName      :: n -> String


-- | Compound names can be extended to create new names.
--   This is useful when generating fresh names during program transformation.
class CompoundName n where
        -- | Build a new name based on the given one.
        extendName      :: n -> String -> n
        
        -- | Build a new name from the given string.
        newVarName      :: String -> n

        -- | Split the extension string from a name.
        splitName       :: n -> Maybe (n, String)


