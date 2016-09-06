
module DDC.Control.Panic
        (panic)
where
import DDC.Data.Pretty


-- | Print an error message and exit the compiler, ungracefully.
--
--   This function should be called when we end up in a state that is definately
--   due to a bug in the compiler. 
--
panic   :: String       -- ^ Package name,
        -> String       -- ^ Function name.
        -> Doc          -- ^ Error message that makes some suggestion of what
                        --   caused the error.
        -> a

panic pkg fun msg
        = error $ renderIndent $ vcat
        [ text "PANIC in" <+> text pkg <> text "." <> text fun 
        , indent 2 msg 
        , empty]
