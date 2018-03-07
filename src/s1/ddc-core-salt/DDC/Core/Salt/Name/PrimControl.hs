
module DDC.Core.Salt.Name.PrimControl
        ( PrimControl (..)
        , readPrimControl)
where
import DDC.Data.Pretty
import Control.DeepSeq
import Data.List
import Data.Char


-- | Primitive non-returning control flow.
data PrimControl
        -- | Ungraceful failure -- just abort the program.
        --   This is called on internal errors in the runtime system.
        --   There is no further debugging info provided, so you'll need to
        --   look at the stack trace to debug it.
        = PrimControlFail

        -- | Return from the enclosing function with the given value.
        | PrimControlReturn

        -- | Perform a standard function call where the address is not
        --   statically known. All the arguments are boxed heap objects.
        | PrimControlCall     Int

        -- | Tailcall a statically known functions,
        --   where the arguments can be boxed or unboxed.
        | PrimControlTailCall Int
        deriving (Eq, Ord, Show)


instance NFData PrimControl where
 rnf (PrimControlCall     i)  = rnf i
 rnf (PrimControlTailCall i)  = rnf i
 rnf !_ = ()


instance Pretty PrimControl where
 ppr pc
  = case pc of
        PrimControlFail
         -> text "fail#"

        PrimControlReturn
         -> text "return#"

        PrimControlCall  arity
         -> text "call"     <> int arity <> text "#"

        PrimControlTailCall arity
         -> text "tailcall" <> int arity <> text "#"


readPrimControl :: String -> Maybe PrimControl
readPrimControl str
        -- callN#
        | Just rest     <- stripPrefix "call" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 0
        = Just $ PrimControlCall n

        -- tailcallN#
        | Just rest     <- stripPrefix "tailcall" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n > 0
        = Just $ PrimControlTailCall n

        | otherwise
        = case str of
                "fail#"         -> Just $ PrimControlFail
                "return#"       -> Just $ PrimControlReturn
                _               -> Nothing

