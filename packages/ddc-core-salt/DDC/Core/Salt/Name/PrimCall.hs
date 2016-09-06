
module DDC.Core.Salt.Name.PrimCall
        ( PrimCall (..)
        , readPrimCall)
where
import DDC.Data.Pretty
import Control.DeepSeq
import Data.Char
import Data.List


-- | Primitive ways of invoking a function, 
--   where control flow returns back to the caller.
data PrimCall
        -- | Perform a standard function call where the address is not
        --   statically known. All the arguments are boxed heap objects.
        = PrimCallStd     Int

        -- | Tailcall a statically known functions,
        --   where the arguments can be boxed or unboxed.
        | PrimCallTail    Int
        deriving (Eq, Ord, Show)


instance NFData PrimCall where
 rnf (PrimCallStd  i)   = rnf i
 rnf (PrimCallTail i)   = rnf i


instance Pretty PrimCall where
 ppr pc
  = case pc of
        PrimCallStd  arity
         -> text "call"     <> int arity <> text "#"

        PrimCallTail arity
         -> text "tailcall" <> int arity <> text "#"


readPrimCall :: String -> Maybe PrimCall
readPrimCall str

        -- callN#
        | Just rest     <- stripPrefix "call" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n >= 0
        = Just $ PrimCallStd n

        -- tailcallN#
        | Just rest     <- stripPrefix "tailcall" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n > 0
        = Just $ PrimCallTail n

        | otherwise
        = Nothing
