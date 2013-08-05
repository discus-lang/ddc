
module DDC.Core.Salt.Name.PrimCall
        ( PrimCall (..)
        , readPrimCall)
where
import DDC.Base.Pretty
import Control.DeepSeq
import Data.Char
import Data.List


-- | Primitive ways of invoking a function, 
--   where control flow returns back to the caller.
data PrimCall
        -- | Tailcall a function
        = PrimCallTail    Int
        deriving (Eq, Ord, Show)


instance NFData PrimCall where
 rnf (PrimCallTail i)   = rnf i


instance Pretty PrimCall where
 ppr pc
  = case pc of
        PrimCallTail    arity
         -> text "tailcall" <> int arity <> text "#"


readPrimCall :: String -> Maybe PrimCall
readPrimCall str

        -- tailcallN#
        | Just rest     <- stripPrefix "tailcall" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , n > 0
        = Just $ PrimCallTail n

        | otherwise
        = Nothing
