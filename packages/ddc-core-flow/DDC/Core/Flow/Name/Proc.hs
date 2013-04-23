
-- | Procedure related names.
module DDC.Core.Flow.Name.Proc
        ( LoopOp(..),   readLoopOp
        , StoreOp(..),  readStoreOp)
where
import DDC.Base.Pretty
import Control.DeepSeq


-- LoopOp ---------------------------------------------------------------------
data LoopOp
        = LoopOpLoop
        deriving (Eq, Ord, Show)

instance NFData LoopOp

instance Pretty LoopOp where
 ppr fo
  = case fo of
        LoopOpLoop              -> text "loop#"


-- | Read a baked-in loop operator.
readLoopOp :: String -> Maybe LoopOp
readLoopOp str
        | str == "loop#"
        = Just $ LoopOpLoop

        | otherwise
        = Nothing


-- StoreOp --------------------------------------------------------------------
data StoreOp
        = StoreOpNew            -- ^ @new#@   operator.
        | StoreOpRead           -- ^ @read#@  operator.
        | StoreOpWrite          -- ^ @write#@ operator.

        | StoreOpNext           -- ^ @next#@  operator.
        deriving (Eq, Ord, Show)

instance NFData StoreOp

instance Pretty StoreOp where
 ppr so
  = case so of
        StoreOpNew              -> text "new#"
        StoreOpRead             -> text "read#"
        StoreOpWrite            -> text "write#"

        StoreOpNext             -> text "next#"


-- | Read a baked-in store operator.
readStoreOp :: String -> Maybe StoreOp
readStoreOp str
 = case str of
        "new#"          -> Just StoreOpNew
        "read#"         -> Just StoreOpRead
        "write#"        -> Just StoreOpWrite

        "next#"         -> Just StoreOpNext
        _               -> Nothing

