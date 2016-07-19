{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
-- | A light simplification pass before conversion of desugared code to Core.
module DDC.Source.Tetra.Transform.Prep
        ( type S, evalState, newVar
        , prepModule)
where
import DDC.Source.Tetra.Module
-- import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Exp
import Data.Monoid
import Data.Text                        (Text)
-- import qualified DDC.Data.SourcePos     as SP
import qualified Control.Monad.State    as S
import qualified Data.Text              as Text


-------------------------------------------------------------------------------
-- | Source position.
-- type SP = SP.SourcePos


-- | State holding a variable name prefix and counter to 
--   create fresh variable names.
type S  = S.State (Text, Int)


-- | Evaluate a desguaring computation,
--   using the given prefix for freshly introduced variables.
evalState :: Text -> S a -> a
evalState n c
 = S.evalState c (n, 0) 


-- | Allocate a new named variable, yielding its associated bind and bound.
newVar :: Text -> S (Bind, Bound)
newVar pre
 = do   (n, i)   <- S.get
        let name = pre <> "$" <> n <> Text.pack (show i)
        S.put (n, i + 1)
        return  (BName name, UName name)


-------------------------------------------------------------------------------
-- | Prepare a source module for conversion to core.
prepModule :: Module Source -> S (Module Source)
prepModule mm
 = return mm

