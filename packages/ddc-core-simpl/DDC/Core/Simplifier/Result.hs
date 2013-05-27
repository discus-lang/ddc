
module DDC.Core.Simplifier.Result
        ( TransformResult (..)
        , TransformInfo   (..)
        , NoInformation
        , resultDone)
where
import DDC.Base.Pretty
import Data.Typeable
import qualified DDC.Base.Pretty                as P


-- TransformResult ------------------------------------------------------------
-- | Package up the result of applying a single transform.
data TransformResult r
        = TransformResult
        { -- | Transform result proper (eg the new module)
          result         :: r

          -- | Whether this transform made any progess.
          --   
          --   If `False` then the result program must be the same as the
          --   input program, and a simplifer fixpoint won't apply this
          --   transform again to the result program.
        , resultProgress :: Bool

          -- | Whether it might help to run the same transform again.
          -- 
          --   If `False` then a simplifier fixpoint won't apply this transform
          --   again to the result program.
        , resultAgain    :: Bool

          -- | Transform specific log. This might contain a count of what rules
          --   fired, or information about what parts of the program couldn't
          --   be processed.
        , resultInfo     :: TransformInfo }


-- | Existential package for a typeable thing,
--   used in `TransformResult`.
data TransformInfo
        =  forall i
        .  (Typeable i, Pretty i)
        => TransformInfo i


-- | Place-holder type to use when there is no real `TransformResult`.
data NoInformation 
        = NoInformation String
        deriving Typeable


instance Pretty NoInformation where
    ppr (NoInformation name) = text name P.<> text ": No information"


instance Pretty (TransformResult r) where
 ppr (TransformResult _ _ _ (TransformInfo i))
  = ppr i


-- | Create a default result with no transform again.
--  
--   We'll say we made progress, but set `resultAgain` to False
--   so to stop any simplifier fixpoints.
resultDone :: String -> r -> TransformResult r
resultDone name r 
        = TransformResult r True False
        $ TransformInfo 
        $ NoInformation name
