
module DDC.Core.Tetra.Transform.Curry.Interface
        ( FunMap
        , Fun   (..)
        , funMapAddLocalSuper
        , funMapAddForeign

        -- * Utils
        , makeRuns)
where
import DDC.Core.Tetra
import DDC.Core.Tetra.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import Data.Maybe
import Data.Map                                 (Map)
import qualified DDC.Core.Call                  as Call
import qualified Data.Map                       as Map


---------------------------------------------------------------------------------------------------
-- | Map of functional values to their types and arities.
type FunMap
        = Map Name Fun


-- | Enough information about a functional thing to decide how we 
--   should call it. 
data Fun
        -- | A locally defined super.
        --   We can do a saturated call for these directly.
        --   The arity of the super can be determined by inspecting the
        --   definition in the current module.
        = FunLocalSuper
        { _funName      :: Name
        , _funType      :: Type Name 
        , _funArity     :: Int 
        , _funCons      :: [Call.Cons Name] }

        -- | An externally defined super.
        --   These are like local supers, except they were compiled
        --   into a separate module.
        | FunExternSuper
        { _funName      :: Name
        , _funType      :: Type Name
        , _funArity     :: Int }

        -- | A foreign imported function.
        --   We can do a saturated call for these directly.
        --   Foreign functions are not represented as closures, 
        --   so we can determine their arity directly from their types.
        | FunForeignSea
        { _funName      :: Name
        , _funType      :: Type Name
        , _funArity     :: Int }
        deriving Show


-- | Add the type of this binding to the function map.
funMapAddLocalSuper :: FunMap -> (Bind Name, Exp a Name) -> FunMap
funMapAddLocalSuper funs (b, x)
        | BName n t             <- b
        = let   -- Get the value arity of the super, that is, how many
                -- values we need to saturate all the value lambdas.
                (flags, _) = fromMaybe ([], x) (takeXLamFlags x)
                arity      = length $ filter (== False) $ map fst flags

                -- How the super is constructed.
                cons       = Call.takeCallCons x

          in    Map.insert n (FunLocalSuper n t arity cons) funs

        | otherwise
        = funs


-- | Add the type of a foreign import to the function map.
funMapAddForeign :: FunMap -> (Name, ImportValue Name) -> FunMap
funMapAddForeign funs (n, is)

        -- Import from a different DDC compiled module.
        | ImportValueModule _m _n t _ <- is
        = let   (tsArgs, _tResult)                      -- TODO: get real call pattern.
                        = takeTFunArgResult
                        $ eraseTForalls t

                arity   = length tsArgs

          in    Map.insert n (FunExternSuper n t arity) funs


        -- Import from a Sea land.
        | ImportValueSea _ t  <- is
        = let   (tsArgs, _tResult)
                        = takeTFunArgResult
                        $ eraseTForalls t

                arity   = length tsArgs

          in    Map.insert n (FunForeignSea n t arity) funs


        | otherwise
        = funs


---------------------------------------------------------------------------------------------------
-- | Wrap an expression in the given number of 'run' casts.
makeRuns :: a
        -> Int 
        -> Exp a Name 
        -> Exp a Name

makeRuns _a 0 x = x
makeRuns a n x  = XCast a CastRun (makeRuns a (n - 1) x)



