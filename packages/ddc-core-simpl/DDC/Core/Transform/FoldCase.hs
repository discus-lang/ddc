
module DDC.Core.Transform.FoldCase
        ( Config        (..)
        , configZero
        , foldCase
        , foldCaseX )
where
import DDC.Core.Exp
import DDC.Core.Predicates
import DDC.Core.Compounds
import DDC.Core.Transform.TransformDownX
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified DDC.Type.Env    as Env

data Config
        = Config
        { -- | Perform the case-of-constructor transformation.
          configCaseOfConstructor       :: Bool

          -- | Perform the case-of-case transformation.
          --   Not implemented yet.
        , configCaseOfCase              :: Bool }

configZero :: Config
configZero
        = Config
        { configCaseOfConstructor       = False
        , configCaseOfCase              = False }


---------------------------------------------------------------------------------------------------
type FoldCase a n = State (M.Map n (DaCon n, [Exp a n]))

foldCase :: (Ord n, TransformDownMX (FoldCase a n) c)
        => Config
        -> c a n
        -> c a n

foldCase config xx
 = {-# SCC foldCase #-}
   evalState (transformDownMX (\_ _ -> foldCaseX config) Env.empty Env.empty xx) M.empty

foldCaseX :: Ord n
        => Config
        -> Exp a n
        -> FoldCase a n (Exp a n)

-- Collect ----------------------------------------------------------------------------------------
foldCaseX _
        x@(XLet _ (LLet (BName b _) ex) _)
 | Just (dc, args) <- takeXConApps ex
 = do
        modify (M.insert b (dc, args))
        return x


-- Case of Constructor ----------------------------------------------------------------------------
-- @
--      let x = Con y z in
--      case x of
--              Con a b -> x1
--
--  ==> let x = Con y z in
--      let a = y in
--      let b = z in
--      x1
-- @
--
foldCaseX config
        x@(XCase _ (XVar _ (UName n)) [alt])
 | configCaseOfConstructor config
 , AAlt (PData dc binds) rest <- alt
 = do
        seen <- gets (M.lookup n)
        return $ case seen of
          Just (dc', args') | dc == dc'
           -> foldr (\(x', bnd) next -> XLet (annotOfExp x') (LLet bnd x') next)
                rest (zip (filter (not . isXType) args') binds)

          _ -> x


-- Default case.
foldCaseX _ x = return x
