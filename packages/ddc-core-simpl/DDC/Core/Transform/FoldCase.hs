{-# OPTIONS_GHC -Wwarn #-}

module DDC.Core.Transform.FoldCase
        ( Config        (..)
        , configZero
        , foldCase
        , foldCaseX )
where
import DDC.Core.Exp
import DDC.Core.Predicates
import DDC.Core.Compounds.Annot
import DDC.Core.Transform.TransformDownX
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified DDC.Type.Env    as Env

data Config
        = Config
        { -- | Perform the case-of-constructor transformation.
          configCaseOfConstructor       :: Bool

          -- | Perform the case-of-case transformation.
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

foldCaseX config
        nvm@(XCase _ (XVar _ (UName n)) [alt])
 | configCaseOfConstructor config
 , AAlt (PData dc binds) rest <- alt
 = do
        seen <- gets (M.lookup n)
        return $ case seen of
          Just (dc', args')
           | dc == dc'
           -> foldr (\(x', bnd) next -> XLet (annotOfExp x') (LLet bnd x') next)
                rest (zip (filter (not . isXType) args') binds)

          _ -> nvm


-- Case of Case -----------------------------------------------------------------------------------
-- @
--      let y = case x of {
--              A -> C1
--              B -> C2
--              C -> C3
--              D -> C4
--              E -> z4
--              F -> z5
--              _ -> C2 } in
--      let z = case y of {
--              C1 -> x1
--              C2 -> x2
--              C4 -> let ... in ...
--              _  -> x3 } in
--      x3
--
--  ==> let f2'1 () = {let ... in ...} in       -- Pull big expressions into unit abstractions
--      let f1 a = case a of {                  -- Pull non constructor expressions into pre-catch
--              C1 -> x1
--              C2 -> x2
--              C4 -> f2'1 ()
--              _  -> x3 } in
--      let f2 a = case a of {                  -- Perform case of case simple on known constructors
--              A -> x1
--              B -> x2
--              C -> x3
--              D -> f2'1 ()
--              _ -> x2 } in
--      let y = case x of {
--              E -> f1 z4
--              F -> f1 z5
--              _ -> f2 x } in
--      x3
-- @

foldCaseX config
        nvm@(XLet _ (LLet b@(BName yb _) (XCase ay x yalts))
                (XLet az (LLet zb (XCase acz vz@(XVar _ (UName ybm)) zalts)) rest))
 | configCaseOfCase config
 , yb == ybm
 = do
        return nvm

  where
        -- For a particular data constructor in z-case's alts, we need to produce a map which
        -- specifies what binding it will be paired to so we can iterate over the RHS of the y-case's
        -- alts and substitute it with XVars to those bindings.
        --
        -- As mentioned above, if the expression on the RHS of z is more than just a trivial XVar,
        -- we need to convert it to a unit abstracted binding.


-- Default case.
foldCaseX _ x = return x
