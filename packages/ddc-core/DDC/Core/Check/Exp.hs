-- | Type checker for the Disciple Core language.
module DDC.Core.Check.Exp
        ( -- * Checker configuation.
          Config (..)

          -- * Pure checking.
        , AnTEC  (..)
        , checkExp
        , typeOfExp

          -- * Monadic checking.
        , Table  (..)
        , tableOfConfig
        , CheckM
        , checkExpM

          -- * Tagged closures.
        , TaggedClosure(..))
where
import DDC.Core.Check.Exp.VarCon
import DDC.Core.Check.Exp.Abs
import DDC.Core.Check.Exp.App
import DDC.Core.Check.Exp.Let
import DDC.Core.Check.Exp.Case
import DDC.Core.Check.Exp.Cast
import DDC.Core.Check.Exp.Base
import qualified DDC.Type.Env           as Env


-- Wrappers -------------------------------------------------------------------
-- | Type check an expression. 
--
--   If it's good, you get a new version with types attached to all the bound
--   variables, as well its the type, effect and closure. 
--
--   If it's bad, you get a description of the error.
--
--   The returned expression has types attached to all variable occurrences, 
--   so you can call `typeOfExp` on any open subterm.
--
--   The kinds and types of primitives are added to the environments 
--   automatically, you don't need to supply these as part of the 
--   starting environments.
--
checkExp 
        :: (Ord n, Show n, Pretty n)
        => Config n             -- ^ Static configuration.
        -> KindEnv n            -- ^ Starting Kind environment.
        -> TypeEnv n            -- ^ Starting Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> Maybe (Type n)       -- ^ Expected type, if any.
        -> Either (Error a n)
                  ( Exp (AnTEC a n) n
                  , Type n
                  , Effect n
                  , Closure n)

checkExp !config !kenv !tenv !xx !tXX
 = result
 $ do   (xx', t, effs, clos) 
                <- checkExpM (tableOfConfig config)
                        (Env.union kenv (configPrimKinds config))
                        (Env.union tenv (configPrimTypes config))
                        xx tXX
        return  ( xx'
                , t
                , TSum effs
                , closureOfTaggedSet clos)


-- | Like `checkExp`, but only return the value type of an expression.
typeOfExp 
        :: (Ord n, Pretty n, Show n)
        => Config n             -- ^ Static configuration.
        -> KindEnv n            -- ^ Starting Kind environment
        -> TypeEnv n            -- ^ Starting Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> Maybe  (Type n)      -- ^ Expected type, if any.
        -> Either (Error a n) (Type n)

typeOfExp !config !kenv !tenv !xx !tXX
 = case checkExp config kenv tenv xx tXX of
        Left err           -> Left err
        Right (_, t, _, _) -> Right t


-- Monadic Checking -----------------------------------------------------------
-- | Like `checkExp` but using the `CheckM` monad to handle errors.
checkExpM 
        :: (Show n, Pretty n, Ord n)
        => Table a n            -- ^ Static config.
        -> Env n                -- ^ Kind environment.
        -> Env n                -- ^ Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> Maybe (Type n)       -- ^ Expected type, if any.
        -> CheckM a n 
                ( Exp (AnTEC a n) n
                , Type n
                , TypeSum n
                , Set (TaggedClosure n))

-- Dispatch to the checker table based on what sort of AST node we're at.
checkExpM !table !kenv !tenv !xx !tXX
 = case xx of
        XVar{}          -> tableCheckVarCon table table kenv tenv xx tXX
        XCon{}          -> tableCheckVarCon table table kenv tenv xx tXX
        XApp{}          -> tableCheckApp    table table kenv tenv xx tXX
        XLam{}          -> tableCheckAbs    table table kenv tenv xx tXX
        XLAM{}          -> tableCheckAbs    table table kenv tenv xx tXX
        XLet{}          -> tableCheckLet    table table kenv tenv xx tXX
        XCase{}         -> tableCheckCase   table table kenv tenv xx tXX
        XCast{}         -> tableCheckCast   table table kenv tenv xx tXX
        XType{}         -> throw $ ErrorNakedType xx 
        XWitness{}      -> throw $ ErrorNakedWitness xx


-- Table ----------------------------------------------------------------------
tableOfConfig :: Config n -> Table a n
tableOfConfig config
        = Table
        { tableConfig           = config
        , tableCheckExp         = checkExpM
        , tableCheckVarCon      = checkVarCon
        , tableCheckApp         = checkApp
        , tableCheckAbs         = checkAbs
        , tableCheckLet         = checkLet
        , tableCheckCase        = checkCase
        , tableCheckCast        = checkCast }

