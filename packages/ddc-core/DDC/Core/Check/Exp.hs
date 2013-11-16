-- | Type checker for the Disciple Core language.
module DDC.Core.Check.Exp
        ( -- * Checker configuation.
          Config (..)

          -- * Pure checking.
        , AnTEC         (..)
        , Mode          (..)
        , Context
        , emptyContext
        , checkExp
        , typeOfExp

          -- * Monadic checking.
        , Table         (..)
        , makeTable
        , CheckM
        , checkExpM
        , CheckTrace    (..)

          -- * Tagged closures.
        , TaggedClosure(..))
where
import DDC.Core.Check.Exp.VarCon
import DDC.Core.Check.Exp.Abs
import DDC.Core.Check.Exp.App
import DDC.Core.Check.Exp.Let
import DDC.Core.Check.Exp.Case
import DDC.Core.Check.Exp.Cast
import DDC.Core.Check.Exp.Witness
import DDC.Core.Check.Exp.Base
import DDC.Core.Transform.MapT
import Data.Monoid                      hiding ((<>))
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
        => Config n                     -- ^ Static configuration.
        -> KindEnv n                    -- ^ Starting Kind environment.
        -> TypeEnv n                    -- ^ Starting Type environment.
        -> Exp a n                      -- ^ Expression to check.
        -> Mode  n                      -- ^ Check mode.
        -> ( Either (Error a n)
                    ( Exp (AnTEC a n) n
                    , Type n
                    , Effect n
                    , Closure n)
           , CheckTrace)

checkExp !config !kenv !tenv !xx !mode
 = let  ((ct, _, _), result)
         = runCheck (mempty, 0, 0)
         $ do   (xx', t, effs, clos, ctx) 
                 <- checkExpM 
                        (makeTable config
                                (Env.union kenv (configPrimKinds config))
                                (Env.union tenv (configPrimTypes config)))
                        emptyContext xx mode
                
                -- Apply the final context to the annotations in expressions.
                let applyToAnnot (AnTEC t0 e0 c0 x0)
                        = AnTEC (applyContext ctx t0)
                                (applyContext ctx e0)
                                (applyContext ctx c0)
                                x0

                let xx'' = reannotate applyToAnnot 
                         $ mapT (applyContext ctx) xx'

                -- Apply the final context to the overall types of the expression.
                let t'   = applyContext ctx t
                let e'   = applyContext ctx $ TSum effs
                let c'   = applyContext ctx $ closureOfTaggedSet clos

                return  (xx'', t', e', c')
  
  in    (result, ct)


-- | Like `checkExp`, but only return the value type of an expression.
typeOfExp 
        :: (Ord n, Pretty n, Show n)
        => Config n                     -- ^ Static configuration.
        -> KindEnv n                    -- ^ Starting Kind environment
        -> TypeEnv n                    -- ^ Starting Type environment.
        -> Exp a n                      -- ^ Expression to check.
        -> Either (Error a n) (Type n)

typeOfExp !config !kenv !tenv !xx
 = case fst $ checkExp config kenv tenv xx Recon of
        Left err           -> Left err
        Right (_, t, _, _) -> Right t


-- Monadic Checking -----------------------------------------------------------
-- | Like `checkExp` but using the `CheckM` monad to handle errors.
checkExpM 
        :: (Show n, Pretty n, Ord n)
        => Table a n                    -- ^ Static config.
        -> Context n                    -- ^ Input context.
        -> Exp a n                      -- ^ Expression to check.
        -> Mode n                       -- ^ Check mode.
        -> CheckM a n 
                ( Exp (AnTEC a n) n     -- Annotated expression.
                , Type n                -- Output type.
                , TypeSum n             -- Output effect
                , Set (TaggedClosure n) -- Output closure
                , Context n)            -- Output context.

-- Dispatch to the checker table based on what sort of AST node we're at.
checkExpM !table !ctx !xx !mode
 = case xx of
        XVar{}          -> tableCheckVarCon  table table ctx xx mode
        XCon{}          -> tableCheckVarCon  table table ctx xx mode
        XApp{}          -> tableCheckApp     table table ctx xx mode
        XLam{}          -> tableCheckAbs     table table ctx xx mode
        XLAM{}          -> tableCheckAbs     table table ctx xx mode
        XLet{}          -> tableCheckLet     table table ctx xx mode
        XCase{}         -> tableCheckCase    table table ctx xx mode
        XCast{}         -> tableCheckCast    table table ctx xx mode
        XWitness{}      -> tableCheckWitness table table ctx xx mode
        XType    a _    -> throw $ ErrorNakedType    a xx 


-- Table ----------------------------------------------------------------------
makeTable :: Config n -> KindEnv n -> TypeEnv n -> Table a n
makeTable config kenv tenv
        = Table
        { tableConfig           = config
        , tableKindEnv          = kenv
        , tableTypeEnv          = tenv
        , tableCheckExp         = checkExpM
        , tableCheckVarCon      = checkVarCon
        , tableCheckApp         = checkApp
        , tableCheckAbs         = checkAbs
        , tableCheckLet         = checkLet
        , tableCheckCase        = checkCase
        , tableCheckCast        = checkCast 
        , tableCheckWitness     = checkWit }

