-- | Type checker for the Disciple Core language.
--
--   The algorithm is based on:
--    Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism.
--    Joshua Dunfield, Neelakantan R. Krishnaswami, ICFP 2013.
--  
--   Extensions include:
--    * Check let-bindings and case-expressions.
--    * Allow type annotations on function parameters.
--    * Allow explicit type abstraction and application.
--    * Infer the kinds of type parameters.
--    * Insert type applications in the checked expression, so that the 
--      resulting program can be checked by the standard bottom-up algorithm.
--    * Allow explicit hole '?' annotations to indicate a type or kind 
--      that should be inferred.
-- 
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
        , CheckTrace    (..))
where
import DDC.Core.Check.Judge.Type.VarCon
import DDC.Core.Check.Judge.Type.LamT
import DDC.Core.Check.Judge.Type.LamX
import DDC.Core.Check.Judge.Type.AppT
import DDC.Core.Check.Judge.Type.AppX
import DDC.Core.Check.Judge.Type.Let
import DDC.Core.Check.Judge.Type.LetPrivate
import DDC.Core.Check.Judge.Type.Case
import DDC.Core.Check.Judge.Type.Cast
import DDC.Core.Check.Judge.Type.Witness
import DDC.Core.Check.Judge.Type.Base
import DDC.Core.Transform.MapT
import qualified DDC.Type.Env           as Env


-- Wrappers -------------------------------------------------------------------
-- | Type check an expression. 
--
--   If it's good, you get a new version with types attached every AST node, 
--   as well as every binding occurrence of a variable.
--
--   If it's bad, you get a description of the error.
--
--   The kinds and types of primitives are added to the environments 
--   automatically, you don't need to supply these as part of the starting 
--   kind and type environment.
--
checkExp 
        :: (Ord n, Show n, Pretty n)
        => Config n                     -- ^ Static configuration.
        -> KindEnv n                    -- ^ Starting kind environment.
        -> TypeEnv n                    -- ^ Starting type environment.
        -> Exp a n                      -- ^ Expression to check.
        -> Mode  n                      -- ^ Check mode.
        -> ( Either (Error a n)         --   Type error message. 
                    ( Exp (AnTEC a n) n --   Expression with type annots
                    , Type n            --   Type of expression.
                    , Effect n)         --   Effect of expression.
           , CheckTrace)                --   Type checker debug trace.

checkExp !config !kenv !tenv !xx !mode
 = (result, ct)
 where  
  ((ct, _, _), result)
   = runCheck (mempty, 0, 0) 
   $ do
        -- Check the expression, using the monadic checking function.
        (xx', t, effs, ctx) 
         <- checkExpM 
                (makeTable config
                        (Env.union kenv (configPrimKinds config))
                        (Env.union tenv (configPrimTypes config)))
                emptyContext xx mode
                
        -- Apply the final context to the annotations in expressions.
        -- This ensures that existentials are expanded to solved types.
        let applyToAnnot (AnTEC t0 e0 _ x0)
                = AnTEC (applySolved ctx t0)
                        (applySolved ctx e0)
                        (tBot kClosure)
                        x0

        let xx'' = reannotate applyToAnnot 
                 $ mapT (applySolved ctx) xx'

        -- Also apply the final context to the overall type, 
        -- effect and closure of the expression.
        let t'   = applySolved ctx t
        let e'   = applySolved ctx $ TSum effs

        return  (xx'', t', e')


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
        Left err        -> Left err
        Right (_, t, _) -> Right t


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
                , Context n)            -- Output context.

-- Dispatch to the checker table based on what sort of AST node we're at.
checkExpM !table !ctx !xx !mode
 = case xx of
    XVar{}                 -> tableCheckVarCon     table table ctx xx mode
    XCon{}                 -> tableCheckVarCon     table table ctx xx mode
    XApp _ _ XType{}       -> tableCheckAppT       table table ctx xx mode
    XApp{}                 -> tableCheckAppX       table table ctx xx mode
    XLAM{}                 -> tableCheckLamT       table table ctx xx mode
    XLam{}                 -> tableCheckLamX       table table ctx xx mode
    XLet _ LPrivate{} _    -> tableCheckLetPrivate table table ctx xx mode
    XLet{}                 -> tableCheckLet        table table ctx xx mode
    XCase{}                -> tableCheckCase       table table ctx xx mode
    XCast{}                -> tableCheckCast       table table ctx xx mode
    XWitness{}             -> tableCheckWitness    table table ctx xx mode
    XType    a _           -> throw $ ErrorNakedType    a xx 


-- Table ----------------------------------------------------------------------
makeTable :: Config n -> KindEnv n -> TypeEnv n -> Table a n
makeTable config kenv tenv
        = Table
        { tableConfig           = config
        , tableKindEnv          = kenv
        , tableTypeEnv          = tenv
        , tableCheckExp         = checkExpM
        , tableCheckVarCon      = checkVarCon
        , tableCheckAppT        = checkAppT
        , tableCheckAppX        = checkAppX
        , tableCheckLamT        = checkLamT
        , tableCheckLamX        = checkLamX
        , tableCheckLet         = checkLet
        , tableCheckLetPrivate  = checkLetPrivate
        , tableCheckCase        = checkCase
        , tableCheckCast        = checkCast 
        , tableCheckWitness     = checkWit }

