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
        , Demand        (..)
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
import DDC.Core.Check.Judge.Type.Prim
import DDC.Core.Check.Judge.Type.VarCon
import DDC.Core.Check.Judge.Type.LamT
import DDC.Core.Check.Judge.Type.LamX
import DDC.Core.Check.Judge.Type.AppT
import DDC.Core.Check.Judge.Type.AppX
import DDC.Core.Check.Judge.Type.Let
import DDC.Core.Check.Judge.Type.LetPrivate
import DDC.Core.Check.Judge.Type.Case
import DDC.Core.Check.Judge.Type.Cast
import DDC.Core.Check.Judge.Type.Base
import DDC.Core.Transform.MapT


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
        :: (Show a, Ord n, Show n, Pretty n)
        => Config n                     -- ^ Static configuration.
        -> EnvX n                       -- ^ Environment of expression.
        -> Mode n                       -- ^ Check mode.
        -> Demand                       -- ^ Demand placed on the expression.
        -> Exp a n                      -- ^ Expression to check.
        -> ( Either (Error a n)         --   Type error message.
                    ( Exp (AnTEC a n) n --   Expression with type annots
                    , Type n            --   Type of expression.
                    , Effect n)         --   Effect of expression.
           , CheckTrace)                --   Type checker debug trace.

checkExp !config !env !mode !demand !xx
 = (result, ct)
 where
  ((ct, _, _), result)
   = runCheck (mempty, 0, 0)
   $ do
        -- Check the expression, using the monadic checking function.
        (xx', t, effs, ctx)
         <- checkExpM
                (makeTable config)
                (emptyContext { contextEnvX = env })
                mode
                demand
                xx

        -- Apply the final context to the annotations in expressions.
        -- This ensures that existentials are expanded to solved types.
        let applyToAnnot (AnTEC t0 e0 _ x0)
             = do t0' <- applySolved ctx t0
                  e0' <- applySolved ctx e0
                  return $ AnTEC t0' e0' (tBot kClosure) x0

        xx_solved <- mapT (applySolved ctx) xx'
        xx_annot  <- reannotateM applyToAnnot xx_solved

        -- Also apply the final context to the overall type,
        -- effect and closure of the expression.
        t'      <- applySolved ctx t
        e'      <- applySolved ctx $ TSum effs

        return  (xx_annot, t', e')


-- | Like `checkExp`, but only return the value type of an expression.
typeOfExp
        :: (Show a, Ord n, Pretty n, Show n)
        => Config n             -- ^ Static configuration.
        -> EnvX n               -- ^ Environment of expresion.
        -> Exp a n              -- ^ Expression to check.
        -> Either (Error a n) (Type n)

typeOfExp !config !env !xx
 = case fst $ checkExp config env Recon DemandNone xx of
        Left err        -> Left err
        Right (_, t, _) -> Right t


-- Monadic Checking -----------------------------------------------------------
-- | Like `checkExp` but using the `CheckM` monad to handle errors.
checkExpM
        :: (Show a, Show n, Pretty n, Ord n)
        => Table a n                    -- ^ Static config.
        -> Context n                    -- ^ Input context.
        -> Mode n                       -- ^ Check mode.
        -> Demand                       -- ^ Demand placed on the expression.
        -> Exp a n                      -- ^ Expression to check.
        -> CheckM a n
                ( Exp (AnTEC a n) n     -- Annotated expression.
                , Type n                -- Output type.
                , TypeSum n             -- Output effect
                , Context n)            -- Output context.

-- Dispatch to the checker table based on what sort of AST node we're at.
checkExpM !table !ctx !mode !demand !xx
 = case xx of
    XVar{}                 -> tableCheckVarCon     table table ctx mode demand xx
    XPrim{}                -> tableCheckPrim       table table ctx mode demand xx
    XCon{}                 -> tableCheckVarCon     table table ctx mode demand xx
    XApp _ _ RType{}       -> tableCheckAppT       table table ctx mode demand xx
    XApp{}                 -> tableCheckAppX       table table ctx mode demand xx
    XAbs _ (MType _) _     -> tableCheckLamT       table table ctx mode demand xx
    XAbs _ (MTerm _) _     -> tableCheckLamX       table table ctx mode demand xx
    XAbs _ (MImplicit _) _ -> tableCheckLamX       table table ctx mode demand xx
    XLet _ LPrivate{} _    -> tableCheckLetPrivate table table ctx mode demand xx
    XLet{}                 -> tableCheckLet        table table ctx mode demand xx
    XCase{}                -> tableCheckCase       table table ctx mode demand xx
    XCast{}                -> tableCheckCast       table table ctx mode demand xx


-- Table ----------------------------------------------------------------------
makeTable :: Config n -> Table a n
makeTable config
        = Table
        { tableConfig           = config
        , tableCheckExp         = checkExpM
        , tableCheckPrim        = checkPrim
        , tableCheckVarCon      = checkVarCon
        , tableCheckAppT        = checkAppT
        , tableCheckAppX        = checkAppX
        , tableCheckLamT        = checkLamT
        , tableCheckLamX        = checkLamX
        , tableCheckLet         = checkLet
        , tableCheckLetPrivate  = checkLetPrivate
        , tableCheckCase        = checkCase
        , tableCheckCast        = checkCast }
