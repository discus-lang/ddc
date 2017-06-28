{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
-- | Desugar match expressions to case expressions.
--
--   In a match expression if matching fails in one block of guards then
--   we skip to the next block. This introduces join point at the start
--   of every block of guards execpt the first one, which we need to flatten
--   out when converting to plain case expressions.
--
--   We also merge multiple clauses for the same function into a single one
--   while we're here.
--
module DDC.Source.Tetra.Transform.Matches
        ( type S, evalState, newVar
        , desugarModule)
where
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Exp
import Data.Monoid
import Data.Text                        (Text)
import qualified DDC.Data.SourcePos     as SP
import qualified Control.Monad.State    as S
import qualified Data.Text              as Text


-------------------------------------------------------------------------------
-- | Desugar match expressions to case expressions in a module.
desugarModule :: Module Source -> S (Module Source)
desugarModule mm
 = do   ts'     <- desugarTops $ moduleTops mm
        return  $  mm { moduleTops = ts' }


-------------------------------------------------------------------------------
-- | Desugar top-level definitions.
desugarTops :: [Top Source] -> S [Top Source]
desugarTops ts
 = do   let tsType  = [t          | t@TopType{}     <- ts]
        let tsData  = [t          | t@TopData{}     <- ts]
        let spCls   = [(sp, cl)   | TopClause sp cl <- ts]

        -- We may have multiple clauses for the same function in a single
        -- group, so we need to pass them all to the clause group
        -- desugarer at once.
        spCls'  <- desugarClGroup spCls

        return  $  tsType
                ++ tsData
                ++ [TopClause sp cl | (sp, cl) <- spCls']


-------------------------------------------------------------------------------
-- | Desugar a clause group.
desugarClGroup :: [(SP, Clause)] -> S [(SP, Clause)]
desugarClGroup spcls0
 = loop spcls0
 where

  -- We've reached the end of the list of clauses.
  loop []
   = return []

  -- Signatures do not need desugaring.
  loop ((sp, cl@SSig{}) : cls)
   = do cls'    <- loop cls
        return  $  (sp, cl) : cls'

  -- We have a let-clause.
  loop ( (sp, SLet sp1 (XBindVarMT b1 mt1) ps1 gxs1) : cls)
   = loop cls >>= \cls'
   -> case cls' of

        -- Consecutive clauses are for the same function.
        (_, SLet _sp2 (XBindVarMT b2 _mt2) ps2 [GExp xNext]) : clsRest
          | b1 == b2
          -> do
                -- Flatten out guards, wrapping the next expression
                -- with case expressions to implement them.
                xBody_inner <- flattenGXs gxs1 xNext

                -- Recursively desugar the flattened expression
                xBody_rec   <- desugarX sp xBody_inner

                -- Intoduce new let-bindings to handle the case
                -- where different clauses name their parameters
                -- differently.
                (ps1', _ps2', xBody_join)
                            <- joinParams ps1 ps2 xBody_rec

                return  $ (sp, SLet sp1 (XBindVarMT b1 mt1) ps1'
                                        [GExp xBody_join])
                        : clsRest

        -- Consecutive clauses are not for the same function.
        _ -> do let xError  = makeXErrorDefault
                                (Text.pack    $ SP.sourcePosSource sp1)
                                (fromIntegral $ SP.sourcePosLine   sp1)

                -- Flatten out guards, wrapping the error expression
                -- with case expressions to implement them.
                xBody_inner <- flattenGXs gxs1 xError

                -- Recursively desugar the flattened expression.
                xBody'      <- desugarX sp xBody_inner

                return  $ (sp, SLet sp1 (XBindVarMT b1 mt1) ps1
                                        [GExp xBody'])
                        : cls'


-- | Given corresponding parameters for earlier and later clauses,
--   introduce let bindings to handle differences in parameter naming.
joinParams ::    [Param] -> [Param] -> Exp
           -> S ([Param],   [Param],   Exp)

joinParams []   ps2  xx
 = return ([],  ps2, xx)

joinParams ps1  []   xx
 = return (ps1, [],  xx)

joinParams (p1:ps1) (p2:ps2) xx
 = do
        (p1',  p2',  mLets) <- joinParam  p1 p2
        (ps1', ps2', xx')   <- joinParams ps1 ps2 xx

        case mLets of
         Nothing
          -> return (p1' : ps1', p2' : ps2', xx')

         Just lts
          -> return (p1' : ps1', p2' : ps2', XLet lts xx')


-- | Given corresponding parameters for earlier and later clauses,
--   introduce let bindings to handle differences in parameter naming.
joinParam :: Param -> Param
          -> S (Param, Param, Maybe Lets)

joinParam p1 p2
 = case (p1, p2) of
        -- When an earlier pattern does not bind the argument to a variable
        -- then we need to introduce a new variable so we can pass the
        -- same argument to successive clauses.
        (  MTerm pat1               mt1
         , MTerm (PVar (BName n2))  mt2)
         | isAnonPat pat1
         -> do  (b, u)  <- newVar "m"
                let lts = LLet (XBindVarMT (BName n2) mt2) (XVar u)
                return (MTerm (PVar b) mt1, p2, Just lts)

        -- When earlier clauses bind the argument using a different variable
        -- than later ones then we need to add a synonym.
        (  MTerm (PVar (BName n1)) _mt1
         , MTerm (PVar (BName n2)) mt2)
         |   n1 /= n2
         -> do  let lts  = LLet (XBindVarMT (BName n2) mt2) (XVar (UName n1))
                return (p1, p2, Just lts)

        _ -> return (p1, p2, Nothing)


-- | Check if this pattern does not bind a variable.
isAnonPat :: Pat -> Bool
isAnonPat pp
 = case pp of
        PDefault        -> True
        PVar BAnon      -> True
        _               -> False


-------------------------------------------------------------------------------
-- | Desugar an expression.
desugarX :: SP -> Exp -> S Exp
desugarX sp xx
 = case xx of
        -- Boilerplate.
        XAnnot sp' x -> XAnnot sp' <$> desugarX sp' x
        XPrim{}      -> pure xx
        XFrag{}      -> pure xx
        XVar{}       -> pure xx
        XCon{}       -> pure xx
        XAbs  b x    -> XAbs b     <$> desugarX sp x
        XApp  x1 a2  -> XApp       <$> desugarX   sp x1  <*> desugarArg sp a2
        XLet  lts x  -> XLet       <$> desugarLts sp lts <*> desugarX sp x
        XCast c x    -> XCast c    <$> desugarX   sp x
        XDefix a xs  -> XDefix a   <$> mapM (desugarArg sp) xs
        XInfixOp{}   -> pure xx
        XInfixVar{}  -> pure xx

        -- Desugar case expressions.
        XCase x alts
         -> XAnnot sp   <$> (XCase  <$> desugarX sp x
                                    <*> mapM (desugarAC sp) alts)

        -- Desugar match expressions into case expressions.
        XMatch _ alts xFail
         -> do  let gxs =  [gx | AAltMatch gx <- alts]
                xFlat   <- flattenGXs gxs xFail
                xFlat'  <- desugarX sp xFlat
                return  xFlat'

        XWhere sp' x cls
         -> do  x'        <- desugarX sp' x
                let spcls =  [(sp', cl) | cl <- cls]
                spcls'    <- desugarClGroup spcls
                return   $ XWhere sp' x' (map snd spcls')

        XAbsPat  sp' ps w mt x
         ->     XAbsPat sp' ps w mt <$> desugarX sp x

        XLamCase sp' alts
         ->     XLamCase sp' <$> mapM (desugarAC sp) alts


-------------------------------------------------------------------------------
-- | Desugar an argument.
desugarArg :: SP -> Arg -> S Arg
desugarArg sp arg
 = case arg of
        RType{}         -> return arg
        RWitness{}      -> return arg
        RTerm x         -> RTerm     <$> desugarX   sp x
        RImplicit arg'  -> RImplicit <$> desugarArg sp arg'


-------------------------------------------------------------------------------
-- | Desugar some let bindings.
desugarLts :: SP -> Lets -> S Lets
desugarLts sp lts
 = case lts of
        LLet mb x       -> LLet mb  <$> desugarX sp x

        LRec bxs
         -> do  let (bs, xs)    = unzip bxs
                xs'             <- mapM (desugarX sp) xs
                return $ LRec $ zip bs xs'

        LPrivate{}      -> return lts

        LGroup bRec cls
         -> do  let spcls  =  zip (repeat sp) cls
                spcls'     <- desugarClGroup spcls
                return     $ LGroup bRec $ map snd spcls'


-------------------------------------------------------------------------------
-- | Desugar a guarded expression.
desugarGX :: SP -> GuardedExp -> S GuardedExp
desugarGX sp gx
 = case gx of
        GGuard g gx'    -> GGuard <$> desugarG sp g <*> desugarGX sp gx'
        GExp   x        -> GExp   <$> desugarX sp x


-------------------------------------------------------------------------------
-- | Desugar a guard.
desugarG :: SP -> Guard -> S Guard
desugarG sp g
 = case g of
        GPat p x        -> GPat p  <$> desugarX sp x
        GPred x         -> GPred   <$> desugarX sp x
        GDefault        -> pure GDefault


-------------------------------------------------------------------------------
-- | Desugar a case alternative.
desugarAC :: SP -> AltCase -> S AltCase
desugarAC sp (AAltCase p gxs)
 = do   gxs'    <- mapM (desugarGX sp) gxs
        return  $  AAltCase p gxs'


-------------------------------------------------------------------------------
-- | Desugar some guards to a case-expression.
--   At runtime, if none of the guards match then run the provided
--   fall-though computation.
flattenGXs :: [GuardedExp] -> Exp -> S Exp
flattenGXs gs0 fail0
 = go gs0 fail0
 where
        -- Desugar list of guarded expressions.
        go [] cont
         = return cont

        go [g]   cont
         = go1 g cont

        go (g : gs) cont
         = do   gs'     <- go gs cont
                go1 g gs'

        -- Desugar single guarded expression.
        go1 (GExp x1) _
         = return x1

        go1 (GGuard GDefault   gs) cont
         = go1 gs cont

        -- Simple cases where we can avoid introducing the continuation.
        go1 (GGuard (GPred g1)   (GExp x1)) cont
         = return
         $ XCase g1 [ AAltCase PTrue    [GExp x1]
                    , AAltCase PDefault [GExp cont] ]

        go1 (GGuard (GPat p1 g1) (GExp x1)) cont
         = return
         $ XCase g1 [ AAltCase p1        [GExp x1]
                    , AAltCase PDefault  [GExp cont]]

        -- Cases that use a continuation function as a join point.
        -- We need this when desugaring general pattern alternatives,
        -- as each group of guards can be reached from multiple places.
        go1 (GGuard (GPred x1) gs) cont
         = do   (b, u)  <- newVar "m"
                x'      <- go1 gs (XRun (XVar u))
                return
                 $ XLet     (LLet (XBindVarMT b Nothing) (XBox cont))
                 $ XCase x1 [ AAltCase PTrue    [GExp x']
                            , AAltCase PDefault [GExp (XRun (XVar u)) ]]

        go1 (GGuard (GPat p1 x1) gs) cont
         = do   (b, u)  <- newVar "m"
                x'      <- go1 gs (XRun (XVar u))
                return
                 $ XLet     (LLet (XBindVarMT b Nothing) (XBox cont))
                 $ XCase x1 [ AAltCase p1       [GExp x']
                            , AAltCase PDefault [GExp (XRun (XVar u)) ]]


-------------------------------------------------------------------------------
-- | Source position.
type SP = SP.SourcePos


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

