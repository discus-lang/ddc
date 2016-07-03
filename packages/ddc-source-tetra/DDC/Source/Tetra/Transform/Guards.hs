{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

-- | Desugaring Source Tetra guards and patterns to simple case-expressions.
module DDC.Source.Tetra.Transform.Guards
        ( type S, evalState, newVar
        , desugarModule)
where
import DDC.Source.Tetra.Transform.BoundX
import DDC.Source.Tetra.Exp.Compounds
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Exp
import qualified DDC.Data.SourcePos     as SP
import Data.Monoid
import Data.Text                        (Text)
import qualified Data.Text              as Text
import qualified Control.Monad.State    as S


---------------------------------------------------------------------------------------------------
type SP = SP.SourcePos
type S  = S.State (Text, Int)


-- | Evaluate a desguaring computation.
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


---------------------------------------------------------------------------------------------------
-- | Desugar guards in a module.
desugarModule :: Module Source -> S (Module Source)
desugarModule mm
 = do   ts'     <- mapM desugarTop $ moduleTops mm
        return  $ mm { moduleTops = ts' }


---------------------------------------------------------------------------------------------------
-- | Desugar a top-level thing.
desugarTop    :: Top Source -> S (Top Source)
desugarTop tt
 = case tt of
        TopClause sp c  -> TopClause sp <$> desugarCl sp c
        TopData{}       -> return tt
        TopType{}       -> return tt


---------------------------------------------------------------------------------------------------
-- | Desugar a clause.
desugarCl :: SP -> Clause -> S Clause
desugarCl _sp cc
 = case cc of
        SSig{}
         -> return cc

        SLet sp mt ps gxs
         -> do  let xError  = makeXErrorDefault
                                (Text.pack    $ SP.sourcePosSource sp)
                                (fromIntegral $ SP.sourcePosLine   sp)

                -- Desugar the inner guarded expressions.
                let xBody_inner =  desugarGXs gxs xError

                -- Recursively decend into the inner expression.
                xBody_rec       <- desugarX sp xBody_inner

                -- Strip out patterns in the parameter list and add them as new guards
                (ps', gx')      <- stripParams ps (GExp xBody_rec)

                -- Desugar the new guards that we just produced.
                let xBody_flat  =  desugarGXs [gx'] xError

                return $ SLet sp mt ps' [GExp xBody_flat]


-- | Desugar a clause group
desugarClGroup :: SP -> [Clause] -> S [Clause]
desugarClGroup _sp0 cls0
 = loop cls0 
 where
  loop []
   =    return []

  -- Skip over signatures.
  loop (cl@SSig{} : cls) 
   = do cls'    <- loop cls
        return  $  cl : cls'

  -- We have a let-clause.
  loop ( SLet sp1 (XBindVarMT b1 mt1) ps1 gxs1 : cls)
   = loop cls >>= \cls'
   -> case cls' of

        -- Consecutive clauses are for the same function.
        (SLet _sp2 (XBindVarMT b2 _mt2) ps2 [GExp xNext] : clsRest)
          | b1 == b2
          -> do  -- Desugar the inner guarded expressions.
                 let xBody_inner = desugarGXs gxs1 xNext

                 -- Recursively decend into the inner expression.
                 -- TODO: this will make it quadratic. do the recursion in desugarGXs
                 xBody_rec   <- desugarX sp1 xBody_inner

                 -- Strip out pattern in the parameter list and add them as new guards.
                 (ps1', gx') <- stripParams ps1 (GExp xBody_rec)

                 -- Flatten the guards that we've just introduced.
                 let xBody_flat = desugarGXs [gx'] 
                                $ joinClauseParams ps1' ps2 xNext

                 return $ SLet sp1 (XBindVarMT b1 mt1) ps1' [GExp xBody_flat]
                        : clsRest

        -- Consecutive clauses are not for the same function.
        _ -> do let xError  = makeXErrorDefault
                                (Text.pack    $ SP.sourcePosSource sp1)
                                (fromIntegral $ SP.sourcePosLine   sp1)

                -- Desugar the inner guarded expressions.
                let xBody_inner = desugarGXs gxs1 xError

                -- Recursively decend into the inner expression.
                -- TODO: this will make it quadratic. do the recursion in desugarGXs
                xBody_rec   <- desugarX sp1 xBody_inner

                -- Strip out pattern in the parameter list and add them as new guards.
                (ps1', gx') <- stripParams ps1 (GExp xBody_rec)

                -- Flatten the guards that we've just introduced.
                let xBody_flat = desugarGXs [gx'] xError

                return  $ SLet sp1 (XBindVarMT b1 mt1) ps1' [GExp xBody_flat]
                        : cls'


joinClauseParams :: [Param] -> [Param] -> Exp -> Exp
joinClauseParams [] _  xx = xx
joinClauseParams _  [] xx = xx
joinClauseParams (p1: ps1) (p2: ps2) xx
 = case joinClauseParam p1 p2 of
        Nothing  -> joinClauseParams ps1 ps2 xx
        Just lts -> XLet lts (joinClauseParams ps1 ps2 xx)


joinClauseParam :: Param -> Param -> Maybe Lets
joinClauseParam p1 p2
 = case (p1, p2) of
        (  MValue (PVar (BName n1)) _mt1
         , MValue (PVar (BName n2)) mt2)
         |   n1 /= n2
         ->  Just $ LLet (XBindVarMT (BName n2) mt2) (XVar (UName n1))

         |   otherwise
         ->  Nothing

        _ -> Nothing


-- | Strip out patterns in the given parameter list, 
--   wrapping the expression with new guards that implement the patterns.
stripParams :: [Param] -> GuardedExp -> S ([Param], GuardedExp)
stripParams [] gx
 = return ([], gx)

stripParams (p:ps) gxBody
 = case p of
        MType{} 
         -> do  (ps', gx) <- stripParams ps gxBody
                return (p : ps', gx)

        MWitness{} 
         -> do  (ps', gx) <- stripParams ps gxBody
                return (p : ps', gx)

        MValue PDefault   _mt
         -> do  (ps', gx) <- stripParams ps gxBody
                return (p : ps', gx)

        MValue (PVar _b) _mt
         -> do  (ps', gx) <- stripParams ps gxBody
                return (p : ps', gx)

        MValue (PData dc psData) mt
         -> do  (psParam', gxRest) <- stripParams   ps     gxBody
                (psData',  gxData) <- stripPatterns psData gxRest
                (b, u)             <- newVar "p"
                return  ( MValue (PVar b) mt : psParam'
                        , GGuard (GPat (PData dc psData') (XVar u)) gxData)


stripPatterns :: [Pat] -> GuardedExp -> S ([Pat], GuardedExp)
stripPatterns [] gx
 = return ([], gx)

stripPatterns (p:ps) gxBody
 = case p of
        PDefault
         -> do  (ps', gx) <- stripPatterns ps gxBody
                return (p : ps', gx)

        PVar  _b
         -> do  (ps', gx) <- stripPatterns ps gxBody
                return (p : ps', gx)

        PData dc psData
         -> do  (psRest', gxRest) <- stripPatterns ps     gxBody
                (psData', gxData) <- stripPatterns psData gxRest
                (b, u)            <- newVar "p"
                return  ( PVar b : psRest'
                        , GGuard (GPat (PData dc psData') (XVar u)) gxData)


---------------------------------------------------------------------------------------------------
-- | Desugar an expression.
desugarX :: SP -> Exp -> S Exp
desugarX sp xx
 = case xx of
        XAnnot sp' x    -> XAnnot sp' <$> desugarX sp' x
        XVar{}          -> pure xx
        XPrim{}         -> pure xx
        XCon{}          -> pure xx
        XLam  b x       -> XLam b     <$> pure x
        XLAM  b x       -> XLAM b     <$> pure x
        XApp  x1 x2     -> XApp       <$> desugarX   sp x1  <*> desugarX sp x2
        XLet  lts x     -> XLet       <$> desugarLts sp lts <*> desugarX sp x
        XCast c x       -> XCast c    <$> desugarX   sp x
        XType{}         -> pure xx
        XWitness{}      -> pure xx
        XDefix a xs     -> XDefix a   <$> mapM (desugarX sp)  xs
        XInfixOp{}      -> pure xx
        XInfixVar{}     -> pure xx

        XCase xScrut alts
         -- When all the alternatives are simple then we can determine
         -- which expression to evaluate just based on the head pattern,
         -- and thus can translate directly to the core-level case expressions.
         | all isSimpleAltCase alts
         -> do  xScrut' <- desugarX sp xScrut 
                alts'   <- mapM (desugarAC sp) alts
                return  $ XCase xScrut' alts'

         -- Complex alternatives are ones that have include a guard
         -- or some other pattern that may fail.
         --
         | otherwise
         -> do  xScrut' <- desugarX sp xScrut
                let xError = makeXErrorDefault
                                (Text.pack    $ SP.sourcePosSource sp)
                                (fromIntegral $ SP.sourcePosLine   sp)

                (b, u)     <- newVar "xScrut"
                let gxsAlt = concat  [ map (GGuard (GPat p (XVar u))) gxs
                                     | AAltCase p gxs <- alts]

                xFail'     <- desugarX   sp  xError
                let xMatch =  desugarGXs gxsAlt xFail'
                pure    $ XLet (LLet (XBindVarMT b Nothing) xScrut')
                        $ xMatch


        XMatch sp' as xFail
         -> do  let gxs    =  [gx | AAltMatch gx <- as]
                xFail'     <- desugarX sp' xFail
                let xMatch =  desugarGXs   gxs xFail'
                pure xMatch


-- | Check if this is simple Case alternative, which means if the pattern
--   matches then we can run the expression on the right instead of needing
--   to skip to another alternative.
isSimpleAltCase :: AltCase -> Bool
isSimpleAltCase aa
 = case aa of
        AAltCase p [GExp _]  -> isSimplePat p
        _                    -> False


-- | Simple patterns can be converted directly to core.
isSimplePat :: Pat -> Bool
isSimplePat pp
 = case pp of
        PDefault        -> True
        PVar{}          -> True
        PData _  ps     -> all isTrivialPat ps


-- | Trival patterns are the default one and variables,
--   and don't require an actual pattern to be matched.
isTrivialPat :: Pat -> Bool
isTrivialPat pp
 = case pp of
        PDefault        -> True
        PVar{}          -> True
        _               -> False


---------------------------------------------------------------------------------------------------
-- | Desugar some let bindings.
desugarLts :: SP -> Lets -> S Lets
desugarLts sp lts
 = case lts of
        LLet bm x       -> LLet bm  <$> desugarX sp x

        LRec bxs
         -> do  let (bs, xs)    = unzip bxs
                xs'     <- mapM (desugarX sp) xs
                let bxs'        = zip bs xs'
                return  $ LRec bxs'

        LPrivate{}      -> pure lts

        LGroup cs       -> LGroup <$> desugarClGroup sp cs

---------------------------------------------------------------------------------------------------
-- | Desugar a case alternative.
desugarAC :: SP -> AltCase -> S AltCase
desugarAC sp (AAltCase p gxs)
 = do   let xError  = makeXErrorDefault
                        (Text.pack    $ SP.sourcePosSource sp)
                        (fromIntegral $ SP.sourcePosLine   sp)

        let x'        =  desugarGXs gxs xError
        pure $ AAltCase p [GExp x']

{-
wrapGuards :: [Guard] -> GuardedExp -> GuardedExp
wrapGuards [] gx        = gx
wrapGuards (g : gs) gx  = GGuard g (wrapGuards gs gx)


stripPatsToGuards :: [Pat] -> S ([Pat], [Guard])

stripPatsToGuards []
 = return ([], [])

stripPatsToGuards (p:ps)
 = case p of
        PDefault
         -> do  (ps', gs) <- stripPatsToGuards ps
                return (p : ps', gs)

        PVar  _b
         -> do  (ps', gs) <- stripPatsToGuards ps
                return (p : ps', gs)

        PData dc psData
         -> do  (psRest', gsRest) <- stripPatsToGuards ps
                (psData', gsData) <- stripPatsToGuards psData
                (b, u)            <- newVar "p"
                return  (  PVar b : psRest'
                        , [GPat (PData dc psData') (XVar u)] ++ gsData ++ gsRest )
-}

---------------------------------------------------------------------------------------------------
-- | Desugar some guards to a case-expression.
--   At runtime, if none of the guards match then run the provided fail action.
desugarGXs
        :: [GuardedExp]         -- ^ Guarded expressions to desugar.
        -> Exp                  -- ^ Failure action.
        -> Exp 

desugarGXs gs0 fail0
 = go gs0 fail0
 where
        -- Desugar list of guarded expressions.
        go [] cont
         = cont

        go [g]   cont
         = go1 g cont

        go (g : gs) cont
         = go1 g (go gs cont)

        -- Desugar single guarded expression.
        go1 (GExp x1) _
         = x1

        go1 (GGuard GDefault   gs) cont
         = go1 gs cont

        -- Simple cases where we can avoid introducing the continuation.
        go1 (GGuard (GPred g1)   (GExp x1)) cont
         = XCase g1
                [ AAltCase PTrue    [GExp x1]
                , AAltCase PDefault [GExp cont] ]

        go1 (GGuard (GPat p1 g1) (GExp x1)) cont
         = XCase g1
                [ AAltCase p1        [GExp x1]
                , AAltCase PDefault  [GExp cont]]

        -- Cases that use a continuation function as a join point.
        -- We need this when desugaring general pattern alternatives,
        -- as each group of guards can be reached from multiple places.
        go1 (GGuard (GPred x1) gs) cont
         = XLet  (LLet (XBindVarMT BAnon Nothing) (XBox cont))
         $ XCase (liftX 1 x1)
                [ AAltCase PTrue    [GExp (go1 (liftX 1 gs) (XRun (XVar (UIx 0))))]
                , AAltCase PDefault [GExp                   (XRun (XVar (UIx 0))) ]]

        go1 (GGuard (GPat p1 x1) gs) cont
         = XLet  (LLet (XBindVarMT BAnon Nothing) (XBox cont))
         $ XCase (liftX 1 x1)
                [ AAltCase p1       [GExp (go1 (liftX 1 gs) (XRun (XVar (UIx 0))))]
                , AAltCase PDefault [GExp                   (XRun (XVar (UIx 0))) ]]


