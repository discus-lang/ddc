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
                (ps', gx')      <- stripPatterns ps xBody_rec

                -- Desugar the new guards that we just produced.
                let xBody_flat  =  desugarGXs [gx'] xError

                return $ SLet sp mt ps' [GExp xBody_flat]


-- | Strip out patterns in the given parameter list, 
--   wrapping the expression with new guards that implement the patterns.
stripPatterns :: [Param] -> Exp -> S ([Param], GuardedExp)

stripPatterns [] x
 = return ([], GExp x)

stripPatterns (p:ps) xBody
 = case p of
        MType{} 
         -> do  (ps', gx)       <- stripPatterns ps xBody
                return (p : ps', gx)

        MWitness{} 
         -> do  (ps', gx)       <- stripPatterns ps xBody
                return (p : ps', gx)

        MValue PDefault _mt
         -> do  (ps', gx)       <- stripPatterns ps xBody
                return (p : ps', gx)

        MValue (PVar _b) _mt
         -> do  (ps', gx)       <- stripPatterns ps xBody
                return (p : ps', gx)

        MValue (PData dc bs) mt
         -> do  (ps', gx)       <- stripPatterns ps xBody
                (b, u)          <- newVar "p"
                return  ( MValue (PVar b) mt : ps'
                        , GGuard (GPat (PData dc bs) (XVar u)) gx)


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
        XCase x as      -> XCase      <$> desugarX   sp x   <*> mapM (desugarAC sp) as
        XCast c x       -> XCast c    <$> desugarX   sp x
        XType{}         -> pure xx
        XWitness{}      -> pure xx
        XDefix a xs     -> XDefix a   <$> mapM (desugarX sp)  xs
        XInfixOp{}      -> pure xx
        XInfixVar{}     -> pure xx

        XMatch sp' as xFail
         -> do  let gxs    =  [gx | AAltMatch gx <- as]
                xFail'     <- desugarX sp' xFail
                let xMatch =  desugarGXs   gxs xFail'
                pure xMatch


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

        LGroup cs       -> LGroup <$> mapM (desugarCl sp) cs


---------------------------------------------------------------------------------------------------
-- | Desugar a case alternative.
desugarAC :: SP -> AltCase -> S AltCase
desugarAC sp (AAltCase p gxs)
 = do   let xError  = makeXErrorDefault
                        (Text.pack    $ SP.sourcePosSource sp)
                        (fromIntegral $ SP.sourcePosLine   sp)

        let x'  =  desugarGXs gxs xError
        x''     <- desugarX sp x'

        pure $ AAltCase p [GExp x'']


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


