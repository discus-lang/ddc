{-# LANGUAGE TypeFamilies, OverloadedStrings #-}

-- | Desugar guards and nested patterns to match expressions.
module DDC.Source.Tetra.Transform.Guards
        ( type S, evalState, newVar
        , desugarModule)
where
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Prim
import DDC.Source.Tetra.Exp
import Data.Monoid
import Data.Text                        (Text)
import Control.Monad
import qualified DDC.Data.SourcePos     as SP
import qualified Control.Monad.State    as S
import qualified Data.Text              as Text


-------------------------------------------------------------------------------
-- | Desugar guards and nested patterns to match expressions.
desugarModule :: Module Source -> S (Module Source)
desugarModule mm
 = do   ts'     <- mapM desugarTop $ moduleTops mm
        return  $ mm { moduleTops = ts' }


-------------------------------------------------------------------------------
-- | Desugar a top-level thing.
desugarTop    :: Top Source -> S (Top Source)
desugarTop tt
 = case tt of
        TopClause sp c  -> TopClause sp <$> desugarCl sp c
        TopData{}       -> return tt
        TopType{}       -> return tt


-------------------------------------------------------------------------------
-- | Desugar a clause.
desugarCl :: SP -> Clause -> S Clause
desugarCl _sp cc
 = case cc of
        SSig{}
         -> return cc

        SLet sp mt ps gxs
         -> do  (ps', gsParam) <- stripParamsToGuards ps
                gxs'    <- mapM (desugarGX sp >=> (return . cleanGX))
                        $  map  (wrapGuards gsParam) gxs

                return $ SLet sp mt ps' gxs'


-------------------------------------------------------------------------------
-- | Desugar an expression.
desugarX :: SP -> Exp -> S Exp
desugarX sp xx
 = case xx of
        -- Boilerplate.
        XAnnot sp' x    -> XAnnot sp' <$> desugarX sp' x
        XPrim{}         -> pure xx
        XFrag{}         -> pure xx
        XVar{}          -> pure xx
        XCon{}          -> pure xx
        XApp  x1 r2     -> XApp       <$> desugarX   sp x1  <*> desugarArg sp r2
        XLet  lts x     -> XLet       <$> desugarLts sp lts <*> desugarX sp x
        XCast c x       -> XCast c    <$> desugarX   sp x
        XDefix a xs     -> XDefix a   <$> mapM (desugarArg sp)  xs
        XInfixOp{}      -> pure xx
        XInfixVar{}     -> pure xx
        XWhere a x cls  -> XWhere a   <$> desugarX sp x
                                      <*> mapM (desugarCl sp) cls

        -- Desguar patterns in a term abstraction.
        XAbs bParam xBody
         -> case bParam of
                MTerm PDefault _
                 -> XAbs bParam <$> desugarX sp xBody

                MTerm PVar{} _
                 -> XAbs bParam <$> desugarX sp xBody

                MTerm pat mtBind
                  -> do (b, u)  <- newVar "scrut"
                        xBody'  <- desugarX sp
                                $  XCase (XVar u) [AAltCase pat [GExp xBody]]

                        return  $ XAbs (MTerm (PVar b) mtBind) xBody'

                _ -> XAbs bParam <$> desugarX sp xBody


        -- Desugar a case expression.
        XCase xScrut alts
         -- Simple alternatives are ones where we can determine whether they
         -- match just based on the head pattern. If all the alternatives
         -- in a case-expression are simple then we can convert directly
         -- to core-level case expressions.
         | all isSimpleAltCase alts
         -> do  xScrut' <- desugarX sp xScrut
                alts'   <- mapM (desugarAltCase sp) alts
                return  $ XAnnot sp
                        $ XCase xScrut' alts'

         -- Complex alternatives are ones that have include a guard or some
         -- other pattern that may fail, and require us to skip to the next
         -- alternatives. These are compiled as per match expressions.
         | otherwise
         -> do  -- Desugar the scrutinee.
                xScrut' <- desugarX sp xScrut

                -- We bind the scrutinee to a new variable so we can
                -- defer to it multiple times in the body of the match.
                (b, u)  <- newVar "xScrut"

                -- At the start of each guarded expression we match against
                -- the pattern from the original case alternative.
                gxsAlt' <- mapM (desugarGX sp >=> (return . cleanGX))
                        $  concat [ map (GGuard (GPat p (XVar u))) gxs
                                  | AAltCase p gxs <- alts]

                -- Desugar the body of each alternative.
                alts'   <- mapM (desugarAltMatch sp)
                        $  [AAltMatch gx | gx <- gxsAlt']

                -- Result contains a let-binding to bind the scrutinee,
                -- then a match expression that implements the complex
                -- case alternatives.
                pure    $ XAnnot sp
                        $ XLet (LLet (XBindVarMT b Nothing) xScrut')
                        $ XMatch sp alts'
                        $ makeXErrorDefault
                                (Text.pack    $ SP.sourcePosSource sp)
                                (fromIntegral $ SP.sourcePosLine   sp)

        -- Desugar a match expression from the source code.
        XMatch sp' alts xFail
         -> do  alts'     <- mapM (desugarAltMatch sp') alts
                xFail'    <- desugarX sp' xFail
                pure    $ XMatch sp' alts' xFail'


        -- Desugar lambda with a pattern for the parameter.
        XAbsPat _a MSTerm     PDefault mt x
         -> XAnnot sp <$> XAbs  (MTerm      PDefault mt) <$> desugarX sp x

        XAbsPat _a MSTerm     (PVar b) mt x
         -> XAnnot sp <$> XAbs  (MTerm      (PVar b) mt) <$> desugarX sp x

        XAbsPat _a MSImplicit PDefault mt x
         -> XAnnot sp <$> XAbs  (MImplicit  PDefault mt) <$> desugarX sp x

        XAbsPat _a MSImplicit (PVar b) mt x
         -> XAnnot sp <$> XAbs  (MImplicit  (PVar b) mt) <$> desugarX sp x

        XAbsPat _a ps p mt x
         -> do  (b, u)  <- newVar "xScrut"
                x'      <- desugarX sp x
                case ps of
                 MSType
                  -> return xx

                 MSTerm
                  -> desugarX sp
                        $ XAnnot sp
                        $ XAbs  (MTerm (PVar b) mt)
                        $ XCase (XVar u) [ AAltCase p [GExp x'] ]

                 MSImplicit
                  -> desugarX sp
                        $ XAnnot sp
                        $ XAbs  (MImplicit (PVar b) mt)
                        $ XCase (XVar u) [ AAltCase p [GExp x'] ]


        -- Desugar lambda case by inserting the intermediate variable.
        XLamCase _a alts
         -> do  (b, u)  <- newVar "x"
                alts'   <- mapM  (desugarAltCase sp) alts
                desugarX sp
                        $  XAnnot sp
                        $  XAbs  (MTerm (PVar b) Nothing)
                        $  XCase (XVar u) alts'


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
        PAt{}           -> False
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


-------------------------------------------------------------------------------
-- | Desguar an argument.
desugarArg :: SP -> Arg  -> S Arg
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
        LLet bm x       -> LLet bm  <$> desugarX sp x

        LRec bxs
         -> do  let (bs, xs)    = unzip bxs
                xs'     <- mapM (desugarX sp) xs
                let bxs'        = zip bs xs'
                return  $ LRec bxs'

        LPrivate{}      -> pure lts

        LGroup bRec cs  -> LGroup bRec <$> mapM (desugarCl sp) cs


-------------------------------------------------------------------------------
-- | Desugar a guarded expression.
desugarGX :: SP -> GuardedExp -> S GuardedExp
desugarGX sp gx
 = case gx of
        GGuard (GPat p x) gxInner
         -> do  x'        <- desugarX sp x
                (g', gs') <- stripGuardToGuards (GPat p x')
                gxInner'  <- desugarGX sp gxInner
                return  $ GGuard g'
                        $ wrapGuards gs' gxInner'

        GGuard g gx'
         -> GGuard <$> desugarG sp g <*> desugarGX sp gx'

        GExp x
         -> GExp   <$> desugarX sp x


-- | Desugar a guard.
desugarG :: SP -> Guard -> S Guard
desugarG sp g
 = case g of
        GPat p x        -> GPat p <$> desugarX sp x
        GPred x         -> GPred  <$> desugarX sp x
        GDefault        -> pure GDefault


-------------------------------------------------------------------------------
-- | Desugar a case alternative.
desugarAltCase :: SP -> AltCase -> S AltCase
desugarAltCase sp (AAltCase p gxs)
 = do   gxs' <- mapM (desugarGX sp >=> (return . cleanGX)) gxs
        pure $ AAltCase p gxs'


-- | Desugar a match alternative.
desugarAltMatch :: SP -> AltMatch -> S AltMatch
desugarAltMatch sp (AAltMatch gx)
 = do   gx'  <- (desugarGX sp >=> (return . cleanGX)) gx
        pure $ AAltMatch gx'


-------------------------------------------------------------------------------
-- | Strip out patterns in the given parameter list,
--   yielding a list of guards that implement the patterns.
stripParamsToGuards :: [Param] -> S ([Param], [Guard])
stripParamsToGuards []
 = return ([], [])

stripParamsToGuards (p:ps)
 = case p of
        MType{}
         -> do  (ps', gs) <- stripParamsToGuards ps
                return (p : ps', gs)

        MTerm b mt
         -> stripValue MTerm     b mt

        MImplicit b mt
         -> stripValue MImplicit b mt

 where stripValue make p' mt
        = case p' of
           PDefault
            -> do (ps', gs) <- stripParamsToGuards ps
                  return (p : ps', gs)

           PVar _b
            -> do (ps', gs) <- stripParamsToGuards ps
                  return (p : ps', gs)

           PAt b p1
            -> do (psParam', gsRest) <- stripParamsToGuards ps
                  ([p1'],    gsData) <- stripPatsToGuards  [p1]
                  let Just u         = takeBoundOfBind b
                  return  ( make (PVar b) mt : psParam'
                          , GPat p1' (XVar u)
                                : (gsData ++ gsRest))

           PData dc psData
            -> do (psParam', gsRest) <- stripParamsToGuards ps
                  (psData',  gsData) <- stripPatsToGuards   psData
                  (b, u)             <- newVar "p"
                  return  ( make (PVar b) mt : psParam'
                          , GPat (PData dc psData') (XVar u)
                                : (gsData ++ gsRest))


-- | Strip out nested patterns from the given pattern list,
--   yielding a list of guards that implement the patterns.
stripPatsToGuards :: [Pat] -> S ([Pat], [Guard])
stripPatsToGuards []
 = return ([], [])

stripPatsToGuards (p:ps)
 = case p of
        -- Match against defaults directly.
        PDefault
         -> do  (ps', gs) <- stripPatsToGuards ps
                return (p : ps', gs)

        -- Match against vars directly.
        PVar  _b
         -> do  (ps', gs) <- stripPatsToGuards ps
                return (p : ps', gs)

        -- Strip at patterns.
        PAt b p1
         -> do  -- Strip the rest of the patterns.
                (psRest', gsRest)   <- stripPatsToGuards ps

                -- Strip nested patterns from the argument.
                ([p1'],     gsData) <- stripPatsToGuards [p1]
                let Just u      = takeBoundOfBind b

                return  ( PVar b : psRest'
                        , GPat p1' (XVar u)
                                : (gsData ++ gsRest))

        -- Strip out nested patterns in the arguments of a data constructor.
        PData dc psData
         -> do  -- Strip the rest of the patterns.
                (psRest', gsRest) <- stripPatsToGuards ps

                -- Strip nested patterns out of the arguments.
                (psData', gsData) <- stripPatsToGuards psData

                -- Make a new name to bind the value we are matching against.
                (b, u)            <- newVar "p"
                return  ( PVar b : psRest'
                        , GPat (PData dc psData') (XVar u)
                                 : (gsData ++ gsRest) )


-- | Like `stripPatsToGuards` but we take the whole enclosing guards.
--   This gives us access to the expression being scrutinised,
--   which we can match against directly without introducing a new variable.
stripGuardToGuards :: Guard -> S (Guard, [Guard])
stripGuardToGuards g
 = case g of
        -- Match against defaults and vars directly.
        GPat PDefault _ -> return (g, [])
        GPat PVar{} _   -> return (g, [])

        -- As we alerady have the expression being matched we don't
        -- need to introduce a new variable to name it.
        GPat (PAt b p) x
         -> do  ([p'], gsData) <- stripPatsToGuards [p]
                let Just u      = takeBoundOfBind b
                return  ( GPat (PVar b) x
                        , GPat p' (XVar u) : gsData)

        GPat (PData dc psData) x
         -> do  (psData', gsData)  <- stripPatsToGuards psData
                return  ( GPat (PData dc psData') x
                        , gsData)

        GPred{}         -> return (g, [])
        GDefault{}      -> return (g, [])


-- | Wrap more guards around the outside of a guarded expression.
wrapGuards :: [Guard] -> GuardedExp -> GuardedExp
wrapGuards [] gx        = gx
wrapGuards (g : gs) gx  = GGuard g (wrapGuards gs gx)


-- | Clean out default patterns from a guarded expression.
--
--   We end up with default patterns in guards when desugaring default
--   alternatives, but they serve no purpose in the desugared code.
cleanGX :: GuardedExp -> GuardedExp
cleanGX gx
 = case gx of
        GGuard GDefault gx'     -> cleanGX gx'
        GGuard g        gx'     -> GGuard g $ cleanGX gx'
        GExp   x                -> GExp x


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


