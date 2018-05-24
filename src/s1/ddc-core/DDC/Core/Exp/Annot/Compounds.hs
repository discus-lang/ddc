{-# OPTIONS_HADDOCK hide #-}
-- | Utilities for constructing and destructing compound expressions.
--
--   For the annotated version of the AST.
--
module DDC.Core.Exp.Annot.Compounds
        ( module DDC.Type.Exp.Simple.Compounds

          -- * Annotations
        , annotOfExp
        , mapAnnotOfExp

          -- * Lambdas
        , xLAMs
        , xLams
        , makeXLamFlags
        , takeXLAMs
        , takeXLams
        , takeXLamFlags

          -- ** Parameters
        , bindOfParam
        , typeOfParam
        , replaceTypeOfParam
        , ParamTVB(..)
        , takeXLamParamTVB
        , splitParamOfType
        , makeTFunParams

          -- * Applications
        , xApps
        , makeXAppsWithAnnots
        , takeXApps
        , takeXApps1
        , takeXAppsAsList
        , takeXAppsWithAnnots
        , takeXConApps
        , takeXPrimApps
        , takeXNameApps

          -- ** Arguments
        , takeRType
        , takeRTerm
        , takeRWitness
        , takeRImplicit
        , takeExpFromArg
        , takeExpsFromArgs

          -- * Lets
        , xLets,               xLetsAnnot
        , splitXLets,          splitXLetsAnnot
        , bindsOfLets
        , specBindsOfLets
        , valwitBindsOfLets

          -- * Alternatives
        , patOfAlt
        , takeDaConOfAlt

          -- * Patterns
        , bindsOfPat

          -- * Casts
        , makeRuns

          -- * Witnesses
        , wApp
        , wApps
        , annotOfWitness
        , takeWAppsAsList
        , takePrimWiConApps

          -- * Data Constructors
        , xUnit, dcUnit
        , takeNameOfDaConPrim
        , takeNameOfDaConBound
        , takeBaseCtorNameOfDaCon

          -- * Bound Variables
        , takeBoundOfExp
        , takeNameOfExp)
where
import DDC.Core.Exp.Annot.Exp
import DDC.Core.Exp.DaCon
import DDC.Type.Exp.Simple.Compounds
import Data.Maybe (catMaybes)


-- Annotations ----------------------------------------------------------------
-- | Take the outermost annotation from an expression.
annotOfExp :: Exp a n -> a
annotOfExp xx
 = case xx of
        XVar    a _     -> a
        XAbs    a _ _   -> a
        XApp    a _ _   -> a
        XLet    a _ _   -> a
        XAtom   a _     -> a
        XCase   a _ _   -> a
        XCast   a _ _   -> a
        XAsync  a _ _ _ -> a

-- | Apply a function to the annotation of an expression.
mapAnnotOfExp :: (a -> a) -> Exp a n -> Exp a n
mapAnnotOfExp f xx
 = case xx of
        XVar    a u         -> XVar   (f a) u
        XAbs    a b  x      -> XAbs   (f a) b  x
        XApp    a x1 x2     -> XApp   (f a) x1 x2
        XLet    a lt x      -> XLet   (f a) lt x
        XAtom   a t         -> XAtom  (f a) t
        XCase   a x  as     -> XCase  (f a) x  as
        XCast   a c  x      -> XCast  (f a) c  x
        XAsync  a v  e1 e2  -> XAsync (f a) v  e1 e2


-- Lambdas ---------------------------------------------------------------------
-- | Make some nested type lambdas.
xLAMs :: a -> [Bind n] -> Exp a n -> Exp a n
xLAMs a bs x
        = foldr (XLAM a) x bs


-- | Make some nested value or witness lambdas.
xLams :: a -> [Bind n] -> Exp a n -> Exp a n
xLams a bs x
        = foldr (XLam a) x bs


-- | Split type lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLAMs :: Exp a n -> Maybe ([Bind n], Exp a n)
takeXLAMs xx
 = let  go bs (XLAM _ b x) = go (b:bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Split nested value or witness lambdas from the front of an expression,
--   or `Nothing` if there aren't any.
takeXLams :: Exp a n -> Maybe ([Bind n], Exp a n)
takeXLams xx
 = let  go bs (XLam _ b x) = go (b:bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Make some nested lambda abstractions,
--   using a flag to indicate whether the lambda is a
--   level-1 (True), or level-0 (False) binder.
makeXLamFlags :: a -> [(Bool, Bind n)] -> Exp a n -> Exp a n
makeXLamFlags a fbs x
 = foldr (\(f, b) x'
           -> if f then XLAM a b x'
                   else XLam a b x')
                x fbs


-- | Split nested lambdas from the front of an expression,
--   with a flag indicating whether the lambda was a level-1 (True),
--   or level-0 (False) binder.
takeXLamFlags :: Exp a n -> Maybe ([(Bool, Bind n)], Exp a n)
takeXLamFlags xx
 = let  go bs (XLAM _ b x) = go ((True,  b):bs) x
        go bs (XLam _ b x) = go ((False, b):bs) x
        go bs x            = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- Parameters -----------------------------------------------------------------
-- | Take the binder of a parameter.
bindOfParam :: Param n -> Bind n
bindOfParam mm
 = case mm of
        MType b         -> b
        MTerm b         -> b
        MImplicit b     -> b


-- | Take the type of a parameter.
typeOfParam :: Param n -> Type n
typeOfParam mm
        = typeOfBind $ bindOfParam mm


-- | Replace the type of a parameter.
replaceTypeOfParam :: Type n -> Param n -> Param n
replaceTypeOfParam t mm
 = case mm of
        MType b         -> MType     $ replaceTypeOfBind t b
        MTerm b         -> MTerm     $ replaceTypeOfBind t b
        MImplicit b     -> MImplicit $ replaceTypeOfBind t b


-- | Parameters of a function.
data ParamTVB n
        = ParamType  (Bind n)
        | ParamValue (Bind n)
        | ParamBox
        deriving Show


-- | Take the parameters of a function.
takeXLamParamTVB :: Exp a n -> Maybe ([ParamTVB n], Exp a n)
takeXLamParamTVB xx
 = let  go bs (XLAM  _ b x)       = go (ParamType  b : bs) x
        go bs (XLam  _ b x)       = go (ParamValue b : bs) x
        go bs (XCast _ CastBox x) = go (ParamBox     : bs) x
        go bs x                   = (reverse bs, x)
   in   case go [] xx of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Given the type of an abstraction,
--   determine how the abstraction parameterises its body.
splitParamOfType
        :: Type n
        -> Maybe (ParamSort, ParamMode, Bind n, Type n)

splitParamOfType tt
 = case tt of
        TApp (TApp (TCon tycon) t1) t2
         -> case tycon of
                TyConSpec    TcConFunExplicit
                  -> Just (ParamSortTerm,    ParamModeExplicit, BNone t1, t2)

                TyConSpec    TcConFunImplicit
                  -> Just (ParamSortTerm,    ParamModeImplicit, BNone t1, t2)

                TyConWitness TwConImpl
                  -> Just (ParamSortWitness, ParamModeExplicit, BNone t1, t2)

                _ -> Nothing

        TForall b t2
         -> Just (ParamSortType, ParamModeElaborate, b, t2)

        _ -> Nothing


-- | Construct a function type from a list of parameter types and the
--   return type.
makeTFunParams :: [Param n] -> Type n -> Type n
makeTFunParams msParam tResult
 = tFuns' msParam
 where
        tFuns' []
         = tResult

        tFuns' (m : ms)
         = case m of
                MType b
                 ->  TForall b (tFuns' ms)

                MTerm b
                 -> (TCon $ TyConSpec TcConFunExplicit)
                        `tApps` [typeOfBind b, tFuns' ms]

                MImplicit b
                 -> (TCon $ TyConSpec TcConFunImplicit)
                        `tApps` [typeOfBind b, tFuns' ms]


-- Applications ---------------------------------------------------------------
-- | Build sequence of value applications.
xApps   :: a -> Exp a n -> [Arg a n] -> Exp a n
xApps a t1 ts     = foldl (XApp a) t1 ts


-- | Build sequence of applications.
--   Similar to `xApps` but also takes list of annotations for
--   the `XApp` constructors.
makeXAppsWithAnnots :: Exp a n -> [(Arg a n, a)] -> Exp a n
makeXAppsWithAnnots f xas
 = case xas of
        []              -> f
        (arg, a) : as   -> makeXAppsWithAnnots (XApp a f arg) as


-- | Flatten an application into the function part and its arguments.
--
--   Returns `Nothing` if there is no outer application.
takeXApps :: Exp a n -> Maybe (Exp a n, [Arg a n])
takeXApps xx
 = case takeXAppsAsList xx of
        (_,  [])        -> Nothing
        (x1, as)        -> Just (x1, as)


-- | Flatten an application into the function part and its arguments, if any.
takeXAppsAsList :: Exp a n -> (Exp a n, [Arg a n])
takeXAppsAsList xx
 = case xx of
        XApp _ x1 x2
         -> let (f', args') = takeXAppsAsList x1
            in  (f', args' ++ [x2])

        _ -> (xx, [])


-- | Flatten an application into the function part and its arguments.
--
--   This is like `takeXApps` above, except we know there is at least one argument.
takeXApps1 :: Exp a n -> Arg a n -> (Exp a n, [Arg a n])
takeXApps1 x1 x2
 = case takeXApps x1 of
        Nothing          -> (x1,  [x2])
        Just (x11, x12s) -> (x11, x12s ++ [x2])


-- | Destruct sequence of applications.
--   Similar to `takeXAppsAsList` but also keeps annotations for later.
takeXAppsWithAnnots :: Exp a n -> (Exp a n, [(Arg a n, a)])
takeXAppsWithAnnots xx
 = case xx of
        XApp a f arg
         -> let (f', args') = takeXAppsWithAnnots f
            in  (f', args' ++ [(arg,a)])

        _ -> (xx, [])


-- | Flatten an application of an ambient primitive into the primitive name
--   and its arguments.
--
--   Returns `Nothing` if the expression isn't a primitive application.
takeXPrimApps :: Exp a n -> Maybe (Prim, [Arg a n])
takeXPrimApps xx
 = case takeXApps xx of
        Just (XPrim _ p, as)    -> Just (p, as)
        _                       -> Nothing


-- | Flatten an application of a named function into the variable
--   and its arguments.
--
--   Returns `Nothing` if the expression isn't such an application.
takeXNameApps :: Exp a n -> Maybe (n, [Arg a n])
takeXNameApps xx
 = case takeXApps xx of
        Just (XVar _ (UName p), as)   -> Just (p, as)
        _                             -> Nothing

-- | Flatten an application of a data constructor into the constructor
--   and its arguments.
--
--   Returns `Nothing` if the expression isn't a constructor application.
takeXConApps :: Exp a n -> Maybe (DaCon n (Type n), [Arg a n])
takeXConApps xx
 = case takeXApps xx of
        Just (XCon _ dc, as)            -> Just (dc, as)
        _                               -> Nothing


-- Arguments ------------------------------------------------------------------
-- | Take the type of a type argument, if it is one.
takeRType :: Arg a n -> Maybe (Type n)
takeRType aa
 = case aa of
        RType t         -> Just t
        _               -> Nothing


-- | Take a witness from an argument, if it is one.
takeRWitness :: Arg a n -> Maybe (Witness a n)
takeRWitness aa
 = case aa of
        RWitness w      -> Just w
        _               -> Nothing


-- | Take a witness from an argument, if it is one.
takeRTerm :: Arg a n -> Maybe (Exp a n)
takeRTerm aa
 = case aa of
        RTerm x         -> Just x
        _               -> Nothing


-- | Take a witness from an argument, if it is one.
takeRImplicit :: Arg a n -> Maybe (Arg a n)
takeRImplicit aa
 = case aa of
        RImplicit x     -> Just x
        _               -> Nothing


-- | Take the expression from a `RTerm` or `RImplicit argument.
takeExpFromArg :: Arg a n -> Maybe (Exp a n)
takeExpFromArg aa
 = case aa of
        RTerm x         -> Just x
        RImplicit a     -> takeExpFromArg a
        _               -> Nothing


-- | Take any expression arguments
takeExpsFromArgs :: [Arg a n] -> [Exp a n]
takeExpsFromArgs
 = catMaybes . map takeExpFromArg

-- Lets -----------------------------------------------------------------------
-- | Wrap some let-bindings around an expression.
xLets :: a -> [Lets a n] -> Exp a n -> Exp a n
xLets a lts x
 = foldr (XLet a) x lts


-- | Wrap some let-bindings around an expression, with individual annotations.
xLetsAnnot :: [(Lets a n, a)] -> Exp a n -> Exp a n
xLetsAnnot lts x
 = foldr (\(l, a) x' -> XLet a l x') x lts


-- | Split let-bindings from the front of an expression, if any.
splitXLets :: Exp a n -> ([Lets a n], Exp a n)
splitXLets xx
 = case xx of
        XLet _ lts x
         -> let (lts', x')      = splitXLets x
            in  (lts : lts', x')

        _ -> ([], xx)

-- | Split let-bindings from the front of an expression, with annotations.
splitXLetsAnnot :: Exp a n -> ([(Lets a n, a)], Exp a n)
splitXLetsAnnot xx
 = case xx of
        XLet a lts x
         -> let (lts', x')              = splitXLetsAnnot x
            in  ((lts, a) : lts', x')

        _ -> ([], xx)

-- | Take the binds of a `Lets`.
--
--   The level-1 and level-0 binders are returned separately.
bindsOfLets :: Lets a n -> ([Bind n], [Bind n])
bindsOfLets ll
 = case ll of
        LLet b _          -> ([],  [b])
        LRec bxs          -> ([],  map fst bxs)
        LPrivate bs _ bbs -> (bs, bbs)


-- | Like `bindsOfLets` but only take the spec (level-1) binders.
specBindsOfLets :: Lets a n -> [Bind n]
specBindsOfLets ll
 = case ll of
        LLet _ _        -> []
        LRec _          -> []
        LPrivate bs _ _ -> bs


-- | Like `bindsOfLets` but only take the value and witness (level-0) binders.
valwitBindsOfLets :: Lets a n -> [Bind n]
valwitBindsOfLets ll
 = case ll of
        LLet b _        -> [b]
        LRec bxs        -> map fst bxs
        LPrivate _ _ bs -> bs


-- Alternatives ---------------------------------------------------------------
-- | Take the pattern of an alternative.
patOfAlt :: Alt a n -> Pat n
patOfAlt (AAlt pat _)   = pat


-- | Take the constructor name of an alternative, if there is one.
takeDaConOfAlt :: Alt a n -> Maybe (DaCon n (Type n))
takeDaConOfAlt aa
 = case aa of
        AAlt (PData dc _) _     -> Just dc
        _                       -> Nothing


-- Patterns -------------------------------------------------------------------
-- | Take the binds of a `Pat`.
bindsOfPat :: Pat n -> [Bind n]
bindsOfPat pp
 = case pp of
        PDefault          -> []
        PData _ bs        -> bs


-- Casts ----------------------------------------------------------------------
-- | Wrap an expression in the given number of 'run' casts.
makeRuns :: a -> Int -> Exp a n -> Exp a n
makeRuns _a 0 x = x
makeRuns a n x  = XCast a CastRun (makeRuns a (n - 1) x)


-- Witnesses ------------------------------------------------------------------
-- | Construct a witness application
wApp :: a -> Witness a n -> Witness a n -> Witness a n
wApp = WApp


-- | Construct a sequence of witness applications
wApps :: a -> Witness a n -> [Witness a n] -> Witness a n
wApps a = foldl (wApp a)


-- | Take the annotation from a witness.
annotOfWitness :: Witness a n -> a
annotOfWitness ww
 = case ww of
        WVar  a _       -> a
        WCon  a _       -> a
        WApp  a _ _     -> a
        WType a _       -> a


-- | Flatten an application into the function parts and arguments, if any.
takeWAppsAsList :: Witness a n -> [Witness a n]
takeWAppsAsList ww
 = case ww of
        WApp _ w1 w2 -> takeWAppsAsList w1 ++ [w2]
        _          -> [ww]


-- | Flatten an application of a witness into the witness constructor
--   name and its arguments.
--
--   Returns nothing if there is no witness constructor in head position.
takePrimWiConApps :: Witness a n -> Maybe (n, [Witness a n])
takePrimWiConApps ww
 = case takeWAppsAsList ww of
        WCon _ wc : args | WiConBound (UName n) _ <- wc
          -> Just (n, args)
        _ -> Nothing


-- Units -----------------------------------------------------------------------
-- | Construct a value of unit type.
xUnit   :: a -> Exp a n
xUnit a = XCon a dcUnit


-- Bound Variables -------------------------------------------------------------
-- | Pull a variable out of an expression
takeBoundOfExp :: Exp a n -> Maybe (Bound n)
takeBoundOfExp xx
 = case xx of
        -- Should this look through casts?
        XVar _ b -> Just b
        _        -> Nothing

-- | Extract user variable out of an expression
takeNameOfExp :: Exp a n -> Maybe n
takeNameOfExp xx
 = takeBoundOfExp xx >>= takeNameOfBound

