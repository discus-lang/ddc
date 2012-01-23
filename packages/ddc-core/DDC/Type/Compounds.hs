{-# OPTIONS -fno-warn-missing-signatures #-}
module DDC.Type.Compounds
        ( -- * Binds
          takeNameOfBind
        , typeOfBind
        , replaceTypeOfBind
        
          -- * Binders
        , binderOfBind
        , makeBindFromBinder
        , partitionBindsByType
        
          -- * Bounds
        , typeOfBound
        , takeNameOfBound
        , replaceTypeOfBound
        , boundMatchesBind
        , takeSubstBoundOfBind

          -- * Type structure
        , tIx
        , tApp,          ($:)
        , tApps,         takeTApps
        , takeTyConApps, takeDataTyConApps
        , tForall
        , tForalls,      takeTForalls
        , tBot
        , tSum

          -- * Function type construction
        , kFun
        , kFuns,        takeKFun
        , takeKFuns,    takeKFuns',     takeResultKind
        , tFun,         takeTFun,       takeTFunArgResult
        , tFunPE
        , tImpl

          -- * Sort construction
        , sComp, sProp

          -- * Kind construction
        , kData, kRegion, kEffect, kClosure, kWitness

          -- * Effect type constructors
        , tRead,        tDeepRead,      tHeadRead
        , tWrite,       tDeepWrite
        , tAlloc,       tDeepAlloc

          -- * Closure type constructors.
        , tUse,         tDeepUse

          -- * Witness type constructors.
        , tPure
        , tEmpty
        , tGlobal,      tDeepGlobal
        , tConst,       tDeepConst
        , tMutable,     tDeepMutable
        , tLazy,        tHeadLazy
        , tManifest
        , tDistinct
        
        , tConData0,    tConData1)
where
import DDC.Type.Exp
import qualified DDC.Type.Sum   as Sum


-- Binds ----------------------------------------------------------------------
-- | Take the variable name of a bind.
--   If this is an anonymous variable then there won't be a name.
takeNameOfBind  :: Bind n -> Maybe n
takeNameOfBind bb
 = case bb of
        BName n _       -> Just n
        BAnon   _       -> Nothing
        BNone   _       -> Nothing


-- | Take the type of a bind.
typeOfBind :: Bind n -> Type n
typeOfBind bb
 = case bb of
        BName _ t       -> t
        BAnon   t       -> t
        BNone   t       -> t


-- | Replace the kind of a bind with a new one.
replaceTypeOfBind :: Type n -> Bind n -> Bind n
replaceTypeOfBind t bb
 = case bb of
        BName n _       -> BName n t
        BAnon   _       -> BAnon t
        BNone   _       -> BNone t


-- Binders --------------------------------------------------------------------
binderOfBind :: Bind n -> Binder n
binderOfBind bb
 = case bb of
        BName n _       -> RName n
        BAnon _         -> RAnon
        BNone _         -> RNone


makeBindFromBinder :: Binder n -> Type n -> Bind n
makeBindFromBinder bb t
 = case bb of
        RName n         -> BName n t
        RAnon           -> BAnon t
        RNone           -> BNone t


-- | Make lists of binds that have the same type.
partitionBindsByType :: Eq n => [Bind n] -> [([Binder n], Type n)]
partitionBindsByType [] = []
partitionBindsByType (b:bs)
 = let  t       = typeOfBind b
        bsSame  = takeWhile (\b' -> typeOfBind b' == t) bs
        rs      = map binderOfBind (b:bsSame)
   in   (rs, t) : partitionBindsByType (drop (length bsSame) bs)


-- Bounds ---------------------------------------------------------------------
-- | Take the type of a bound variable.
typeOfBound :: Bound n -> Type n
typeOfBound uu
 = case uu of
        UName _ t       -> t
        UPrim _ t       -> t
        UIx   _ t       -> t


-- | Take the variable name of bound variable.
--   If this is an anonymous variable then there won't be a name.
takeNameOfBound :: Bound n -> Maybe n
takeNameOfBound uu
 = case uu of
        UName n _       -> Just n
        UPrim n _       -> Just n
        UIx _ _         -> Nothing


-- | Replace the type of a bound with a new one.
replaceTypeOfBound :: Type n -> Bound n -> Bound n
replaceTypeOfBound t uu
 = case uu of
        UName n _       -> UName n t
        UPrim n _       -> UPrim n t
        UIx   i _       -> UIx   i t


-- | Check whether named `Bound` matches a named `Bind`.
--  
--   Yields `False` for other combinations of bounds and binds.
boundMatchesBind :: Eq n => Bound n -> Bind n -> Bool
boundMatchesBind u b
 = case (u, b) of
        (UName n1 _, BName n2 _) -> n1 == n2
        _                        -> False

-- | Convert a `Bound` to a `Bind`, ready for substitution.
--   
--   Returns `UName` for `BName`, `UIx 0` for `BAnon` 
--   and `Nothing` for `BNone`, because there's nothing to substitute.
takeSubstBoundOfBind :: Bind n -> Maybe (Bound n)
takeSubstBoundOfBind bb
 = case bb of
        BName n t       -> Just $ UName n t
        BAnon t         -> Just $ UIx 0 t
        BNone _         -> Nothing


-- Variables ------------------------------------------------------------------
tIx k i         = TVar (UIx i k)


-- Applications ---------------------------------------------------------------
tBot k          = TSum $ Sum.empty k
tApp            = TApp
($:)            = TApp


-- | Build sequence of type applications.
tApps   :: Type n -> [Type n] -> Type n
tApps t1 ts     = foldl TApp t1 ts


-- | Flatten a sequence ot type applications into the function part and
--   arguments, if any.
takeTApps   :: Type n -> [Type n]
takeTApps tt
 = case tt of
        TApp t1 t2      -> takeTApps t1 ++ [t2]
        _               -> [tt]


-- | Flatten a sequence of type applications, returning the type constructor
--   and arguments, if there is one.
takeTyConApps :: Type n -> Maybe (TyCon n, [Type n])
takeTyConApps tt
 = case takeTApps tt of
        TCon tc : args  -> Just $ (tc, args)
        _               -> Nothing


-- | Flatten a sequence of type applications, returning the type constructor
--   and arguments, if there is one. Only accept data type constructors.
takeDataTyConApps :: Type n -> Maybe (TyCon n, [Type n])
takeDataTyConApps tt
 = case takeTApps tt of
        TCon tc : args  
         | TyConBound (UName _ t)       <- tc
         , TCon (TyConKind KiConData)   <- takeResultKind t
         -> Just (tc, args)

         | TyConBound (UPrim _ t)       <- tc
         , TCon (TyConKind KiConData)   <- takeResultKind t
         -> Just (tc, args)

        _ -> Nothing


-- Foralls --------------------------------------------------------------------
-- | Build an anonymous type abstraction, with a single parameter.
tForall :: Kind n -> (Type n -> Type n) -> Type n
tForall k f
        = TForall (BAnon k) (f (TVar (UIx 0 k)))


-- | Build an anonymous type abstraction, with several parameters.
tForalls  :: [Kind n] -> ([Type n] -> Type n) -> Type n
tForalls ks f
 = let  bs      = [BAnon k | k <- ks]
        us      = reverse [TVar (UIx n  k) | k <- ks | n <- [0..]]
   in   foldr TForall (f us) bs


-- | Split nested foralls from the front of a type, 
--   or `Nothing` if there was no outer forall.
takeTForalls :: Type n -> Maybe ([Bind n], Type n)
takeTForalls tt
 = let  go bs (TForall b t) = go (b:bs) t
        go bs t             = (reverse bs, t)
   in   case go [] tt of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- Sums -----------------------------------------------------------------------
tSum :: Ord n => Kind n -> [Type n] -> Type n
tSum k ts
        = TSum (Sum.fromList k ts)


-- Function Constructors ------------------------------------------------------
-- | Construct a kind function.
kFun :: Kind n -> Kind n -> Kind n
kFun k1 k2      = ((TCon $ TyConKind KiConFun)`TApp` k1) `TApp` k2

infixr `kFun`


-- | Construct some kind functions.
kFuns :: [Kind n] -> Kind n -> Kind n
kFuns []     k1    = k1
kFuns (k:ks) k1    = k `kFun` kFuns ks k1


-- | Destruct a kind function
takeKFun :: Kind n -> Maybe (Kind n, Kind n)
takeKFun kk
 = case kk of
        TApp (TApp (TCon (TyConKind KiConFun)) k1) k2   
                -> Just (k1, k2)
        _       -> Nothing


-- | Destruct a chain of kind functions into the arguments
takeKFuns :: Kind n -> ([Kind n], Kind n)
takeKFuns kk
 = case kk of
        TApp (TApp (TCon (TyConKind KiConFun)) k1) k2
          |  (ks, k2') <- takeKFuns k2
          -> (k1 : ks, k2')

        _ -> ([], kk)


-- | Like takeKFuns, but return argument and return kinds in the same list.
takeKFuns' :: Kind n -> [Kind n]
takeKFuns' kk 
        | (ks, k1) <- takeKFuns kk
        = ks ++ [k1]


-- | Take the result kind of a kind function, or return the same kind
--   unharmed if it's not a kind function.
takeResultKind :: Kind n -> Kind n
takeResultKind kk
 = case kk of
        TApp (TApp (TCon (TyConKind KiConFun)) _) k2
                -> takeResultKind k2
        _       -> kk


-- | Construct a value type function, 
--   with the provided effect and closure.
tFun    :: Type n -> Effect n -> Closure n -> Type n -> Type n
tFun t1 eff clo t2
        = (TCon $ TyConComp TcConFun) `tApps` [t1, eff, clo, t2]

infixr `tFun`


-- | Destruct the type of a value function.
takeTFun :: Type n -> Maybe (Type n, Effect n, Closure n, Type n)
takeTFun tt
 = case tt of
        TApp (TApp (TApp (TApp (TCon (TyConComp TcConFun)) t1) eff) clo) t2
         ->  Just (t1, eff, clo, t2)
        _ -> Nothing


-- | Destruct the type of a value function, returning just the argument
--   and result types.
takeTFunArgResult :: Type n -> ([Type n], Type n)
takeTFunArgResult tt
 = case tt of
        TApp (TApp (TApp (TApp (TCon (TyConComp TcConFun)) t1) _eff) _clo) t2
          -> let (tsMore, tResult) = takeTFunArgResult t2
             in  (t1 : tsMore, tResult)

        _ -> ([], tt)


-- | Construct a pure and empty value type function.
tFunPE  :: Type n -> Type n -> Type n
tFunPE t1 t2    = tFun t1 (tBot kEffect) (tBot kClosure) t2


-- | Construct a witness implication type.
tImpl :: Type n -> Type n -> Type n
tImpl t1 t2      
        = ((TCon $ TyConWitness TwConImpl) `tApp` t1) `tApp` t2

infixr `tImpl`


-- Level 3 constructors (sorts) -----------------------------------------------
sComp           = TCon $ TyConSort SoConComp
sProp           = TCon $ TyConSort SoConProp


-- Level 2 constructors (kinds) -----------------------------------------------
kData           = TCon $ TyConKind KiConData
kRegion         = TCon $ TyConKind KiConRegion
kEffect         = TCon $ TyConKind KiConEffect
kClosure        = TCon $ TyConKind KiConClosure
kWitness        = TCon $ TyConKind KiConWitness


-- Level 1 constructors (witness and computation types) -----------------------

-- Effect type constructors
tRead           = tcCon1 TcConRead
tHeadRead       = tcCon1 TcConHeadRead
tDeepRead       = tcCon1 TcConDeepRead
tWrite          = tcCon1 TcConWrite
tDeepWrite      = tcCon1 TcConDeepWrite
tAlloc          = tcCon1 TcConAlloc
tDeepAlloc      = tcCon1 TcConDeepAlloc

-- Closure type constructors.
tUse            = tcCon1 TcConUse
tDeepUse        = tcCon1 TcConDeepUse

-- Witness type constructors.
tPure           = twCon1 TwConPure
tEmpty          = twCon1 TwConEmpty
tGlobal         = twCon1 TwConGlobal
tDeepGlobal     = twCon1 TwConDeepGlobal
tConst          = twCon1 TwConConst
tDeepConst      = twCon1 TwConDeepConst
tMutable        = twCon1 TwConMutable
tDeepMutable    = twCon1 TwConDeepMutable
tLazy           = twCon1 TwConLazy
tHeadLazy       = twCon1 TwConHeadLazy
tManifest       = twCon1 TwConManifest
tDistinct       = twConN TwConDistinct 

tcCon1 tc t  = (TCon $ TyConComp    tc) `tApp` t
twCon1 tc t  = (TCon $ TyConWitness tc) `tApp` t
twConN tc ts = (TCon $ TyConWitness   (tc (length ts))) `tApps` ts


-- | Build a nullary type constructor of the given kind.
tConData0 :: n -> Kind n -> Type n
tConData0 n k    = TCon (TyConBound (UName n k))


-- | Build a type constructor application of one argumnet.
tConData1 :: n -> Kind n -> Type n -> Type n
tConData1 n k t1 = TApp (TCon (TyConBound (UName n k))) t1


