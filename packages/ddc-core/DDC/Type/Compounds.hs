{-# OPTIONS -fno-warn-missing-signatures #-}
module DDC.Type.Compounds
        (  -- * Binds
          takeNameOfBind
        , typeOfBind
        , replaceTypeOfBind
        
          -- * Binders
        , binderOfBind
        , makeBindFromBinder
        , partitionBindsByType
        
          -- * Bounds
        , takeNameOfBound
        , boundMatchesBind
        , namedBoundMatchesBind
        , takeSubstBoundOfBind
        , takeSubstBoundsOfBinds
        , replaceTypeOfBound

          -- * Kinds
        , kFun
        , kFuns
        , takeKFun
        , takeKFuns
        , takeKFuns'
        , takeResultKind

         -- * Quantifiers
        , tForall,  tForall'
        , tForalls, tForalls'
        , takeTForalls,  eraseTForalls

          -- * Sums
        , tBot
        , tSum

          -- * Applications
        , tApp,          ($:)
        , tApps,         takeTApps
        , takeTyConApps
        , takePrimTyConApps
        , takeDataTyConApps
        , takePrimeRegion

          -- * Functions
        , tFun,         tFunOfList
        , tFunPE,       tFunOfListPE
        , tFunEC
        , takeTFun,     takeTFunEC
        , takeTFunArgResult
        , takeTFunWitArgResult
        , takeTFunAllArgResult
        , arityOfType

          -- * Suspensions
        , tSusp

          -- * Implications
        , tImpl

          -- * Units
        , tUnit

          -- * Variables
        , tIx

          -- * Sort construction
        , sComp, sProp

          -- * Kind construction
        , kData, kRegion, kEffect, kClosure, kWitness

          -- * Effect type constructors
        , tRead,        tDeepRead,      tHeadRead
        , tWrite,       tDeepWrite
        , tAlloc,       tDeepAlloc

          -- * Closure type constructors
        , tUse,         tDeepUse

          -- * Witness type constructors
        , tPure
        , tEmpty
        , tGlobal,      tDeepGlobal
        , tConst,       tDeepConst
        , tMutable,     tDeepMutable
        , tDistinct
        , tLazy,        tHeadLazy
        , tManifest
        , tConData0,    tConData1)
where
import DDC.Type.Exp
import qualified DDC.Type.Sum   as Sum


-- Binds ----------------------------------------------------------------------
-- | Take the variable name of a bind.
--   If this is an anonymous binder then there won't be a name.
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


-- | Replace the type of a bind with a new one.
replaceTypeOfBind :: Type n -> Bind n -> Bind n
replaceTypeOfBind t bb
 = case bb of
        BName n _       -> BName n t
        BAnon   _       -> BAnon t
        BNone   _       -> BNone t


-- Binders --------------------------------------------------------------------
-- | Take the binder of a bind.
binderOfBind :: Bind n -> Binder n
binderOfBind bb
 = case bb of
        BName n _       -> RName n
        BAnon _         -> RAnon
        BNone _         -> RNone


-- | Make a bind from a binder and its type.
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
-- | Take the name of bound variable.
--   If this is a deBruijn index then there won't be a name.
takeNameOfBound :: Bound n -> Maybe n
takeNameOfBound uu
 = case uu of
        UName n         -> Just n
        UPrim n _       -> Just n
        UIx{}           -> Nothing


-- | Check whether a bound maches a bind.
--    `UName`    and `BName` match if they have the same name.
--    @UIx 0 _@  and @BAnon _@ always match.
--   Yields `False` for other combinations of bounds and binds.
boundMatchesBind :: Eq n => Bound n -> Bind n -> Bool
boundMatchesBind u b
 = case (u, b) of
        (UName n1, BName n2 _)  -> n1 == n2
        (UIx 0,    BAnon _)     -> True
        _                       -> False


-- | Check whether a named bound matches a named bind. 
--   Yields `False` if they are not named or have different names.
namedBoundMatchesBind :: Eq n => Bound n -> Bind n -> Bool
namedBoundMatchesBind u b
 = case (u, b) of
        (UName n1, BName n2 _)  -> n1 == n2
        _                       -> False


-- | Convert a `Bind` to a `Bound`, ready for substitution.
--   
--   Returns `UName` for `BName`, @UIx 0@ for `BAnon` 
--   and `Nothing` for `BNone`, because there's nothing to substitute.
takeSubstBoundOfBind :: Bind n -> Maybe (Bound n)
takeSubstBoundOfBind bb
 = case bb of
        BName n _       -> Just $ UName n 
        BAnon _         -> Just $ UIx 0 
        BNone _         -> Nothing


-- | Convert some `Bind`s to `Bounds`
takeSubstBoundsOfBinds :: [Bind n] -> [Bound n]
takeSubstBoundsOfBinds bs
 = go 1 bs
 where  go _level []               = []
        go level (BName n _ : bs') = UName n           : go level bs'
        go level (BAnon _   : bs') = UIx (len - level) : go (level + 1) bs'
        go level (BNone _   : bs') =                     go level bs'

        len = length [ () | BAnon _ <- bs]


-- | If this `Bound` is a `UPrim` then replace it's embedded type with a new
--   one, otherwise return it unharmed.
replaceTypeOfBound :: Type n -> Bound n -> Bound n
replaceTypeOfBound t uu
 = case uu of
        UName{}         -> uu
        UPrim n _       -> UPrim n t
        UIx{}           -> uu


-- Variables ------------------------------------------------------------------
-- | Construct a deBruijn index.
tIx :: Kind n -> Int -> Type n
tIx _ i         = TVar (UIx i)


-- Applications ---------------------------------------------------------------
-- | Construct an empty type sum.
tBot :: Kind n -> Type n
tBot k          = TSum $ Sum.empty k


-- | Construct a type application.
tApp, ($:) :: Type n -> Type n -> Type n
tApp            = TApp
($:)            = TApp

-- | Construct a sequence of type applications.
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
--   and arguments, if there is one. Only accept primitive type constructors.
takePrimTyConApps :: Type n -> Maybe (n, [Type n])
takePrimTyConApps tt
 = case takeTApps tt of
        TCon tc : args  
         | TyConBound (UPrim n _) _     <- tc
         -> Just (n, args)

        _ -> Nothing


-- | Flatten a sequence of type applications, returning the type constructor
--   and arguments, if there is one. Only accept data type constructors.
takeDataTyConApps :: Type n -> Maybe (TyCon n, [Type n])
takeDataTyConApps tt
 = case takeTApps tt of
        TCon tc : args  
         | TyConBound (UName _) k       <- tc
         , TCon (TyConKind KiConData)   <- takeResultKind k
         -> Just (tc, args)

         | TyConBound  UPrim{}  k       <- tc
         , TCon (TyConKind KiConData)   <- takeResultKind k
         -> Just (tc, args)

        _ -> Nothing


-- | Take the prime region variable of a data type.
--   This corresponds to the region the outermost constructor is allocated into.
takePrimeRegion :: Type n -> Maybe (Type n)
takePrimeRegion tt
 = case takeTApps tt of
        TCon _ : tR@(TVar _) : _
          -> Just tR

        _ -> Nothing


-- Foralls --------------------------------------------------------------------
-- | Build an anonymous type abstraction, with a single parameter.
tForall :: Kind n -> (Type n -> Type n) -> Type n
tForall k f
        = TForall (BAnon k) (f (TVar (UIx 0)))

-- | Build an anonymous type abstraction, with a single parameter.
--   Starting the next index from the given value.
tForall' :: Int -> Kind n -> (Type n -> Type n) -> Type n
tForall' ix k f
        = TForall (BAnon k) (f (TVar (UIx ix)))


-- | Build an anonymous type abstraction, with several parameters.
--   Starting the next index from the given value.
tForalls  :: [Kind n] -> ([Type n] -> Type n) -> Type n
tForalls ks f
 = let  bs      = [BAnon k | k <- ks]
        us      = map (\i -> TVar (UIx i)) [0 .. (length ks - 1)]
   in   foldr TForall (f $ reverse us) bs


-- | Build an anonymous type abstraction, with several parameters.
--   Starting the next index from the given value.
tForalls'  :: Int -> [Kind n] -> ([Type n] -> Type n) -> Type n
tForalls' ix ks f
 = let  bs      = [BAnon k | k <- ks]
        us      = map (\i -> TVar (UIx i)) [ix .. ix + (length ks - 1)]
   in   foldr TForall (f $ reverse us) bs


-- | Split nested foralls from the front of a type, 
--   or `Nothing` if there was no outer forall.
takeTForalls :: Type n -> Maybe ([Bind n], Type n)
takeTForalls tt
 = let  go bs (TForall b t) = go (b:bs) t
        go bs t             = (reverse bs, t)
   in   case go [] tt of
         ([], _)        -> Nothing
         (bs, body)     -> Just (bs, body)


-- | Erase all `TForall` quantifiers from a type.
eraseTForalls :: Ord n => Type n -> Type n
eraseTForalls tt
 = case tt of
        TVar{}          -> tt
        TCon{}          -> tt
        TForall _ t     -> eraseTForalls t
        TApp t1 t2      -> TApp (eraseTForalls t1) (eraseTForalls t2)
        TSum ts         -> TSum $ Sum.fromList (Sum.kindOfSum ts) 
                                $ map eraseTForalls $ Sum.toList ts


-- Sums -----------------------------------------------------------------------
tSum :: Ord n => Kind n -> [Type n] -> Type n
tSum k ts
        = TSum (Sum.fromList k ts)


-- Unit -----------------------------------------------------------------------
tUnit :: Type n
tUnit           = TCon (TyConSpec TcConUnit)


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


-- | Like `takeKFuns`, but return argument and return kinds in the same list.
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


-- Function types -------------------------------------------------------------
-- | Construct a pure function type.
tFun      :: Type n -> Type n -> Type n
tFun t1 t2
        = (TCon $ TyConSpec TcConFun)  `tApps` [t1, t2]
infixr `tFun`


-- | Construct a value type function, 
--   with the provided effect and closure.
tFunEC    :: Type n -> Effect n -> Closure n -> Type n -> Type n
tFunEC t1 eff clo t2
        = (TCon $ TyConSpec TcConFunEC) `tApps` [t1, eff, clo, t2]
infixr `tFunEC`


-- | Construct a pure and empty value type function.
tFunPE  :: Type n -> Type n -> Type n
tFunPE t1 t2    = tFunEC t1 (tBot kEffect) (tBot kClosure) t2
infixr `tFunPE`


-- | Construct a pure and empty function from a list containing the 
--   parameter and return type. Yields `Nothing` if the list is empty.
tFunOfList :: [Type n] -> Maybe (Type n)
tFunOfList ts
  = case reverse ts of
        []      -> Nothing
        (t : tsArgs)       
         -> let tFuns' []             = t
                tFuns' (t' : ts')     = t' `tFun` tFuns' ts'
            in  Just $ tFuns' (reverse tsArgs)


-- | Construct a pure and empty function from a list containing the 
--   parameter and return type. Yields `Nothing` if the list is empty.
tFunOfListPE :: [Type n] -> Maybe (Type n)
tFunOfListPE ts
  = case reverse ts of
        []      -> Nothing
        (t : tsArgs)       
         -> let tFunPEs' []             = t
                tFunPEs' (t' : ts')     = t' `tFunPE` tFunPEs' ts'
            in  Just $ tFunPEs' (reverse tsArgs)


-- | Yield the argument and result type of a function type.
--   
--   Works for both `TcConFun` and `TcConFunEC`.
takeTFun :: Type n -> Maybe (Type n, Type n)
takeTFun tt
 = case tt of
        TApp (TApp (TCon (TyConSpec TcConFun)) t1) t2
         ->  Just (t1, t2)

        TApp (TApp (TApp (TApp (TCon (TyConSpec TcConFunEC)) t1) _eff) _clo) t2
         ->  Just (t1, t2)

        _ -> Nothing


-- | Yield the argument and result type of a function type.
takeTFunEC :: Type n -> Maybe (Type n, Effect n, Closure n, Type n)
takeTFunEC tt
 = case tt of
        TApp (TApp (TApp (TApp (TCon (TyConSpec TcConFunEC)) t1) eff) clo) t2
         ->  Just (t1, eff, clo, t2)

        _ -> Nothing


-- | Destruct the type of a function, returning just the argument and result types.
--
--   Works for both `TcConFun` and `TcConFunEC`.
takeTFunArgResult :: Type n -> ([Type n], Type n)
takeTFunArgResult tt
 = case tt of
        TApp (TApp (TCon (TyConSpec TcConFun)) t1) t2
         -> let (tsMore, tResult) = takeTFunArgResult t2
            in  (t1 : tsMore, tResult)

        TApp (TApp (TApp (TApp (TCon (TyConSpec TcConFunEC)) t1) _eff) _clo) t2
         -> let (tsMore, tResult) = takeTFunArgResult t2
            in  (t1 : tsMore, tResult)

        _ -> ([], tt)


-- | Destruct the type of a function,
--   returning the witness argument, value argument and result types.
--   The function type must have the witness implications before 
--   the value arguments, eg  @T1 => T2 -> T3 -> T4 -> T5@.
--
--   Works for both `TcConFun` and `TcConFunEC`.
--
takeTFunWitArgResult :: Type n -> ([Type n], [Type n], Type n)
takeTFunWitArgResult tt
 = case tt of
        TApp (TApp (TCon (TyConWitness TwConImpl)) t1) t2
         ->  let (twsMore, tvsMore, tResult) = takeTFunWitArgResult t2
             in  (t1 : twsMore, tvsMore, tResult)

        _ -> let (tvsMore, tResult)          = takeTFunArgResult tt
             in  ([], tvsMore, tResult)


-- | Destruct the type of a possibly polymorphic function
--   returning all kinds of quantifiers, witness arguments, 
--   and value arguments in the order they appear, along with 
--   the type of the result.
takeTFunAllArgResult :: Type n -> ([Type n], Type n)
takeTFunAllArgResult tt
 = case tt of
        TVar{}          -> ([], tt)
        TCon{}          -> ([], tt)

        TForall b t     
         -> let (tsMore, tResult)       = takeTFunAllArgResult t
            in  (typeOfBind b : tsMore, tResult)

        TApp (TApp (TCon (TyConSpec TcConFun)) t1) t2
         -> let (tsMore, tResult) = takeTFunAllArgResult t2
            in  (t1 : tsMore, tResult)

        TApp (TApp (TApp (TApp (TCon (TyConSpec TcConFunEC)) t1) _eff) _clo) t2
         -> let (tsMore, tResult) = takeTFunAllArgResult t2
            in  (t1 : tsMore, tResult)

        TApp (TApp (TCon (TyConWitness TwConImpl)) t1) t2
         -> let (tsMore, tResult) = takeTFunAllArgResult t2
            in  (t1 : tsMore, tResult)

        _ -> ([], tt)


-- | Determine the arity of an expression by looking at its type.
--   Count all the function arrows, and foralls.
arityOfType :: Type n -> Int
arityOfType tt
 = case tt of
        TForall _ t     -> 1 + arityOfType t
        t               -> length $ fst $ takeTFunArgResult t


-- Implications ---------------------------------------------------------------
-- | Construct a witness implication type.
tImpl :: Type n -> Type n -> Type n
tImpl t1 t2      
        = ((TCon $ TyConWitness TwConImpl) `tApp` t1) `tApp` t2
infixr `tImpl`


-- Suspensions ----------------------------------------------------------------
tSusp  :: Effect n -> Type n -> Type n
tSusp tE tA
        = (TCon $ TyConSpec TcConSusp) `tApp` tE `tApp` tA


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
tDistinct n     = twCon2 (TwConDistinct n)
tLazy           = twCon1 TwConLazy
tHeadLazy       = twCon1 TwConHeadLazy
tManifest       = twCon1 TwConManifest

tcCon1 tc t     = (TCon $ TyConSpec    tc) `tApp` t
twCon1 tc t     = (TCon $ TyConWitness tc) `tApp` t

twCon2 tc ts    = tApps (TCon $ TyConWitness tc) ts


-- | Build a nullary type constructor of the given kind.
tConData0 :: n -> Kind n -> Type n
tConData0 n k   = TCon (TyConBound (UName n) k)


-- | Build a type constructor application of one argumnet.
tConData1 :: n -> Kind n -> Type n -> Type n
tConData1 n k t1 = TApp (TCon (TyConBound (UName n) k)) t1

