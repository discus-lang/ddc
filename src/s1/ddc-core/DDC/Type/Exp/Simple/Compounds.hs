{-# OPTIONS -fno-warn-missing-signatures #-}
module DDC.Type.Exp.Simple.Compounds
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

          -- * Kinds
        , kFun,         kFuns
        , takeKFun
        , takeKFuns,    takeKFuns'
        , takeResultKind

         -- * Quantifiers
        , tForall,      tForall'
        , tForalls,     tForalls'
        , takeTForalls, eraseTForalls

          -- * Sums
        , tBot
        , tSum

          -- * Applications
        , tApp,          ($:)
        , tApps,         takeTApps
        , takeTyConApps
        , takeNameTyConApps
        , takeDataTyConApps
        , takePrimeRegion

          -- * Functions
        , tFun
        , tFunOfList
        , tFunOfParamResult
        , takeTFun
        , takeTFunCon
        , takeTFunCons
        , takeTFunArgResult
        , takeTFunWitArgResult
        , takeTFunAllArgResult
        , arityOfType
        , dataArityOfType

          -- * Suspensions
        , tSusp
        , takeTSusp
        , takeTSusps

          -- * Implications
        , tImpl
        , takeTImpl

          -- * Units
        , tUnit

          -- * Variables
        , tIx
        , takeTExists

          -- * Sort construction
        , sComp, sProp

          -- * Kind construction
        , kData, kRegion, kEffect, kClosure, kWitness

          -- * Effect type constructors
        , tRead,        tDeepRead,      tHeadRead
        , tWrite,       tDeepWrite
        , tAlloc,       tDeepAlloc

          -- * Witness type constructors
        , tPure
        , tConst,       tDeepConst
        , tMutable,     tDeepMutable
        , tDistinct

          -- * Type constructor wrappers.
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


-- Variables ------------------------------------------------------------------
-- | Construct a deBruijn index.
tIx :: Kind n -> Int -> Type n
tIx _ i         = TVar (UIx i)


-- Existentials ---------------------------------------------------------------
-- | Take an existential variable from a type.
takeTExists :: Type n -> Maybe Int
takeTExists tt
 = case tt of
        TCon (TyConExists n _)  -> Just n
        _                       -> Nothing


-- Applications ---------------------------------------------------------------
-- | Construct an empty type sum.
tBot :: Kind n -> Type n
tBot k = TSum $ Sum.empty k


-- | Construct a type application.
tApp, ($:) :: Type n -> Type n -> Type n
tApp   = TApp
($:)   = TApp


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
--   and arguments, if there is one. Only accept named constructors.
takeNameTyConApps :: Type n -> Maybe (n, [Type n])
takeNameTyConApps tt
 = case takeTApps tt of
        TCon tc : args
         |  TyConBound (UName n) _ <- tc
         -> Just (n, args)
        _ -> Nothing


-- | Flatten a sequence of type applications, returning the type constructor
--   and arguments, if there is one. Only accept data type constructors.
takeDataTyConApps :: Type n -> Maybe (TyCon n, [Type n])
takeDataTyConApps tt
 = case takeTApps tt of
        TCon tc : args
         | TyConBound (UName{}) k       <- tc
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
        TAbs{}          -> tt
        TApp t1 t2      -> TApp (eraseTForalls t1) (eraseTForalls t2)
        TForall _ t     -> eraseTForalls t
        TSum ts         -> TSum $ Sum.fromList (Sum.kindOfSum ts)
                                $ map eraseTForalls $ Sum.toList ts


-- Sums -----------------------------------------------------------------------
tSum :: Ord n => Kind n -> [Type n] -> Type n
tSum k ts = TSum (Sum.fromList k ts)


-- Unit -----------------------------------------------------------------------
-- | The unit type.
tUnit :: Type n
tUnit = TCon (TyConSpec TcConUnit)


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
        = (TCon $ TyConSpec TcConFunExplicit)  `tApps` [t1, t2]
infixr `tFun`


-- | Construct a function type from a list of parameter types and the
--   return type.
tFunOfParamResult :: [Type n] -> Type n -> Type n
tFunOfParamResult tsArg tResult
 = let  tFuns' []        = tResult
        tFuns' (t': ts') = t' `tFun` tFuns' ts'
   in   tFuns' tsArg


-- | Construct a function type from a list containing the parameter
--   and return types. Yields `Nothing` if the list is empty.
tFunOfList :: [Type n] -> Maybe (Type n)
tFunOfList ts
  = case reverse ts of
        []      -> Nothing
        (t : tsArgs)
         -> let tFuns' []             = t
                tFuns' (t' : ts')     = t' `tFun` tFuns' ts'
            in  Just $ tFuns' (reverse tsArgs)


-- | Yield the argument and result type of a function type.
takeTFun :: Type n -> Maybe (Type n, Type n)
takeTFun tt
 = case tt of
        TApp (TApp (TCon (TyConSpec TcConFunExplicit)) t1) t2
          -> Just (t1, t2)
        _ -> Nothing


-- | Split a function type into the function type constructor,
--   parameter and result types.
--   This works for both the Explicit and Implicit function constructors.
takeTFunCon :: Type n -> Maybe (TcCon, Type n, Type n)
takeTFunCon tt
 = case tt of
        TApp (TApp (TCon (TyConSpec tc)) t1) t2
         -> case tc of
                TcConFunExplicit  -> Just (tc, t1, t2)
                TcConFunImplicit  -> Just (tc, t1, t2)
                _                 -> Nothing
        _                         -> Nothing


-- | Like `takeTFunCon` but produce a list of arguments.
takeTFunCons :: Type n -> ([(TcCon, Type n)], Type n)
takeTFunCons tt
 = case takeTFunCon tt of
        Just (tc, t1, t2)
         -> case takeTFunCons t2 of
                (ts, t2')  -> ((tc, t1) : ts, t2')

        Nothing -> ([], tt)



-- | Destruct the type of a function, returning just the argument and result types.
takeTFunArgResult :: Type n -> ([Type n], Type n)
takeTFunArgResult tt
 = case tt of
        TApp (TApp (TCon (TyConSpec TcConFunExplicit)) t1) t2
          -> let (tsMore, tResult) = takeTFunArgResult t2
             in  (t1 : tsMore, tResult)
        _ -> ([], tt)


-- | Destruct the type of a function,
--   returning the witness argument, value argument and result types.
--   The function type must have the witness implications before
--   the value arguments, eg  @T1 => T2 -> T3 -> T4 -> T5@.
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

        TApp (TApp (TCon (TyConSpec TcConFunExplicit)) t1) t2
         -> let (tsMore, tResult) = takeTFunAllArgResult t2
            in  (t1 : tsMore, tResult)

        TApp (TApp (TCon (TyConWitness TwConImpl)) t1) t2
         -> let (tsMore, tResult) = takeTFunAllArgResult t2
            in  (t1 : tsMore, tResult)

        _ -> ([], tt)


-- | Determine the arity of an expression by looking at its type.
--   Count all the function arrows, and foralls.
--
--   This assumes the type is in prenex form, meaning that all the quantifiers
--   are at the front.
arityOfType :: Type n -> Int
arityOfType tt
 = case tt of
        TForall _ t     -> 1 + arityOfType t
        t               -> length $ fst $ takeTFunArgResult t


-- | The data arity of a type is the number of data values it takes.
--   Unlike `arityOfType` we ignore type and witness parameters.
dataArityOfType :: Type n -> Int
dataArityOfType tt
 = case tt of
        TVar{}          -> 0
        TCon{}          -> 0

        TForall _ t     -> dataArityOfType t

        TApp (TApp (TCon (TyConSpec    TcConFunExplicit)) _) t2
         -> 1 + dataArityOfType t2

        TApp (TApp (TCon (TyConWitness TwConImpl)) _) t2
         -> dataArityOfType t2

        _ -> 0


-- Implications ---------------------------------------------------------------
-- | Construct a witness implication type.
tImpl :: Type n -> Type n -> Type n
tImpl t1 t2
        = ((TCon $ TyConWitness TwConImpl) `tApp` t1) `tApp` t2
infixr `tImpl`


-- | Yield the argument and result type of an implication.
takeTImpl :: Type n -> Maybe (Type n, Type n)
takeTImpl tt
 = case tt of
        TApp (TApp (TCon (TyConWitness TwConImpl)) t1) t2
          -> Just (t1, t2)
        _ -> Nothing


-- Suspensions ----------------------------------------------------------------
-- | Construct a suspension type.
tSusp  :: Effect n -> Type n -> Type n
tSusp tE tA
        = (TCon $ TyConSpec TcConSusp) `tApp` tE `tApp` tA


-- | Take the effect and result type of a suspension type.
takeTSusp :: Type n -> Maybe (Effect n, Type n)
takeTSusp tt
 = case tt of
        TApp (TApp (TCon (TyConSpec TcConSusp)) tE) tA
          -> Just (tE, tA)
        _ -> Nothing


-- | Split off enclosing suspension types.
takeTSusps :: Type n -> ([Effect n], Type n)
takeTSusps tt
 = case tt of
        TApp (TApp (TCon (TyConSpec TcConSusp)) tE) tRest
          -> let (tEs, tA) = takeTSusps tRest
             in  (tE : tEs, tA)
        _ -> ([], tt)


-- Level 3 constructors (sorts) -----------------------------------------------
-- | Sort of kinds of computational types.
sComp           = TCon $ TyConSort SoConComp

-- | Sort of kinds of propositional types.
sProp           = TCon $ TyConSort SoConProp


-- Level 2 constructors (kinds) -----------------------------------------------
-- | Kind of data types.
kData           = TCon $ TyConKind KiConData

-- | Kind of region types.
kRegion         = TCon $ TyConKind KiConRegion

-- | Kind of effect types.
kEffect         = TCon $ TyConKind KiConEffect

-- | Kind of closure types.
kClosure        = TCon $ TyConKind KiConClosure

-- | Kind of witness types.
kWitness        = TCon $ TyConKind KiConWitness


-- Level 1 constructors (witness and computation types) -----------------------
-- Effect type constructors
-- | Read effect type constructor.
tRead           = tcCon1 TcConRead

-- | Head Read effect type constructor.
tHeadRead       = tcCon1 TcConHeadRead

-- | Deep Read effect type constructor.
tDeepRead       = tcCon1 TcConDeepRead

-- | Write effect type  constructor.
tWrite          = tcCon1 TcConWrite

-- | Deep Write effect type constructor.
tDeepWrite      = tcCon1 TcConDeepWrite

-- | Alloc effect type constructor.
tAlloc          = tcCon1 TcConAlloc

-- | Deep Alloc effect type constructor.
tDeepAlloc      = tcCon1 TcConDeepAlloc

-- Witness type constructors.
-- | Pure witness type constructor.
tPure           = twCon1 TwConPure

-- | Const witness type constructor.
tConst          = twCon1 TwConConst

-- | Deep Const witness type constructor.
tDeepConst      = twCon1 TwConDeepConst

-- | Mutable witness type constructor.
tMutable        = twCon1 TwConMutable

-- | Deep Mutable witness type constructor.
tDeepMutable    = twCon1 TwConDeepMutable

-- | Distinct witness type constructor.
tDistinct n     = twCon2 (TwConDistinct n)

-- | Wrap a computation type constructor applied to a single argument.
tcCon1 tc t     = (TCon $ TyConSpec    tc) `tApp` t

-- | Wrap a witness type constructor applied to a single argument.
twCon1 tc t     = (TCon $ TyConWitness tc) `tApp` t

-- | Wrap a witness type constructor applied to two arguments.
twCon2 tc ts    = tApps (TCon $ TyConWitness tc) ts

-- | Build a nullary type constructor of the given kind.
tConData0 :: n -> Kind n -> Type n
tConData0 n k   = TCon (TyConBound (UName n) k)

-- | Build a type constructor application of one argumnet.
tConData1 :: n -> Kind n -> Type n -> Type n
tConData1 n k t1 = TApp (TCon (TyConBound (UName n) k)) t1

