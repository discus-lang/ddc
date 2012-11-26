
-- | Utilities for working with `TypeSum`s.
--
module DDC.Type.Sum 
        ( -- * Constructors
          empty
        , singleton
        , union
        , unions
        , insert

          -- * Conversion
        , toList
        , fromList

          -- * Projection
        , kindOfSum
        , elem

          -- * Deletion
        , delete
        , difference

          -- * Hashing
        , hashTyCon
        , hashTyConRange
        , unhashTyCon)
where
import DDC.Type.Exp
import Data.Array
import qualified Data.List              as L
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import Prelude                          hiding (elem)


-- | Construct an empty type sum of the given kind.
empty :: Kind n -> TypeSum n
empty k = TypeSumBot k


-- | Construct an empty type sum of the given kind, but in `TypeSumSet` form.
--   This isn't exported.
emptySet :: Kind n -> TypeSum n
emptySet k 
        = TypeSumSet
        { typeSumKind           = k
        , typeSumElems          = listArray hashTyConRange (repeat Set.empty)
        , typeSumBoundNamed     = Map.empty
        , typeSumBoundAnon      = Map.empty
        , typeSumSpill          = [] }


-- | Construct a type sum containing a single element.
singleton :: Ord n => Kind n -> Type n -> TypeSum n
singleton k t
        = insert t (empty k)


-- | Check whether an element is a member of a sum.
--
--   * Returns True when the first argument is $0 or !0.
--
--   * Returns False when the first argument is another sum.
--
--   * May return False if the first argument is miskinded but still
--     alpha-equivalent to some component of the sum.
elem :: (Eq n, Ord n) => Type n -> TypeSum n -> Bool
elem _ TypeSumBot{}      =  False
elem t ts@TypeSumSet{}
 = case t of
        TVar (UName n)   -> Map.member n (typeSumBoundNamed ts)
        TVar (UIx   i)   -> Map.member i (typeSumBoundAnon  ts)
        TVar (UPrim n _) -> Map.member n (typeSumBoundNamed ts)
        TCon{}           -> L.elem t (typeSumSpill ts)

        -- Foralls can't be a part of well-kinded sums.
        --  Just check whether the types are strucutrally equal
        --  without worring about alpha-equivalence.
        TForall{}        -> L.elem t (typeSumSpill ts)

        TApp (TCon _) _
         |  Just (h, vc) <- takeSumArrayElem t
         ,  tsThere      <- typeSumElems ts ! h
         -> Set.member vc tsThere

        TApp{}           -> L.elem t (typeSumSpill ts) 

        -- Treat bottom effect and closures as always
        -- being part of the sum.
        TSum ts1
         -> case toList ts1 of
             [] | TCon (TyConKind KiConEffect)  <- typeSumKind ts1 
                , TCon (TyConKind KiConEffect)  <- typeSumKind ts
                -> True

                | TCon (TyConKind KiConClosure) <- typeSumKind ts1
                , TCon (TyConKind KiConClosure) <- typeSumKind ts
                -> True

             _ -> False


-- | Insert a new element into a sum.
insert :: Ord n => Type n -> TypeSum n -> TypeSum n
insert t (TypeSumBot k)   = insert t (emptySet k)
insert t ts@TypeSumSet{}
 = let k        = typeSumKind ts
   in case t of
        TVar (UName n)  -> ts { typeSumBoundNamed = Map.insert n k (typeSumBoundNamed ts) }
        TVar (UIx   i)  -> ts { typeSumBoundAnon  = Map.insert i k (typeSumBoundAnon  ts) }
        TVar (UPrim n _)-> ts { typeSumBoundNamed = Map.insert n k (typeSumBoundNamed ts) }
        TCon{}          -> ts { typeSumSpill      = t : typeSumSpill ts }

        -- Foralls can't be part of well-kinded sums.
        --  Just add them to the splill lists so that we can still
        --  pretty print such mis-kinded types.
        TForall{}        -> ts { typeSumSpill      = t : typeSumSpill ts }

        TApp (TCon _) _
         |  Just (h, vc)  <- takeSumArrayElem t
         ,  tsThere       <- typeSumElems ts ! h
         -> if Set.member vc tsThere
                then ts
                else ts { typeSumElems = (typeSumElems ts) // [(h, Set.insert vc tsThere)] }
        
        TApp{}           -> ts { typeSumSpill      = t : typeSumSpill ts }
        
        TSum ts'         -> foldr insert ts (toList ts')


-- | Delete an element from a sum.
delete :: Ord n => Type n -> TypeSum n -> TypeSum n
delete _ ts@TypeSumBot{} = ts
delete t ts@TypeSumSet{}
 = case t of
        TVar (UName n)  -> ts { typeSumBoundNamed = Map.delete n (typeSumBoundNamed ts) }
        TVar (UIx   i)  -> ts { typeSumBoundAnon  = Map.delete i (typeSumBoundAnon  ts) }
        TVar (UPrim n _)-> ts { typeSumBoundNamed = Map.delete n (typeSumBoundNamed ts) }
        TCon{}          -> ts { typeSumSpill      = L.delete t (typeSumSpill ts) }
        TForall{}       -> ts { typeSumSpill      = L.delete t (typeSumSpill ts) }
        
        TApp (TCon _) _
         | Just (h, vc) <- takeSumArrayElem t
         , tsThere      <- typeSumElems ts ! h
         -> ts { typeSumElems = (typeSumElems ts) // [(h, Set.delete vc tsThere)] }
         
        TApp{}          -> ts { typeSumSpill       = L.delete t (typeSumSpill ts) }
        
        TSum ts'        -> foldr delete ts (toList ts')


-- | Add two type sums.
union     :: Ord n => TypeSum n -> TypeSum n -> TypeSum n
union ts1 ts2 
        = foldr insert ts2 (toList ts1)


-- | Union a list of `TypeSum`s together.
unions    :: Ord n => Kind n -> [TypeSum n] -> TypeSum n
unions k []       = empty k
unions _ (t:ts)   = foldr union t ts



-- | Delete all members of the second sum from the first one.
difference :: Ord n => TypeSum n -> TypeSum n -> TypeSum n
difference ts1 ts2
        = foldr delete ts1 (toList ts2)


-- | Take the kind of a sum.
kindOfSum :: TypeSum n -> Kind n
kindOfSum ts
        = typeSumKind ts


-- | Flatten out a sum, yielding a list of individual terms.
toList :: TypeSum n -> [Type n]
toList TypeSumBot{}       
 = []

toList TypeSumSet
        { typeSumKind           = _kind
        , typeSumElems          = sumElems
        , typeSumBoundNamed     = named
        , typeSumBoundAnon      = anon
        , typeSumSpill          = spill}

 =      [ makeSumArrayElem h vc
                | (h, ts) <- assocs sumElems, vc <- Set.toList ts] 
        ++ [TVar $ UName n | (n, _) <- Map.toList named]
        ++ [TVar $ UIx   i | (i, _) <- Map.toList anon]
        ++ spill
                

-- | Convert a list of types to a `TypeSum`
fromList :: Ord n => Kind n -> [Type n] -> TypeSum n
fromList k ts
        = foldr insert (empty k) ts


-- | Yield the `TyConHash` of a `TyCon`, or `Nothing` if there isn't one.
hashTyCon :: TyCon n -> Maybe TyConHash
hashTyCon tc
 = case tc of
        TyConSpec tc'   -> hashTcCon tc'
        _               -> Nothing
        

-- | Yield the `TyConHash` of a `TyConBuiltin`, or `Nothing` if there isn't one.
hashTcCon :: TcCon -> Maybe TyConHash
hashTcCon tc
 = case tc of
        TcConRead       -> Just $ TyConHash 0
        TcConDeepRead   -> Just $ TyConHash 1
        TcConWrite      -> Just $ TyConHash 2
        TcConDeepWrite  -> Just $ TyConHash 3
        TcConAlloc      -> Just $ TyConHash 4
        TcConUse        -> Just $ TyConHash 5
        TcConDeepUse    -> Just $ TyConHash 6
        _               -> Nothing


-- | The range of hashes that can be produced by `hashTyCon`.
hashTyConRange :: (TyConHash, TyConHash)
hashTyConRange
 =      ( TyConHash 0
        , TyConHash 6)
                

-- | Yield the `TyCon` corresponding to a `TyConHash`, or `error` if there isn't one.
unhashTyCon :: TyConHash -> TyCon n
unhashTyCon (TyConHash i)
 = TyConSpec
 $ case i of
        0               -> TcConRead
        1               -> TcConDeepRead
        2               -> TcConWrite
        3               -> TcConDeepWrite
        4               -> TcConAlloc
        5               -> TcConUse
        6               -> TcConDeepUse

        -- This should never happen, because we only produce hashes
        -- with the above 'hashTyCon' function.
        _ -> error $ "DDC.Type.Sum: bad TyConHash " ++ show i


-- | If this type can be put in one of our arrays then split it
--   into the hash and the argument.
takeSumArrayElem :: Type n -> Maybe (TyConHash, TypeSumVarCon n)
takeSumArrayElem (TApp (TCon tc) t2)
        | Just h        <- hashTyCon tc
        = case t2 of
                TVar u                  -> Just (h, TypeSumVar u)
                TCon (TyConBound u k)   -> Just (h, TypeSumCon u k)
                _                       -> Nothing
        
takeSumArrayElem _ = Nothing


-- | Inverse of `takeSumArrayElem`.
makeSumArrayElem :: TyConHash -> TypeSumVarCon n -> Type n
makeSumArrayElem h vc
 = let  tc       = unhashTyCon h
   in   case vc of
         TypeSumVar u   -> TApp (TCon tc) (TVar u)
         TypeSumCon u k -> TApp (TCon tc) (TCon (TyConBound u k))


-- Type Equality --------------------------------------------------------------
-- Code for type equality is in this module because we need to normalise sums
-- when deciding if two types are equal.

deriving instance Eq n => Eq (TyCon n)
deriving instance Eq n => Eq (Bound n)
deriving instance Eq n => Eq (Bind n)


instance Eq n => Eq (Type n) where
 (==) t1 t2
  = case (normalise t1, normalise t2) of
        (TVar u1,        TVar u2)        -> u1  == u2
        (TCon tc1,       TCon tc2)       -> tc1 == tc2
        (TForall b1 t11, TForall b2 t22) -> b1  == b2  && t11 == t22
        (TApp t11 t12,   TApp t21 t22)   -> t11 == t21 && t12 == t22
        (TSum ts1,       TSum ts2)       -> ts1 == ts2
        (_, _)                           -> False

        -- Unwrap single element sums into plain types.
  where normalise (TSum ts)
         | [t'] <- toList ts    = t'
         | []   <- toList ts    = TSum $ empty (typeSumKind ts)

        normalise t'            = t'


instance Eq n => Eq (TypeSum n) where
 (==) ts1 ts2

        -- If the sum is empty, then just compare the kinds.
        | []    <- toList ts1 
        , []    <- toList ts2
        = typeSumKind ts1 == typeSumKind ts2

        | TypeSumBot{}  <- normalise ts1
        , TypeSumBot{}  <- normalise ts2
        = typeSumKind ts1 == typeSumKind ts2

        -- If both sums have elements, then compare them directly and ignore the
        -- kind. This allows us to use (tBot sComp) as the typeSumKind field
        -- when we want to compute the real kind based on the elements. 
        | TypeSumSet{} <- ts1
	, TypeSumSet{} <- ts2
        =  typeSumElems ts1      == typeSumElems ts2
        && typeSumBoundNamed ts1 == typeSumBoundNamed ts2
        && typeSumBoundAnon  ts1 == typeSumBoundAnon ts2
        && typeSumSpill      ts1 == typeSumSpill ts2

	-- One is a set and one is bottom, so they are not equal.
	| otherwise
	= False

  where normalise ts
         | []   <- toList ts    = empty (typeSumKind ts)
        normalise ts            = ts


instance Ord n => Ord (Bound n) where
 compare (UName n1)     (UName n2)      = compare n1 n2
 compare (UIx   i1)     (UIx   i2)      = compare i1 i2
 compare (UPrim n1 _)   (UPrim n2 _)    = compare n1 n2
 compare UIx{}          _               = LT
 compare UName{}        UIx{}           = GT
 compare UName{}        UPrim{}         = LT
 compare UPrim{}        _               = GT


instance Eq n => Eq (TypeSumVarCon n) where
 (==) (TypeSumVar u1)   (TypeSumVar u2)     = u1 == u2
 (==) (TypeSumCon u1 _) (TypeSumCon u2 _)   = u1 == u2
 (==) _ _                                   = False

instance Ord n => Ord (TypeSumVarCon n) where
 compare (TypeSumVar u1)   (TypeSumVar u2)    = compare u1 u2
 compare (TypeSumCon u1 _) (TypeSumCon u2 _)  = compare u1 u2
 compare (TypeSumVar _)    _                  = LT
 compare (TypeSumCon _ _)  _                  = GT

