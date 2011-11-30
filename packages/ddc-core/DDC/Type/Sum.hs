
-- | Utilities for working with TypeSums.
--
-- TODO: defailt Eq instance is wrong because we much check the spill fields.
--       We want eq operation to be fast.
module DDC.Type.Sum 
        ( hashTyCon, hashTyConRange
        , unhashTyCon
        , empty
        , insert
        , plus
        , kindOfSum
        , toList, fromList)
where
import DDC.Type.Exp
-- import DDC.Type.Compounds
import Data.Array
import qualified Data.Map       as Map


-- | Yield the `TyConHash` of a `TyCon`, or `Nothing` if there isn't one.
hashTyCon :: TyCon n -> Maybe TyConHash
hashTyCon tc
 = case tc of
        TyConComp tc'   -> hashTcCon tc'
        _               -> Nothing
        

-- | Yield the `TyConHash` of a `TyConBuiltin`, or `Nothing` if there isn't one.
hashTcCon :: TcCon n -> Maybe TyConHash
hashTcCon tc
 = case tc of
        TcConRead       -> Just $ TyConHash 0
        TcConDeepRead   -> Just $ TyConHash 1
        TcConWrite      -> Just $ TyConHash 2
        TcConDeepWrite  -> Just $ TyConHash 3
        TcConAlloc      -> Just $ TyConHash 4
        TcConFree       -> Just $ TyConHash 5
        TcConDeepFree   -> Just $ TyConHash 6
        _               -> Nothing


-- | Range of hashes that can be produced by `hashTyCon`.
hashTyConRange :: (TyConHash, TyConHash)
hashTyConRange
 =      ( TyConHash 0
        , TyConHash 6)
                

-- | Yield the TyCon corresponding to a TyConHash, 
--   or `error` if there isn't one.
unhashTyCon :: TyConHash -> TyCon n
unhashTyCon (TyConHash i)
 = TyConComp
 $ case i of
        0               -> TcConRead
        1               -> TcConDeepRead
        2               -> TcConWrite
        3               -> TcConDeepWrite
        4               -> TcConAlloc
        5               -> TcConFree
        6               -> TcConDeepFree

        -- This should never happen, because we only produce hashes
        -- with the above 'hashTyCon' function.
        _ -> error $ "DDC.Type.Sum: bad TyConHash " ++ show i


-- | An empty type sum of the given kind.
empty :: Kind n -> TypeSum n
empty k = TypeSum
        { typeSumKind           = k
        , typeSumElems          = listArray hashTyConRange (repeat [])
        , typeSumBoundNamed     = Map.empty
        , typeSumBoundAnon      = Map.empty
        , typeSumSpill          = [] }


-- | Insert a new element into a sum.
insert :: Ord n => Type n -> TypeSum n -> TypeSum n
insert t ts
 = case t of
        TVar (UName n k) -> ts { typeSumBoundNamed = Map.insert n k (typeSumBoundNamed ts) }
        TVar (UIx   i k) -> ts { typeSumBoundAnon  = Map.insert i k (typeSumBoundAnon  ts) }
        TCon{}           -> ts { typeSumSpill      = t : typeSumSpill ts }
        TForall{}        -> ts { typeSumSpill      = t : typeSumSpill ts }

        TApp (TCon tc) t2
         |  Just h       <- hashTyCon tc
         ,  tsThere      <- typeSumElems ts ! h
         -> if elem t tsThere
                then ts
                else ts { typeSumElems = (typeSumElems ts) // [(h, t2 : tsThere)] }
        
        TApp{}           -> ts { typeSumSpill      = t : typeSumSpill ts }
        
        TSum ts'         -> foldr insert ts (toList ts')
        TBot{}           -> ts


-- | Add two type sum.
-- 
--   TODO: make this more efficiet. Directly combine the components.
plus     :: Ord n => TypeSum n -> TypeSum n -> TypeSum n
plus ts1 ts2 = foldr insert ts2 (toList ts1)


-- | Take the kind of a sum.
kindOfSum :: TypeSum n -> Kind n
kindOfSum ts
        = typeSumKind ts


-- | Flatten out a sum, yielding a list of individual terms.
toList :: TypeSum n -> [Type n]
toList TypeSum
        { typeSumKind           = _kind
        , typeSumElems          = sumElems
        , typeSumBoundNamed     = named
        , typeSumBoundAnon      = anon
        , typeSumSpill          = spill}

 =      [ TApp (TCon (unhashTyCon h)) t 
                | (h, ts) <- assocs sumElems, t <- ts] 
        ++ [TVar $ UName n k | (n, k) <- Map.toList named]
        ++ [TVar $ UIx   i k | (i, k) <- Map.toList anon]
        ++ spill
                

-- | Convert a list of types to a `TypeSum`
fromList :: Ord n => Kind n -> [Type n] -> TypeSum n
fromList k ts
        = foldr insert (empty k) ts


