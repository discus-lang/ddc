
-- | Utilities for working with TypeSums.
module DDC.Type.Sum 
        ( hashTyCon, hashTyConRange
        , unhashTyCon
        , empty
        , insert
        , kindOfSum
        , toList, fromList)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import Data.Array
import qualified Data.Map       as Map


-- | Yield the `TyConHash` of a `TyCon`, or `Nothing` if there isn't one.
hashTyCon :: TyCon n -> Maybe TyConHash
hashTyCon tc
 = case tc of
        TyConUser{}      -> Nothing
        TyConBuiltin tcb -> hashTyConBuiltin tcb
        

-- | Yield the `TyConHash` of a `TyConBuiltin`, or `Nothing` if there isn't one.
hashTyConBuiltin :: TyConBuiltin -> Maybe TyConHash
hashTyConBuiltin tc
 = case tc of
        TyConRead       -> Just $ TyConHash 0
        TyConDeepRead   -> Just $ TyConHash 1
        TyConWrite      -> Just $ TyConHash 2
        TyConDeepWrite  -> Just $ TyConHash 3
        TyConAlloc      -> Just $ TyConHash 4
        TyConFree       -> Just $ TyConHash 5
        TyConDeepFree   -> Just $ TyConHash 6
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
 = TyConBuiltin
 $ case i of
        0               -> TyConRead
        1               -> TyConDeepRead
        2               -> TyConWrite
        3               -> TyConDeepWrite
        4               -> TyConAlloc
        5               -> TyConFree
        6               -> TyConDeepFree

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

        TApp (TCon (TConType tc)) t2
         |  Just h       <- hashTyCon tc
         ,  tsThere      <- typeSumElems ts ! h
         -> if elem t tsThere
                then ts
                else ts { typeSumElems = (typeSumElems ts) // [(h, t2 : tsThere)] }
        
        TApp{}           -> ts { typeSumSpill      = t : typeSumSpill ts }
        
        TSum ts'         -> foldr insert ts (toList ts')
        TBot{}           -> ts


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

 =      [ TCon (TConType (unhashTyCon h)) $: t 
                | (h, ts) <- assocs sumElems, t <- ts] 
        ++ [TVar $ UName n k | (n, k) <- Map.toList named]
        ++ [TVar $ UIx   i k | (i, k) <- Map.toList anon]
        ++ spill
                

-- | Convert a list of types to a `TypeSum`
fromList :: Ord n => Kind n -> [Type n] -> TypeSum n
fromList k ts
        = foldr insert (empty k) ts


