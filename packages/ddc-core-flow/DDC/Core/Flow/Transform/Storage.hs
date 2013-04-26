
module DDC.Core.Flow.Transform.Storage
        (storageModule)
where
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds
import DDC.Core.Module
import DDC.Core.Exp


-- | Assign references to array storage in a core module.
storageModule :: Module () Name -> Module () Name
storageModule mm
        = mm { moduleBody    = storageX $ moduleBody mm}


-- Exp ------------------------------------------------------------------------
storageX :: Exp () Name -> Exp () Name
storageX xx
 = case xx of
    XVar{}              -> xx
    XCon{}              -> xx
    XLAM  a b x         -> XLAM  a b (storageX x)
    XLam  a b x         -> XLam  a b (storageX x)
    XApp  a x1 x2       -> XApp  a   (storageX x1)    (storageX x2)
    XCase a x alts      -> XCase a   (storageX x)     (map storageA alts)
    XCast a c x         -> XCast a c (storageX x)
    XType{}             -> xx
    XWitness{}          -> xx


    XLet  a lts x2      
     -- Demote allocs ---------------------------
     --    let x = new#        [a] val    in ...
     --
     -- => let x = newArray#   [a] 1#     in
     --    let _ = writeArray# [a] 0# val in ...
     --
     | LLet m b x1      <- lts
     , Just (NameOpStore OpStoreNew, [XType tA, xVal])        
                <- takeXPrimApps x1

     -> let b'      = replaceTypeOfBind (tArray tA) b
            Just u' = takeSubstBoundOfBind b'
        in  XLet a (LLet m b'            
                        $ xNewArray   tA (xNat () 8))                   -- SIZE WRONG
          $ XLet a (LLet m (BNone tVoid) 
                        $ xWriteArray tA (XVar () u') (xNat () 0) xVal)
          $ storageX x2


     -- Demote reads ----------------------------
     --    let x = read#       [a] arr    in ...
     --
     -- => let x = readArray#  [a] arr 0# in ...
     | LLet m b x1      <- lts
     , Just (NameOpStore OpStoreRead, [XType tA, xArr])
                <- takeXPrimApps x1
     -> XLet a (LLet m b $ xReadArray tA xArr (xNat () 0))
      $ storageX x2


     -- Demote writes ---------------------------
     --    let  x = write#      [a] arr x    in ...
     --
     -- => let  x = writeArray# [a] arr 0# x in ...
     | LLet m b x1      <- lts
     , Just (NameOpStore OpStoreWrite, [XType tA, xArr, xVal])
                <- takeXPrimApps x1
     -> XLet a (LLet m b $ xWriteArray tA xArr (xNat () 0) xVal)
      $ storageX x2

   
     | otherwise        
     -> XLet  a  (storageLts lts) (storageX x2)


-- Lets -----------------------------------------------------------------------
storageLts :: Lets () Name -> Lets () Name
storageLts lts
 = case lts of
    LLet m b x          -> LLet m b (storageX x)
    LRec bxs            -> LRec [(b, storageX x) | (b, x) <- bxs]
    LLetRegions{}       -> lts
    LWithRegion{}       -> lts


-- Alts -----------------------------------------------------------------------
storageA   :: Alt () Name -> Alt () Name
storageA aa
 = case aa of
    AAlt w x            -> AAlt w (storageX x)
