

-- TODO: This is dead code. 
--       The Storage transform isn't used anymore.
--       Please delete me.
--
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
     -- => let x = newVector#   [a] 1#     in
     --    let _ = writeVector# [a] 0# val in ...
     --
     | LLet b x1      <- lts
     , Just (NameOpStore OpStoreNew, [XType tA, xVal])        
                <- takeXPrimApps x1

     -> let b'      = replaceTypeOfBind (tVector tA) b
            Just u' = takeSubstBoundOfBind b'
        in  XLet a (LLet b'            
                        $ xNewVector   tA (xNat () 1))
          $ XLet a (LLet (BNone tUnit) 
                        $ xWriteVector tA (XVar () u') (xNat () 0) xVal)
          $ storageX x2


     -- Demote reads ----------------------------
     --    let x = read#        [a] arr    in ...
     --
     -- => let x = readVector#  [a] arr 0# in ...
     | LLet b x1      <- lts
     , Just (NameOpStore OpStoreRead, [XType tA, xArr])
                <- takeXPrimApps x1
     -> XLet a (LLet b $ xReadVector tA xArr (xNat () 0))
      $ storageX x2


     -- Demote writes ---------------------------
     --    let  x = write#      [a] arr x    in ...
     --
     -- => let  x = writeVector# [a] arr 0# x in ...
     | LLet b x1      <- lts
     , Just (NameOpStore OpStoreWrite, [XType tA, xArr, xVal])
                <- takeXPrimApps x1
     -> XLet a (LLet b $ xWriteVector tA xArr (xNat () 0) xVal)
      $ storageX x2

   
     | otherwise        
     -> XLet  a  (storageLts lts) (storageX x2)


-- Lets -----------------------------------------------------------------------
storageLts :: Lets () Name -> Lets () Name
storageLts lts
 = case lts of
    LLet b x            -> LLet b (storageX x)
    LRec bxs            -> LRec [(b, storageX x) | (b, x) <- bxs]
    LLetRegions{}       -> lts
    LWithRegion{}       -> lts


-- Alts -----------------------------------------------------------------------
storageA   :: Alt () Name -> Alt () Name
storageA aa
 = case aa of
    AAlt w x            -> AAlt w (storageX x)
