

-- TODO: This is dead code. 
--       The Storage transform isn't used anymore.
--       Please delete me.
--
module DDC.Core.Flow.Transform.Storage
        (storageModule)
where
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import DDC.Core.Transform.Annotate
import DDC.Core.Transform.Deannotate
import DDC.Core.Module


-- | Assign references to array storage in a core module.
storageModule :: Module () Name -> Module () Name
storageModule mm
 = let  x'      = annotate ()
                $ storageX 
                $ deannotate (const Nothing) 
                $ moduleBody mm 

   in   mm { moduleBody = x' }


-- Exp ------------------------------------------------------------------------
storageX :: ExpF -> ExpF
storageX xx
 = case xx of
    XAnnot{}            -> xx
    XVar{}              -> xx
    XCon{}              -> xx
    XLAM  b x           -> XLAM  b (storageX x)
    XLam  b x           -> XLam  b (storageX x)
    XApp  x1 x2         -> XApp    (storageX x1)    (storageX x2)
    XCase x alts        -> XCase   (storageX x)     (map storageA alts)
    XCast c x           -> XCast c (storageX x)
    XType{}             -> xx
    XWitness{}          -> xx


    XLet  lts x2      
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
        in  XLet (LLet b' $ xNewVector   tA (xNat 1))
          $ XLet (LLet (BNone tUnit) 
                          $ xWriteVector tA (XVar u') (xNat 0) xVal)
          $ storageX x2


     -- Demote reads ----------------------------
     --    let x = read#        [a] arr    in ...
     --
     -- => let x = readVector#  [a] arr 0# in ...
     | LLet b x1      <- lts
     , Just (NameOpStore OpStoreRead, [XType tA, xArr])
                <- takeXPrimApps x1
     -> XLet (LLet b $ xReadVector tA xArr (xNat 0))
      $ storageX x2


     -- Demote writes ---------------------------
     --    let  x = write#      [a] arr x    in ...
     --
     -- => let  x = writeVector# [a] arr 0# x in ...
     | LLet b x1      <- lts
     , Just (NameOpStore OpStoreWrite, [XType tA, xArr, xVal])
                <- takeXPrimApps x1
     -> XLet (LLet b $ xWriteVector tA xArr (xNat 0) xVal)
      $ storageX x2

   
     | otherwise        
     -> XLet (storageLts lts) (storageX x2)


-- Lets -----------------------------------------------------------------------
storageLts :: LetsF -> LetsF
storageLts lts
 = case lts of
    LLet b x            -> LLet b (storageX x)
    LRec bxs            -> LRec [(b, storageX x) | (b, x) <- bxs]
    LLetRegions{}       -> lts
    LWithRegion{}       -> lts


-- Alts -----------------------------------------------------------------------
storageA   :: AltF -> AltF
storageA aa
 = case aa of
    AAlt w x            -> AAlt w (storageX x)

