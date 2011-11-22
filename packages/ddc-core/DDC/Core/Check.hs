
module DDC.Core.Check
        ( typeOfWitness
        , typeOfWiCon)
where
import DDC.Core.Exp
import DDC.Type.Compounds

typeOfWitness :: Witness n -> Type n
typeOfWitness ww
 = case ww of
        WCon wc
         -> typeOfWiCon wc

        _ -> error "typeOfWitness: not handled yet"
        

-- | Take the type of a witness constructor.
typeOfWiCon :: WiCon -> Type n
typeOfWiCon wc
 = case wc of
        WiConPure     -> tPure  (tBot kEffect)
        WiConEmpty    -> tEmpty (tBot kClosure)

        WiConConst    
         -> tForall kRegion $ \r -> tConst r

        WiConMutable
         -> tForall kRegion $ \r -> tMutable r

        WiConLazy
         -> tForall kRegion $ \r -> tLazy r

        WiConDirect
         -> tForall kRegion $ \r -> tDirect r

        WiConRead
         -> tForall kRegion $ \r -> (tConst r) `tImpl`  (tPure  $ tRead r)

        WiConFree
         -> tForall kRegion $ \r -> (tConst r)  `tImpl` (tEmpty $ tFree r)

        WiConDistinct n
         -> tForalls (replicate n kRegion) $ \rs -> tDistinct rs

