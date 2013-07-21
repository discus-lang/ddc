
module DDC.Source.Tetra.Compounds
        ( takeAnnotOfExp

         -- * Data Constructors
        , dcUnit
        , mkDaConAlg
        , mkDaConSolid
        , takeNameOfDaCon
        , typeOfDaCon)
where
import DDC.Source.Tetra.Exp

import DDC.Core.Compounds
        ( dcUnit
        , mkDaConAlg
        , mkDaConSolid
        , takeNameOfDaCon
        , typeOfDaCon)
        
-- Annotations ----------------------------------------------------------------
-- | Take the outermost annotation from an expression,
--   or Nothing if this is an `XType` or `XWitness` without an annotation.
takeAnnotOfExp :: Exp a n -> Maybe a
takeAnnotOfExp xx
 = case xx of
        XVar  a _       -> Just a
        XCon  a _       -> Just a
        XLAM  a _ _     -> Just a
        XLam  a _ _     -> Just a
        XApp  a _ _     -> Just a
        XLet  a _ _     -> Just a
        XCase a _ _     -> Just a
        XCast a _ _     -> Just a
        XType{}         -> Nothing
        XWitness{}      -> Nothing
