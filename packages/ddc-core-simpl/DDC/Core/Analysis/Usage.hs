
-- | Annotate let bindings with how the bound variable is used.
module DDC.Core.Analysis.Usage
        ( Used    (..)
        , UsedMap (..)
        , usageModule
        , usageX)
where
import DDC.Core.Module
import DDC.Core.Exp
import Data.List
import Data.Map                 (Map)
import qualified Data.Map       as Map


-- Used -----------------------------------------------------------------------
-- | Tracks how a bound variable is used.
data Used
        -- | Bound variable has no uses.
        = UsedNone

        -- | Bound variable is used as the function of an application.
        | UsedFunction

        -- | Bound variable is destructed by a case-expression.
        | UsedDestruct

        -- | Bound variable has a single occurrence that is not one of the above.
        | UsedOcc

        -- | Usage is inside a Lambda abstraction (either type or value)
        | UsedInLambda Used

        -- | Usage is inside a case alternative.
        | UsedInAlt    Used
        deriving (Eq, Show)


-- UsedMap --------------------------------------------------------------------
-- | Map of bound name to how the variable is used.
data UsedMap n
        = UsedMap (Map n [Used])
        deriving Show

-- | An empty usage map.
empty :: UsedMap n
empty   = UsedMap (Map.empty)


accUsed :: Ord n => Bound n -> Used -> UsedMap n -> UsedMap n
accUsed u used um@(UsedMap m)
 = case u of
        UName n         -> UsedMap $ Map.insertWith (++) n [used] m 
        _               -> um


plusUsedMap :: Ord n => UsedMap n -> UsedMap n -> UsedMap n
plusUsedMap (UsedMap map1) (UsedMap map2)
        = UsedMap $ Map.unionWith (++) map1 map2


sumUsedMap :: Ord n => [UsedMap n] -> UsedMap n
sumUsedMap []   = UsedMap Map.empty
sumUsedMap (m:ms)
        = foldl' plusUsedMap m ms


-- Usage ----------------------------------------------------------------------
usageModule :: Ord n => Module a n -> Module (UsedMap n, a) n
usageModule 
        (ModuleCore
                { moduleName            = name
                , moduleExportKinds     = exportKinds
                , moduleExportTypes     = exportTypes
                , moduleImportKinds     = importKinds
                , moduleImportTypes     = importTypes
                , moduleBody            = body })

 =       ModuleCore
                { moduleName            = name
                , moduleExportKinds     = exportKinds
                , moduleExportTypes     = exportTypes
                , moduleImportKinds     = importKinds
                , moduleImportTypes     = importTypes
                , moduleBody            = snd $ usageX body }


-- | Usage analysis.
--   Annotates binding occurrences of variables with how they are used.
usageX :: Ord n => Exp a n -> (UsedMap n, Exp (UsedMap n, a) n)
usageX xx
 = case xx of
        XVar a u
         | used         <- accUsed u UsedOcc empty
         -> ( used
            , XVar (used, a) u)

        XCon a u
         -> ( empty
            , XCon (empty, a) u)

        -- Wrap usages from the body in UsedInLambda to singla that if we move
        -- the definition here then it might not be demanded at runtime.
        XLAM a b1 x2
         |  ( used2, x2')  <- usageX x2
         ,  UsedMap us2    <- used2
         ,  used2'         <- UsedMap (Map.map (map UsedInLambda) us2)
         -> ( used2'
            , XLAM (used2', a) b1 x2')

        -- Wrap usages from the body in UsedInLambda to signal that if we move
        -- the definition here then it might not be demanded at runtime.
        XLam a b1 x2
         |  ( used2, x2')  <- usageX x2
         ,  UsedMap us2    <- used2
         ,  used2'         <- UsedMap (Map.map (map UsedInLambda) us2)
         -> ( used2'
            , XLam (used2', a) b1 x2')

        XApp a x1 x2
         -- application of a function variable.
         |  XVar a1 u      <- x1
         ,  used1          <- accUsed u UsedFunction empty
         ,  (used2, x2')   <- usageX x2
         ,  used'          <- used1 `plusUsedMap` used2
         -> ( used'
            , XApp (used', a) (XVar (used1, a1) u) x2')

         -- General application.
         |  ( used1, x1')  <- usageX x1
         ,  ( used2, x2')  <- usageX x2
         ,  used'          <- used1 `plusUsedMap` used2
         -> ( used'
            , XApp (used', a) x1' x2')

        XLet a lts x2
         |  ( used1, lts')  <- usageLets lts
         ,  ( used2, x2')   <- usageX x2
         ,  used'           <- used1 `plusUsedMap` used2
         -> ( used'
            , XLet (used', a) lts' x2')

        -- Wrap usages in the Alts in UsedInAlt to signal that if we move
        -- the definition here then it might not be demanded at runtime.
        XCase a x1 alts
         |  ( used1, x1')   <- usageX x1
         ,  ( usedA, alts') <- unzip $ map usageAlt alts
         ,  UsedMap usA     <- sumUsedMap usedA
         ,  usedA'          <- UsedMap (Map.map (map UsedInAlt) usA)
         ,  used'           <- plusUsedMap used1 usedA'
         -> ( used'
            , XCase (used', a) x1' alts' )

        XCast _ _ x1
         -> usageX x1

        XType t        
         -> (empty, XType t)

        XWitness w     
         -> (empty, XWitness w)


-- | Annotate binding occurrences of named variables with usage information.
usageAlt  
        :: Ord n 
        => Alt a n  
        -> (UsedMap n, Alt (UsedMap n, a) n)

usageAlt (AAlt p x)
 = let  (used, x')      = usageX x
   in   (used, AAlt p x')


-- | Annotate binding occurences of named variables with usage information.
usageLets 
        :: Ord n
        => Lets a n 
        -> (UsedMap n, Lets (UsedMap n, a) n)

usageLets lts
 = case lts of
        LLet mode b x
         |  (used, x')   <- usageX x
         -> (used, LLet mode b x')

        LRec bxs
         |  (bs, xs)      <- unzip bxs
         ,  (useds', xs') <- unzip $ map usageX xs
         ,  used'         <- sumUsedMap useds'
         -> (used', LRec $ zip bs xs')

        LLetRegion b bs 
         -> (empty, LLetRegion b bs)

        LWithRegion b
         -> (empty, LWithRegion b)

