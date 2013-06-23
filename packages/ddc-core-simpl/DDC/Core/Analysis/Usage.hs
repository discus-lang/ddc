
-- | Annotate let bindings with how their bound variables are used.
module DDC.Core.Analysis.Usage
        ( -- * Usage map
          Used    (..)
        , UsedMap (..)

          -- * Usage analysis
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
        -- | Bound variable is used as the function in an application.
        = UsedFunction

        -- | Bound variable is destructed by a case-expression.
        | UsedDestruct

	-- | Bound variable is used inside a @weakclo@ cast.
	| UsedInCast

        -- | Bound variable has an occurrence that is not one of the above.
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


-- | Add a single usage to a usage map.
accUsed :: Ord n => Bound n -> Used -> UsedMap n -> UsedMap n
accUsed u used um@(UsedMap m)
 = case u of
        UName n         -> UsedMap $ Map.insertWith (++) n [used] m 
        _               -> um

-- | Combine two usage maps.
plusUsedMap :: Ord n => UsedMap n -> UsedMap n -> UsedMap n
plusUsedMap (UsedMap map1) (UsedMap map2)
        = UsedMap $ Map.unionWith (++) map1 map2


-- | Combine a list of usage maps into a single one.
sumUsedMap :: Ord n => [UsedMap n] -> UsedMap n
sumUsedMap []   = UsedMap Map.empty
sumUsedMap (m:ms)
        = foldl' plusUsedMap m ms


-- Module ---------------------------------------------------------------------
-- | Annotate all binding occurrences of variables in an expression
--   with how they are used.
usageModule 
        :: Ord n
        => Module a n
        -> Module (UsedMap n, a) n
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
                , moduleBody            = usageX body }


-- Exp ------------------------------------------------------------------------
-- | Annotate all binding occurrences of variables in an expression
--   with how they are used.
usageX  :: Ord n 
        => Exp a n 
        -> Exp (UsedMap n, a) n
usageX xx = snd $ usageX' xx


usageX' :: Ord n 
        => Exp a n 
        -> (UsedMap n, Exp (UsedMap n, a) n)

usageX' xx
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
         |  ( used2, x2')  <- usageX' x2
         ,  UsedMap us2    <- used2
         ,  used2'         <- UsedMap (Map.map (map UsedInLambda) us2)
         -> ( used2'
            , XLAM (used2', a) b1 x2')

        -- Wrap usages from the body in UsedInLambda to signal that if we move
        -- the definition here then it might not be demanded at runtime.
        XLam a b1 x2
         |  ( used2, x2')  <- usageX' x2
         ,  UsedMap us2    <- used2
         ,  used2'         <- UsedMap (Map.map (map UsedInLambda) us2)
         -> ( used2'
            , XLam (used2', a) b1 x2')

        XApp a x1 x2
         -- application of a function variable.
         |  XVar a1 u      <- x1
         ,  used1          <- accUsed u UsedFunction empty
         ,  (used2, x2')   <- usageX' x2
         ,  used'          <- used1 `plusUsedMap` used2
         -> ( used'
            , XApp (used', a) (XVar (used1, a1) u) x2')

         -- General application.
         |  ( used1, x1')  <- usageX' x1
         ,  ( used2, x2')  <- usageX' x2
         ,  used'          <- used1 `plusUsedMap` used2
         -> ( used'
            , XApp (used', a) x1' x2')

        XLet a lts x2
         |  ( used1, lts')  <- usageLets lts
         ,  ( used2, x2')   <- usageX' x2
         ,  used'           <- used1 `plusUsedMap` used2
         -> ( used'
            , XLet (used', a) lts' x2')

        -- Wrap usages in the Alts in UsedInAlt to signal that if we move
        -- the definition here then it might not be demanded at runtime.
        XCase a x1 alts
         |  ( used1, x1')   <- usageX' x1
         ,  ( usedA, alts') <- unzip $ map usageAlt alts
         ,  UsedMap usA     <- sumUsedMap usedA
         ,  usedA'          <- UsedMap (Map.map (map UsedInAlt) usA)
         ,  used'           <- plusUsedMap used1 usedA'
         -> ( used'
            , XCase (used', a) x1' alts' )

        XCast a c x1
         |  (used1, x1')   <- usageX' x1
         ,  (used2, c')    <- usageCast c
         ,  used'          <- plusUsedMap used1 used2
         -> ( used'
            , XCast (used', a) c' x1')

        XType t        
         -> (empty, XType t)

        XWitness w     
         | (used', w')    <- usageWitness w
         -> ( used'
            , XWitness w')


-- | Annotate binding occurences of named variables with usage information.
usageLets 
        :: Ord n
        => Lets a n 
        -> (UsedMap n, Lets (UsedMap n, a) n)

usageLets lts
 = case lts of
        LLet mode b x
         |  (used1', x')        <- usageX' x
         ,  (used2', mode')     <- usageMode mode
         ,  used'               <- plusUsedMap used1' used2'
         -> (used', LLet mode' b x')

        LRec bxs
         |  (bs, xs)      <- unzip bxs
         ,  (useds', xs') <- unzip $ map usageX' xs
         ,  used'         <- sumUsedMap useds'
         -> (used', LRec $ zip bs xs')

        LLetRegions b bs 
         -> (empty, LLetRegions b bs)

        LWithRegion b
         -> (empty, LWithRegion b)


-- | Annotate binding occurrences of named value variables with
--   usage information.
usageMode 
        :: Ord n
        => LetMode a n
        -> (UsedMap n, LetMode (UsedMap n, a) n)

usageMode mm
 = case mm of
        LetStrict       
         -> (empty, LetStrict)
        
        LetLazy Nothing 
         -> (empty, LetLazy Nothing)
        
        LetLazy (Just w)      
         | (used', w')  <- usageWitness w
         -> (used', LetLazy (Just w'))



-- | Annotate binding occurrences of named value variables with 
--  usage information.
usageCast  
        :: Ord n
        => Cast a n
        -> (UsedMap n, Cast (UsedMap n, a) n)
usageCast cc
 = case cc of
        CastWeakenEffect eff    
         -> (empty, CastWeakenEffect eff)

        CastWeakenClosure xs
         | (useds, xs')         <- unzip $ map usageX' xs
         , UsedMap used'        <- sumUsedMap useds
	 , usedCasts		<- Map.map (map $ const UsedInCast) used'
         -> (UsedMap usedCasts, CastWeakenClosure xs')

        CastPurify w
         | (used, w')   <- usageWitness w
         -> (used, CastPurify w')

        CastForget w
         | (used, w')   <- usageWitness w
         -> (used, CastForget w')


-- | Annotate binding occurrences of named level-0 variables with
--   usage information.
usageAlt  
        :: Ord n 
        => Alt a n  
        -> (UsedMap n, Alt (UsedMap n, a) n)

usageAlt (AAlt p x)
 = let  (used, x')      = usageX' x
   in   (used, AAlt p x')


-- | Annotate binding occurrences of named level-0 variables with
--   usage information.
usageWitness
        :: Ord n
        => Witness a n
        -> (UsedMap n, Witness (UsedMap n, a) n)

usageWitness ww
 = case ww of
        WVar a u
         -> (empty, WVar (empty, a) u)

        WCon a c
         -> (empty, WCon (empty, a) c)

        WApp a w1 w2
         | (used1, w1') <- usageWitness w1
         , (used2, w2') <- usageWitness w2
         , used'        <- plusUsedMap used1 used2
         -> (empty, WApp (used', a) w1' w2')

        WJoin a w1 w2
         | (used1, w1') <- usageWitness w1
         , (used2, w2') <- usageWitness w2
         , used'        <- plusUsedMap used1 used2
         -> (empty, WJoin (used', a) w1' w2')

        WType a t
         -> (empty, WType (empty, a) t)
