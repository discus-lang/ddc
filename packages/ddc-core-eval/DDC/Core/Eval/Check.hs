
-- | Check for conflicting store capabilities in the initial program.
module DDC.Core.Eval.Check 
        ( checkCapsModule
        , checkCapsX
        , Error(..))
where
import DDC.Core.Eval.Compounds
import DDC.Core.Eval.Name
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Core.Transform.Reannotate
import DDC.Base.Pretty
import Control.Monad
import Data.Maybe
import Data.Set                                 (Set)
import DDC.Control.Monad.Check                  (throw, result)
import qualified DDC.Control.Monad.Check        as G
import qualified Data.Set                       as Set


-- | Capability Checking monad.
type CheckM a x 
        = G.CheckM (Error a) x


-- | Check for conflicting store capabilities in a module.
checkCapsModule :: Module a Name -> Maybe (Error a)
checkCapsModule mm
        = checkCapsX $ moduleBody mm


-- | Check for conflicting store capabilities in an expression.
checkCapsX :: Exp a Name -> Maybe (Error a)
checkCapsX xx 
 = case result $ checkCapsXM xx of
        Left err        -> Just err
        Right ws
         -> let caps    = foldr mustInsertCap emptyCapSet ws
            in  checkCapSet caps


-- CapSet --------------------------------------------------------------------
-- | Set of used capabilities.
data CapSet 
        = CapSet
        { capsGlobal    :: Set Rgn 
        , capsConst     :: Set Rgn
        , capsMutable   :: Set Rgn
        , capsDistinct  :: Set [Rgn]
        , capsLazy      :: Set Rgn
        , capsManifest  :: Set Rgn }
        deriving Show


-- | An empty capability set
emptyCapSet :: CapSet
emptyCapSet 
        = CapSet
        { capsGlobal    = Set.empty
        , capsConst     = Set.empty
        , capsMutable   = Set.empty
        , capsDistinct  = Set.empty
        , capsLazy      = Set.empty
        , capsManifest  = Set.empty }


-- | Insert a capability, or `error` if this isn't one.
mustInsertCap :: Witness a Name -> CapSet -> CapSet
mustInsertCap ww caps
 | WApp _ (WCon  _ (WiConBound       (UPrim nc _) _)) 
          (WType _ (TCon (TyConBound (UPrim nh _) _))) <- ww
 , NameCap c     <- nc
 , NameRgn r     <- nh
 = case c of
        CapGlobal       -> caps { capsGlobal   = Set.insert r (capsGlobal  caps) }
        CapConst        -> caps { capsConst    = Set.insert r (capsConst   caps) }
        CapMutable      -> caps { capsMutable  = Set.insert r (capsMutable caps) }
        CapLazy         -> caps { capsLazy     = Set.insert r (capsLazy    caps) }
        CapManifest     -> caps { capsManifest = Set.insert r (capsManifest caps)}
        _               -> error "mustInsertCap: invalid witness application"
        
 | Just (NameCap (CapDistinct _), ws) <- takePrimWiConApps ww
 , ws'                                <- map takeNameRgn ws  
 , all isJust ws'
 = caps { capsDistinct = Set.insert (catMaybes ws') (capsDistinct caps) }
 
 | otherwise
 = error "mustInsertCap: not a capability"


-- | Take a region name from a witness argument.
takeNameRgn :: Witness a Name -> Maybe Rgn
takeNameRgn (WType _ (TCon (TyConBound (UPrim (NameRgn r) _) _))) = Just r
takeNameRgn _                                                     = Nothing


-- | Check a capability set for conflicts between the capabilities.
checkCapSet :: CapSet -> Maybe (Error a)
checkCapSet cs 
        | r : _  <- Set.toList 
                 $  Set.intersection (capsConst cs) (capsMutable  cs)
        = Just $ ErrorConflict r CapConst CapMutable 

        | r : _  <- Set.toList 
                 $  Set.intersection (capsLazy  cs) (capsManifest cs)
        = Just $ ErrorConflict r CapLazy  CapManifest

        | otherwise
        = Nothing


-- Error ----------------------------------------------------------------------
-- | Things that can go wrong with the capabilities in a program.
data Error a
        -- | Conflicting capabilities in program.
        = ErrorConflict 
        { errorRegions  :: Rgn
        , errorCap1     :: Cap
        , errorCap2     :: Cap }

        -- | A partially applied capability constructor.
        --   In the formal semantics, capabilities are atomic, so this isn't
        --   a problem. However, as we're representing them with general witness
        --   appliction we need to ensure the constructors aren't partially 
        --   applied.
        | ErrorPartial
        { errorWitness  :: Witness () Name }

        -- | A capability constructor applied to a non-region handle.
        --   As with `ErrorPartial` we only need to check for this because we're
        --   using general witness application to represent capabilities, instead
        --   of having an atomic form. 
        | ErrorNonHandle
        { errorWitness  :: Witness () Name }


instance Pretty (Error a) where
 ppr err
  = case err of
        ErrorConflict r c1 c2
         -> vcat [ text "Conflicting capabilities in core program."
                 , text "        region: "              <> ppr r
                 , text " can't be both: "              <> ppr c1
                 , text "           and: "              <> ppr c2 ]

        ErrorPartial w1
         -> vcat [ text "Partially applied capability constructor."
                 , text "with: "                        <> ppr w1]

        ErrorNonHandle w1
         -> vcat [ text "Capability constructor applied to a non-region handle."
                 , text "with: "                        <> ppr w1]


-------------------------------------------------------------------------------
-- | Collect the list of capabilities in an expression, 
--   and check that they are well-formed.
checkCapsXM :: Exp a Name -> CheckM a [Witness a Name]
checkCapsXM xx
 = let none    = return []
   in case xx of
        XVar{}          -> none
        XCon{}          -> none
        XLAM _ _ x      -> checkCapsXM x
        XLam _ _ x      -> checkCapsXM x
        XApp _   x1 x2  -> liftM2 (++)  (checkCapsXM x1)  (checkCapsXM x2)
        XLet _ lts x1   -> liftM2 (++)  (checkCapsLM lts) (checkCapsXM x1)
        XCase _ x1 alts -> liftM2 (++)  (checkCapsXM x1)    
                                        (liftM concat $ mapM checkCapsAM alts)
        XCast _ cc x1   -> liftM2 (++)  (checkCapsCM cc)  (checkCapsXM x1)
        XType{}         -> none
        XWitness w      -> checkCapsWM w


checkCapsCM :: Cast a Name -> CheckM a [Witness a Name]
checkCapsCM cc
 = let none     = return []
   in case cc of
        CastWeakenEffect{}
         -> none

        CastWeakenClosure xs
         -> liftM concat $ mapM checkCapsXM xs

        CastPurify w
         -> checkCapsWM w

        CastForget w
         -> checkCapsWM w


checkCapsLM :: Lets a Name -> CheckM a [Witness a Name]
checkCapsLM ll
 = let none     = return []
   in case ll of
        LLet _ x        -> checkCapsXM x
        LRec bxs        -> liftM  concat (mapM checkCapsXM $ map snd bxs)
        LLetRegions{}   -> none
        LWithRegion{}   -> none


checkCapsAM :: Alt a Name  -> CheckM a [Witness a Name]
checkCapsAM aa
 = case aa of
        AAlt _ x        -> checkCapsXM x


checkCapsWM :: Witness a Name -> CheckM a [Witness a Name]
checkCapsWM ww
 = let none     = return []
   in case ww of
        WVar{}             -> none

        WCon{}
         | isCapConW ww    -> throw $ ErrorPartial (reannotate (const ()) ww)
         | otherwise       -> none


        WApp _ w1@WCon{} w2@(WType _ tR)
         | isCapConW w1
         -> if isJust $ takeHandleT tR 
                then return [ww]
                else throw $ ErrorNonHandle (reannotate (const ()) ww)

         |  otherwise
         -> liftM2 (++) (checkCapsWM w1) (checkCapsWM w2)

        WApp  _ w1 w2       -> liftM2 (++) (checkCapsWM w1) (checkCapsWM w2)
        WJoin _ w1 w2      -> liftM2 (++) (checkCapsWM w1) (checkCapsWM w2)
        WType{}            -> none

