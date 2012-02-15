
module DDC.Core.Eval.Check 
        ( checkCapsX
        , Error(..))
where
import DDC.Core.Eval.Compounds
import DDC.Core.Eval.Name
import DDC.Core.Exp
import DDC.Type.Check.Monad             (throw, result)
import DDC.Base.Pretty
import qualified DDC.Type.Check.Monad   as G
import Control.Monad
import Data.Maybe

type CheckM a = G.CheckM (Error a)


checkCapsX :: Exp a Name -> Either (Error a) [Witness Name]
checkCapsX xx = result $ checkCapsXM xx


-- Error ----------------------------------------------------------------------
-- | Things that can go wrong with the capabilities in a program.
data Error a 
        -- | Conflicting capabilities in program.
        = ErrorConflict 
        { errorWitness1 :: Witness Name
        , errorWitness2 :: Witness Name }

        -- | A partially applied capability constructor.
        --  
        --   In the formal semantics, capabilities are atomic, so this isn't
        --   a problem. However, as we're representing them with general witness
        --   appliction we need to ensure the constructors aren't partially 
        --   applied.
        | ErrorPartial
        { errorWitness  :: Witness Name }

        -- | A capability constructor applied to a non-region handle.
        --
        --   As with `ErrorPartial` we only need to check for this because we're
        --   using general witness application to represent capabilities, instead
        --   of having an atomic form. 
        | ErrorNonHandle
        { errorWitness  :: Witness Name }


instance Pretty (Error a) where
 ppr err
  = case err of
        ErrorConflict w1 w2
         -> vcat [ text "Conflicting capabilities in core program."
                 , text "      capability: "            <> ppr w1
                 , text "  conflicts with: "            <> ppr w2 ]

        ErrorPartial w1
         -> vcat [ text "Partially applied capability constructor."
                 , text "with: "                        <> ppr w1]

        ErrorNonHandle w1
         -> vcat [ text "Capability constructor applied to a non-region handle."
                 , text "with: "                        <> ppr w1]


-------------------------------------------------------------------------------
-- | Collect the list of capabilities in an expression, 
--   and check that they are well-formed.
checkCapsXM :: Exp a Name -> CheckM a [Witness Name]
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


checkCapsCM :: Cast Name -> CheckM a [Witness Name]
checkCapsCM cc
 = let none     = return []
   in case cc of
        CastWeakenEffect{}      -> none
        CastWeakenClosure{}     -> none
        CastPurify w            -> checkCapsWM w
        CastForget w            -> checkCapsWM w


checkCapsLM :: Lets a Name -> CheckM a [Witness Name]
checkCapsLM ll
 = let none     = return []
   in case ll of
        LLet m _ x      -> liftM2 (++) (checkCapsMM m) (checkCapsXM x)
        LRec bxs        -> liftM  concat (mapM checkCapsXM $ map snd bxs)
        LLetRegion{}    -> none
        LWithRegion{}   -> none


checkCapsMM :: LetMode Name -> CheckM a [Witness Name]
checkCapsMM mm
 = let none     = return []
   in case mm of
        LetStrict        -> none
        LetLazy (Just w) -> checkCapsWM w
        LetLazy Nothing  -> none


checkCapsAM :: Alt a Name  -> CheckM a [Witness Name]
checkCapsAM aa
 = case aa of
        AAlt _ x        -> checkCapsXM x


checkCapsWM :: Witness Name -> CheckM a [Witness Name]
checkCapsWM ww
 = let none     = return []
   in case ww of
        WVar{}             -> none

        WCon{}
         | isCapConW ww    -> throw $ ErrorPartial ww
         | otherwise       -> none


        WApp w1@WCon{} w2@(WType tR)
         | isCapConW w1
         -> if isJust $ takeHandleT tR 
                then return [ww]
                else throw $ ErrorNonHandle ww

         |  otherwise
         -> liftM2 (++) (checkCapsWM w1) (checkCapsWM w2)

        WApp w1 w2         -> liftM2 (++) (checkCapsWM w1) (checkCapsWM w2)
        WJoin w1 w2        -> liftM2 (++) (checkCapsWM w1) (checkCapsWM w2)
        WType{}            -> none

