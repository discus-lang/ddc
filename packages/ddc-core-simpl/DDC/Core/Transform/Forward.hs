
-- | Float let-bindings with a single use forward into their use-sites.
module DDC.Core.Transform.Forward
        ( ForwardInfo   (..)
        , FloatControl  (..)
        , Config(..)
        , forwardModule
        , forwardX)
where
import DDC.Base.Pretty
import DDC.Core.Analysis.Usage
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Core.Simplifier.Base
import DDC.Core.Transform.Reannotate
import DDC.Core.Fragment
import DDC.Core.Predicates
import DDC.Core.Compounds
import Data.Map                 (Map)
import Control.Monad
import Control.Monad.Writer     (Writer, runWriter, tell)
import Data.Monoid              (Monoid, mempty, mappend)
import Data.Typeable
import qualified Data.Map                               as Map
import qualified DDC.Core.Transform.SubstituteXX        as S

-------------------------------------------------------------------------------
-- | Summary of number of bindings floated.
data ForwardInfo
        = ForwardInfo
        { -- | Number of bindings inspected.
          infoInspected :: !Int

          -- | Number of trivial @v1 = v2@ bindings inlined.
        , infoSubsts    :: !Int

          -- | Number of bindings floated forwards.
        , infoBindings  :: !Int }
        deriving Typeable


instance Pretty ForwardInfo where
 ppr (ForwardInfo inspected substs bindings)
  =  text "Forward:"
  <$> indent 4 (vcat
      [ text "Total bindings inspected:      " <> int inspected
      , text "  Trivial substitutions made:  " <> int substs
      , text "  Bindings moved forward:      " <> int bindings ])


instance Monoid ForwardInfo where
 mempty = ForwardInfo 0 0 0
 mappend (ForwardInfo i1 s1 b1)(ForwardInfo i2 s2 b2)
        = ForwardInfo (i1 + i2) (s1 + s2) (b1 + b2)


-------------------------------------------------------------------------------
-- | Fine control over what should be floated.
data FloatControl
        = FloatAllow    -- ^ Allow binding to be floated, but don't require it.
        | FloatDeny     -- ^ Prevent a binding being floated, at all times.
        | FloatForce    -- ^ Force   a binding to be floated, at all times.
        deriving (Eq, Show)

data Config a n
        = Config
        { configFloatControl    :: Lets a n -> FloatControl
        , configFloatLetBody    :: Bool }

-------------------------------------------------------------------------------
-- | Float let-bindings in a module with a single use forward into
--   their use sites.
forwardModule 
        :: Ord n
        => Profile n    -- ^ Language profile
        -> Config a n
        -> Module a n 
        -> TransformResult (Module a n)

forwardModule profile config mm
 = let  (mm', info)
         = runWriter
                $ forwardWith profile config Map.empty 
                $ usageModule mm

        progress (ForwardInfo _ s f)
                = s + f > 0

   in   TransformResult
         { result         = mm'
         , resultProgress = progress info
         , resultAgain    = False
         , resultInfo     = TransformInfo info }


-- | Float let-bindings in an expression with a single use forward into
--   their use-sites.
forwardX :: Ord n
         => Profile n   -- ^ Language profile.
         -> Config a n 
         -> Exp a n                      
         -> TransformResult (Exp a n)

forwardX profile config xx
 = let  (x',info) = runWriter
                  $ forwardWith profile config Map.empty
                  $ usageX xx

        progress (ForwardInfo _ s f) 
                = s + f > 0

   in  TransformResult
        { result         = x'
        , resultProgress = progress info
        , resultAgain    = False
        , resultInfo     = TransformInfo info }


-------------------------------------------------------------------------------
class Forward (c :: * -> * -> *) where
 -- | Carry bindings forward and downward into their use-sites.
 forwardWith 
        :: Ord n
        => Profile n            -- ^ Language profile.
        -> Config a n
        -> Map n (Exp a n)      -- ^ Bindings currently being carried forward.
        -> c (UsedMap n, a) n
        -> Writer ForwardInfo (c a n)

instance Forward Module where
 forwardWith profile config bindings 
        (ModuleCore
                { moduleName            = name
                , moduleExportKinds     = exportKinds
                , moduleExportTypes     = exportTypes
                , moduleImportKinds     = importKinds
                , moduleImportTypes     = importTypes
                , moduleDataDefsLocal   = dataDefsLocal
                , moduleBody            = body })

  = do  body' <- forwardWith profile config bindings body
        return ModuleCore
                { moduleName            = name
                , moduleExportKinds     = exportKinds
                , moduleExportTypes     = exportTypes
                , moduleImportKinds     = importKinds
                , moduleImportTypes     = importTypes
                , moduleDataDefsLocal   = dataDefsLocal
                , moduleBody            = body' }


instance Forward Exp where
 forwardWith profile config bindings xx
  = {-# SCC forwardWith #-}
    let down    = forwardWith profile config bindings 
    in case xx of
        XVar a u@(UName n)
         -> case Map.lookup n bindings of
                Just xx'        -> do
                    tell mempty { infoSubsts = 1 }
                    return xx'
                Nothing         ->
                    return $ XVar (snd a) u

        XVar a u        -> return $ XVar (snd a) u
        XCon a u        -> return $ XCon (snd a) u
        XLAM a b x      -> liftM    (XLAM (snd a) b) (down x)
        XLam a b x      -> liftM    (XLam (snd a) b) (down x)
        XApp a x1 x2    -> liftM2   (XApp (snd a))   (down x1) (down x2)

        -- Always float last let-binding into its use.
        --   let x = exp in x => exp
        XLet _ (LLet b x1) (XVar _ u)
         |  boundMatchesBind u b
         ,  configFloatLetBody config
         -> down x1

        -- Always float atomic bindings (variables, constructors)
        XLet _ (LLet b x1) x2
         | isAtomX x1
         -> do 
                -- Record that we've moved this binding.
                tell mempty { infoInspected = 1
                            , infoBindings  = 1 }

                -- Slow, but handles anonymous binders and shadowing
                down $ S.substituteXX b x1 x2

        XLet (UsedMap um, a') lts@(LLet (BName n _) x1) x2
         -> do  
                let control    = configFloatControl config 
                               $ reannotate snd lts

                let isFun      = isXLam x1 || isXLAM x1

                let isApplied
                     | Just usage       <- Map.lookup n um
                     , [UsedFunction]   <- filterUsedInCasts usage
                                        = True
                     | otherwise        = False

                let shouldFloat
                     = case control of
                        FloatDeny       -> False
                        FloatForce      -> True
                        FloatAllow      -> isFun && isApplied

                if shouldFloat 
                 then do
                        -- Record that we've moved this binding.
                        tell mempty { infoInspected = 1
                                    , infoBindings  = 1 }

                        x1'             <- down x1
                        let bindings'   = Map.insert n x1' bindings
                        forwardWith profile config bindings' x2

                 else do        
                        tell mempty { infoInspected = 1}
                        liftM2 (XLet a') (down lts) (down x2)

        XLet (_, a') lts x     
         ->     liftM2 (XLet a') (down lts) (down x)

        XCase a x alts  -> liftM2 (XCase    (snd a)) (down x) (mapM down alts)
        XCast a c x     -> liftM2 (XCast    (snd a)) (down c) (down x)
        XType a t       -> return (XType    (snd a) t)
        XWitness a w    -> return (XWitness (snd a) (reannotate snd w))


filterUsedInCasts :: [Used] -> [Used]
filterUsedInCasts = filter notCast
 where  notCast UsedInCast      = False
        notCast _               = True


instance Forward Cast where
 forwardWith profile config bindings xx
  = let down    = forwardWith profile config bindings
    in case xx of
        CastWeakenEffect eff    -> return $ CastWeakenEffect eff
        CastWeakenClosure xs    -> liftM    CastWeakenClosure (mapM down xs)
        CastPurify w            -> return $ CastPurify (reannotate snd w)
        CastForget w            -> return $ CastForget (reannotate snd w)
        CastSuspend             -> return $ CastSuspend
        CastRun                 -> return $ CastRun


instance Forward Lets where
 forwardWith profile config bindings lts
  = let down    = forwardWith profile config bindings
    in case lts of
        LLet b x   
         -> liftM (LLet b) (down x)

        LRec bxs        
         -> liftM LRec
         $  mapM (\(b,x) 
                    -> do x' <- down x
                          return (b, x')) 
            bxs

        LLetRegions b bs -> return $ LLetRegions b bs
        LWithRegion b    -> return $ LWithRegion b


instance Forward Alt where
 forwardWith profile config bindings (AAlt p x)
  = liftM (AAlt p) (forwardWith profile config bindings x)

