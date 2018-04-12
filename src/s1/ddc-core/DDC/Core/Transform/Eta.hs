
-- Suppress Data.Monoid warnings during GHC 8.4.1 transition
{-# OPTIONS  -Wno-unused-imports #-}

-- Eta-expand functional values.
---
-- NOTE: This is module currently just does eta-expansion, but in future
--       we should expand the config to also make it do expansion/contraction
--       based on the real arity of bindings.
--
module DDC.Core.Transform.Eta
        ( Config(..)
        , configZero
        , Info  (..)
        , etaModule
        , etaX)
where
import qualified DDC.Core.Check as Check
import DDC.Core.Module
import DDC.Core.Exp.Annot
import DDC.Core.Fragment
import DDC.Core.Transform.BoundX
import DDC.Core.Transform.BoundT
import DDC.Core.Simplifier.Result
import DDC.Type.Transform.AnonymizeT
import Control.Monad.Writer     (Writer, tell, runWriter)
import Data.Typeable
import DDC.Core.Env.EnvX                        (EnvX)
import qualified DDC.Core.Env.EnvX              as EnvX
import qualified DDC.Core.Codec.Text.Pretty     as P
import Prelude                                  hiding ((<$>))

-- GHC 8.2 -> 8.4 transition.
import Data.Semigroup                   (Semigroup(..), Monoid(..))


-------------------------------------------------------------------------------
data Config
        = Config
        { configExpand  :: Bool }
        deriving Show


-- | Empty eta configuration with all flags set to False.
configZero :: Config
configZero
        = Config
        { configExpand  = False }


-------------------------------------------------------------------------------
data Info
        = Info
        { -- | Number of level-1 lambdas added.
          infoExpandedXLAMs     :: Int

          -- | Number of level-0 lambdas added.
        , infoExpandedXLams     :: Int }
        deriving Typeable


instance P.Pretty Info where
 ppr (Info ex1 ex0)
  = P.text "Eta Transform"
  P.<$> P.indent 4 (P.vcat
      [ P.text "level-1 lambdas added:     " P.<> P.int ex1
      , P.text "level-0 lambdas added:     " P.<> P.int ex0 ])


instance Semigroup Info where
 (<>)           = unionInfo


instance Monoid Info where
 mempty         = emptyInfo
 mappend        = unionInfo


-- | Construct an empty Info record.
emptyInfo :: Info
emptyInfo       = Info 0 0


-- | Union two info records.
unionInfo :: Info -> Info -> Info
unionInfo (Info ex1 ex0) (Info ex1' ex0')
  = Info (ex1 + ex1') (ex0 + ex0')


-- Module ---------------------------------------------------------------------
-- | Eta-transform expressions in a module.
etaModule
        :: (Ord n, Show n, P.Pretty n, Show a)
        => Profile n
        -> Config
        -> Module a n
        -> TransformResult (Module a n)

etaModule profile config  mm
 = let
        -- Slurp type checker config.
        cconfig = Check.configOfProfile profile

        -- Slurp the top level environment.
        env     = moduleEnvX
                        (profilePrimKinds    profile)
                        (profilePrimTypes    profile)
                        (profilePrimDataDefs profile)
                        mm

        -- Run the eta transform.
        (mm', info) = runWriter $ etaM config cconfig env mm

        -- Check if any actual work was performed
        progress
         = case info of
                Info ex1 ex0
                 -> ex1 + ex0 > 0

   in   TransformResult
         { result         = mm'
         , resultAgain    = False
         , resultProgress = progress
         , resultInfo     = TransformInfo info }


-- | Eta-transform an expression.
etaX    :: (Ord n, Show n, Show a, P.Pretty n)
        => Profile n            -- ^ Language profile.
        -> Config               -- ^ Eta-transform config.
        -> EnvX    n            -- ^ Type checker environment.
        -> Exp a n              -- ^ Expression to transform.
        -> TransformResult (Exp a n)

etaX profile config env xx
 = let  cconfig = Check.configOfProfile profile

        -- Run the eta transform.
        (xx', info)
                = runWriter
                $ etaM config cconfig env xx

        -- Check if any actual work was performed
        progress
         = case info of
                Info ex1 ex0
                 -> ex1 + ex0 > 0

   in   TransformResult
         { result         = xx'
         , resultAgain    = False
         , resultProgress = progress
         , resultInfo     = TransformInfo info }


-------------------------------------------------------------------------------
class Eta (c :: * -> * -> *) where
 etaM   :: (Show a, Ord n, P.Pretty n, Show n)
        => Config               -- ^ Eta-transform config.
        -> Check.Config n       -- ^ Type checker config.
        -> EnvX n               -- ^ Type checker environment.
        -> c a n                -- ^ Do eta-expansion in this thing.
        -> Writer Info (c a n)


instance Eta Module where
 etaM config cconfig envx mm
  = do  -- The top level environment of the module is added
        -- by the etaModule wrapper, so we don't need to do it again.
        xx' <- etaM config cconfig envx (moduleBody mm)
        return  $ mm { moduleBody = xx' }


instance Eta Exp where
 etaM config cconfig env xx
  = let down = etaM config cconfig env
    in case xx of

        XVar a _
         | configExpand config
         , Right tX     <- Check.typeOfExp cconfig env xx
         -> do  etaExpand a tX xx

        XApp a _ _
         |  configExpand config
         ,  Right tX    <- Check.typeOfExp cconfig env xx
         -> do
                -- Decend into the arguments first.
                --   We don't need to decend into the function part because
                --   we're eta-expanding that here.
                let (x, as)    =  takeXAppsAsList xx
                as_eta          <- mapM down as

                -- Now eta expand the result.
                etaExpand a tX $ xApps a x as_eta

        XLAM a b x
         -> do  let env'        = EnvX.extendT b env
                x'              <- etaM config cconfig env' x
                return $ XLAM a b x'

        XLam a b x
         -> do  let env'        = EnvX.extendX b env
                x'              <- etaM config cconfig env' x
                return $ XLam a b x'

        XLet a lts x2
         -> do  lts'            <- down lts
                let (bs1, bs0)  = bindsOfLets lts

                let env'        = EnvX.extendsT bs1
                                $ EnvX.extendsX bs0 env

                x2'             <- etaM config cconfig env' x2
                return $ XLet a lts' x2'

        XCase a x alts
         -> do  x'              <- down x
                alts'           <- mapM (etaM config cconfig env) alts
                return $ XCase a x' alts'

        XCast a cc x
         -> do  x'              <- down x
                return $ XCast a cc x'

        _ -> return xx

instance Eta Arg where
 etaM config cconfig env aa
  = let down    = etaM config cconfig env
    in case aa of
        RType{}         -> return aa
        RWitness{}      -> return aa
        RTerm x         -> fmap RTerm     $ down x
        RImplicit x     -> fmap RImplicit $ down x


instance Eta Lets where
 etaM config cconfig env lts
  = let down    = etaM config cconfig env
    in case lts of
        LLet b x
         -> do  x'      <- down x
                return  $ LLet b x'

        LRec bxs
         -> do  let bs    = map fst bxs
                let env'  = EnvX.extendsX bs env
                xs'       <- mapM (etaM config cconfig env')
                          $  map snd bxs
                return    $ LRec (zip bs xs')

        LPrivate{}
         -> return lts


instance Eta Alt where
 etaM config cconfig env alt
  = case alt of
        AAlt p x
         -> do  let bs    = bindsOfPat p
                let env'  = EnvX.extendsX bs env
                x'        <- etaM config cconfig env' x
                return  $ AAlt p x'


-- Expand ---------------------------------------------------------------------
-- | Eta expand an expression.
etaExpand
        :: Ord n
        => a                    -- ^ Annotation to use for new AST nodes.
        -> Type n               -- ^ Type of the expression.
        -> Exp a n              -- ^ Inner expression to wrap.
        -> Writer Info (Exp a n)

etaExpand a tX xx
        -- Anonymize the type, so any references to foralls will become anonymous.
        -- Then, when we add the anonymous bindings, it will work out.
 = do   let btsMore     = expandableArgs $ anonymizeT tX
        xx'             <- etaExpand' a 0 0 [] btsMore xx
        return xx'


-- | Decide what type arguments need to be eta-expanded.
expandableArgs :: Type n -> [(Bool, Type n)]
expandableArgs tt
        | TForall b t'          <- tt
        = (True, typeOfBind b)  : expandableArgs t'

        | Just (t1, t2)         <- takeTFun tt
        = (False, t1)           : expandableArgs t2

        | otherwise
        = []


-- | Eta-expand an expression.
etaExpand'
        :: Ord n
        => a                -- ^ Annotation to use for the new AST nodes.
        -> Int              -- ^ Number of level-1 lambdas we've added so far.
        -> Int              -- ^ Number of level-0 lambdas we've added so far.
        -> [Arg a n]        -- ^ Accumulate arguments we need to add to the
                            --      inner expression.
        -> [(Bool, Type n)] -- ^ Types of bindings we need to add, along with
                            --   a flag to indicate level-1 or level-0 binder
        -> Exp a n          -- ^ Inner expression that is being applied.
        -> Writer Info (Exp a n)

etaExpand' a levels1 levels0 args [] xx
 = do   let xx' = liftT levels1 $ liftX levels0 xx
        return  $ xApps a xx' args

etaExpand' a levels1 levels0 args ((True, t) : ts) xx
 = do   let depth1 = length $ filter ((== True) . fst) ts
        xx'     <- etaExpand' a (levels1 + 1) levels0
                        (args ++ [RType (TVar (UIx depth1))])
                        ts
                        xx

        tell mempty { infoExpandedXLAMs = 1 }
        return  $ XLAM a (BAnon t) xx'

etaExpand' a levels1 levels0 args ((False, t) : ts) xx
 = do   let depth0 = length $ filter ((== False) . fst) ts
        xx'     <- etaExpand' a
                        levels1 (levels0 + 1)
                        (args ++ [RTerm (XVar a (UIx depth0))])
                        ts
                        xx

        tell mempty { infoExpandedXLams = 1 }
        return  $ XLam a (BAnon t) xx'

