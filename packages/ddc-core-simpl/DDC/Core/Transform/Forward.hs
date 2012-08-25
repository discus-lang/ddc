
module DDC.Core.Transform.Forward
        ( forwardModule
        , forwardX)
where
import DDC.Base.Pretty
import DDC.Core.Analysis.Usage
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Core.Simplifier.Base
import DDC.Core.Predicates
import Data.Map                 (Map)
import qualified Data.Map       as Map
import Control.Monad		(liftM, liftM2)
import Control.Monad.Writer	(Writer, runWriter, tell)
import Data.Monoid		(Monoid, mempty, mappend)
import Data.Typeable		(Typeable)

import qualified DDC.Core.Transform.SubstituteXX	as S

forwardModule 
        :: (Show n, Ord n)
        => Module a n -> Module a n

forwardModule mm
        = fst
	$ runWriter
	$ forwardWith Map.empty 
        $ usageModule mm


forwardX :: (Show n, Ord n)
         => Exp a n -> TransformResult (Exp a n)
forwardX xx
 = let (x',info) = runWriter
		 $ forwardWith Map.empty
		 $ snd $ usageX xx
   in  TransformResult
	{ result	 = x'
	, resultProgress = progress info
	, resultInfo	 = TransformInfo info }
 where
  progress (ForwardInfo s _) = s > 0


class Forward (c :: * -> * -> *) where
 -- | Carry bindings forward and downward into their use-sites.
 forwardWith 
        ::  (Show n, Ord n)
        => Map n (Exp a n)
        -> c (UsedMap n, a) n
        -> Writer ForwardInfo (c a n)

-- TODO: want a nicer way of transforming the body of a module.
--       the body type changes. Just write this boilerplate once.
instance Forward Module where
 forwardWith bindings 
        (ModuleCore
                { moduleName            = name
                , moduleExportKinds     = exportKinds
                , moduleExportTypes     = exportTypes
                , moduleImportKinds     = importKinds
                , moduleImportTypes     = importTypes
                , moduleBody            = body })

  = do	body' <- forwardWith bindings body
	return ModuleCore
		{ moduleName            = name
		, moduleExportKinds     = exportKinds
		, moduleExportTypes     = exportTypes
		, moduleImportKinds     = importKinds
		, moduleImportTypes     = importTypes
		, moduleBody            = body' }


instance Forward Exp where
 forwardWith bindings xx
  = let down    = forwardWith bindings 
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

        XLet (UsedMap um, _) (LLet _mode (BName n _) x1) x2
         | isXLam x1 || isXLAM x1
         , Just usage     <- Map.lookup n um
         , [UsedFunction] <- usage
	 -> do
	    tell mempty { infoBindings = 1 }
	    x1'           <- down x1
            forwardWith (Map.insert n x1' bindings) x2

	-- Always forward atomic bindings (variables, constructors)
        XLet _ (LLet _mode b x1) x2
	 | isAtomX x1
	 -> do
	    tell mempty { infoBindings = 1 }
	    -- Slow, but handles anonymous binders and shadowing
	    down $ S.substituteXX b x1 x2

        XLet (_, a') lts x     
         -> liftM2 (XLet a') (down lts) (down x)

        XCase a x alts  -> liftM2   (XCase (snd a)) (down x) (mapM down alts)
        XCast a c x     -> liftM2   (XCast (snd a)) (down c) (down x)
        XType t         -> return $ XType t
        XWitness w      -> return $ XWitness w


instance Forward Cast where
 forwardWith bindings xx
  = let down    = forwardWith bindings
    in case xx of
        CastWeakenEffect eff    -> return $ CastWeakenEffect eff
        CastWeakenClosure xs    -> liftM    CastWeakenClosure (mapM down xs)
        CastPurify w            -> return $ CastPurify w
        CastForget w            -> return $ CastForget w


instance Forward Lets where
 forwardWith bindings lts
  = let down    = forwardWith bindings
    in case lts of
        LLet mode b x   -> liftM (LLet mode b) (down x)
        LRec bxs        -> liftM LRec
		(mapM (\(b,x) -> do
			x' <- down x
			return (b, x')) bxs)
        LLetRegion b bs -> return $ LLetRegion b bs
        LWithRegion b   -> return $ LWithRegion b


instance Forward Alt where
 forwardWith bindings (AAlt p x)
  = liftM (AAlt p) (forwardWith bindings x)



-- | Summary of number forwards performed
data ForwardInfo
    = ForwardInfo
    { infoSubsts   :: Int
    , infoBindings :: Int }
    deriving Typeable


instance Pretty ForwardInfo where
 ppr (ForwardInfo substs bindings)
  =  text "Forward:"
  <$> indent 4 (vcat
      [ text "Substitutions:  "	<> int substs
      , text "Bindings:       "	<> int bindings ])


instance Monoid ForwardInfo where
 mempty = ForwardInfo 0 0
 mappend
    (ForwardInfo s1 b1)
    (ForwardInfo s2 b2)
  =  ForwardInfo (s1+s2) (b1+b2)

