
module DDC.Core.Transform.Forward
        ( forwardModule
        , forwardX)
where
import DDC.Core.Analysis.Usage
import DDC.Core.Module
import DDC.Core.Exp
import Data.Map                 (Map)
import qualified Data.Map       as Map


forwardModule 
        :: (Show n, Ord n)
        => Module a n -> Module a n

forwardModule mm
        = forwardWith Map.empty 
        $ usageModule mm


forwardX :: (Show n, Ord n)
         => Exp a n -> Exp a n
forwardX xx
        = forwardWith Map.empty
        $ snd $ usageX xx


class Forward (c :: * -> * -> *) where
 -- | Carry bindings forward and downward into their use-sites.
 forwardWith 
        ::  (Show n, Ord n)
        => Map n (Exp a n)
        -> c (UsedMap n, a) n
        -> c a n

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

  =      ModuleCore
                { moduleName            = name
                , moduleExportKinds     = exportKinds
                , moduleExportTypes     = exportTypes
                , moduleImportKinds     = importKinds
                , moduleImportTypes     = importTypes
                , moduleBody            = forwardWith bindings body }


instance Forward Exp where
 forwardWith bindings xx
  = let down    = forwardWith bindings 
    in case xx of
        XVar a u@(UName n _t)
         -> case Map.lookup n bindings of
                Just xx'        -> xx'
                Nothing         -> XVar (snd a) u

        XVar a u        -> XVar (snd a) u
        XCon a u        -> XCon (snd a) u
        XLAM a b x      -> XLAM (snd a) b (down x)
        XLam a b x      -> XLam (snd a) b (down x)
        XApp a x1 x2    -> XApp (snd a) (down x1) (down x2)

        XLet (UsedMap um, _) (LLet _mode (BName n _) (x1@XLam{})) x2
         | Just usage     <- Map.lookup n um
         , [UsedFunction] <- usage
         , x1'            <- down x1
         -> forwardWith (Map.insert n x1' bindings) x2

        XLet (_, a') lts x     
         -> XLet a' (down lts) (down x)

        XCase a x alts  -> XCase (snd a) (down x) (map down alts)
        XCast a c x     -> XCast (snd a) c (down x)
        XType t         -> XType t
        XWitness w      -> XWitness w


instance Forward Lets where
 forwardWith bindings lts
  = let down    = forwardWith bindings
    in case lts of
        LLet mode b x   -> LLet mode b (down x)
        LRec bxs        -> LRec [(b, down x) | (b, x) <- bxs]
        LLetRegion b bs -> LLetRegion b bs
        LWithRegion b   -> LWithRegion b


instance Forward Alt where
 forwardWith bindings (AAlt p x)
  = AAlt p (forwardWith bindings x)



