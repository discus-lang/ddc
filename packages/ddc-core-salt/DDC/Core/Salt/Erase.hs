-- | Extract region variables from DDC Core Salt
--   
--   For now these variables are discarded
--
module DDC.Core.Salt.Erase
        (eraseM)
where

import DDC.Type.Exp
import DDC.Type.Sum
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Core.Transform.TransformX

import Control.Arrow (first, second)
import qualified Data.Map as Map

-- | Completely erase region types and kinds from a module
eraseM :: (Show n, Ord n) => Module a n -> Module a n
eraseM mm@(ModuleCore { moduleExportKinds = eks
                      , moduleExportTypes = ets
                      , moduleImportKinds = iks
                      , moduleImportTypes = its
                      , moduleBody        = xx })
  = mm { moduleExportKinds = Map.map eraseT eks
       , moduleExportTypes = Map.map eraseT ets
       , moduleImportKinds = Map.map (second eraseT) iks
       , moduleImportTypes = Map.map (second eraseT) its
       , moduleBody        = transformUpX' eraseX xx }


-------------------------------------------------------------------------------        
class EraseT (c :: * -> *) where
  eraseT :: (Show n, Ord n) => c n -> c n
  
instance EraseT Type where
  eraseT tt
    = case tt of
        TVar    u                       -> TVar (eraseT u)
        TCon    c                       -> TCon (eraseT c)
        TForall b t
          | isRegionKind $ typeOfBind b -> eraseT t
          | otherwise                   -> TForall (eraseT b) (eraseT t)  
                
        -- Erase region type applications
        TApp   t1 t2
          | isRegionTypeVar t2          -> eraseT t1   
          | isRegionKind t2             -> eraseT t1    
        
        -- Erase witness type applications. Witnesses themselves are implications, 
        --   so we must match on the witness constructor inside.
        TApp (TApp t1 _) t2
          | isWitnessType t1            -> eraseT t2

        TApp   t1 t2                    -> TApp (eraseT t1) (eraseT t2)
        TSum   ts                       -> TSum (eraseT ts)
        
instance EraseT TypeSum where
  eraseT ts = fromList (eraseT $ kindOfSum ts) $ map eraseT $ toList ts
  
instance EraseT Bind where
  eraseT bb
    = case bb of
        BName n t
          -- Erase region binders
          | isRegionKind t -> BNone (eraseT t)  
          
          -- Erase witness binders
          | isWitnessType t-> BNone (eraseT t)
                   
          | otherwise      -> BName n (eraseT t)                
        BAnon t            -> BAnon (eraseT t)
        BNone t            -> BNone (eraseT t)              

instance EraseT Bound where
  eraseT _uu
        = error "eraseT needs environment"
 {-}   | isRegionKind $ typeOfBound uu 
    = UHole (typeOfBound uu)

    | otherwise                     
    = case uu of
        UIx   i k -> UIx   i (eraseT k)
        UName n k -> UName n (eraseT k)
        UPrim n k -> UPrim n (eraseT k)
        UHole k   -> UHole   (eraseT k)
-}
    
instance EraseT TyCon where
  eraseT cc
    = case cc of
        TyConBound u t -> TyConBound (eraseT u) (eraseT t)
        _              -> cc
        
        
-------------------------------------------------------------------------------        
class EraseX (c :: * -> * -> *) where
  eraseX :: (Show n, Ord n) => c a n -> c a n
 
instance EraseX Exp where
  eraseX xx
    = case xx of
        XVar a u               -> XVar a (eraseT u)
        XCon a u               -> XCon a (eraseT u)
        XLAM a b x             -> XLAM a (eraseT b) x
        XLam a b x             -> XLam a (eraseT b) x
        
        -- Erase region type applications
        XApp _ x (XType t)
          | isRegionTypeVar t  -> x
        
        -- Erase witness arguments
        XApp _ x (XWitness _) -> x
          
        -- transformUpX doesn't descend into the bindings, so we do it here
        XLet a (LLet m b x1) x -> XLet a (LLet m (eraseT b) x1) x
        XLet a (LRec bxs) x    -> XLet a (LRec $ map (first eraseT) bxs) x
        
        XType t                -> XType (eraseT t)
        
        _                      -> xx
        
isRegionTypeVar :: Type n -> Bool
isRegionTypeVar = error "isRegionTypeVar needs environment"
{-}
isRegionTypeVar (TVar u) | isRegionKind $ typeOfBound u = True
isRegionTypeVar _                                       = False

-}
