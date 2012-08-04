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
import Control.Arrow            (first, second)
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env   as Env
import qualified Data.Map       as Map


-- | Completely erase region types and kinds from a module
eraseM :: (Show n, Ord n) => Module a n -> Module a n
eraseM mm@(ModuleCore { moduleExportKinds = eks
                      , moduleExportTypes = ets
                      , moduleImportKinds = iks
                      , moduleImportTypes = its
                      , moduleBody        = xx })
  = mm { moduleExportKinds = Map.map (eraseT Env.empty) eks
       , moduleExportTypes = Map.map (eraseT Env.empty) ets
       , moduleImportKinds = Map.map (second (eraseT Env.empty)) iks
       , moduleImportTypes = Map.map (second (eraseT Env.empty)) its
       , moduleBody        = transformUpX eraseX Env.empty Env.empty xx }


-------------------------------------------------------------------------------        
class EraseT (c :: * -> *) where
  eraseT :: (Show n, Ord n) 
         => Env n -> c n -> c n
  
instance EraseT Type where
  eraseT kenv tt
    = case tt of
        TVar    u                       -> TVar (eraseT kenv u)
        TCon    c                       -> TCon (eraseT kenv c)
        TForall b t
          | isRegionKind $ typeOfBind b 
          -> eraseT (Env.extend b kenv) t
          
          | otherwise                   
          -> let kenv'  = Env.extend b kenv
             in  TForall (eraseT kenv' b) (eraseT kenv' t)  
                
        -- Erase region type applications
        TApp   t1 t2
          | isRegionTypeVar t2          -> eraseT t1   
          | isRegionKind t2             -> eraseT t1                          
          
        TApp   t1 t2                    -> TApp (eraseT kenv t1) (eraseT kenv t2)
        TSum   ts                       -> TSum (eraseT kenv ts)
        

instance EraseT TypeSum where
  eraseT kenv ts 
        = fromList (eraseT kenv $ kindOfSum ts) 
        $ map (eraseT kenv) 
        $ toList ts
  

instance EraseT Bind where
  eraseT kenv bb
    = case bb of
        BName n t
          | isRegionKind t -> BNone (eraseT t) 
          | otherwise      -> BName n (eraseT t)                
        BAnon t            -> BAnon (eraseT t)
        BNone t            -> BNone (eraseT t)              

instance EraseT Bound where
  eraseT kenv uu
    | Just k    <- Env.lookup uu kenv
    , isRegionKind k
    = UHole kRegion

    | otherwise                     
    = case uu of
        UIx   i   -> UIx   i
        UName n   -> UName n
        UPrim n k -> UPrim n k
        UHole k   -> UHole   k

    
instance EraseT TyCon where
  eraseT kenv cc
    = case cc of
        TyConBound u t -> TyConBound (eraseT kenv u) (eraseT kenv t)
        _              -> cc
        
        
-------------------------------------------------------------------------------        
class EraseX (c :: * -> * -> *) where
  eraseX :: (Show n, Ord n) 
        => Env n -> Env n -> c a n -> c a n
 
instance EraseX Exp where
  eraseX kenv _tenv xx
    = case xx of
        XVar a u               -> XVar a (eraseT kenv u)
        XCon a u               -> XCon a (eraseT kenv u)
        XLAM a b x             -> XLAM a (eraseT kenv b) x
        XLam a b x             -> XLam a (eraseT kenv b) x
        
        -- Erase region type applications
        XApp _ x (XType t)
          | isRegionTypeVar t  -> x
          
        -- transformUpX doesn't descend into the bindings, so we do it here
        XLet a (LLet m b x1) x -> XLet a (LLet m (eraseT kenv b) x1) x
        XLet a (LRec bxs) x    -> XLet a (LRec $ map (first (eraseT kenv)) bxs) x
        
        XType t                -> XType (eraseT t)
        _                      -> xx
        
isRegionTypeVar :: Ord n => Env n -> Type n -> Bool
isRegionTypeVar kenv tt
        | TVar u        <- tt
        , Just k        <- Env.lookup u kenv
        = isRegionKind k

        | otherwise
        = False
