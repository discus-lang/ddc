
-- | Collect the free variables in a core expression.
module DDC.Core.Collect.FreeX
        (FreeX(..))
where
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Type.Universe
import DDC.Type.Compounds
import DDC.Type.Pretty
import DDC.Type.Env                     (Env)
import Data.Set                         (Set)
import qualified DDC.Type.Env           as Env
import qualified Data.Set               as Set


class FreeX n a where
 -- | Determine the set of value and witness variables not bound in 
 --   the given environment.
 freeX  :: Ord n 
        => Env n        -- ^ Type environment.
        -> a            -- ^ Determine free variables in this thing.
        -> Set (Bound n)


-- | Add value or witness variables to the set if they are not in the 
--   environment. Use freeT for type variables.
instance FreeX n (Bound n) where
 freeX tenv u
  | Env.member u tenv   = Set.empty
  | otherwise             
  = case u of
        UName{}         -> Set.singleton u
        UPrim{}         -> Set.empty
        UIx i t         -> Set.singleton $ UIx (i - Env.depth tenv) t


instance Pretty n => FreeX n (Exp a n) where
 freeX tenv xx
  = case xx of
        XVar _ u       -> freeX tenv u
        XCon _ u       -> freeX tenv u

        XApp _ x1 x2    
         -> Set.unions  [ freeX tenv x1
                        , freeX tenv x2]

        -- TODO: Urgh. 
        -- We shouldn't need to look at the universe to know what environment
        -- to push the variable onto. Maybe we just need to add a second
        -- lambda form.
        XLam _ b  x
         -> case universeFromType1 (typeOfBind b) of
             Just UniverseComp
              -> freeX (Env.extend b tenv) x

             Just UniverseWitness
              -> freeX (Env.extend b tenv) x

             Just UniverseSpec
              -> freeX tenv x 

             _ -> error $ "freeX: malformed type" 
                        ++ (pretty $ ppr (typeOfBind b) <+> text (show (universeOfType (typeOfBind b))))

        XLet _ lts x    
         -> freeX (Env.extends (valwitBindsOfLets lts) tenv) x

        XCase _ x alts
         -> Set.unions  [ freeX tenv x
                        , Set.unions $ map (freeX tenv) alts]

        XCast _ x _    -> freeX tenv x
        XType _        -> Set.empty
        XWitness w     -> freeX tenv w


instance Pretty n => FreeX n (Alt a n) where
 freeX tenv alt
  = case alt of
        AAlt p x
         -> freeX (Env.extends (bindsOfPat p) tenv) x


instance Pretty n => FreeX n (Lets a n) where
 freeX tenv lts 
  = case lts of
        LLet m _ x        
         -> Set.unions   [ freeX tenv m
                         , freeX tenv x ]

        LRec bxs        
         -> let (bs, xs) = unzip bxs
                tenv'    = Env.extends bs tenv
            in  Set.unions $ map (freeX tenv') xs
        
        LLetRegion{}    -> Set.empty
        LWithRegion{}   -> Set.empty
       

instance FreeX n (LetMode n) where
 freeX tenv mm
  = case mm of
        LetStrict               -> Set.empty
        LetLazy Nothing         -> Set.empty
        LetLazy (Just w)        -> freeX tenv w


instance FreeX n (Cast n) where
 freeX tenv cc
  = case cc of
        CastWeakenEffect{}      -> Set.empty
        CastWeakenClosure{}     -> Set.empty
        CastPurify w            -> freeX tenv w
        CastForget w            -> freeX tenv w


instance FreeX n (Witness n) where
 freeX tenv ww
  = case ww of
        WCon{}          -> Set.empty

        WVar u          -> freeX tenv u

        WApp  w1 w2     
         -> Set.unions  [ freeX tenv w1
                        , freeX tenv w2 ]

        WJoin w1 w2     
         -> Set.unions  [ freeX tenv w1
                        , freeX tenv w2 ]

        WType{}         -> Set.empty
        
