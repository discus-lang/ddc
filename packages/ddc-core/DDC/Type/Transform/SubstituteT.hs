
-- | Capture avoiding substitution of types in types.
module DDC.Type.Transform.SubstituteT
        ( substituteT
        , substituteTs
        , substituteBoundT
        , SubstituteT(..)

        , BindStack(..)
        , pushBind
        , pushBinds
        , substBound)
where
import DDC.Type.Collect
import DDC.Type.Compounds
import DDC.Type.Transform.LiftT
import DDC.Type.Transform.Crush
import DDC.Type.Transform.Trim
import DDC.Type.Transform.Rename
import DDC.Type.Exp
import Data.Maybe
import qualified DDC.Type.Sum   as Sum
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.Set                 (Set)


-- | Substitute a `Type` for the `Bound` corresponding to some `Bind` in a thing.
substituteT :: (SubstituteT c, Ord n) => Bind n -> Type n -> c n -> c n
substituteT b t x
 = case takeSubstBoundOfBind b of
    Just u      -> substituteBoundT u t x
    _           -> x


-- | Wrapper for `substituteT` to substitute multiple things.
substituteTs :: (SubstituteT c, Ord n) => [(Bind n, Type n)] -> c n -> c n
substituteTs bts x
        = foldr (uncurry substituteT) x bts


-- | Substitute a `Type` for `Bound` in some thing.
substituteBoundT :: (SubstituteT c, Ord n) => Bound n -> Type n -> c n -> c n
substituteBoundT u t x
 = let -- Determine the free names in the type we're subsituting.
       -- We'll need to rename binders with the same names as these
       freeNames       = Set.fromList
                       $ mapMaybe takeNameOfBound 
                       $ Set.toList 
                       $ freeT Env.empty t

       stack           = BindStack [] [] 0 0
 
  in   substituteWithT u t freeNames stack x


-- SubstituteT ----------------------------------------------------------------
class SubstituteT (c :: * -> *) where

 -- | Substitute a type into some thing.
 --   In the target, if we find a named binder that would capture a free variable
 --   in the type to substitute, then we rewrite that binder to anonymous form,
 --   avoiding the capture.
 substituteWithT
        :: forall n. Ord n
        => Bound n       -- ^ Bound variable that we're subsituting into.
        -> Type n        -- ^ Type to substitute.
        -> Set  n        -- ^ Names of free varaibles in the type to substitute.
        -> BindStack n   -- ^ Bind stack.
        -> c n -> c n


-- Instances ------------------------------------------------------------------
instance SubstituteT Bind where
 substituteWithT u fvs t stack bb
  = let k'      = substituteWithT u fvs t stack $ typeOfBind bb
    in  replaceTypeOfBind k' bb
 
 
instance SubstituteT Type where
 substituteWithT u t fns stack tt
  = let down    = substituteWithT u t fns stack
    in  case tt of
         TCon{}          -> tt

         -- Crush out compound effects and closures as we substitute them.
         TApp t1 t2
          -> case t1 of
                TCon (TyConSpec TcConHeadRead)  
                  -> crushEffect      (TApp t1 (down t2))

                TCon (TyConSpec TcConDeepRead)  
                  -> crushEffect      (TApp t1 (down t2))

                TCon (TyConSpec TcConDeepWrite) 
                  -> crushEffect      (TApp t1 (down t2))

                TCon (TyConSpec TcConDeepAlloc) 
                  -> crushEffect      (TApp t1 (down t2))

                -- If the closure is miskinded then trimClosure can 
                -- return Nothing, so we leave it untrimmed.
                TCon (TyConSpec TcConDeepUse)
                  -> fromMaybe tt (trimClosure (TApp t1 (down t2)))

                _ -> TApp (down t1) (down t2)

         TSum ss        
          -> TSum (down ss)

         TForall b tBody
          | namedBoundMatchesBind u b -> tt
          | otherwise
          -> let -- Substitute into the annotation on the binder.
                 bSub            = down b

                 -- Push bind onto stack, and anonymise to avoid capture if needed
                 (stack', b')    = pushBind fns stack bSub
                
                 -- Substitute into body.
                 tBody'          = substituteWithT u t fns stack' tBody

             in  TForall b' tBody'

         TVar u'
          -> case substBound stack u u' of
                Left  u'' -> TVar u''
                Right n   -> liftT n t
                

instance SubstituteT TypeSum where
 substituteWithT u n fns stack ss
  = let k       = substituteWithT u n fns stack
                $ Sum.kindOfSum ss
    in  Sum.fromList k 
                $ map (substituteWithT u n fns stack)
                $ Sum.toList ss

