
module DDC.Type.Transform.AnonymizeT
        ( AnonymizeT(..)
        , pushAnonymizeBindT)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import Data.List
import qualified DDC.Type.Sum           as T


class AnonymizeT (c :: * -> *) where

 -- | Rewrite all binders in a thing to be of anonymous form.
 --   
 --   The stack contains existing anonymous binders that we have entered into,
 --   and named binders that we have rewritten. All bound occurrences of variables
 --   will be replaced by references into this stack.
 anonymizeT :: forall n. Ord n => [Bind n] -> c n -> c n


-- Instances ------------------------------------------------------------------
instance AnonymizeT Type where
 anonymizeT kstack tt
  = case tt of
        TVar u
         -> TVar $ anonymizeT kstack u

        TCon{}          
         -> tt

        TForall b t     
         -> let (kstack', b') = pushAnonymizeBindT kstack b
            in  TForall b' (anonymizeT kstack' t)


        TApp t1 t2      
         -> TApp (anonymizeT kstack t1) (anonymizeT kstack t2)

        TSum ss 
         -> TSum (anonymizeT kstack ss)


instance AnonymizeT TypeSum where
 anonymizeT kstack ss
        = T.fromList (anonymizeT kstack $ T.kindOfSum ss)
        $ map (anonymizeT kstack)
        $ T.toList ss


instance AnonymizeT Bound where 
 anonymizeT kstack bb
  = case bb of
        UName _ t
         | Just ix      <- findIndex (boundMatchesBind bb) kstack
         -> UIx ix (anonymizeT kstack t)
         
        _ -> bb


-- Push ----------------------------------------------------------------------
-- Push a binding occurrence of a type variable on the stack, 
--  returning the anonyized binding occurrence and the new stack.
pushAnonymizeBindT :: Ord n => [Bind n] -> Bind n -> ([Bind n], Bind n)
pushAnonymizeBindT kstack b
 = let  t'      = typeOfBind b
        kstack' = case b of
                        BName{} -> b : kstack
                        BAnon{} -> b : kstack
                        _       -> kstack
   in   (kstack', BAnon t')

