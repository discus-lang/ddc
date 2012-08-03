
module DDC.Type.Transform.AnonymizeT
        ( anonymizeT
        , AnonymizeT(..)
        , pushAnonymizeBindT)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import Data.List
import qualified DDC.Type.Sum           as T


-- | Rewrite all binders in a thing to be of anonymous form.
anonymizeT :: (Ord n, AnonymizeT c) => c n -> c n
anonymizeT xx
        = anonymizeWithT [] xx


-------------------------------------------------------------------------------
class AnonymizeT (c :: * -> *) where

 -- | Rewrite all binders in a thing to be of anonymous form.
 --   
 --   The stack contains existing anonymous binders that we have entered into,
 --   and named binders that we have rewritten. All bound occurrences of variables
 --   will be replaced by references into this stack.
 anonymizeWithT :: forall n. Ord n => [Bind n] -> c n -> c n


instance AnonymizeT Type where
 anonymizeWithT kstack tt
  = case tt of
        TVar u
         -> TVar $ anonymizeWithT kstack u

        TCon{}          
         -> tt

        TForall b t     
         -> let (kstack', b') = pushAnonymizeBindT kstack b
            in  TForall b' (anonymizeWithT kstack' t)

        TApp t1 t2      
         -> TApp (anonymizeWithT kstack t1) (anonymizeWithT kstack t2)

        TSum ss 
         -> TSum (anonymizeWithT kstack ss)


instance AnonymizeT TypeSum where
 anonymizeWithT kstack ss
        = T.fromList (anonymizeWithT kstack $ T.kindOfSum ss)
        $ map (anonymizeWithT kstack)
        $ T.toList ss


instance AnonymizeT Bound where 
 anonymizeWithT kstack bb
  = case bb of
        UName _
         | Just ix      <- findIndex (boundMatchesBind bb) kstack
         -> UIx ix
         
        _ -> bb


-- Push ----------------------------------------------------------------------
-- Push a binding occurrence of a type variable on the stack, 
--  returning the anonyized binding occurrence and the new stack.
pushAnonymizeBindT :: Ord n => [Bind n] -> Bind n -> ([Bind n], Bind n)
pushAnonymizeBindT kstack b
 = let  t'      = typeOfBind b
        kstack' = b : kstack
   in   (kstack', BAnon t')

