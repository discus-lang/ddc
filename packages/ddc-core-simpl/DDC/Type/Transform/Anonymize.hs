
module DDC.Type.Transform.Anonymize
        ( Anonymize(..)
        , pushAnonymizeBind)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import Data.List
import qualified DDC.Type.Sum           as T


class Anonymize (c :: * -> *) where

 -- | Rewrite all binders in a thing to be of anonymous form.
 --   
 --   The stack contains existing anonymous binders that we have entered into,
 --   and named binders that we have rewritten. All bound occurrences of variables
 --   will be replaced by references into this stack.
 anonymize :: forall n. Ord n => [Bind n] -> c n -> c n


-- Utils ----------------------------------------------------------------------
-- Push a binding occurrence on the stack, 
--  returning the anonyized binding occurrence and the new stack.
-- Used in the definition of `anonymize`.
pushAnonymizeBind :: Ord n => Bind n -> [Bind n] -> (Bind n, [Bind n])
pushAnonymizeBind b stack
 = let  b'      = anonymize stack b
        t'      = typeOfBind b'
        stack'  = case b' of
                        BName{} -> b' : stack
                        BAnon{} -> b' : stack
                        _       -> stack
   in   (BAnon t', stack')


-- Instances ------------------------------------------------------------------
instance Anonymize Bind where
 anonymize stack bb
  = replaceTypeOfBind (anonymize stack $ typeOfBind bb) bb 


instance Anonymize Bound where 
 anonymize stack bb
  = case bb of
        UName _ t
         | Just ix      <- findIndex (boundMatchesBind bb) stack    
         -> UIx ix (anonymize stack t)
         
        _ -> bb



        

instance Anonymize Type where
 anonymize stack tt
  = case tt of
        TVar u
         -> TVar $ anonymize stack u

        TCon{}          
         -> tt

        TForall b t     
         -> let (b', stack')    = pushAnonymizeBind b stack 
            in  TForall b' (anonymize stack' t)


        TApp t1 t2      
         -> TApp (anonymize stack t1) (anonymize stack t2)

        TSum ss 
         -> TSum (anonymize stack ss)


instance Anonymize TypeSum where
 anonymize stack ss
        = T.fromList (anonymize stack $ T.kindOfSum ss)
        $ map (anonymize stack)
        $ T.toList ss

