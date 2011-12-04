
module DDC.Type.Transform.Anonymize
        (Anonymize(..))
where
import DDC.Type.Exp
import DDC.Type.Compounds
import Data.List
import qualified DDC.Type.Sum           as T


class Anonymize (c :: * -> *) where

 -- | Rewrite all binders in a thing to be of anonymous form.
 --   
 --   The stack contains binders the names of binders that have already been rewritten,
 --   and any bound occurrences will be replaced by references into this stack.
 anonymize :: forall n. Ord n => [Bind n] -> c n -> c n


instance Anonymize Bind where
 anonymize stack bb
  = replaceTypeOfBind (anonymize stack $ typeOfBind bb) bb 


instance Anonymize Bound where 
 anonymize stack bb
  = case bb of
        UName _ t
         | Just ix      <- findIndex (boundMatchesBind bb) stack    
         -> UIx ix t
         
        _ -> bb
        

instance Anonymize Type where
 anonymize stack tt
  = case tt of
        TVar u          -> TVar $ anonymize stack u
        TCon{}          -> tt

        -- Add the new binder to the stack.
        TForall b t     
         -> let b'      = anonymize stack b
                t'      = typeOfBind b'
                stack'  = case b' of
                                BName{} -> b' : stack
                                BAnon{} -> b' : stack
                                _       -> stack
                                
            in  TForall (BAnon t') (anonymize stack' t)

        TApp t1 t2      -> TApp (anonymize stack t1) (anonymize stack t2)
        TSum ss         -> TSum (anonymize stack ss)


instance Anonymize TypeSum where
 anonymize stack ss
        = T.fromList (anonymize stack $ T.kindOfSum ss)
        $ map (anonymize stack)
        $ T.toList ss

