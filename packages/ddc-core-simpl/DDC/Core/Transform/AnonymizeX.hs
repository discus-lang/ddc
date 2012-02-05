
module DDC.Core.Transform.AnonymizeX
        (AnonymizeX(..))
where
import DDC.Core.Exp
import DDC.Type.Transform.AnonymizeT
import DDC.Type.Compounds
import Control.Monad
import Data.List


class AnonymizeX (c :: * -> *) where

 -- | Rewrite all binders in a thing to be of anonymous form.
 --   
 --   The stack contains existing anonymous binders that we have entered into,
 --   and named binders that we have rewritten. All bound occurrences of variables
 --   will be replaced by references into this stack.
 anonymizeX 
        :: forall n. Ord n 
        => [Bind n]     -- ^ Stack for Spec binders (kind environment)
        -> [Bind n]     -- ^ Stack for Value and Witness binders (type environment)
        -> c n -> c n


instance AnonymizeX (Exp a) where
 anonymizeX kstack tstack xx
  = let down = anonymizeX kstack tstack
    in case xx of
        XVar a u
         -> XVar a (down u)

        XCon a u
         -> XCon a (down u)

        XApp a x1 x2
         -> XApp a (down x1) (down x2)

        XLAM a b x
         -> let (kstack', b')   = pushAnonymizeBindT kstack b
            in  XLAM a b' (anonymizeX kstack' tstack x)

        XLam a b x
         -> let (tstack', b')   = pushAnonymizeBindX kstack tstack b
            in  XLam a b' (anonymizeX kstack tstack' x)

        XLet a lts x
         -> let (kstack', tstack', lts')  
                  = pushAnonymizeLets kstack tstack lts
            in  XLet a lts' (anonymizeX kstack' tstack' x)

        XCase a x alts
         -> XCase a (down x) (map (anonymizeX kstack tstack) alts)
        
        XCast a c x
         -> XCast a (down c) (down x)

        XType t         -> XType    (anonymizeT kstack t)
        XWitness w      -> XWitness (down w)


instance AnonymizeX Cast where
 anonymizeX kstack tstack cc
  = case cc of
        CastWeakenEffect eff
         -> CastWeakenEffect  (anonymizeT kstack eff)
        
        CastWeakenClosure clo
         -> CastWeakenClosure (anonymizeT kstack clo)

        CastPurify w
         -> CastPurify (anonymizeX kstack tstack w)

        CastForget w
         -> CastForget (anonymizeX kstack tstack w)


instance AnonymizeX LetMode where
 anonymizeX kstack tstack lm
  = case lm of
        LetStrict       -> lm
        LetLazy mw      -> LetLazy $ liftM (anonymizeX kstack tstack) mw


instance AnonymizeX (Alt a) where
 anonymizeX = error "anonymize alts"


instance AnonymizeX Witness where
 anonymizeX = error "anonymize witness"


instance AnonymizeX Bind where
 anonymizeX kstack _tstack bb
  = let t'      = anonymizeT kstack $ typeOfBind bb
    in  replaceTypeOfBind t' bb 


instance AnonymizeX Bound where 
 anonymizeX kstack tstack bb
  = case bb of
        UName _ t
         | Just ix      <- findIndex (boundMatchesBind bb) tstack
         -> UIx ix (anonymizeT kstack t)
         
        _ -> bb


-- Push ----------------------------------------------------------------------
-- Push a binding occurrence of a type variable on the stack, 
--  returning the anonyized binding occurrence and the new stack.
pushAnonymizeBindX 
        :: Ord n 
        => [Bind n]     -- ^ Stack for Spec binders (kind environment)
        -> [Bind n]     -- ^ Stack for Value and Witness binders (type environment)
        -> Bind n 
        -> ([Bind n], Bind n)

pushAnonymizeBindX kstack tstack b
 = let  b'      = anonymizeX kstack tstack b
        t'      = typeOfBind b'
        tstack' = case b' of
                        BName{} -> b' : tstack
                        BAnon{} -> b' : tstack
                        _       -> tstack
   in   (tstack', BAnon t')


-- Push a binding occurrence on the stack, 
--  returning the anonyized binding occurrence and the new stack.
-- Used in the definition of `anonymize`.
pushAnonymizeBindXs 
        :: Ord n 
        => [Bind n]     -- ^ Stack for Spec binders (kind environment)
        -> [Bind n]     -- ^ Stack for Value and Witness binders (type environment)
        -> [Bind n] 
        -> ([Bind n], [Bind n])

pushAnonymizeBindXs kstack tstack bs
  = mapAccumL   (\tstack' b -> pushAnonymizeBindX kstack tstack' b)
                tstack bs


pushAnonymizeLets 
        :: Ord n 
        => [Bind n] -> [Bind n] 
        -> Lets a n 
        -> ([Bind n], [Bind n], Lets a n)

pushAnonymizeLets kstack tstack lts
 = case lts of
        LLet mode b x
         -> let mode'           = anonymizeX kstack tstack mode
                (tstack', b')   = pushAnonymizeBindX kstack tstack b
                x'              = anonymizeX kstack tstack' x
            in  (kstack, tstack', LLet mode' b' x')

        LLetRegion b bs
         -> let (kstack', b')   = pushAnonymizeBindT  kstack b
                (tstack', bs')  = pushAnonymizeBindXs kstack' tstack bs
            in  (kstack', tstack', LLetRegion b' bs')

        _ -> error "anonymize lets"


