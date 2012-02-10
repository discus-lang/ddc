
module DDC.Core.Transform.AnonymizeX
        ( anonymizeX
        , AnonymizeX(..))
where
import DDC.Core.Exp
import DDC.Type.Transform.AnonymizeT
import DDC.Type.Compounds
import Control.Monad
import Data.List


-- | Rewrite all binders in a thing to be of anonymous form.
anonymizeX :: (Ord n, AnonymizeX c) => c n -> c n
anonymizeX xx
        = anonymizeWithX [] [] xx


-------------------------------------------------------------------------------
class AnonymizeX (c :: * -> *) where

 -- | Rewrite all binders in a thing to be anonymous.
 --   The stacks contains existing anonymous binders that we have entered into,
 --   and named binders that we have rewritten. All bound occurrences of variables
 --   will be replaced by references into these stacks.
 anonymizeWithX 
        :: forall n. Ord n 
        => [Bind n]     -- ^ Stack for Spec binders.
        -> [Bind n]     -- ^ Stack for Data and Witness binders.
        -> c n -> c n


instance AnonymizeX (Exp a) where
 anonymizeWithX kstack tstack xx
  = let down = anonymizeWithX kstack tstack
    in case xx of
        XVar a u        -> XVar a (down u)
        XCon a u        -> XCon a (down u)
        XApp a x1 x2    -> XApp a (down x1) (down x2)

        XLAM a b x
         -> let (kstack', b')   = pushAnonymizeBindT kstack b
            in  XLAM a b'   (anonymizeWithX kstack' tstack x)

        XLam a b x
         -> let (tstack', b')   = pushAnonymizeBindX kstack tstack b
            in  XLam a b'   (anonymizeWithX kstack tstack' x)

        XLet a lts x
         -> let (kstack', tstack', lts')  
                 = pushAnonymizeLets kstack tstack lts
            in  XLet a lts' (anonymizeWithX kstack' tstack' x)

        XCase a x alts  -> XCase a  (down x) (map down alts)
        XCast a c x     -> XCast a  (down c) (down x)
        XType t         -> XType    (anonymizeWithT kstack t)
        XWitness w      -> XWitness (down w)


instance AnonymizeX Cast where
 anonymizeWithX kstack tstack cc
  = case cc of
        CastWeakenEffect eff    -> CastWeakenEffect  (anonymizeWithT kstack eff)
        CastWeakenClosure clo   -> CastWeakenClosure (anonymizeWithT kstack clo)
        CastPurify w            -> CastPurify   (anonymizeWithX kstack tstack w)
        CastForget w            -> CastForget   (anonymizeWithX kstack tstack w)


instance AnonymizeX LetMode where
 anonymizeWithX kstack tstack lm
  = case lm of
        LetStrict       -> lm
        LetLazy mw      -> LetLazy $ liftM (anonymizeWithX kstack tstack) mw


instance AnonymizeX (Alt a) where
 anonymizeWithX kstack tstack alt
  = case alt of
        AAlt PDefault x
         -> AAlt PDefault (anonymizeWithX kstack tstack x)

        AAlt (PData uCon bs) x
         -> let (tstack', bs')  = pushAnonymizeBindXs kstack tstack bs
                x'              = anonymizeWithX kstack tstack' x
            in  AAlt (PData uCon bs') x'


instance AnonymizeX Witness where
 anonymizeWithX kstack tstack ww
  = let down = anonymizeWithX kstack tstack 
    in case ww of
        WVar  u         -> WVar  (down u)
        WCon  c         -> WCon  c
        WApp  w1 w2     -> WApp  (down w1) (down w2)
        WJoin w1 w2     -> WJoin (down w1) (down w2)
        WType t         -> WType (anonymizeWithT kstack t)


instance AnonymizeX Bind where
 anonymizeWithX kstack _tstack bb
  = let t'      = anonymizeWithT kstack $ typeOfBind bb
    in  replaceTypeOfBind t' bb 


instance AnonymizeX Bound where 
 anonymizeWithX kstack tstack bb
  = case bb of
        UName _ t
         | Just ix      <- findIndex (boundMatchesBind bb) tstack
         -> UIx ix (anonymizeWithT kstack t)
         
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
 = let  b'      = anonymizeWithX kstack tstack b
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
         -> let mode'           = anonymizeWithX     kstack tstack mode
                (tstack', b')   = pushAnonymizeBindX kstack tstack b
                x'              = anonymizeWithX     kstack tstack' x
            in  (kstack, tstack', LLet mode' b' x')

        LRec bxs 
         -> let (bs, xs)        = unzip bxs
                (tstack', bs')  = pushAnonymizeBindXs kstack tstack   bs
                xs'             = map (anonymizeWithX kstack tstack') xs
                bxs'            = zip bs' xs'
            in  (kstack, tstack', LRec bxs')

        LLetRegion b bs
         -> let (kstack', b')   = pushAnonymizeBindT  kstack b
                (tstack', bs')  = pushAnonymizeBindXs kstack' tstack bs
            in  (kstack', tstack', LLetRegion b' bs')

        LWithRegion{}
         -> (kstack, tstack, lts)

