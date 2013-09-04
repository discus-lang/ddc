
-- | Rewrite all binders to anonymous deBruijn form.
module DDC.Core.Transform.AnonymizeX
        ( anonymizeX
        , AnonymizeX(..)
        , pushAnonymizeBindX)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Transform.AnonymizeT
import DDC.Type.Compounds
import Data.List
import Data.Set                         (Set)
import qualified Data.Set               as Set
import qualified Data.Map               as Map

-- | Rewrite all binders in a thing to anonymous form.
anonymizeX :: (Ord n, AnonymizeX c) => c n -> c n
anonymizeX xx
        = anonymizeWithX Set.empty [] [] xx


-------------------------------------------------------------------------------
class AnonymizeX (c :: * -> *) where

 -- | Rewrite all binders in a thing to be anonymous.
 --   The stacks contains existing anonymous binders that we have entered into,
 --   and named binders that we have rewritten. All bound occurrences of variables
 --   will be replaced by references into these stacks.
 anonymizeWithX 
        :: forall n. Ord n 
        => Set n        -- ^ Don't anonymize level-0 binders with these names.
        -> [Bind n]     -- ^ Stack for Spec binders (level-1).
        -> [Bind n]     -- ^ Stack for Data and Witness binders (level-0).
        -> c n -> c n

instance AnonymizeX (Module a) where
 anonymizeWithX keep kstack tstack mm@ModuleCore{}
  = let 
        -- We need to keep exported names, 
        -- because the export list can't deal with anonymous binders.
        keep'   = Set.union keep 
                        $ Set.fromList 
                        $ Map.keys $ moduleExportTypes mm

        x'      = anonymizeWithX keep' kstack tstack (moduleBody mm)

    in  mm { moduleBody = x' }


instance AnonymizeX (Exp a) where
 anonymizeWithX keep kstack tstack xx
  = {-# SCC anonymizeWithX #-}
    let down = anonymizeWithX keep kstack tstack
    in case xx of
        XVar _ UPrim{}  -> xx
        XCon{}          -> xx      

        XVar a u@(UName{})       
         |  Just ix      <- findIndex (boundMatchesBind u) tstack
         -> XVar a (UIx ix)

        XVar a u
         -> XVar a u

        XApp a x1 x2    -> XApp a (down x1) (down x2)

        XLAM a b x
         -> let (kstack', b')   = pushAnonymizeBindT kstack b
            in  XLAM a b'   (anonymizeWithX keep kstack' tstack x)

        XLam a b x
         -> let (tstack', b')   = pushAnonymizeBindX keep kstack tstack b
            in  XLam a b'   (anonymizeWithX keep kstack tstack' x)

        XLet a lts x
         -> let (kstack', tstack', lts')  
                 = pushAnonymizeLets keep kstack tstack lts
            in  XLet a lts' (anonymizeWithX keep kstack' tstack' x)

        XCase a x alts  -> XCase a    (down x) (map down alts)
        XCast a c x     -> XCast a    (down c) (down x)
        XType a t       -> XType a    (anonymizeWithT kstack t)
        XWitness a w    -> XWitness a (down w)


instance AnonymizeX (Cast a) where
 anonymizeWithX keep kstack tstack cc
  = let down = anonymizeWithX keep kstack tstack
    in case cc of
        CastWeakenEffect eff    -> CastWeakenEffect  (anonymizeWithT kstack eff)
        CastWeakenClosure xs    -> CastWeakenClosure (map down xs)
        CastPurify w            -> CastPurify        (down w)
        CastForget w            -> CastForget        (down w)
        CastSuspend             -> CastSuspend
        CastRun                 -> CastRun


instance AnonymizeX (Alt a) where
 anonymizeWithX keep kstack tstack alt
  = let down = anonymizeWithX keep kstack tstack
    in case alt of
        AAlt PDefault x
         -> AAlt PDefault (down x)

        AAlt (PData uCon bs) x
         -> let (tstack', bs')  = pushAnonymizeBindXs keep kstack tstack bs
                x'              = anonymizeWithX keep kstack tstack' x
            in  AAlt (PData uCon bs') x'


instance AnonymizeX (Witness a) where
 anonymizeWithX keep kstack tstack ww
  = let down = anonymizeWithX keep kstack tstack 
    in case ww of
        WVar a u@(UName _)
         |  Just ix      <- findIndex (boundMatchesBind u) tstack
         -> WVar a (UIx ix)

        WVar  a u       -> WVar  a u
        WCon  a c       -> WCon  a c
        WApp  a w1 w2   -> WApp  a (down w1) (down w2)
        WJoin a w1 w2   -> WJoin a (down w1) (down w2)
        WType a t       -> WType a (anonymizeWithT kstack t)


instance AnonymizeX Bind where
 anonymizeWithX _keep kstack _tstack bb
  = let t'      = anonymizeWithT kstack $ typeOfBind bb
    in  replaceTypeOfBind t' bb 


-- Push ----------------------------------------------------------------------
-- | Push a binding occurrence of a level-0 on the stack, 
--   returning the anonyized binding occurrence and the new stack.
pushAnonymizeBindX 
        :: Ord n 
        => Set n        -- ^ Don't anonymize binders with these names.
        -> [Bind n]     -- ^ Stack for Spec binders (level-1)
        -> [Bind n]     -- ^ Stack for Value and Witness binders (level-0)
        -> Bind n 
        -> ([Bind n], Bind n)

pushAnonymizeBindX keep kstack tstack b@(BName n _)
 | Set.member n keep
 = let  b'      = anonymizeWithX keep kstack tstack b
   in  (tstack, b')

pushAnonymizeBindX keep kstack tstack b@BNone{}
 = let  b'      = anonymizeWithX keep kstack tstack b
        t'      = typeOfBind b'
   in   (tstack,  BNone t')

pushAnonymizeBindX keep kstack tstack b
 = let  b'      = anonymizeWithX keep kstack tstack b
        t'      = typeOfBind b'
        tstack' = b' : tstack
   in   (tstack', BAnon t')


-- | Push a binding occurrence on the stack, 
--   returning the anonyized binding occurrence and the new stack.
--  Used in the definition of `anonymize`.
pushAnonymizeBindXs 
        :: Ord n 
        => Set n        -- ^ Don't anonymize binders with these names.
        -> [Bind n]     -- ^ Stack for Spec binders (level-1)
        -> [Bind n]     -- ^ Stack for Value and Witness binders (level-0)
        -> [Bind n] 
        -> ([Bind n], [Bind n])

pushAnonymizeBindXs keep kstack tstack bs
  = mapAccumL
        (\tstack' b -> pushAnonymizeBindX keep kstack tstack' b)
        tstack bs


pushAnonymizeLets 
        :: Ord n 
        => Set n
        -> [Bind n] 
        -> [Bind n] 
        -> Lets a n 
        -> ([Bind n], [Bind n], Lets a n)

pushAnonymizeLets keep kstack tstack lts
 = case lts of
        LLet b x
         -> let x'              = anonymizeWithX     keep kstack tstack x
                (tstack', b')   = pushAnonymizeBindX keep kstack tstack b
            in  (kstack, tstack', LLet b' x')

        LRec bxs 
         -> let (bs, xs)        = unzip bxs
                (tstack', bs')  = pushAnonymizeBindXs keep kstack tstack   bs
                xs'             = map (anonymizeWithX keep kstack tstack') xs
                bxs'            = zip bs' xs'
            in  (kstack, tstack', LRec bxs')

        LLetRegions b bs
         -> let (kstack', b')   = mapAccumL pushAnonymizeBindT kstack b
                (tstack', bs')  = pushAnonymizeBindXs keep kstack' tstack bs
            in  (kstack', tstack', LLetRegions b' bs')

        LWithRegion{}
         -> (kstack, tstack, lts)

