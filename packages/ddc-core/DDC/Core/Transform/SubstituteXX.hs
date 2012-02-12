
-- | Capture avoiding substitution of expressions in expressions.
module DDC.Core.Transform.SubstituteXX
        ( SubstituteXX(..)
        , substituteXX
        , substituteXXs
        , substituteXArg
        , substituteXArgs)
where
import DDC.Core.Exp
import DDC.Core.Collect
import DDC.Core.Transform.LiftX
import DDC.Type.Compounds
import DDC.Core.Transform.SubstituteWX
import DDC.Core.Transform.SubstituteTX
import DDC.Type.Transform.SubstituteT
import Data.Maybe
import qualified DDC.Type.Sum   as Sum
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.Set                 (Set)
import Data.List

-- | Wrapper for `substituteWithX` that determines the set of free names in the
--   expression being substituted, and starts with an empty binder stack.
substituteXX 
        :: (Ord n, SubstituteXX c)
        => Bind n -> Exp a n -> c a n -> c a n

substituteXX b xArg xx
 | Just u       <- takeSubstBoundOfBind b
 = substituteWithXX xArg
        ( Sub 
        { subBound      = u

          -- Rewrite level-1 binders that have the same name as any
          -- of the free variables in the expression to substitute, 
          -- or any level-1 binders that expression binds itself.
        , subConflict1  
                = Set.fromList
                $  (mapMaybe takeNameOfBound $ Set.toList $ freeT Env.empty xArg) 
                ++ (mapMaybe takeNameOfBind  $ collectSpecBinds xArg)

          -- Rewrite level-0 binders that have the same name as any
          -- of the free variables in the expression to substitute.
        , subConflict0
                = Set.fromList
                $ mapMaybe takeNameOfBound 
                $ Set.toList 
                $ freeX Env.empty xArg
                
        , subStack1     = BindStack [] [] 0 0
        , subStack0     = BindStack [] [] 0 0
        , subShadow     = False })
        xx

 | otherwise    = xx


-- | Wrapper for `substituteX` to substitute multiple expressions.
substituteXXs 
        :: (Ord n, SubstituteXX c)
        => [(Bind n, Exp a n)] -> c a n -> c a n
substituteXXs bts x
        = foldr (uncurry substituteXX) x bts


-- | Substitute the argument of an application into an expression.
--   Perform type substitution for an `XType` 
--    and witness substitution for an `XWitness`
substituteXArg 
        :: (Ord n, SubstituteXX c, SubstituteWX (c a), SubstituteTX (c a))
        => Bind n -> Exp a n -> c a n -> c a n

substituteXArg b arg x
 = case arg of
        XType t         -> substituteTX  b t x
        XWitness w      -> substituteWX  b w x
        _               -> substituteXX  b arg x


-- | Wrapper for `substituteXArgs` to substitute multiple arguments.
substituteXArgs
        :: (Ord n, SubstituteXX c, SubstituteWX (c a), SubstituteTX (c a))
        => [(Bind n, Exp a n)] -> c a n -> c a n

substituteXArgs bas x
        = foldr (uncurry substituteXArg) x bas


-- SubstituteX ----------------------------------------------------------------
-- | Environment for SubstituteXX
data Sub n
        = Sub
        { -- | Bound variable that we're substituting for.
          subBound      :: Bound n

          -- | Level-1 names that need to be rewritten to avoid capture.
        , subConflict1  :: Set n

          -- | Level-0 names that need to be rewritten to avoid capture.
        , subConflict0  :: Set n 

          -- | Rewriting stack for level-1 names.
        , subStack1     :: BindStack n

          -- | Rewriting stack for level-0 names.
        , subStack0     :: BindStack n 

          -- | We've decended past a binder that shadows the one that
          --   we're substituting for. In this case we can no longer perform
          --   the actual substitution, but we still may need to update names
          --   whose binders were rewritten to avoid capture.
        , subShadow     :: Bool }


-- | Push a level-1 binder on the rewrite stack.
bindT :: Ord n => Sub n -> Bind n -> (Sub n, Bind n)
bindT sub b 
 = let  (stackT', b')     = pushBind (subConflict1 sub) (subStack1 sub) b
   in   (sub { subStack1  = stackT' }, b')


-- Check for shadowing and switch to shadow mode if nessesary
-- | Push a level-0 binder on the rewrite stack.
bindX :: Ord n => Sub n -> Bind n -> (Sub n, Bind n)
bindX sub b 
 = let  (stackX', b')      = pushBind (subConflict0 sub) (subStack0 sub) b      -- rewrite type of binder
   in   ( sub { subStack0  = stackX'
              , subShadow  = namedBoundMatchesBind (subBound sub) b}
        , b')


-- | Push some level-0 binders on the rewrite stack.
bindXs :: Ord n => Sub n -> [Bind n] -> (Sub n, [Bind n])
bindXs = mapAccumL bindX


-- | Rewrite spec binder.
useT    :: Ord n => Sub n -> Bound n -> Bound n
useT sub u
        | UName _ t             <- u
        , BindStack binds _ _ _ <- subStack1 sub
        , Just ix               <- findIndex (boundMatchesBind u) binds
        = UIx ix t

        | otherwise
        = u

-- | Rewrite witness binder.
useW    :: Ord n => Sub n -> Bound n -> Bound n
useW sub u
        | UName _ t             <- u
        , BindStack binds _ _ _ <- subStack0 sub
        , Just ix               <- findIndex (boundMatchesBind u) binds
        = UIx ix t

        | otherwise
        = u


-- | Rewrite or substitute into a level-0 variable.             -- check that vars in annots are rewritten if needed 
useX    :: Ord n => Exp a n -> Sub n -> Bound n 
        -> Either (Bound n) (Exp a n)

useX xArg sub u
  = case substBound (subStack0 sub) (subBound sub) u of
        Left  u' -> Left u'
        Right n  
         | not $ subShadow sub  -> Right (liftX n xArg)
         | otherwise            -> Left u


-------------------------------------------------------------------------------
class SubstituteXX (c :: * -> * -> *) where
 substituteWithXX 
        :: forall a n. Ord n 
        => Exp a n -> Sub n -> c a n -> c a n 


instance SubstituteXX Exp where 
 substituteWithXX xArg sub xx
  = let down    = substituteWithXX xArg
        into    = rewriteWith
    in case xx of
        XVar a u
         -> case useX xArg sub u of
                Left  u' -> XVar a u'
                Right x  -> x

        XCon{}           -> xx
        XApp a x1 x2     -> XApp a (down sub x1) (down sub x2)

        XLAM a b x
         -> let (sub1, b')      = bindT sub b
                x'              = down  sub1 x
            in  XLAM a b' x'

        XLam a b x
         -> let (sub1, b')      = bindX sub  b
                x'              = down  sub1 x
            in  XLam a b' x'

        XLet a (LLet m b x1) x2
         -> let m'              = into  sub  m
                x1'             = down  sub  x1
                (sub1, b')      = bindX sub  b
                x2'             = down  sub1 x2
            in  XLet a (LLet m' b' x1') x2'

        XLet a (LRec bxs) x2
         -> let (bs, xs)        = unzip  bxs
                (sub1, bs')     = bindXs sub bs
                xs'             = map (down sub1) xs
                x2'             = down sub1 x2
            in  XLet a (LRec (zip bs' xs')) x2'

        XLet a (LLetRegion b bs) x2
         -> let (sub1, b')      = bindT  sub  b
                (sub2, bs')     = bindXs sub1 bs
                x2'             = down sub2 x2
            in  XLet a (LLetRegion b' bs') x2'

        XLet a (LWithRegion uR) x2
         -> XLet a (LWithRegion uR) (down sub x2)

        XCase a x1 alts -> XCase a    (down sub x1) (map (down sub) alts)
        XCast a cc x1   -> XCast a cc (down sub x1)                             -- into casts
        XType t         -> XType      (into sub t)
        XWitness w      -> XWitness   (into sub w)
 

instance SubstituteXX Alt where
 substituteWithXX xArg sub aa
  = let down = substituteWithXX xArg
    in case aa of
        AAlt PDefault xBody
         -> AAlt PDefault $ down sub xBody
        
        AAlt (PData uCon bs) x
         -> let (sub1, bs')     = bindXs sub bs
                x'              = down   sub1 x
            in  AAlt (PData uCon bs') x'


-------------------------------------------------------------------------------
class Rewrite (c :: * -> *) where
 rewriteWith :: Ord n => Sub n -> c n -> c n 


instance Rewrite LetMode where
 rewriteWith sub lm
  = case lm of
        LetStrict        -> lm
        LetLazy (Just t) -> LetLazy (Just $ rewriteWith sub t) 
        LetLazy Nothing  -> LetLazy Nothing


instance Rewrite Type where
 rewriteWith sub tt 
  = let down    = rewriteWith 
    in case tt of
        TVar u          -> TVar (useT sub u)
        TCon{}          -> tt

        TForall b t
         -> let (sub1, b')      = bindT sub b
                t'              = down  sub1 t
            in  TForall b' t'

        TApp t1 t2      -> TApp (down sub t1) (down sub t2)
        TSum ts         -> TSum (down sub ts)


instance Rewrite TypeSum where
 rewriteWith sub ts
        = Sum.fromList (Sum.kindOfSum ts)
        $ map (rewriteWith sub)
        $ Sum.toList ts


instance Rewrite Witness where
 rewriteWith sub ww
  = let down    = rewriteWith 
    in case ww of
        WVar u          -> WVar  (useW sub u)
        WCon{}          -> ww
        WApp  w1 w2     -> WApp  (down sub w1) (down sub w2)
        WJoin w1 w2     -> WJoin (down sub w1) (down sub w2)
        WType t         -> WType (down sub t)

