
module DDC.Core.Transform.Rewrite
    (rewrite)
where
import DDC.Core.Exp
import DDC.Core.Transform.Rewrite.Rule
import DDC.Type.Sum()
{-
import qualified DDC.Type.Exp as T
import qualified DDC.Type.Compounds as T
import qualified DDC.Core.Transform.AnonymizeX as A
import qualified DDC.Core.Transform.LiftX as L

-}
import qualified DDC.Core.Transform.SubstituteXX as S

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- I would eventually like to change this to take Map String (RewriteRule..)
-- so that we can record how many times each rule is fired?
rewrite :: (Show a, Show n, Ord n) => [RewriteRule a n] -> Exp a n -> Exp a n
rewrite rules x0
 =  down x0
 where
    down x = go x []
    go (XApp a f arg)	args = go f ((down arg,a):args)
    go x@(XVar{})	args = rewrites x args
    go x@(XCon{})	args = rewrites x args
    go (XLAM a b e)	args = rewrites (XLAM a b $ down e) args
    go (XLam a b e)	args = rewrites (XLam a b $ down e) args
    go (XLet a l e)	args = rewrites (XLet a (goLets l) (down e)) args
    go (XCase a e alts)	args = rewrites (XCase a (down e) (map goAlts alts)) args
    go (XCast a c e)	args = rewrites (XCast a c $ down e) args
    go x@(XType{})	args = rewrites x args
    go x@(XWitness{})	args = rewrites x args

    goLets (LLet lm b e)
     = LLet lm b $ down e
    goLets (LRec bs)
     = LRec $ zip (map fst bs) (map (down.snd) bs)
    goLets l
     = l

    goAlts (AAlt p e)
     = AAlt p (down e)

    rewrites f args = rewrites' rules f args
    rewrites' [] f args
     = mkApps f args
    rewrites' (r:rs) f args
     = case rewriteX r f args of
	Nothing -> rewrites' rs f args
	Just x  -> go x []

rewriteX :: (Show n, Show a, Ord n) => RewriteRule a n -> Exp a n -> [(Exp a n,a)] -> Maybe (Exp a n)
rewriteX (RewriteRule bs _constrs lhs rhs) f args
 = do	let m	= Map.empty
	l:ls   <- return $ flatApps lhs
	m'     <- constrain m bs l f
	go m' ls args
 where
    go m [] rest
     =	    return $ mkApps (subst m) rest
    go m (l:ls) ((r,_):rs)
     = do   m' <- constrain m bs l r
	    go m' ls rs
    go _ _ _ = Nothing

    -- TODO the annotations here will be rubbish
    -- TODO type-check?
    -- TODO constraints
    subst m
     =	    let bas = Maybe.catMaybes $ map (lookupz m) bs in
	    S.substituteXArgs bas rhs

    lookupz m b@(BName n _)
     = do   x <- Map.lookup n m
	    return (b,x)
    lookupz _ _ = Nothing
    
constrain :: (Show a, Show n, Ord n) => Map.Map n (Exp a n) -> [Bind n] -> Exp a n -> Exp a n -> Maybe (Map.Map n (Exp a n))
{-
constrain _m _bs l r
 | trace ("L:"++show l++"\nR:"++show r) False
 = Nothing
 -}
constrain m bs (XVar _ (UName n _)) r
 | n `elem` (map nm bs)
 = case Map.lookup n m of
   Nothing -> return $ Map.insert n r m
   Just x  -> constrain m [] x r
 where
    nm (BName name _) = name
    nm _ = error "no anonymous binders in rewrite rules, please"

constrain m bs (XType (TVar (UName n _))) r
 | n `elem` (map nm bs)
 = case Map.lookup n m of
   Nothing -> return $ Map.insert n r m
   Just x  -> constrain m [] x r
 where
    nm (BName name _) = name
    nm _ = error "no anonymous binders in rewrite rules, please"

constrain m _ (XVar _ l) (XVar _ r)
 | l == r	= Just m

constrain m _ (XCon _ l) (XCon _ r)
 | l == r	= Just m

constrain m bs (XApp _ lf la) (XApp _ rf ra)
 = do	m' <- constrain m bs lf rf
	constrain m' bs la ra

constrain m bs (XCast _ lc le) (XCast _ rc re)
 | lc == rc	= constrain m bs le re

constrain m _ (XType l) (XType r)
 | l == r	= return m

constrain m _ (XWitness l) (XWitness r)
 | eqWit l r	= return m

constrain _ _ _ _ = Nothing

{-
 don't allow binders in lhs of rule, so don't need to worry about these...

-- we could anonymise these bindings. probably should
constrain m bs (XLAM _ l lb) (XLAM _ r rb)
 | l == r	= constrain m bs lb rb

constrain m bs (XLam _ l lb) (XLam _ r rb)
 | l == r	= constrain m bs lb rb

constrain m bs (XLet _ ll le) (XLet _ rl re)
 = do	m' <- constrainLets m ll rl
	constrain m' le re

constrainLets m (LLet _ lb le) (LLet _ rb re)
 | l == r	= constrain m le re
constrainLets m (LRec ls) (LRec rs)
 | all $ zipWith (==) (map fst ls) (map fst rs)
		= map 
-}

-- TODO witness equality
eqWit _ _ = True

flatApps :: (Show a, Show n, Ord n) => Exp a n -> [Exp a n]
flatApps (XApp _ lhs rhs) = flatApps lhs ++ [rhs]
flatApps x = [x]

-- | Put the poles in the holes
mkApps f []
 = f
mkApps f ((arg,a):as)
 = mkApps (XApp a f arg) as

