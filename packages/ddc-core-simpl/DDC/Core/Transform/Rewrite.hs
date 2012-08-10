module DDC.Core.Transform.Rewrite
        ( RewriteRule(..)
        , rewrite )
where
import DDC.Core.Exp
import qualified DDC.Core.Compounds			as X
import qualified DDC.Core.Transform.AnonymizeX          as A
import qualified DDC.Core.Transform.Rewrite.Disjoint	as RD
import qualified DDC.Core.Transform.Rewrite.Env		as RE
import qualified DDC.Core.Transform.Rewrite.Match	as RM
import		 DDC.Core.Transform.Rewrite.Rule	(RewriteRule(..))
import qualified DDC.Core.Transform.SubstituteXX	as S
import qualified DDC.Type.Transform.SubstituteT		as S

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set


-- I would eventually like to change this to take Map String (RewriteRule..)
-- so that we can record how many times each rule is fired?
-- (no, I think just [(String,Rule)] because don't want to require unique names
rewrite :: (Show a, Show n, Ord n) => [RewriteRule a n] -> Exp a n -> Exp a n
rewrite rules x0
 =  down x0 RE.empty
 where
    down x ws = go x [] ws
    go (XApp a f arg)	args ws = go f ((down arg ws,a):args) ws
    go x@(XVar{})	args ws = rewrites x args ws
    go x@(XCon{})	args ws = rewrites x args ws
    go (XLAM a b e)	args ws = rewrites (XLAM a b $ down e (RE.lift b ws)) args ws
    go (XLam a b e)	args ws = rewrites (XLam a b $ down e (RE.extend b ws)) args ws
    go (XLet a l e)	args ws = rewrites (XLet a (goLets l ws) (down e (RE.extendLets l ws))) args ws
    go (XCase a e alts)	args ws = rewrites (XCase a (down e ws) (map (goAlts ws) alts)) args ws
    go (XCast a c e)	args ws = rewrites (XCast a c $ down e ws) args ws
    go x@(XType{})	args ws = rewrites x args ws
    go x@(XWitness{})	args ws = rewrites x args ws

    goLets (LLet lm b e) ws
     = LLet lm b $ down e ws
    goLets (LRec bs) ws
     = LRec $ zip (map fst bs) (map (flip down ws.snd) bs)
    goLets l _
     = l

    goAlts ws (AAlt p e)
     = AAlt p (down e ws)

    rewrites f args ws = rewrites' rules f args ws
    rewrites' [] f args _
     = mkApps f args
    rewrites' (r:rs) f args ws
     = case rewriteX r f args ws of
	Nothing -> rewrites' rs f args ws
	Just x  -> go x [] ws

rewriteX
    :: (Show n, Show a, Ord n)
    => RewriteRule a n
    -> Exp a n
    -> [(Exp a n,a)]
    -> RE.RewriteEnv n
    -> Maybe (Exp a n)
rewriteX (RewriteRule binds constrs lhs rhs eff clo) f args ws
 = do	let m	= RM.emptySubstInfo
	l:ls   <- return $ X.takeXAppsAsList lhs
	m'     <- RM.match m vs l f
	go m' ls args
 where
    bs = map snd binds
    vs = Set.fromList $ map nm bs

    nm (BName name _) = name
    nm _ = error "no anonymous binders in rewrite rules, please"

    go m [] rest
     = do   s <- subst m
	    return $ mkApps s rest
    go m (l:ls) ((r,_):rs)
     = do   m' <- RM.match m vs l r
	    go m' ls rs
    go _ _ _ = Nothing

    -- TODO the annotations here will be rubbish
    -- TODO type-check?
    -- TODO constraints
    subst m
     =	    let bas  = Maybe.catMaybes $ map (lookupz m) bs
	        bas' = map (\(b,a) -> (A.anonymizeX b, a)) bas
		rhs' = A.anonymizeX rhs
	    in  checkConstrs bas' constrs
		$ weakeff bas' eff
		$ weakclo bas' clo
		$ S.substituteXArgs bas' rhs'
    
    anno = snd $ head args

    weakeff _ Nothing x = x
    weakeff bas (Just e) x
     = XCast anno (CastWeakenEffect $ S.substituteTs (Maybe.catMaybes $ map lookupT bas) e) x

    weakclo _ Nothing x = x
    weakclo bas (Just c) x
     = XCast anno (CastWeakenClosure $ S.substituteTs (Maybe.catMaybes $ map lookupT bas) c) x

    checkConstrs _ [] x = Just x
    checkConstrs bas (c:cs) x = do
	let c' = S.substituteTs (Maybe.catMaybes $ map lookupT bas) c
	if RE.containsWitness c' ws || RD.checkDisjoint c' ws
	    then checkConstrs bas cs x
	    else Nothing

    lookupT (b,XType t) = Just (b,t)
    lookupT _ = Nothing

    lookupz (xs,_) b@(BName n _)
     | Just x <- Map.lookup n xs
     = Just (b,x)
    lookupz (_,tys) b@(BName n _)
     | Just t <- Map.lookup n tys
     = Just (b,XType t)

    lookupz _ _ = Nothing
    
-- | Build sequence of applications.
-- Similar to makeXApps but also takes list of annotations for XApp
mkApps :: Exp a n -> [(Exp a n, a)] -> Exp a n
mkApps f []
 = f
mkApps f ((arg,a):as)
 = mkApps (XApp a f arg) as

