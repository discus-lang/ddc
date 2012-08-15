{-# OPTIONS_GHC -Wwarn #-}

module DDC.Core.Transform.Rewrite
        ( RewriteRule(..)
        , rewrite )
where
import DDC.Base.Pretty

import DDC.Core.Exp
import qualified DDC.Core.Compounds			as X
import qualified DDC.Core.Transform.AnonymizeX          as A
import qualified DDC.Core.Transform.Rewrite.Disjoint	as RD
import qualified DDC.Core.Transform.Rewrite.Env		as RE
import qualified DDC.Core.Transform.Rewrite.Match	as RM
import		 DDC.Core.Transform.Rewrite.Rule	(RewriteRule(..), BindMode(..))
import qualified DDC.Core.Transform.SubstituteXX	as S
import qualified DDC.Type.Transform.SubstituteT		as S
import qualified DDC.Core.Transform.LiftX		as L
import qualified DDC.Type.Compounds			as T

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

import Debug.Trace




-- I would eventually like to change this to take Map String (RewriteRule..)
-- so that we can record how many times each rule is fired?
-- (no, I think just [(String,Rule)] because don't want to require unique names
rewrite :: (Show a, Show n, Ord n, Pretty n) => [RewriteRule a n] -> Exp a n -> Exp a n
rewrite rules x0
 =  down x0 RE.empty
 where
    down x ws = go x [] ws
    go (XApp a f arg)	args ws = go f ((down arg ws,a):args) ws
    go x@(XVar{})	args ws = rewrites x args ws
    go x@(XCon{})	args ws = rewrites x args ws
    go (XLAM a b e)	args ws = rewrites (XLAM a b $ down e (RE.lift b ws)) args ws
    go (XLam a b e)	args ws = rewrites (XLam a b $ down e (RE.extend b ws)) args ws
    go (XLet a l e)	args ws = rewrites (goDefHoles a (goLets l ws) e ws) args ws
    go (XCase a e alts)	args ws = rewrites (XCase a (down e ws) (map (goAlts ws) alts)) args ws
    go (XCast a c e)	args ws = rewrites (XCast a c $ down e ws) args ws
    go x@(XType{})	args ws = rewrites x args ws
    go x@(XWitness{})	args ws = rewrites x args ws

    goDefHoles a l@(LLet LetStrict b def) e ws
     =	let subs = checkHoles def ws
	in  case subs of
	    (((sub,[]),RewriteRule bs _cs hole Nothing _rhs _e _c):_) ->
		    -- only get value-level bindings
		let bs'	       = map snd $ filter ((==BMType).fst) bs
		    (_,bas')   = lookups bs' sub
		    -- surround whole expression with anon lets from sub
		    values     = map	 (\(b,v) ->     (BAnon (T.typeOfBind b), v)) bas'
		    -- replace 'def' with LHS-HOLE[sub => ^n]
		    anons      = zipWith (\(b,_) i -> (b, XVar a (UIx i))) bas' [0..]
		    lets       = map (\(b,v) -> LLet LetStrict b v) values
		    def'       = S.substituteXArgs anons hole
		    lets'      = lets ++ [LLet LetStrict b def']
		    -- lift e by (length bas)
		    e'	       = L.liftX (length bas') e
		    -- SAVE in wit env
		    ws'	       = foldl (flip RE.extendLets) ws lets'
		in  X.makeXLets a lets' $ down e ws'
	    _ -> XLet a l (down e (RE.extendLets l ws))

    goDefHoles a l e ws = XLet a l (down e (RE.extendLets l ws))

    checkHoles def ws
     =  let rules'   = Maybe.catMaybes $ map holeRule rules
	    (f,args) = takeApps def
	    -- TODO most specific?
	in  Maybe.catMaybes
	  $ map (\r -> fmap (\s -> (s,r)) $ rewriteSubst r f args ws)
	    rules'

    holeRule (RewriteRule bs cs _lhs (Just hole) rhs e c)
	= Just (RewriteRule bs cs hole Nothing rhs e c) -- LOL undefineds
    holeRule _ = Nothing

    goLets (LLet lm b e) ws
     = LLet lm b $ down e ws
    goLets (LRec bs) ws
     = LRec $ zip (map fst bs) (map (flip down ws.snd) bs)
    goLets l _
     = l

    goAlts ws (AAlt p e)
     = AAlt p (down e ws)

    -- TODO this should find the "most specific".
    -- ben suggested that size of substitution map might be good indicator
    -- (smaller is better)
    rewrites f args ws = rewrites' rules f args ws
    rewrites' [] f args _
     = mkApps f args
    rewrites' (r:rs) f args ws
     = case rewriteX r f args ws of
	Nothing -> rewrites' rs f args ws
	Just x  -> go x [] ws

rewriteSubst
    :: (Show n, Show a, Ord n, Pretty n)
    => RewriteRule a n
    -> Exp a n
    -> [(Exp a n,a)]
    -> RE.RewriteEnv a n
    -> Maybe (RM.SubstInfo a n, [(Exp a n, a)]) -- ^ substitution map and remaining args
-- TODO check constraints?
rewriteSubst (RewriteRule binds _ lhs Nothing _ _ _) f args _
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
     = do   return $ (m, rest)
    go m (l:ls) ((r,_):rs)
     = do   m' <- RM.match m vs l r
	    go m' ls rs
    go _ _ _ = Nothing

rewriteSubst (RewriteRule binds constrs lhs (Just hole) rhs eff clo) f args ws
 =  case rewriteSubst rule_full f args ws of
    Just s  -> Just s
    Nothing ->
      -- try inlining without hole
      case rewriteSubst rule_some f args ws of
      Just (sub,((XVar a b, a'):as)) -> 
	case RE.getDef b ws of
	Just d' -> let (fd,ad) = takeApps d' in
		    -- TODO match with 'sub' as well?
		   case rewriteSubst rule_hole fd ad ws of
		   -- TODO merge subs
		   Just (subd,asd) -> Just (subd,asd ++ as)
		   Nothing	-> Nothing
	Nothing -> Nothing
      s -> s
 where
  lhs_full  = XApp undefined lhs hole
  rule_full = RewriteRule binds constrs lhs_full Nothing rhs eff clo

  rule_some = RewriteRule binds constrs lhs Nothing rhs eff clo
  rule_hole = RewriteRule binds constrs hole Nothing rhs eff clo

rewriteX
    :: (Show n, Show a, Ord n, Pretty n)
    => RewriteRule a n
    -> Exp a n
    -> [(Exp a n,a)]
    -> RE.RewriteEnv a n
    -> Maybe (Exp a n)
rewriteX rule@(RewriteRule binds constrs _ lhs rhs eff clo) f args ws
 = do	(m,rest) <- rewriteSubst rule f args ws
	s	 <- subst m
	return   $  mkApps s rest
 where
    bs = map snd binds

    -- TODO the annotations here will be rubbish
    -- TODO type-check?
    -- TODO constraints
    subst m
     =	    let (bas,bas') = lookups bs m
		rhs'	   = A.anonymizeX rhs
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
     = x
        -- TODO: weakclo is broken.
        --       need to update to use new plan for CastWeakenClosure,
        --       it contains expressions now instead of Use types.
        -- XCast anno (CastWeakenClosure $ S.substituteTs (Maybe.catMaybes $ map lookupT bas) c) x

    checkConstrs _ [] x = Just x
    checkConstrs bas (c:cs) x = do
	let c' = S.substituteTs (Maybe.catMaybes $ map lookupT bas) c
	if RE.containsWitness c' ws || RD.checkDisjoint c' ws
	    then checkConstrs bas cs x
	    else Nothing

    lookupT (b,XType t) = Just (b,t)
    lookupT _ = Nothing

lookups bs m
 =  let bas  = Maybe.catMaybes $ map (lookupX m) bs
        bas' = map (\(b,a) -> (A.anonymizeX b, A.anonymizeX a)) bas
    in  (bas, bas')

lookupX (xs,_) b@(BName n _)
 | Just x <- Map.lookup n xs
 = Just (b,x)
lookupX (_,tys) b@(BName n _)
 | Just t <- Map.lookup n tys
 = Just (b,XType t)

lookupX _ _ = Nothing


-- | Build sequence of applications.
-- Similar to makeXApps but also takes list of annotations for XApp
mkApps :: Exp a n -> [(Exp a n, a)] -> Exp a n
mkApps f []
 = f
mkApps f ((arg,a):as)
 = mkApps (XApp a f arg) as

-- | Destruct sequence of applications.
-- Similar to takeXApps but also keeps annotations for later
takeApps :: Exp a n -> (Exp a n, [(Exp a n, a)])
takeApps (XApp a f arg)
 = let (f', args') = takeApps f
   in  (f', args' ++ [(arg,a)])
takeApps x
 = (x, [])

