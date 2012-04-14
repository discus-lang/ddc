module DDC.Core.Transform.Rewrite
    (rewrite)
where
import DDC.Core.Exp
import DDC.Core.Transform.Rewrite.Rule
import DDC.Type.Sum()
import qualified DDC.Type.Exp as T
import qualified DDC.Type.Compounds as T
import qualified DDC.Type.Predicates as T
import qualified DDC.Type.Transform.LiftT as L
{-
import qualified DDC.Core.Transform.AnonymizeX as A
import qualified DDC.Core.Transform.LiftX as L

-}
import qualified DDC.Core.Transform.SubstituteXX as S
import qualified DDC.Type.Transform.SubstituteT as S

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- I would eventually like to change this to take Map String (RewriteRule..)
-- so that we can record how many times each rule is fired?
rewrite :: (Show a, Show n, Ord n) => [RewriteRule a n] -> Exp a n -> Exp a n
rewrite rules x0
 =  down x0 emptyWitness
 where
    down x ws = go x [] ws
    go (XApp a f arg)	args ws = go f ((down arg ws,a):args) ws
    go x@(XVar{})	args ws = rewrites x args ws
    go x@(XCon{})	args ws = rewrites x args ws
    go (XLAM a b e)	args ws = rewrites (XLAM a b $ down e (liftWitness b ws)) args ws
    go (XLam a b e)	args ws = rewrites (XLam a b $ down e (extendWitness b ws)) args ws
    go (XLet a l e)	args ws = rewrites (XLet a (goLets l ws) (down e (extendWitLets l ws))) args ws
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

type WitMap n = [[T.Type n]]
emptyWitness :: Ord n => WitMap n
emptyWitness = []
extendWitness :: (Ord n,Show n) => Bind n -> WitMap n -> WitMap n
-- originally was checking universe here but type of binds is
-- TApp (TCon TyConWitness) (TVar "r"...)
-- which isn't in wit?
extendWitness b ws
    | T.isWitnessType ty
    = extend ws
 where
    ty = T.typeOfBind b
    extend (w:ws')	= (ty:w) : ws'
    extend []		= [[ty]]
extendWitness _ ws
    = ws

extendWitLets (LLetRegion b cs) ws =
    foldl (flip extendWitness) (liftWitness b ws) cs
extendWitLets _ ws = ws

-- | check if witness map contains given type
-- tries each set, lowering c by -1 after each failure
-- c may end up with negative indices,
-- not such a big deal since sets certainly won't match that
containsWitness c (w:ws) =
    c `elem` w || containsWitness (L.liftT (-1) c) ws
containsWitness _ [] = False

-- | raise all elements in witness map if binder is anonymous
-- only call with type binders ie XLAM, not XLam
liftWitness (BAnon _) ws = []:ws
liftWitness _ ws = []:ws

rewriteX
    :: (Show n, Show a, Ord n)
    => RewriteRule a n
    -> Exp a n
    -> [(Exp a n,a)]
    -> WitMap n
    -> Maybe (Exp a n)
rewriteX (RewriteRule binds constrs lhs rhs eff clo) f args ws
 = do	let m	= Map.empty
	l:ls   <- return $ flatApps lhs
	m'     <- constrain m bs l f
	go m' ls args
 where
    bs = map snd binds

    go m [] rest
     = do   s <- subst m
	    return $ mkApps s rest
    go m (l:ls) ((r,_):rs)
     = do   m' <- constrain m bs l r
	    go m' ls rs
    go _ _ _ = Nothing

    -- TODO the annotations here will be rubbish
    -- TODO type-check?
    -- TODO constraints
    subst m
     =	    let bas = Maybe.catMaybes $ map (lookupz m) bs in
	    checkConstrs bas constrs $ weakeff bas eff $ weakclo bas clo $ S.substituteXArgs bas rhs
    
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
	if containsWitness c' ws
	    then checkConstrs bas cs x
	    else Nothing

    lookupT (b,XType t) = Just (b,t)
    lookupT _ = Nothing

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

