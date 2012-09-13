module DDC.Core.Transform.Rewrite
        ( RewriteRule(..)
        , rewrite )
where
import DDC.Base.Pretty
import DDC.Core.Exp
import DDC.Core.Simplifier.Base (TransformResult(..), TransformInfo(..))
import qualified DDC.Core.Compounds			as X
import qualified DDC.Core.Transform.AnonymizeX          as A
import qualified DDC.Core.Transform.Rewrite.Disjoint	as RD
import qualified DDC.Core.Transform.Rewrite.Env		as RE
import qualified DDC.Core.Transform.Rewrite.Match	as RM
import		 DDC.Core.Transform.Rewrite.Rule	(RewriteRule(..), BindMode(..), isBMType)
import qualified DDC.Core.Transform.SubstituteXX	as S
import qualified DDC.Type.Transform.SubstituteT		as S
import qualified DDC.Core.Transform.Trim                as Trim
import qualified DDC.Core.Transform.LiftX		as L
import qualified DDC.Type.Compounds			as T
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Control.Monad.Writer (tell, runWriter)
import Data.List	    (partition)
import Data.Typeable	    (Typeable)


data RewriteInfo = RewriteInfo [RewriteLog]
    deriving Typeable
instance Pretty RewriteInfo where
 ppr (RewriteInfo rules) = text "Rules fired:"
		       <$> indent 4 (vcat $ map ppr rules)

data RewriteLog
    = LogRewrite String
    | LogUnfold  String
    deriving Typeable
instance Pretty RewriteLog where
 ppr (LogRewrite name) = text "Rewrite: " <> text name
 ppr (LogUnfold  name) = text "Unfold:  " <> text name

isProgress = not . null

-- | Perform rewrites top-down, repeatedly.
--
rewrite :: (Show a, Show n, Ord n, Pretty n) => [(String,RewriteRule a n)] -> Exp a n -> TransformResult (Exp a n)
rewrite rules x0
 =  let (x,info) = runWriter $ down x0 RE.empty
    in  TransformResult
	{ result   	 = x
	, resultProgress = isProgress info
	, resultInfo	 = TransformInfo (RewriteInfo info) }
 where
    down x ws = go x [] ws

    go (XApp a f arg)	args ws = do
	arg' <- down arg ws
	go f ((arg',a):args) ws

    go x@(XVar{})	args ws = rewrites x args ws

    go x@(XCon{})	args ws = rewrites x args ws

    go (XLAM a b e)	args ws = do
	e' <- down e (RE.lift b ws)
	rewrites (XLAM a b e') args ws

    go (XLam a b e)	args ws = do
	e' <- down e (RE.extend b ws)
	rewrites (XLam a b e') args ws

    go (XLet a l@(LRec _) e) args ws	= do
	let ws' = RE.extendLets l ws
	l' <- goLets l ws'
	e' <- down e ws'
	rewrites (XLet a l' e') args ws'

    go (XLet a l e)	args ws = do
	l' <- goLets l ws
	dh <- goDefHoles a l' e ws
	rewrites dh args ws

    go (XCase a e alts)	args ws = do
	e'    <- down e ws
	alts' <- mapM (goAlts ws) alts
	rewrites (XCase a e' alts') args ws

    go (XCast a c e)	args ws = do
	e' <- down e ws
	rewrites (XCast a c e') args ws

    go x@(XType{})	args ws = rewrites x args ws

    go x@(XWitness{})	args ws = rewrites x args ws

    -- If definitions match the holes of any rules,
    -- clean it up and record it for later.
    -- Eg with this rule,
    --	 RULE unbox {box s} = s
    --
    -- this expression:
    --   let x = box (some expensive op)
    --	 in  ...
    --
    -- will be transformed to
    --   let ^ = some expensive op
    --       x = box ^0
    --   in ...
    --
    goDefHoles a l@(LLet LetStrict let_bind def) e ws
     =	let subs = checkHoles def ws
	in  case subs of
	    (((sub, []), name, RewriteRule { ruleBinds = bs, ruleLeft = hole }):_) ->
		    -- only get value-level bindings
		let bs'	       = map snd $ filter (isBMType.fst) bs
		    bas'       = lookups bs' sub

		    -- check if it looks like something has already been unfolded
		    isUIx x = case x of 
			      XVar _ (UIx _)	 -> True
			      XVar _ (UPrim _ _) -> True
			      _			 -> False
		    already_done= all isUIx $ map snd bas'

		    -- find kind-values and sub those in as well
		    bsK'       = map snd $ filter ((==BMKind).fst) bs
		    basK       = lookups bsK' sub

		    basK'      = concatMap (\(b,x) -> case X.takeXType x of
						      Just t -> [(b,t)]
						      Nothing-> []) basK

		    -- surround whole expression with anon lets from sub
		    values     = map	 (\(b,v) ->   (BAnon (S.substituteTs basK' $ T.typeOfBind b), v)) (reverse bas')
		    -- replace 'def' with LHS-HOLE[sub => ^n]
		    anons      = zipWith (\(b,_) i -> (b, XVar a (UIx i))) bas' [0..]
		    lets       = map (\(b,v) -> LLet LetStrict b v) values

		    def'       = S.substituteXArgs basK
			       $ S.substituteXArgs anons hole

		    let_bind'  = S.substituteTs basK' let_bind
		    lets'      = lets ++ [LLet LetStrict let_bind' def']
		    -- lift e by (length bas)
		    depth      = case let_bind of
				 BAnon _ -> 1 
				 _       -> 0
		    e'	       = L.liftAtDepthX (length bas') depth e
		    -- SAVE in wit env
		    ws'	       = foldl (flip RE.extendLets) ws lets'
		in  if already_done
		    then do
			e'' <- down e (RE.extendLets l ws)
			return $ XLet a l e''
		    else do
			tell [LogUnfold name]
			e'' <- down e' ws'
			return $ X.makeXLets a lets' e''
	    _ -> do
		e' <- down e (RE.extendLets l ws)
		return $ XLet a l e'

    goDefHoles a l e ws = do
	e' <- down e (RE.extendLets l ws)
	return $ XLet a l e'

    -- Match a let-definition against the holes in all the rules
    checkHoles def ws
     =  let rules'   = Maybe.catMaybes $ map holeRule rules
	    (f,args) = takeApps def
	    -- TODO most specific?
	in  Maybe.catMaybes
	  $ map (\(name,r) -> fmap (\s -> (s,name,r)) $ rewriteSubst r f args ws RM.emptySubstInfo)
	    rules'

    holeRule (name, rule@RewriteRule { ruleLeftHole	= Just hole })
	= Just (name,
	    rule { ruleLeft	= hole
		 , ruleLeftHole	= Nothing })
    holeRule _ = Nothing

    goLets (LLet lm b e) ws = do
	e' <- down e ws
	return $ LLet lm b e'
    goLets (LRec bs) ws = do
	bs' <- mapM (flip down ws . snd) bs
	return $ LRec $ zip (map fst bs) bs'
    goLets l _ =
	return $ l

    goAlts ws (AAlt p e) = do
	e' <- down e ws
	return $ AAlt p e'

    -- TODO this should find the "most specific".
    -- ben suggested that size of substitution map might be good indicator
    -- (smaller is better)
    rewrites f args ws = rewrites' rules f args ws
    rewrites' [] f args _
     = return $ mkApps f args
    rewrites' ((n,r):rs) f args ws
     = case rewriteX r f args ws of
	Nothing -> rewrites' rs f args ws
	Just x  -> tell [LogRewrite n] >> go x [] ws


-- | Attempt to find a rewrite substitution to match expression against rule.
--   Returns substitution and the left-over arguments that weren't matched against.
--   EG:
--
--   rewriteSubst
--	(RULE mapMapId [a b : *] (f : a -> b) (xs : List a).
--	      map [:a b:] f (map [:a a:] id xs)
--	    = map [:a b:] f xs)
--	map
--	[ [Int], [Int], (\x -> f), (map [:Int Int:] id [1,2,3]) ]
--
--	env
--
--	emptySubstInfo
--   ==>
--     Just ({a |-> Int, b |-> Int, f |-> (\x -> f), xs |-> [1,2,3] }, [])
--
--   However if we had passed a substitution such as {a |-> Float} instead of emptySubstInfo,
--	it would not have matched.
--
--   The environment is used for 'hole' rules, that can look up bound definitions
--   and match if inlining would let them, even when inlining won't occur.
--
rewriteSubst
    :: (Show n, Show a, Ord n, Pretty n)
    => RewriteRule a n		-- ^ Rule
    -> Exp a n			-- ^ Bottom-most (function) of expression
    -> [(Exp a n,a)]		-- ^ Arguments applied to function, with XApp's annotation
    -> RE.RewriteEnv a n	-- ^ Environment to rewrite in: witnesses for constraints,
    -> RM.SubstInfo  a n	-- ^ Existing substitution to match with
    -> Maybe (RM.SubstInfo a n, [(Exp a n, a)])
				-- ^ Substitution map and remaining (unmatched) args
rewriteSubst
    RewriteRule
	{ ruleBinds	= binds
	, ruleLeft	= lhs
	, ruleLeftHole	= Nothing
	, ruleFreeVars	= free}
    f args env sub
 = do	-- First check if any of the free variables have been re-bound: instafail
	checkFreeVars

	l:ls   <- return $ X.takeXAppsAsList lhs
	sub'     <- RM.match sub vs l f
	go sub' ls args
 where
    checkFreeVars
     | any (flip RE.isDef env) free
     = Nothing

     | otherwise
     = return ()

    bs = map snd binds
    vs = Set.fromList $ map nm bs

    -- Get name from a Bind. We disallow BAnon and BNone earlier, so this is safe.
    nm (BName name _) = name
    nm _ = error "Impossible! No anonymous binders in rewrite rules, please"

    -- Match each left-hand rule against the arguments
    go m [] rest
     = do   return $ (m, rest)
    go m (l:ls) ((r,_):rs)
     = do   m' <- RM.match m vs l r
	    go m' ls rs
    go _ _ _ = Nothing

-- Find substitution for rules with a 'hole'.
-- An example rule with a holes is:
--	RULE (i : Int). unbox {box i} = i
rewriteSubst
    rule@(RewriteRule{ ruleLeftHole = Just hole })
    f args ws sub
 =  -- Try to match against entire rule with no inlining. Eg (unbox (box 5))
    case rewriteSubst rule_full f args ws sub of
    Just s  -> Just s
    Nothing ->
      -- Try to match without the hole:
      --    rewriteSubst (RULE (i : Int). unbox) (unbox x)
      -- which will return a substitution (empty here), and the leftover argument 'x'.
      case rewriteSubst rule_some f args ws sub of
      Just (sub',((XVar _ b, _):as)) -> 
        -- Check if hole variable is bound to a value
	case RE.getDef b ws of
	Just d' -> let (fd,ad) = takeApps d' in
		   -- Merge the outer substitution with one for the hole
		   case rewriteSubst rule_hole fd ad ws sub' of
		   Just (subd,asd) -> Just (subd,asd ++ as)
		   Nothing	   -> Nothing
	-- No definition in environment
	Nothing -> Nothing
      -- rule_some didn't match properly: failure
      _ -> Nothing
 where
  lhs_full  = XApp undefined (ruleLeft rule) hole
  rule_full = rule
	      { ruleLeft	= lhs_full
	      , ruleLeftHole	= Nothing }

  rule_some = rule
	      { ruleLeft	= ruleLeft rule
	      , ruleLeftHole	= Nothing }

  rule_hole = rule
	      { ruleLeft	= hole
	      , ruleLeftHole	= Nothing }

-- | Perform rewrite rule on expression if a valid substitution exists,
--   and constraints are satisfied.
--
rewriteX
    :: (Show n, Show a, Ord n, Pretty n)
    => RewriteRule a n
    -> Exp a n
    -> [(Exp a n,a)]
    -> RE.RewriteEnv a n
    -> Maybe (Exp a n)
rewriteX
    rule@(RewriteRule
	    { ruleBinds		= binds
	    , ruleConstraints	= constrs
	    , ruleRight		= rhs
	    , ruleWeakEff	= eff
	    , ruleWeakClo	= clo })
    f args ws
 = do	-- Find a substitution
	(m,rest) <- rewriteSubst rule f args ws RM.emptySubstInfo
	-- Check constraints, perform substitution and add weakens if necessary
	s	 <- subst m
	-- Add the remaining arguments that weren't matched by rule
	return   $  mkApps s rest
 where
    bs = map snd binds

    -- TODO the annotations here will be rubbish because they are from the rule's source location
    subst m
     =	    let bas2        = lookups bs m
		rhs2	    = A.anonymizeX rhs
		(bas3,lets) = wrapLets bas2
		rhs3	    = L.liftX (length lets) rhs2

	    in  checkConstrs bas3 constrs
		$ X.makeXLets anno lets
		$ weakeff bas3
		$ weakclo bas3
		$ S.substituteXArgs bas3 rhs3

    wrapLets bas
     =	let (as,bs'') = partition isMkLet (bas `zip` binds)
	    as'	    = map fst as
	    bs'	    = map fst bs''

	    anons   = zipWith (\(b,_) i -> (b, XVar anno (UIx i))) as' [0..]
	    values  = map     (\(b,v) ->   (BAnon (substT bs' $ T.typeOfBind b), v)) (reverse as')
	    lets    = map (\(b,v) -> LLet LetStrict b v) values

	in  (bs'++anons, lets)

    isMkLet (_,(BMType i,_)) = i /= 1
    isMkLet _                = False

    -- Dummy annotation for the casts
    anno = case args of
	   (_,a):_ -> a
	   _	   -> undefined

    weakeff bas x
     = case eff of
       Nothing	-> x
       Just e	-> XCast anno
		   (CastWeakenEffect $ substT bas e)
		   x

    weakclo bas x
     = case clo of
       []	-> x
       _	-> XCast anno 
		   (CastWeakenClosure
                   $ Trim.trimClosures anno
		   $ map (S.substituteXArgs bas) clo)
		   x

    checkConstrs _ [] x = Just x
    checkConstrs bas (c:cs) x = do
	let c' = substT bas c
	if RE.containsWitness c' ws || RD.checkDisjoint c' ws
	    then checkConstrs bas cs x
	    else Nothing
    
    substT bas x = S.substituteTs (Maybe.catMaybes $ map lookupT bas) x

    lookupT (b,XType t) = Just (b,t)
    lookupT _ = Nothing

lookups bs m
 =  let bas  = Maybe.catMaybes $ map (lookupX m) bs
        bas' = map (\(b,a) -> (A.anonymizeX b, A.anonymizeX a)) bas
    in  bas'

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

