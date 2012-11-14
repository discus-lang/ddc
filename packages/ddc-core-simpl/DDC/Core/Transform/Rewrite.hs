
-- | Apply rewrite rules.
module DDC.Core.Transform.Rewrite
        ( RewriteRule(..)
        , rewriteModule
        , rewriteX)
where
import DDC.Base.Pretty
import DDC.Core.Exp
import DDC.Core.Module
import Data.Map                                         (Map)
import DDC.Core.Simplifier.Base (TransformResult(..), TransformInfo(..))
import qualified DDC.Core.Compounds                     as X
import qualified DDC.Core.Transform.AnonymizeX          as A
import qualified DDC.Core.Transform.Rewrite.Disjoint    as RD
import qualified DDC.Core.Transform.Rewrite.Env         as RE
import qualified DDC.Core.Transform.Rewrite.Match       as RM
import           DDC.Core.Transform.Rewrite.Rule
import qualified DDC.Core.Transform.SubstituteXX        as S
import qualified DDC.Type.Transform.SubstituteT         as S
import qualified DDC.Core.Transform.Trim                as Trim
import qualified DDC.Core.Transform.LiftX               as L
import qualified DDC.Type.Compounds                     as T
import qualified Data.Map                               as Map
import qualified Data.Maybe                             as Maybe
import qualified Data.Set                               as Set
import Control.Monad
import Control.Monad.Writer (tell, runWriter)
import Data.List
import Data.Typeable


-- Log ------------------------------------------------------------------------
-- | Tracks which rewrite rules fired.
data RewriteInfo 
        = RewriteInfo [RewriteLog]
        deriving Typeable

data RewriteLog
        = LogRewrite String
        | LogUnfold  String
        deriving Typeable


instance Pretty RewriteInfo where
 ppr (RewriteInfo rules) 
        =   text "Rules fired:"
        <$> indent 4 (vcat $ map ppr rules)


instance Pretty RewriteLog where
 ppr (LogRewrite name) = text "Rewrite: " <> text name
 ppr (LogUnfold  name) = text "Unfold:  " <> text name

isProgress = not . null


-- Rewrite --------------------------------------------------------------------
-- | Apply rewrite rules to a module.
rewriteModule
        :: (Show a, Show n, Ord n, Pretty n)
        => [NamedRewriteRule a n]       -- ^ Rewrite rule database.
        -> Module a n                   -- ^ Rewrite in this module.
        -> Module a n

rewriteModule rules mm
 = mm { moduleBody = result $ rewriteX rules $ moduleBody mm }


-- | Perform rewrites top-down, repeatedly.
rewriteX 
        :: (Show a, Show n, Ord n, Pretty n) 
        => [NamedRewriteRule a n]       -- ^ Rewrite rules database.
        -> Exp a n                      -- ^ Rewrite in this expression.
        -> TransformResult (Exp a n)

rewriteX rules x0
 = let  (x,info) = runWriter $ down RE.empty x0 
   in   TransformResult
         { result               = x
         , resultAgain          = isProgress info
         , resultProgress       = isProgress info
         , resultInfo           = TransformInfo (RewriteInfo info) }
 where
        down env x  
         = go env x []

        -- ISSUE #280:  Rewrites should be done with the most specific rule.
        --   The rewrite engine should apply the most specific rule, instead
        --   of the first one that it finds that matches. If not, then we
        --   should give some warning about overlapping rules.
        -- 
        -- Look for rules in the list that match the given expression,
        -- and apply the first one that matches.
        rewrites env f args
         = rewrites' rules env f args

        rewrites' [] _ f args
         = return $ X.makeXAppsWithAnnots f args

        rewrites' ((n, rule) : rs) env f args
         = case rewriteWithX rule env f args of
                Nothing -> rewrites' rs env f args
                Just x  -> tell [LogRewrite n] >> go env x []

        -- Decend into the expression, looking for applications that we 
        -- might be able to apply rewrites to.
        go env (XApp a f arg) args
         = do   arg' <- down env arg
                go env f ((arg',a):args)

        go env x@XVar{}   args 
         =      rewrites env x args

        go env x@XCon{}   args 
         =      rewrites env x args

        go env (XLAM a b e) args
         = do   e' <- down (RE.lift b env) e 
                rewrites env (XLAM a b e') args

        go env (XLam a b e) args
         = do   e' <- down (RE.extend b env) e 
                rewrites env (XLam a b e') args

        go env (XLet a l@(LRec _) e) args
         = do   let env' = RE.extendLets l env
                l'      <- goLets l env'
                e'      <- down env' e 
                rewrites env' (XLet a l' e') args

        go env (XLet a l e)     args
         = do   l'      <- goLets l env
                dh      <- goDefHoles rules a l' e env down
                rewrites env dh args 

        go env (XCase a e alts) args
         = do   e'      <- down env e
                alts'   <- mapM (goAlts env) alts
                rewrites env (XCase a e' alts') args

        go env (XCast a c e)    args
         = do   e'      <- down env e
                rewrites env (XCast a c e') args

        go env x@(XType{})      args
         =      rewrites env x args

        go env x@(XWitness{})   args
         =      rewrites env x args


        goLets (LLet lm b e) ws 
         = do   e' <- down ws e 
                return $ LLet lm b e'

        goLets (LRec bs) ws 
         = do   bs'     <- mapM (down ws) $ map snd bs
                return $ LRec $ zip (map fst bs) bs'


        goLets l _ 
         = return $ l

        goAlts ws (AAlt p e) 
         = do   e' <- down ws e 
                return $ AAlt p e'


-- If definitions match the holes of any rules,
-- clean it up and record it for later.
-- Eg with this rule,
--   RULE unbox {box s} = s
--
-- this expression:
--   let x = box (some expensive op)
--   in  ...
--
-- will be transformed to
--   let ^ = some expensive op
--       x = box ^0
--   in ...
--
goDefHoles rules a l@(LLet LetStrict let_bind def) e ws down
 = let subs = checkHoles rules def ws
   in case subs of
       (((sub, []), name, RewriteRule { ruleBinds = bs, ruleLeft = hole }):_) ->
            -- only get value-level bindings
        let bs'        = map snd $ filter (isBMType.fst) bs
            bas'       = lookups bs' sub

            -- check if it looks like something has already been unfolded
            isUIx x = case x of 
                      XVar _ (UIx _)     -> True
                      XVar _ (UPrim _ _) -> True
                      _                  -> False
            already_done= all isUIx $ map snd bas'

            -- find kind-values and sub those in as well
            bsK'       = map snd $ filter ((==BMKind).fst) bs
            basK       = lookups bsK' sub

            basK'      = concatMap (\(b,x) -> case X.takeXType x of
                                              Just t -> [(b,t)]
                                              Nothing-> []) basK

            -- surround whole expression with anon lets from sub
            values     = map     (\(b,v) ->   (BAnon (S.substituteTs basK' $ T.typeOfBind b), v)) (reverse bas')
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
            e'         = L.liftAtDepthX (length bas') depth e
            -- SAVE in wit env
            ws'        = foldl (flip RE.extendLets) ws lets'
        in  if already_done
            then do
                e'' <- down (RE.extendLets l ws) e
                return $ XLet a l e''
            else do
                tell [LogUnfold name]
                e'' <- down ws' e'
                return $ X.xLets a lets' e''
       _ -> do
                e' <- down (RE.extendLets l ws) e
                return $ XLet a l e'

goDefHoles _rules a l e ws down
 = do   e' <- down (RE.extendLets l ws) e
        return $ XLet a l e'


-- Match a let-definition against the holes in all the rules
checkHoles rules def ws
 = let  rules'   = Maybe.catMaybes $ map holeRule rules
        (f,args) = X.takeXAppsWithAnnots def
        -- TODO most specific?
   in  Maybe.catMaybes
        $ map (\(name,r) -> fmap (\s -> (s,name,r)) $ rewriteSubst r f args ws RM.emptySubstInfo)
            rules'

holeRule (name, rule@RewriteRule { ruleLeftHole     = Just hole })
 = Just ( name
        , rule { ruleLeft     = hole
               , ruleLeftHole = Nothing })

holeRule _ = Nothing


-------------------------------------------------------------------------------
-- | Perform rewrite rule on expression if a valid substitution exists,
--   and constraints are satisfied.
rewriteWithX
        :: (Show n, Show a, Ord n, Pretty n)
        => RewriteRule a n
        -> RE.RewriteEnv a n
        -> Exp a n
        -> [(Exp a n, a)]
        -> Maybe (Exp a n)

rewriteWithX
    rule@(RewriteRule
            { ruleBinds         = binds
            , ruleConstraints   = constrs
            , ruleRight         = rhs
            , ruleWeakEff       = eff
            , ruleWeakClo       = clo })
    env f args 
 = do   
        -- Try to find a substitution for the left of the rule.
        (m, rest) <- rewriteSubst rule f args env RM.emptySubstInfo

        -- Check constraints, perform substitution and add weakens if necessary.
        s         <- subst m

        -- Add the remaining arguments that weren't matched by rule
        return   $  X.makeXAppsWithAnnots s rest
 where
    bs = map snd binds

    -- TODO the annotations here will be rubbish because they are from the rule's source location
    subst m
     =      let bas2        = lookups bs m
                rhs2        = A.anonymizeX rhs
                (bas3,lets) = wrapLets anno binds bas2
                rhs3        = L.liftX (length lets) rhs2

            in  checkConstrs bas3 constrs
                $ X.xLets anno lets
                $ weakeff bas3
                $ weakclo bas3
                $ S.substituteXArgs bas3 rhs3

    -- Dummy annotation for the casts
    anno = case args of
           (_,a):_ -> a
           _       -> undefined

    weakeff bas x
     = case eff of
       Nothing  -> x
       Just e   -> XCast anno
                   (CastWeakenEffect $ substT bas e)
                   x

    weakclo bas x
     = case clo of
       []       -> x
       _        -> XCast anno 
                   (CastWeakenClosure
                   $ Trim.trimClosures anno
                   $ map (S.substituteXArgs bas) clo)
                   x

    checkConstrs _ [] x = Just x
    checkConstrs bas (c:cs) x = do
        let c' = substT bas c
        if RE.containsWitness c' env || RD.checkDisjoint c' env || RD.checkDistinct c' env
            then checkConstrs bas cs x
            else Nothing
    

wrapLets  
        :: Ord n 
        => a 
        -> [(BindMode, Bind n)]         -- ^ Variables bound by the rule.
        -> [(Bind n,   Exp a n)]        -- ^ Substitution for the left of the rule.
        -> ( [(Bind n, Exp a n)]
           , [Lets a n])

wrapLets a binds bas 
 = let  isMkLet (_, (BMType i, _)) = i /= 1
        isMkLet _                   = False

        (as, bs'') = partition isMkLet (bas `zip` binds)
        as'     = map fst as
        bs'     = map fst bs''

        anons   = zipWith (\(b,_) i -> (b, XVar a (UIx i))) as' [0..]
        values  = map     (\(b,v) ->   (BAnon (substT bs' $ T.typeOfBind b), v)) (reverse as')
        lets    = map (\(b,v) -> LLet LetStrict b v) values

   in   (bs' ++ anons, lets)


substT :: Ord n => [(Bind n, Exp a n)] -> Type n -> Type n
substT bas x 
 = let  sub     = [(b, t) | (b, XType t) <- bas ] 
   in   S.substituteTs sub x



-- | Attempt to find a rewrite substitution to match expression against rule.
--   Returns substitution and the left-over arguments that weren't matched against.
--   EG:
--
--   rewriteSubst
--      (RULE mapMapId [a b : *] (f : a -> b) (xs : List a).
--            map [:a b:] f (map [:a a:] id xs)
--          = map [:a b:] f xs)
--      map
--      [ [Int], [Int], (\x -> f), (map [:Int Int:] id [1,2,3]) ]
--
--      env
--
--      emptySubstInfo
--   ==>
--     Just ({a |-> Int, b |-> Int, f |-> (\x -> f), xs |-> [1,2,3] }, [])
--
--   However if we had passed a substitution such as {a |-> Float} instead of emptySubstInfo,
--      it would not have matched.
--
--   The environment is used for 'hole' rules, that can look up bound definitions
--   and match if inlining would let them, even when inlining won't occur.
--
rewriteSubst
        :: (Show n, Show a, Ord n, Pretty n)
        => RewriteRule a n          -- ^ Rule
        -> Exp a n                  -- ^ Bottom-most (function) of expression
        -> [(Exp a n,a)]            -- ^ Arguments applied to function, with XApp's annotation
        -> RE.RewriteEnv a n        -- ^ Environment to rewrite in: witnesses for constraints,
        -> RM.SubstInfo  a n        -- ^ Existing substitution to match with
        -> Maybe (RM.SubstInfo a n, [(Exp a n, a)])
                                -- ^ Substitution map and remaining (unmatched) args
rewriteSubst
    RewriteRule
        { ruleBinds     = binds
        , ruleLeft      = lhs
        , ruleLeftHole  = Nothing
        , ruleFreeVars  = free}
    f args env sub
 = do   
        -- Check that none of the free variables have been rebound.
        when (any (flip RE.hasDef env) free)
         $ Nothing

        -- Get names of all the variables bound by the rule.
        --  This should always match because checked rules are guaranteed not to
        --  have `BAnon` or `BNone` binders.
        let Just vs 
                = liftM Set.fromList
                $ sequence 
                $ map T.takeNameOfBind
                $ map snd binds

        l:ls    <- return $ X.takeXAppsAsList lhs
        sub'    <- RM.match sub vs l f

        -- Match each left-hand rule against the arguments
        let go m [] rest
             = do return $ (m, rest)

            go m (l':ls') ((r,_):rs)
             = do m' <- RM.match m vs l' r
                  go m' ls' rs

            go _ _ _ 
             = Nothing

        go sub' ls args


-- Find substitution for rules with a 'hole'.
-- An example rule with a holes is:
--      RULE (i : Int). unbox {box i} = i
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
        Just d' -> let (fd,ad) = X.takeXAppsWithAnnots d' in
                   -- Merge the outer substitution with one for the hole
                   case rewriteSubst rule_hole fd ad ws sub' of
                   Just (subd,asd) -> Just (subd,asd ++ as)
                   Nothing         -> Nothing
        -- No definition in environment
        Nothing -> Nothing
      -- rule_some didn't match properly: failure
      _ -> Nothing
 where
  lhs_full  = XApp undefined (ruleLeft rule) hole
  rule_full = rule
              { ruleLeft        = lhs_full
              , ruleLeftHole    = Nothing }

  rule_some = rule
              { ruleLeft        = ruleLeft rule
              , ruleLeftHole    = Nothing }

  rule_hole = rule
              { ruleLeft        = hole
              , ruleLeftHole    = Nothing }


lookups :: Ord n
        => [Bind n]
        -> (Map n (Exp a n), Map n (Type n))
        -> [(Bind n, Exp a n)]

lookups bs m
 = let  bas  = Maybe.catMaybes $ map (lookupX m) bs
   in   map (\(b,a) -> (A.anonymizeX b, A.anonymizeX a)) bas
   


lookupX (xs,_) b@(BName n _)
 | Just x <- Map.lookup n xs
 = Just (b,x)

lookupX (_,tys) b@(BName n _)
 | Just t <- Map.lookup n tys
 = Just (b,XType t)

lookupX _ _ = Nothing


