-- | Constructing and checking whether rewrite rules are valid
module DDC.Core.Transform.Rewrite.Rule 
        ( RewriteRule   (..)
	, BindMode     (..)
	, isBMKind, isBMType
	, checkRewriteRule
	, mkRewriteRule)
where
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Core.Pretty                          ()
import DDC.Type.Pretty                          ()
import DDC.Core.Transform.Rewrite.Error
import DDC.Core.Collect.Support
import qualified DDC.Core.Analysis.Usage	as U
import qualified DDC.Core.Check.CheckExp        as C
import qualified DDC.Core.Collect               as C
import qualified DDC.Core.Transform.SpreadX     as S
import qualified DDC.Type.Check                 as T
import qualified DDC.Type.Compounds             as T
import qualified DDC.Type.Env                   as T
import qualified DDC.Type.Equiv                 as T
import qualified DDC.Type.Predicates            as T
import qualified DDC.Type.Subsumes              as T
import qualified DDC.Type.Transform.SpreadT     as S
import qualified Data.Map                       as Map
import qualified Data.Maybe                     as Maybe
import qualified Data.Set                       as Set
import qualified DDC.Type.Env                   as Env

-- | A rewrite rule
--
--   RULE [r1 r2 r3 : %] (x : Int r1).
--	addInt  [:r1 r2 r3:] x (0 [r2] ()
--    = copyInt [:r1 r3:]    x
--
data RewriteRule a n
        = RewriteRule
	{ ruleBinds	  :: [(BindMode,Bind n)]--  ^ Bindings & their types
	, ruleConstraints :: [Type n]		--  ^ Requirements for rules to fire
	, ruleLeft	  :: Exp a n		--  ^ Left-hand side to match on
	, ruleLeftHole	  :: Maybe (Exp a n)	--  ^ Extra part of left-hand side,
						--    but allow this bit to be out-of-context
	, ruleRight	  :: Exp a n		--  ^ Replacement
	, ruleWeakEff	  :: Maybe (Effect n)	--  ^ Effect of lhs if needs weaken
	, ruleWeakClo	  :: [Exp a n]		--  ^ Closure of lhs if needs weaken
	, ruleFreeVars	  :: [Bound n]		--  ^ References to environment:
						--    used to check if rule is shadowed.
	} deriving (Eq, Show)

data BindMode = BMKind | BMType Int -- ^ number of usages
    deriving (Eq, Show)

isBMKind :: BindMode -> Bool
isBMKind BMKind = True
isBMKind _	= False

isBMType :: BindMode -> Bool
isBMType (BMType _) = True
isBMType _	    = False

-- | Construct a rewrite rule, do not check if it's valid
mkRewriteRule
    :: Ord n
    => [(BindMode,Bind n)]	-- ^ Binders
    -> [Type n]			-- ^ Constraints
    -> Exp a n			-- ^ Left-hand side
    -> Maybe (Exp a n)		-- ^ Extra part of left, can be out of context
    -> Exp a n			-- ^ Right-hand side (replacement)
    -> RewriteRule a n
mkRewriteRule bs cs lhs hole rhs
 = RewriteRule bs cs lhs hole rhs Nothing [] []


instance (Pretty n, Eq n) => Pretty (RewriteRule a n) where
 ppr (RewriteRule bs cs lhs hole rhs _ _ _)
  = pprBinders bs <> pprConstrs cs <> ppr lhs <> pprHole <> text " = " <> ppr rhs
  where
      pprBinders []	= text ""
      pprBinders bs'	= foldl1 (<>) (map pprBinder bs') <> text ". "
      pprBinder (BMKind,b) = text "[" <> ppr b <> text "] "
      pprBinder (BMType _,b) = text "(" <> ppr b <> text ") "
      
      pprConstrs []	= text ""
      pprConstrs (c:cs')= ppr c <> text " => " <> pprConstrs cs'

      pprHole
        | Just h <- hole
	= text " {" <> ppr h <> text "}"
	| otherwise
	= text ""


-- | Take a rule, make sure it's valid and fill in type, closure and effect information.
--   The left-hand side of rule can't have any binders (lambdas, lets etc).
--   All binders must appear in the left-hand side, otherwise they would match with no value.
--   Both sides must have the same types, but the right can have fewer effects and smaller closure.
--   Anonymous binders aren't allowed (for no real reason other than laziness)
checkRewriteRule
    :: (Ord n, Show n, Pretty n)        
    => C.Config n               -- ^ Type checker config.
    -> T.Env n                  -- ^ Kind environment.
    -> T.Env n                  -- ^ Type environment.
    -> RewriteRule a n	        -- ^ Rule to check
    -> Either (Error a n)
	      (RewriteRule (C.AnTEC a n) n)

checkRewriteRule config kenv tenv
    (RewriteRule bs cs lhs hole rhs _ _ _)
 = do	let (kenv',tenv',bs') = extendBinds bs kenv tenv
	let cs' = map (S.spreadT kenv') cs
	-- Check that all constraints are valid types.
	mapM_ (\c -> checkConstraint config kenv' c ErrorBadConstraint) cs'

	-- Typecheck, spread and annotate with type information
	(lhs',_,_,_)    <- check config kenv' tenv' lhs (ErrorTypeCheck Lhs)

	-- If the extra left part is there, typecheck and annotate it
	hole' <- case hole of
	         Just h  -> do
		    (h',_,_,_) <- check config kenv' tenv' h (ErrorTypeCheck Lhs)
		    return $ Just h'
	         Nothing -> return Nothing

	-- Build application from lhs and the hole so we can check its type against rhs
	let lhs_full = maybe lhs
		       (XApp undefined lhs)
		       hole
	(_   ,tl,el,cl) <- check config kenv' tenv' lhs_full (ErrorTypeCheck Lhs)

	(rhs',tr,er,cr) <- check config kenv' tenv' rhs	     (ErrorTypeCheck Rhs)

	let err = ErrorTypeConflict (tl,el,cl) (tr,er,cr)
	-- Check that types are equivalent, or error
	equiv tl tr err
	-- Error if right's effect is bigger, or add a weaken cast if necessary
	e <- weakEff T.kEffect el er err
	c <- weakClo bs lhs_full rhs

	-- Make sure all binders are in left-hand side
	checkUnmentionedBinders bs' lhs_full
	-- No BAnons allowed
	checkAnonymousBinders bs'
	-- No lets or lambdas in left-hand side
	checkValidPattern lhs_full

	-- Count how many times each binder is used in rhs
	bs'' <- countBinderUsage bs' rhs

	let binds = Set.fromList
		  $ Maybe.catMaybes
		  $ map (T.takeSubstBoundOfBind . snd) bs
	let freeVars = Set.toList
		     $ (C.freeX T.empty lhs_full `Set.union` C.freeX T.empty rhs)
		     `Set.difference` binds

	return $ RewriteRule 
                        bs'' cs' 
                        lhs' hole' rhs'
                        e c
			freeVars


-- | Extend kind and type environments with a rule's binders.
--   Which environment a binder goes into depends on its BindMode.
--   Also return list of binders which have been spread.
extendBinds 
        :: Ord n 
        => [(BindMode, Bind n)] 
        -> T.Env n 
        -> T.Env n 
        -> (T.Env n, T.Env n, [(BindMode, Bind n)])

extendBinds binds kenv tenv
 = go binds kenv tenv []
 where
   go []	  k t acc
     = (k,t,acc)
   go ((bm,b):bs) k t acc
     = let b'      = S.spreadX k t b
           (k',t') = case bm of
		     BMKind   -> (T.extend b' k, t)
		     BMType _ -> (k, T.extend b' t)
       in  go bs k' t' (acc ++ [(bm,b')])

-- | Typecheck an expression or return an error
check defs kenv tenv xx onError
 = let xx' = S.spreadX kenv tenv xx in
   case C.checkExp defs kenv tenv xx' of
   Left err -> Left $ onError xx' err
   Right rhs -> return rhs

-- | Typecheck a constraint or return an error
checkConstraint defs kenv xx onError
 = case T.checkType (C.configDataDefs defs) kenv xx of
   Left _err -> Left $ onError xx
   Right rhs ->
	if   T.isWitnessType xx
	then return rhs
	else Left $ onError xx


-- | Check equivalence of types or error
equiv l r _ | T.equivT l r
 = return ()
equiv _ _ onError
 = Left onError

-- | Determine whether weaken cast is necessary,
--   or error if right has more effects than left
weakEff _ l r _ | T.equivT l r
 = return Nothing
weakEff k l r _ | T.subsumesT k l r
 = return $ Just l
weakEff _ _ _ onError
 = Left onError


-- | Build the closure weakening for a rule.
--   This contains a closure term for all variables that are present
--   in the left of a rule but not in the right.
weakClo :: Ord n
        => [(BindMode, Bind n)]
        -> Exp a n -> Exp a n 
        -> Either (Error a n) [Exp (C.AnTEC a n) n]

weakClo _bs lhs rhs
 = let  supportLeft  = support Env.empty Env.empty lhs
        daLeft  = supportDaVar supportLeft
        wiLeft  = supportWiVar supportLeft
        spLeft  = supportSpVar supportLeft

        supportRight = support Env.empty Env.empty rhs
        daRight = supportDaVar supportRight
        wiRight = supportWiVar supportRight
        spRight = supportSpVar supportRight

   in   Right
         $  [XVar (error "weakClo annot") u 
                | u <- Set.toList $ daLeft `Set.difference` daRight ]

         ++ [XWitness (WVar u)
                | u <- Set.toList $ wiLeft `Set.difference` wiRight ]

         ++ [XType (TVar u)
                | u <- Set.toList $ spLeft `Set.difference` spRight ]



checkUnmentionedBinders
    :: (Ord n, Show n)
    => [(BindMode, Bind n)]
    -> Exp a n
    -> Either (Error a n) ()
checkUnmentionedBinders bs expr
 = let used  = C.freeX T.empty expr `Set.union` C.freeT T.empty expr
       binds = Set.fromList
	     $ Maybe.catMaybes
	     $ map (T.takeSubstBoundOfBind . snd) bs
   in if   binds `Set.isSubsetOf` used
      then return ()
      else Left ErrorVarUnmentioned


checkAnonymousBinders :: [(BindMode, Bind n)] -> Either (Error a n) ()
checkAnonymousBinders bs
 | (b:_) <- filter anony $ map snd bs
 = Left $ ErrorAnonymousBinder b
 | otherwise
 = return ()
 where
    anony (BAnon _) = True
    anony _	    = False


checkValidPattern :: Exp a n -> Either (Error a n) ()
checkValidPattern expr
 = go expr
 where
    go (XVar _ _)	= return ()
    go (XCon _ _)	= return ()
    go x@(XLAM _ _ _)	= Left $ ErrorNotFirstOrder x
    go x@(XLam _ _ _)	= Left $ ErrorNotFirstOrder x
    go (XApp _ l r)	= go l >> go r
    go x@(XLet _ _ _)	= Left $ ErrorNotFirstOrder x
    go x@(XCase _ _ _)  = Left $ ErrorNotFirstOrder x
    go (XCast _ _ x)	= go x
    go (XType t)	= go_t t
    go (XWitness _)	= return ()

    go_t (TVar _)	= return ()
    go_t (TCon _)	= return ()
    go_t t@(TForall _ _)= Left $ ErrorNotFirstOrder (XType t)
    go_t (TApp l r)	= go_t l >> go_t r
    go_t (TSum _)	= return ()

-- Count how many times each binder is used in rhs
countBinderUsage :: Ord n => [(BindMode, Bind n)] -> Exp a n -> Either (Error a n) [(BindMode, Bind n)]
countBinderUsage bs x
 = let	(U.UsedMap um,_)    = U.usageX x
   in	return $ map (get um) bs
 where
   get um (BMType _, BName n t)
     = (BMType
        $ length
        $ Maybe.fromMaybe []
        $ Map.lookup n um
       , BName n t)
   get _  b
     = b
