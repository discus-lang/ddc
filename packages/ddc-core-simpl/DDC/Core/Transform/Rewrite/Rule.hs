-- | Constructing and checking whether rewrite rules are valid
module DDC.Core.Transform.Rewrite.Rule 
        ( RewriteRule   (..)
        , NamedRewriteRule

          -- * Binding modes
        , BindMode      (..)
        , isBMKind
        , isBMType

          -- * Construction
        , mkRewriteRule
        , checkRewriteRule)
where
import DDC.Core.Transform.Rewrite.Error
import DDC.Core.Exp
import DDC.Core.Pretty                          ()
import DDC.Core.Collect
import DDC.Core.Compounds
import DDC.Type.Pretty                          ()
import DDC.Type.Env                             (KindEnv, TypeEnv)
import DDC.Base.Pretty
import Control.Monad
import qualified DDC.Core.Analysis.Usage        as U
import qualified DDC.Core.Check                 as C
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


-- | A rewrite rule. For example:
--
--   @ RULE [r1 r2 r3 : %] (x : Int r1)
--      . addInt  [:r1 r2 r3:] x (0 [r2] ()
--      = copyInt [:r1 r3:]    x
--   @
data RewriteRule a n
        = RewriteRule
        { -- | Variables bound by the rule.
          ruleBinds       :: [(BindMode, Bind n)]

          -- | Extra constraints on the rule.
          --   These must all be satisfied for the rule to fire.
        , ruleConstraints :: [Type n]           

          -- | Left-hand side of the rule.
          --   We match on this part.
        , ruleLeft        :: Exp a n            

          -- | Extra part of left-hand side,
          --   but allow this bit to be out-of-context.
        , ruleLeftHole    :: Maybe (Exp a n)    

          -- | Right-hand side of the rule.
          --   We replace the matched expression with this part.
        , ruleRight       :: Exp a n            

          -- | Effects that are caused by the left but not the right.
          --   When applying the rule we add an effect weakning to ensure
          --   the rewritten expression has the same effects.
        , ruleWeakEff     :: Maybe (Effect n)   

          -- | Closure that the left has that is not present in the right.
          --   When applying the rule we add a closure weakening to ensure
          --   the rewritten expression has the same closure.
        , ruleWeakClo     :: [Exp a n]          

          -- | References to environment. 
          --   Used to check whether the rule is shadowed.
        , ruleFreeVars    :: [Bound n]          
        } deriving (Eq, Show)


type NamedRewriteRule a n
        = (String, RewriteRule a n)


instance (Pretty n, Eq n) => Pretty (RewriteRule a n) where
 ppr (RewriteRule bs cs lhs hole rhs _ _ _)
  = pprBinders bs <> pprConstrs cs <> ppr lhs <> pprHole <> text " = " <> ppr rhs
  where pprBinders []          = text ""
        pprBinders bs'         = foldl1 (<>) (map pprBinder bs') <> text ". "

        pprBinder (BMKind,b)   = text "[" <> ppr b <> text "] "
        pprBinder (BMType _,b) = text "(" <> ppr b <> text ") "
      
        pprConstrs []          = text ""
        pprConstrs (c:cs')     = ppr c <> text " => " <> pprConstrs cs'

        pprHole
         | Just h <- hole
         = text " {" <> ppr h <> text "}"

         | otherwise
         = text ""


-- BindMode -------------------------------------------------------------------
-- | Binding level for the binders in a rewrite rule.
data BindMode 
        = BMKind 
        | BMType Int -- ^ number of usages
        deriving (Eq, Show)


-- | Check if a `BindMode` is a `BMKind`.
isBMKind :: BindMode -> Bool
isBMKind BMKind         = True
isBMKind _              = False


-- | Check if a `BindMode` is a `BMType`.
isBMType :: BindMode -> Bool
isBMType (BMType _)     = True
isBMType _              = False


-- Make -----------------------------------------------------------------------
-- | Construct a rewrite rule, but do not check if it's valid.
--
--   You then need to apply 'checkRewriteRule' to check it.
--
mkRewriteRule
        :: Ord n
        => [(BindMode,Bind n)]  -- ^ Variables bound by the rule.
        -> [Type n]             -- ^ Extra constraints on the rule.
        -> Exp a n              -- ^ Left-hand side of the rule.
        -> Maybe (Exp a n)      -- ^ Extra part of left, can be out of context.
        -> Exp a n              -- ^ Right-hand side (replacement)
        -> RewriteRule a n

mkRewriteRule  bs cs lhs hole rhs
 = RewriteRule bs cs lhs hole rhs Nothing [] []


-- Check ----------------------------------------------------------------------
-- | Take a rule, make sure it's valid and fill in type, closure and effect
--   information.
--
--   The left-hand side of rule can't have any binders (lambdas, lets etc).
--
--   All binders must appear in the left-hand side, otherwise they would match
--   with no value.
--
--   Both sides must have the same types, but the right can have fewer effects
--   and smaller closure.
--
--   We don't handle anonymous binders on either the left or right.
--
checkRewriteRule
    :: (Ord n, Show n, Pretty n)        
    => C.Config n               -- ^ Type checker config.
    -> T.Env n                  -- ^ Kind environment.
    -> T.Env n                  -- ^ Type environment.
    -> RewriteRule a n          -- ^ Rule to check
    -> Either (Error a n)
              (RewriteRule (C.AnTEC a n) n)

checkRewriteRule config kenv tenv
        (RewriteRule bs cs lhs hole rhs _ _ _)
 = do   
        -- Extend the environments with variables bound by the rule.
        let (kenv', tenv', bs')  = extendBinds bs kenv tenv
        let csSpread             = map (S.spreadT kenv') cs

        -- Check that all constraints are valid types.
        mapM_ (checkConstraint config kenv') csSpread

        -- Typecheck, spread and annotate with type information
        (lhs', _, _, _)
                <- checkExp config kenv' tenv' Lhs lhs 

        -- If the extra left part is there, typecheck and annotate it.
        hole' <- case hole of
                  Just h  
                   -> do  (h',_,_,_)  <- checkExp config kenv' tenv' Lhs h 
                          return $ Just h'

                  Nothing -> return Nothing

        -- Build application from lhs and the hole so we can check its
        -- type against rhs
        let lhs_full = maybe lhs (XApp undefined lhs) hole

        -- Check the full left hand side.
        (lhs_full', tLeft, effLeft, cloLeft)
                <- checkExp config kenv' tenv' Lhs lhs_full

        -- Check the full right hand side.
        (rhs', tRight, effRight, cloRight)
                <- checkExp config kenv' tenv' Rhs rhs 

        -- Check that types of both sides are equivalent.
        let err = ErrorTypeConflict 
                        (tLeft,  effLeft,  cloLeft) 
                        (tRight, effRight, cloRight)

        checkEquiv tLeft tRight err

        -- Check the effect of the right is smaller than that 
        -- of the left, and add a weakeff cast if nessesary
        effWeak        <- makeEffectWeakening  T.kEffect effLeft effRight err

        -- Check that the closure of the right is smaller than that
        -- of the left, and add a weakclo cast if nessesary.
        cloWeak        <- makeClosureWeakening lhs_full' rhs'

        -- Check that all the bound variables are mentioned
        -- in the left-hand side.
        checkUnmentionedBinders bs' lhs_full'

        -- No BAnons allowed.
        --  We don't handle deBruijn binders.
        checkAnonymousBinders bs'

        -- No lets or lambdas in left-hand side.
        --  We can't match against these.
        checkValidPattern lhs_full

        -- Count how many times each binder is used in the right-hand side.
        bs''    <- countBinderUsage bs' rhs

        -- Get the free variables of the rule.
        let binds     = Set.fromList
                      $ Maybe.catMaybes
                      $ map (T.takeSubstBoundOfBind . snd) bs

        let freeVars  = Set.toList
                      $ (C.freeX T.empty lhs_full' 
                         `Set.union` C.freeX T.empty rhs)
                      `Set.difference` binds

        return  $ RewriteRule 
                        bs'' csSpread
                        lhs' hole' rhs'
                        effWeak cloWeak
                        freeVars


-- | Extend kind and type environments with a rule's binders.
--   Which environment a binder goes into depends on its BindMode.
--   Also return list of binders which have been spread.
extendBinds 
        :: Ord n 
        => [(BindMode, Bind n)] 
        -> KindEnv n -> TypeEnv n 
        -> (T.KindEnv n, T.TypeEnv n, [(BindMode, Bind n)])

extendBinds binds kenv tenv
 = go binds kenv tenv []
 where
        go []          k t acc
         = (k,t,acc)

        go ((bm,b):bs) k t acc
         = let  b'      = S.spreadX k t b
                (k',t') = case bm of
                             BMKind   -> (T.extend b' k, t)
                             BMType _ -> (k, T.extend b' t)

           in  go bs k' t' (acc ++ [(bm,b')])


-- | Type check the expression on one side of the rule.
checkExp 
        :: (Ord n, Show n, Pretty n)
        => C.Config n 
        -> KindEnv n    -- ^ Kind environment of expression.
        -> TypeEnv n    -- ^ Type environment of expression.
        -> Side         -- ^ Side that the expression appears on for errors.
        -> Exp a n      -- ^ Expression to check.
        -> Either (Error a n) 
                  (Exp (C.AnTEC a n) n, Type n, Effect n, Closure n)

checkExp defs kenv tenv side xx
 = let xx' = S.spreadX kenv tenv xx 
   in  case C.checkExp defs kenv tenv xx' of
        Left err  -> Left $ ErrorTypeCheck side xx' err
        Right rhs -> return rhs


-- | Type check a constraint on the rule.
checkConstraint
        :: (Ord n, Show n, Pretty n)
        => C.Config n
        -> KindEnv n    -- ^ Kind environment of the constraint.
        -> Type n       -- ^ The constraint type to check.
        -> Either (Error a n) (Kind n)

checkConstraint defs kenv tt
 = case T.checkType (C.configPrimDataDefs defs) kenv tt of
        Left _err               -> Left $ ErrorBadConstraint tt
        Right k
         | T.isWitnessType tt   -> return k
         | otherwise            -> Left $ ErrorBadConstraint tt


-- | Check equivalence of types or error
checkEquiv
        :: Ord n
        => Type n       -- ^ Type of left of rule.
        -> Type n       -- ^ Type of right of rule.
        -> Error a n    -- ^ Error to report if the types don't match.
        -> Either (Error a n) ()

checkEquiv tLeft tRight err
        | T.equivT tLeft tRight  = return ()
        | otherwise              = Left err


-- Weaken ---------------------------------------------------------------------
-- | Make the effect weakening for a rule.
--   This contains the effects that are caused by the left of the rule
--   but not the right. 
--   If the right has more effects than the left then return an error.
--
makeEffectWeakening
        :: (Ord n, Show n)
        => Kind n       -- ^ Should be the effect kind.
        -> Effect n     -- ^ Effect of the left of the rule.
        -> Effect n     -- ^ Effect of the right of the rule.
        -> Error a n    -- ^ Error to report if the right is bigger.
        -> Either (Error a n) (Maybe (Type n))

makeEffectWeakening k effLeft effRight onError
        -- When the effect of the left matches that of the right
        -- then we don't have to do anything else.
        | T.equivT effLeft effRight
        = return Nothing

        -- When the effect of the right is smaller than that of
        -- the left then we need to wrap it in an effect weaking
        -- so the rewritten expression retains its original effect.
        | T.subsumesT k effLeft effRight
        = return $ Just effLeft

        -- When the effect of the right is more than that of the left
        -- then this is an error. The rewritten expression can't have
        -- can't have more effects than the source.
        | otherwise
        = Left onError


-- | Make the closure weakening for a rule.
--   This contains a closure term for all variables that are present
--   in the left of a rule but not in the right.
--
makeClosureWeakening 
        :: (Ord n, Pretty n)
        => Exp (C.AnTEC a n) n      -- ^ Expression on the left of the rule.
        -> Exp (C.AnTEC a n) n      -- ^ Expression on the right of the rule.
        -> Either (Error a n) 
                  [Exp (C.AnTEC a n) n]

makeClosureWeakening lhs rhs
 = let  supportLeft  = support Env.empty Env.empty lhs
        daLeft  = supportDaVar supportLeft
        wiLeft  = supportWiVar supportLeft
        spLeft  = supportSpVar supportLeft

        supportRight = support Env.empty Env.empty rhs
        daRight = supportDaVar supportRight
        wiRight = supportWiVar supportRight
        spRight = supportSpVar supportRight

        Just a  = takeAnnotOfExp lhs

   in   Right 
         $  [XVar a u 
                | u <- Set.toList $ daLeft `Set.difference` daRight ]

         ++ [XWitness (WVar u)
                | u <- Set.toList $ wiLeft `Set.difference` wiRight ]

         ++ [XType (TVar u)
                | u <- Set.toList $ spLeft `Set.difference` spRight ]


-- Structural Checks ----------------------------------------------------------
-- | Check for rule variables that have no uses.
checkUnmentionedBinders
        :: (Ord n, Show n)
        => [(BindMode, Bind n)]
        -> Exp (C.AnTEC a n) n
        -> Either (Error a n) ()

checkUnmentionedBinders bs expr
 = let  used  = C.freeX T.empty expr `Set.union` C.freeT T.empty expr

        binds = Set.fromList
              $ Maybe.catMaybes
              $ map (T.takeSubstBoundOfBind . snd) bs

   in   if binds `Set.isSubsetOf` used
         then return ()
         else Left ErrorVarUnmentioned


-- | Check for anonymous binders in the rule. We don't handle these.
checkAnonymousBinders 
        :: [(BindMode, Bind n)] 
        -> Either (Error a n) ()

checkAnonymousBinders bs
        | (b:_) <- filter T.isBAnon $ map snd bs
        = Left $ ErrorAnonymousBinder b

        | otherwise
        = return ()


-- | Check whether the form of the left-hand side of the rule is valid
--   we can only match against nested applications, and not general
--   expressions containing let-bindings and the like.
checkValidPattern :: Exp a n -> Either (Error a n) ()
checkValidPattern expr
 = go expr
 where  go (XVar _ _)           = return ()
        go (XCon _ _)           = return ()
        go x@(XLAM _ _ _)       = Left $ ErrorNotFirstOrder x
        go x@(XLam _ _ _)       = Left $ ErrorNotFirstOrder x
        go (XApp _ l r)         = go l >> go r
        go x@(XLet _ _ _)       = Left $ ErrorNotFirstOrder x
        go x@(XCase _ _ _)      = Left $ ErrorNotFirstOrder x
        go (XCast _ _ x)        = go x
        go (XType t)            = go_t t
        go (XWitness _)         = return ()

        go_t (TVar _)           = return ()
        go_t (TCon _)           = return ()
        go_t t@(TForall _ _)    = Left $ ErrorNotFirstOrder (XType t)
        go_t (TApp l r)         = go_t l >> go_t r
        go_t (TSum _)           = return ()


-- | Count how many times each binder is used in right-hand side.
countBinderUsage 
        :: Ord n 
        => [(BindMode, Bind n)] 
        -> Exp a n 
        -> Either (Error a n) [(BindMode, Bind n)]

countBinderUsage bs x
 = let  Just (U.UsedMap um)
                = liftM fst $ takeAnnotOfExp $ U.usageX x

        get (BMType _, BName n t)
         = (BMType
                $ length
                $ Maybe.fromMaybe []
                $ Map.lookup n um
           , BName n t)

        get b
         = b

   in   return $ map get bs
