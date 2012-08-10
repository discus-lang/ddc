
-- | Rewriting one expression to another
module DDC.Core.Transform.Rewrite.Rule 
        ( RewriteRule   (..)
	, BindMode     (..)
	, checkRewriteRule
	, mkRewriteRule)
where
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Core.Pretty()
import DDC.Type.Pretty()
import DDC.Core.Transform.Rewrite.Error
import qualified DDC.Core.Check.CheckExp        as C
import qualified DDC.Core.Collect               as C
import qualified DDC.Core.Transform.SpreadX     as S
import qualified DDC.Type.Check                 as T
import qualified DDC.Type.Compounds             as T
import qualified DDC.Type.Env                   as T
import qualified DDC.Type.Equiv                 as T
import qualified DDC.Type.Subsumes              as T
import qualified DDC.Type.Transform.SpreadT     as S

import qualified Data.Maybe		as Maybe
import qualified Data.Set               as Set

-- | A substitution rule
-- rhs should have same or weaker type as lhs
--
-- TODO want to split into dumb @RewriteRule@ that parser needs
-- and then merge into nicer @RewriteRuleSet@ with index, effect etc
-- 
-- TODO list of binds might need to be list of *groups* of binds...
-- or perhaps we just need them to be named:
--	[^:%] (x y : Int ^0)
-- no.. should be fine....
data RewriteRule a n
        = RewriteRule
	    [(BindMode,Bind n)]	--  bindings & their types
	    [Type n]		--  requirements for rules to fire
	    (Exp a n)		--  left-hand side to match on
	    (Exp a n)		--  replacement
	    (Maybe (Effect n))	--  effect of lhs if needs weaken
	    (Maybe (Closure n)) --  closure of lhs if needs weaken
        deriving (Eq, Show)

data BindMode = BMKind | BMType
    deriving (Eq, Show)

mkRewriteRule :: Ord n => [(BindMode,Bind n)] -> [Type n] ->
	         Exp a n -> Exp a n -> RewriteRule a n
mkRewriteRule bs cs lhs rhs
 = RewriteRule bs cs lhs rhs Nothing Nothing


instance (Pretty n, Eq n) => Pretty (RewriteRule a n) where
 ppr (RewriteRule bs cs lhs rhs _ _)
  = pprBinders bs <> pprConstrs cs <> ppr lhs <> text " = " <> ppr rhs
  where
      pprBinders []	= text ""
      pprBinders bs'	= foldl1 (<>) (map pprBinder bs') <> text ". "
      pprBinder (BMKind,b) = text "[" <> ppr b <> text "] "
      pprBinder (BMType,b) = text "(" <> ppr b <> text ") "
      
      pprConstrs []	= text ""
      pprConstrs (c:cs')= ppr c <> text " => " <> pprConstrs cs'


-- | Create rule
-- Make sure expressions are valid, lhs is only allowed to contain XApps
-- TODO return multiple errors?
checkRewriteRule
    :: (Ord n, Show n, Pretty n)        
    => C.Config n               -- ^ Type checker config.
    -> T.Env n                  -- ^ Kind environment.
    -> T.Env n                  -- ^ Type environment.
    -> RewriteRule a n	        -- ^ Rule to check
    -> Either (Error a n)
	      (RewriteRule (C.AnTEC a n) n)

checkRewriteRule config kenv tenv
    (RewriteRule bs cs lhs rhs _ _)
 = do	let (kenv',tenv') = extendBinds bs kenv tenv
	let cs' = map (S.spreadT kenv') cs
	mapM_ (\c -> checkTy config kenv' c ErrorBadConstraint) cs'

	(lhs',tl,el,cl) <- check config kenv' tenv' lhs (ErrorTypeCheck Lhs)
	(rhs',tr,er,cr) <- check config kenv' tenv' rhs (ErrorTypeCheck Rhs)

	let err = ErrorTypeConflict (tl,el,cl) (tr,er,cr)

	equiv tl tr err
	e      <- weaken T.kEffect el er err
	c      <- weaken T.kClosure cl cr err

	checkUnmentionedBinders bs lhs
	checkAnonymousBinders bs
	checkValidPattern lhs

	return $ RewriteRule 
                        bs cs' 
                        lhs' rhs'
                        e c


extendBinds 
        :: Ord n 
        => [(BindMode, Bind n)] 
        -> T.Env n 
        -> T.Env n 
        -> (T.Env n, T.Env n)

extendBinds [] kenv tenv = (kenv,tenv)
extendBinds ((bm,b):bs) ke te
 = let b' = S.spreadX ke te b in
   let (ke',te') =
	case bm of
	BMKind -> (T.extend b' ke, te)
	BMType -> (ke, T.extend b' te)
    in
   extendBinds bs ke' te'

check defs kenv tenv xx onError
 = let xx' = S.spreadX kenv tenv xx in
   case C.checkExp defs kenv tenv xx' of
   Left err -> Left $ onError xx' err
   Right rhs -> return rhs

checkTy defs kenv xx onError
 = case T.checkType (C.configDataDefs defs) kenv xx of
   Left _err -> Left $ onError xx
   Right rhs -> return rhs


equiv l r _ | T.equivT l r
 = return ()
equiv _ _ onError
 = Left onError

weaken _ l r _ | T.equivT l r
 = return Nothing
weaken k l r _ | T.subsumesT k l r
 = return $ Just l
weaken _ _ _ onError
 = Left onError

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
checkAnonymousBinders bs | (b:_) <- filter anony $ map snd bs
 = Left $ ErrorAnonymousBinder b
 where
    anony (BAnon _) = True
    anony _		= False

checkAnonymousBinders _
 = return ()


checkValidPattern :: Exp a n -> Either (Error a n) ()
checkValidPattern expr = go expr
 where
    go (XVar _ _) = return ()
    go (XCon _ _) = return ()
    go x@(XLAM _ _ _) = Left $ ErrorNotFirstOrder x
    go x@(XLam _ _ _) = Left $ ErrorNotFirstOrder x
    go (XApp _ l r) = go l >> go r
    go x@(XLet _ _ _) = Left $ ErrorNotFirstOrder x
    go x@(XCase _ _ _)= Left $ ErrorNotFirstOrder x
    go (XCast _ _ x)= go x
    go (XType t)	= go_t t
    go (XWitness _) = return ()

    go_t (TVar _)   = return ()
    go_t (TCon _)	= return ()
    go_t t@(TForall _ _) = Left $ ErrorNotFirstOrder (XType t)
    go_t (TApp l r)	= go_t l >> go_t r
    go_t (TSum _)	= return ()



{-
-- | Check if expression is valid as a rule left-hand side
-- (Only simple applications, no binders)
isLhsOk :: Ord n => Exp a n -> Bool
-- Simple expressions are OK
isLhsOk (XVar _ _) = True
isLhsOk (XCon _ _) = True
isLhsOk (XType _) = True
isLhsOk (XWitness _) = True

-- Casts are OK if their expression is
isLhsOk (XCast _ _ x) = isLhsOk x
-- Applications are OK if both sides are
isLhsOk (XApp _ l r) = isLhsOk l && isLhsOk r

-- Anything else is rubbish
isLhsOk _ = False

-}

