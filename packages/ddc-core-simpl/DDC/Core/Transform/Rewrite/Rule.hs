
-- | Rewriting one expression to another
module DDC.Core.Transform.Rewrite.Rule 
        (RewriteRule(..)
	,BindMode(..)
	,checkRewriteRule
	,mkRewriteRule
	)

where
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Core.Pretty()
import DDC.Type.Pretty()

import DDC.Core.Transform.Rewrite.Error

import qualified DDC.Core.DataDef as C
import qualified DDC.Core.Check.CheckExp as C

import qualified DDC.Type.Compounds as T
import qualified DDC.Type.Env as T
import qualified DDC.Type.Equiv as T
import qualified DDC.Type.Subsumes as T

import qualified DDC.Core.Transform.SpreadX as S
import qualified DDC.Type.Transform.SpreadT as S


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
checkRewriteRule
    :: (Ord n, Pretty n)
    => C.DataDefs n           -- ^ Data type definitions.
    -> T.Env n                -- ^ Kind environment.
    -> T.Env n                -- ^ Type environment.
    -> RewriteRule a n	      -- ^ Rule to check
    -> Either (Error a n)
	      (RewriteRule a n)
checkRewriteRule defs kenv tenv
    (RewriteRule bs cs lhs rhs _ _)
 = do	let (kenv',tenv') = extendBinds bs kenv tenv
	let cs' = map (S.spreadT kenv') cs
	(lhs',tl,el,cl) <- check defs kenv' tenv' lhs ErrorTypeCheckLhs
	(rhs',tr,er,cr) <- check defs kenv' tenv' rhs ErrorTypeCheckRhs
	let err = ErrorTypeConflict (tl,el,cl) (tr,er,cr)
	equiv tl tr err
	e <- weaken T.kEffect el er err
	c <- weaken T.kClosure cl cr err
	return $ RewriteRule bs cs' lhs' rhs' e c

extendBinds :: Ord n => [(BindMode, Bind n)] ->
		T.Env n -> T.Env n ->
		(T.Env n, T.Env n)
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

