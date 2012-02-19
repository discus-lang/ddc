
-- | Rewriting one expression to another
module DDC.Core.Transform.Rewrite.Rule 
        (RewriteRule(..)
	,mkRewriteRule
	,Constraint(..))

where
import DDC.Core.Exp
import DDC.Base.Pretty
import DDC.Core.Pretty()
import DDC.Type.Pretty()


-- | A substitution rule
-- rhs should have same or weaker type as lhs
data RewriteRule a n
        = RewriteRule
	    [Bind n]		-- ^ bindings & their types
	    [Constraint n]	-- ^ requirements for rules to fire
	    (Exp a n)		-- ^ left-hand side to match on
	    (Exp a n)		-- ^ replacement
        deriving (Eq, Show)

instance (Pretty n, Eq n) => Pretty (RewriteRule a n) where
 ppr (RewriteRule bs cs lhs rhs)
  = pprBinders bs <> pprConstrs cs <> ppr lhs <> text " = " <> ppr rhs
  where
      pprBinders []	= text ""
      pprBinders bs'	= ppr bs' <> text ". "
      
      pprConstrs []	= text ""
      pprConstrs (c:cs')= ppr c <> text " => " <> pprConstrs cs'


-- | Create rule
-- Make sure expressions are valid, lhs is only allowed to contain XApps
mkRewriteRule
    :: Ord n
    => [Bind n] -> [Constraint n] -> Exp a n -> Exp a n
    -> Maybe (RewriteRule a n)
mkRewriteRule bs cs lhs rhs | isLhsOk lhs
 = Just $ RewriteRule bs cs lhs rhs
mkRewriteRule _ _ _ _ = Nothing

-- | Constraints over rules,
-- this is probably dumb, would just a Type n on its own be better?
data Constraint n
	= Constraint
	    (Type n) -- ^ typeclass/thing
	    [Type n] -- ^ arguments
	deriving (Eq, Show)

instance (Pretty n, Eq n) => Pretty (Constraint n) where
 ppr (Constraint t ts)
  = ppr t <> ppr ts <> text "=>"


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


