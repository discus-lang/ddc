
-- | Common simplifier recipies that combine multiple transforms.
module DDC.Core.Simplifier.Recipie
        ( anormalize
	, rewriteSimp)
where
import DDC.Core.Simplifier.Base
import DDC.Core.Transform.Namify
import DDC.Type.Env
import Data.Monoid

import DDC.Core.Transform.Rewrite.Rule (RewriteRule)

-- | Conversion to administrative normal-form.
anormalize 
        :: (Env n -> Namifier s n)
        -> (Env n -> Namifier s n)
        -> Simplifier s a n

anormalize namK namT
        =  Trans Snip 
        <> Trans Flatten 
        <> Trans (Namify namK namT)

-- | Intersperse rewrites and beta reduction
rewriteSimp
        :: [RewriteRule a n]
	-> Simplifier s a n

rewriteSimp rules
        = let r = Trans $ Rewrite rules
	      b = Trans Beta
	  in
	  r <> b <> r <> b <> r <> b



