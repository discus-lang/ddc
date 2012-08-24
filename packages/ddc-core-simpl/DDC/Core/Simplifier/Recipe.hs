
-- | Common simplifier recipes that combine multiple transforms.
module DDC.Core.Simplifier.Recipe
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
        :: [(String,RewriteRule a n)]
	-> Simplifier s a n

rewriteSimp rules
 = let  rewrite = Trans $ Rewrite rules
        bubble  = Trans Bubble
        beta    = Trans Beta
   in   Fix 20 (rewrite <> bubble <> beta)



