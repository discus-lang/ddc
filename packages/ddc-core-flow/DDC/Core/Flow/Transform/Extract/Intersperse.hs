
module DDC.Core.Flow.Transform.Extract.Intersperse
        (intersperseStmts)
where
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Exp
import DDC.Core.Collect
import DDC.Type.Env

import qualified Data.Set as Set

-- | Given two lists of lets, order them so that it 'makes sense',
-- ...
intersperseStmts :: [Lets () Name] -> [Lets () Name] -> [Lets () Name]
intersperseStmts ls rs
 = let bls = map takeSubstBoundsOfBinds $ map valwitBindsOfLets ls
       brs = map takeSubstBoundsOfBinds $ map valwitBindsOfLets rs
   in  intersperse' ls bls rs brs


intersperse' :: [Lets () Name] -> [[Bound Name]]
             -> [Lets () Name] -> [[Bound Name]]
             -> [Lets () Name]

-- If one is empty, just return the other one
intersperse' ls _bls [] _brs
 = ls
intersperse' [] _bls rs _brs
 = rs

-- Both non-empty
intersperse' (l:ls) bls (r:rs) brs
 | f        <- freeXLets l
 -- Check if any of the free variables in l are bound later on in rs.
 -- If so, take one from rs instead.
 -- We could save time by taking multiple from rs, but that's not important right now.
 , any (flip Set.member f) (concat brs)
 = r : intersperse' (l:ls) bls rs (tail brs)
 -- Otherwise, take one from ls
 | otherwise
 = l : intersperse' ls (tail bls) (r:rs) brs


freeXLets :: Lets () Name -> Set.Set (Bound Name)
freeXLets ll
 -- Cheating because there's no BindStruct Lets instance!
 = freeX empty (XLet () ll (XCon () (dcBool True)))
