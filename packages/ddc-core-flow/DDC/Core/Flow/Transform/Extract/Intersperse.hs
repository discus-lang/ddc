
module DDC.Core.Flow.Transform.Extract.Intersperse
        (intersperseStmts)
where
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import DDC.Core.Collect
import DDC.Core.Transform.Annotate
import DDC.Type.Env
import Data.List (partition, (\\))
import qualified Data.Set as Set


-- | Given two lists of lets, order them so that any variables are bound before use.
intersperseStmts :: [LetsF] -> [LetsF] -> [LetsF]
intersperseStmts ls rs
 = let bls = nubbish $ map takeSubstBoundsOfBinds $ map valwitBindsOfLets ls
       brs = nubbish $ map takeSubstBoundsOfBinds $ map valwitBindsOfLets rs
   in  intersperse' (ls `zip` bls ++ rs `zip` brs)


-- Because a name might be bound a couple of times 
-- (see extractStmtEnd:EndVecSlice)
-- ignore the later times... HACK
nubbish :: [[Bound Name]] -> [[Bound Name]]
nubbish bs' = go bs' []
 where
  go [] _        = []
  go (b:bs) accs = (b \\ accs) : go bs (accs ++ b)


intersperse' 
        :: [(Lets () Name, [Bound Name])]
        -> [Lets () Name]

intersperse' []
 = []

intersperse' ((x,b):bxs)
 | f        <- freeXLets x
 -- Check if any of the free variables in x are bound later on.
 -- If so, defer this binding...
 -- HACK this might not terminate
 , (r:rs,os)  <- partition (any (flip Set.member f) . snd) bxs
 = intersperse' (r : rs ++ (x,b) : os)

 -- Otherwise it's a valid binding
 | otherwise
 = x : intersperse' bxs


freeXLets :: LetsF -> Set.Set (Bound Name)
freeXLets ll
 -- Cheating because there's no BindStruct Lets instance!
 = freeX empty $ annotate () (XLet ll (XCon (dcBool True)))
