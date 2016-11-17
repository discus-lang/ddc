
module DDC.Core.Salt.Transform.Slotify.Inject
        (injectX)
where
import DDC.Core.Exp.Annot
import Data.Map                         (Map)
import qualified DDC.Core.Salt          as A
import qualified Data.Map               as Map


---------------------------------------------------------------------------------------------------
-- Inject a code transformation just after a name is bound

injectX :: Map A.Name (Exp a A.Name -> Exp a A.Name)
        -> Exp a A.Name
        -> Exp a A.Name

injectX injs xx
 = case xx of
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM  a b x     -> XLAM  a b   (injectX injs x) -- Should we error? Salt
        XLam  a b x     -> XLam  a b   (injectX injs x) -- doesn't have lambdas.
        XApp  a x1 x2   -> XApp  a     (injectX injs x1)          (injectX injs x2)
        XLet  a lts x   -> XLet  a lts (injectionsOfLets injs lts (injectX injs x))
        XCase a x alts  -> XCase a     (injectX injs x)      (map (injectA injs) alts)
        XCast a c x     -> XCast a c   (injectX injs x)
        XType{}         -> xx
        XWitness{}      -> xx


injectA :: Map A.Name (Exp a A.Name -> Exp a A.Name)
        -> Alt a A.Name
        -> Alt a A.Name

injectA injs (AAlt pp xx)
 = AAlt pp (injectionsOfPat injs pp (injectX injs xx))


---------------------------------------------------------------------------------------------------
-- Construct the transformation to inject, given a set of names

injectionsOfLets  :: Map A.Name (exp -> exp) -> Lets a A.Name -> exp -> exp
injectionsOfLets injs lts = injectionsOfBinds injs (valwitBindsOfLets lts)

injectionsOfPat   :: Map A.Name (exp -> exp) -> Pat A.Name -> exp -> exp
injectionsOfPat injs pp = injectionsOfBinds injs (bindsOfPat pp)

injectionsOfBinds :: Map A.Name (exp -> exp) -> [Bind A.Name] -> exp -> exp
injectionsOfBinds injs binds
 = let
        names   = Map.fromList [(n, ()) | BName n _ <- binds]
        matches = injs `Map.intersection` names
   in
        Map.foldr (.) id matches


