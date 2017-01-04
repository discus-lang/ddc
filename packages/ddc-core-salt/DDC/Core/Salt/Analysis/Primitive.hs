
-- | Collect information about the use of various primitives in a module.
module DDC.Core.Salt.Analysis.Primitive
        ( collectModule
        , Support (..))
where
import DDC.Core.Module                  as C
import DDC.Core.Exp.Annot               as C

import Data.Text                        (Text)
import Data.Set                         (Set)
import qualified DDC.Core.Salt.Name     as A
import qualified Data.Set               as Set


-------------------------------------------------------------------------------
--- | Support of some term.
data Support
        = Support
        { -- | Names of static globals mentioned using the global# primitive.
          supportGlobal :: Set Text
        }
        deriving Show

instance Monoid Support where
 mempty = Support Set.empty

 mappend s1 s2
        = Support 
        { supportGlobal = Set.union (supportGlobal s1) (supportGlobal s2) }


-------------------------------------------------------------------------------
-- | Collect support of a module.
collectModule :: Module a A.Name  -> Support
collectModule mm
 = collectExp (moduleBody mm)


-- | Collect support of an expression.
collectExp :: Exp a A.Name -> Support
collectExp xx
 -- Collect names of global variables defined with the static# primitive.
 | Just ( A.NamePrimOp (A.PrimStore A.PrimStoreGlobal)
        , [RType _t, RTerm x])             <- takeXFragApps xx
 , XCon _ (C.DaConPrim name _)             <- x
 , A.NamePrimLit (A.PrimLitTextLit txName) <- name
 = mempty { supportGlobal = Set.singleton txName }

 -- boilerplate.
 | otherwise
 = case xx of
        XPrim{}         -> mempty
        XCon{}          -> mempty
        XVar{}          -> mempty
        XAbs  _ _ xBody -> collectExp xBody
        XApp  _ x1 a2   -> mappend (collectExp  x1)  (collectArg a2)
        XLet  _ lts x   -> mappend (collectLets lts) (collectExp x)
        XCase _ x alts  -> mappend (collectExp  x)   (mconcat $ map collectAlt alts)
        XCast _ _ x     -> collectExp x


-- | Collect the support of an argument.
collectArg  :: Arg a A.Name  -> Support
collectArg aa
 = case aa of
        RTerm x         -> collectExp x
        RImplicit a     -> collectArg a
        _               -> mempty


-- | Collect the suppport of some let bindings.
collectLets :: Lets a A.Name -> Support
collectLets lts
 = case lts of
        LLet _ x        -> collectExp x
        LRec bxs        -> mconcat $ map collectExp $ map snd bxs
        LPrivate{}      -> mempty


-- | Collect the support of an alternative.
collectAlt  :: Alt a A.Name -> Support
collectAlt aa
 = case aa of
        AAlt _ x        -> collectExp x

