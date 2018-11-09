
-- Suppress Data.Monoid warnings during GHC 8.4.1 transition
{-# OPTIONS  -Wno-unused-imports #-}

-- | Collect information about the use of various primitives in a module.
module DDC.Core.Salt.Analysis.Primitive
        ( collectModule
        , Support (..))
where
import DDC.Core.Module                  as C
import DDC.Core.Exp.Annot               as C

import Data.Text                        (Text)
import Data.Map                         (Map)
import qualified DDC.Core.Salt.Name     as A
import qualified Data.Map               as Map

-- GHC 8.2 -> 8.4 transition.
import Data.Semigroup                   (Semigroup(..))
import Data.Monoid                      (Monoid(..))


-------------------------------------------------------------------------------
--- | Support of some term.
data Support
        = Support
        { -- | Names of static globals mentioned using the global# primitive,
          --   and whether we should define the symbol in the current module.
          supportGlobal :: Map Text [(Type A.Name, Bool)]
        }
        deriving Show


instance Semigroup Support where
 (<>)           = unionSupport


-- | Union two supports.
instance Monoid Support where
 mempty         = emptySupport
 mappend        = unionSupport


-- | Construct an empty support.
emptySupport :: Support
emptySupport = Support Map.empty


-- | Union two supports
unionSupport :: Support -> Support -> Support
unionSupport s1 s2
        = Support
        { supportGlobal
          = Map.unionWith (++) (supportGlobal s1) (supportGlobal s2) }


-------------------------------------------------------------------------------
-- | Collect support of a module.
collectModule :: Module a A.Name  -> Support
collectModule mm
 = collectExp (moduleBody mm)


-- | Collect support of an expression.
collectExp :: Exp a A.Name -> Support
collectExp xx
 -- Collect names of global variables defined with the globali# primitive.
 | Just ( A.NamePrimOp (A.PrimStore (A.PrimStoreGlobal bDefineHere))
        , [RType t, RTerm x])   <- takeXNameApps xx
 , XCon _ (C.DaConPrim name)    <- x
 , A.NamePrimLit (A.PrimLitTextLit txName) <- name
 = mempty { supportGlobal = Map.singleton txName [(t, bDefineHere)] }

 -- boilerplate.
 | otherwise
 = case xx of
        XVar{}           -> mempty
        XAbs  _ _ xBody  -> collectExp xBody
        XApp  _ x1 a2    -> mappend (collectExp  x1)  (collectArg a2)
        XLet  _ lts x    -> mappend (collectLets lts) (collectExp x)
        XAtom{}          -> mempty
        XCase _ x alts   -> mappend (collectExp  x)   (mconcat $ map collectAlt alts)
        XCast _ _ x      -> collectExp x
        XAsync _ _ e1 e2 -> mappend (collectExp e1)   (collectExp e2)


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

