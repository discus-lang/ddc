
module DDC.Source.Tetra.ToCore
        (toCoreModule)
where
import qualified DDC.Source.Tetra.Module        as S
import qualified DDC.Source.Tetra.Exp           as S
import qualified DDC.Source.Tetra.Prim          as S

import qualified DDC.Core.Tetra.Prim            as C
import qualified DDC.Core.Compounds             as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Exp                   as C

import qualified DDC.Type.Sum                   as Sum
import qualified Data.Map                       as Map
import Data.Maybe

-- Things shared between both Source and Core languages.
import DDC.Core.Exp
        ( Bind    (..)
        , Bound   (..)
        , Type    (..)
        , TyCon   (..)
        , Witness (..)
        , WiCon   (..))


-- Module ---------------------------------------------------------------------
toCoreModule :: a -> S.Module a S.Name -> C.Module a C.Name
toCoreModule a mm
        = C.ModuleCore
        { C.moduleName          = S.moduleName mm
        , C.moduleExportKinds   = Map.empty
        , C.moduleExportTypes   = Map.empty
        , C.moduleImportKinds   = Map.empty
        , C.moduleImportTypes   = Map.empty
        , C.moduleBody          = C.xUnit a }


letsOfTops :: [S.Top a n] -> C.Lets a n
letsOfTops tops
 = C.LRec $ mapMaybe bindOfTop tops


bindOfTop  :: S.Top a n -> (Bind n, C.Exp a n)
bindOfTop (S.TopBind a b x) 
                = Just (toCoreB b, toCoreX x)
bindOfTop _     = Nothing


-- Type -----------------------------------------------------------------------
toCoreT :: Type S.Name -> Bind C.Name
toCoreT tt
 = case tt of
        TVar    u       -> TVar (toCoreU  u)
        TCon    tc      -> TCon (toCoreTC tc)        
        TForall b t     -> TForall (toCoreB b) (toCoreT t)
        TApp    t1 t2   -> TApp (toCoreT t1) (toCoreT t2)
        TSum    ts      -> TSum $ Sum.fromList (Sum.kindOfSum ts)
                                $ map toCoreT 
                                $ Sum.toList ts  


-- TyCon ----------------------------------------------------------------------
toCoreTC :: TyCon S.Name -> TyCon C.Name
toCoreTC tc
 = case tc of
        TyConSort sc    -> TyConSort sc
        TyConKind kc    -> TyConKind kc
        TyConWitness wc -> TyConWitness wc
        TyConSpec sc    -> TyConSpec sc
        TyConBound u k  -> TyConBound (toCoreB u) (toCoreT k)


-- Exp ------------------------------------------------------------------------
toCoreX :: S.Exp a S.Name -> C.Exp a C.Name
toCoreX xx
 = case xx of
        S.XVar  a u      -> C.XVar  a  (toCoreU  u)
        S.XCon  a dc     -> C.XCon  a  (toCoreDC dc)
        S.XLAM  a b x    -> C.XLAM  a  (toCoreB b)  (toCoreX x)
        S.XLam  a b x    -> C.XLam  a  (toCoreB b)  (toCoreX x)
        S.XApp  a x1 x2  -> C.XApp  a  (toCoreX x1) (toCoreX x2)
        S.XLet  a lts x  -> C.XLet  a  (toCoreLts lts) (toCoreX x)
        S.XCase a x alts -> C.XCase a  (toCoreX x)  (map toCoreA alts)
        S.XCast a c x    -> C.XCast a  (toCoreC c)  (toCoreX x)
        S.XType t        -> C.XType    (toCoreT t)
        S.XWitness w     -> C.XWitness (toCoreW w)


-- Cast -----------------------------------------------------------------------
toCoreC :: S.Cast a S.Name -> C.Cast a C.Name
toCoreC cc
 = case cc of
        S.CastWeakenEffect eff  -> C.CastWeakenEffect (toCoreT eff)
        S.CastPurify   w        -> C.CastPurify       (toCoreW w)
        S.CastSuspend           -> C.CastSuspend
        S.CastRun               -> C.CastRun


-- Witness --------------------------------------------------------------------
toCoreW :: Witness a S.Name -> Witness a C.Name
toCoreW ww
 = case ww of
        S.WVar  a u     -> C.WVar  a (toCoreU  u)
        S.WCon  a wc    -> C.WVar  a (toCoreWC wc)
        S.WApp  a w1 w2 -> C.WApp  a (toCoreW  w1) (toCoreW w2)
        S.WJoin a w1 w2 -> C.WJoin a (toCoreW  w1) (toCoreW w2)
        S.WType a t     -> C.WType a (toCoreT  t)


-- WiCon ----------------------------------------------------------------------
toCoreWC :: WiCon a S.Name -> WiCon a C.Name
toCoreWC wc
 = case wc of
        WiConBuiltin wb -> WiConBuiltin wb
        WiConBound u t  -> WiConBound (toCoreU u) (toCoreT t)


-- Bind -----------------------------------------------------------------------
toCoreB :: Bind S.Name -> Bind C.Name
toCoreB bb
 = case bb of
        BName n t       -> BName (toCoreN n) (toCoreT t)
        BAnon t         -> BName (toCoreT t)
        BNone           -> BNone


-- Bound ----------------------------------------------------------------------
toCoreU :: Bound S.Name -> Bound C.Name
toCoreU uu
 = case uu of
        UName u         -> UName (toCoreU u)
        UIx   i         -> UIx   i
        UPrim n t       -> UPrim (toCoreN n) (toCoreT t)


-- Name -----------------------------------------------------------------------
toCoreN :: S.Name -> C.Name
toCoreN nn
 = case nn of
        S.NameVar       str -> C.NameVar        str
        S.NameCon       str -> C.NameCon        str
        S.NameTyConData tc  -> C.NameTyConData  tc
        S.NameOpStore   tc  -> C.NameOpStore    tc
        S.NamePrimTyCon p   -> C.NamePrimTyCon  p
        S.NamePrimArith p   -> C.NamePrimArith  p
        S.NameLitBool   b   -> C.NameLitBool    b
        S.NameLitNat    n   -> C.NameLitNat     n
        S.NameLitInt    i   -> C.NameLitInt     i
        S.NameLitWord   w   -> C.NameLitWord    w
        S.NameHole          -> C.NameHole

