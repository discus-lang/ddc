
module DDC.Core.Salt.Exp where

import qualified DDC.Core.Salt.Name     as A
import qualified DDC.Core.Generic.Exp   as G
import qualified DDC.Core.Annot.Exp     as N
import qualified DDC.Type.Exp           as C


type Exp        = G.Exp         (C.Bind A.Name) (C.Bound A.Name) A.Name A.Name
type Lets       = G.Lets        (C.Bind A.Name) (C.Bound A.Name) A.Name A.Name
type Alt        = G.Alt         (C.Bind A.Name) (C.Bound A.Name) A.Name A.Name
type Pat        = G.Pat         (C.Bind A.Name) (C.Bound A.Name) A.Name A.Name
type Cast       = G.Cast        (C.Bind A.Name) (C.Bound A.Name) A.Name A.Name
type Witness    = G.Witness     (C.Bind A.Name) (C.Bound A.Name) A.Name A.Name
type WiCon      = G.WiCon       (C.Bind A.Name) (C.Bound A.Name) A.Name A.Name


---------------------------------------------------------------------------------------------------
class FromAnnot c1 c2 | c1 -> c2 where
 fromAnnot :: c1 -> Either Error c2

data Error
 = Error


instance FromAnnot (N.Exp () A.Name) Exp where
 fromAnnot xx
  = case xx of
        N.XVar  _ u             -> G.XVar     <$> fromAnnot u
        N.XCon  _ c             -> G.XCon     <$> fromAnnot c
        N.XLAM  _ b x           -> G.XLAM     <$> fromAnnot b   <*> fromAnnot x
        N.XLam  _ b x           -> G.XLam     <$> fromAnnot b   <*> fromAnnot x
        N.XApp  _ x1 x2         -> G.XApp     <$> fromAnnot x1  <*> fromAnnot x2
        N.XLet  _ lts x         -> G.XLet     <$> fromAnnot lts <*> fromAnnot x
        N.XCase _ x alts        -> G.XCase    <$> fromAnnot x   <*> fromAnnots alts
        N.XCast _ c x           -> G.XCast    <$> fromAnnot c   <*> fromAnnot x
        N.XType _ t             -> G.XType    <$> fromAnnot t
        N.XWitness _ w          -> G.XWitness <$> fromAnnot w


instance FromAnnot (N.Lets () A.Name) Lets where
 fromAnnot lts
  = case lts of
        N.LLet u x              -> G.LLet     <$> fromAnnot u <*> fromAnnot x
        N.LRec bxs              -> G.LRec     <$> (sequence $ fmap fromAnnot2 bxs)
        N.LPrivate rs mt wt     -> G.LPrivate <$> fromAnnots rs <*> fromAnnotM mt <*> fromAnnots wt


instance FromAnnot (N.Alt () A.Name) Alt where
 fromAnnot aa
  = case aa of
        N.AAlt w x              -> G.AAlt <$> fromAnnot w <*> fromAnnot x


instance FromAnnot (N.Pat A.Name) Pat where
 fromAnnot pp
  = case pp of
        N.PDefault              -> pure G.PDefault
        N.PData dc bs           -> G.PData <$> pure dc <*> fromAnnots bs


instance FromAnnot (N.Cast () A.Name) Cast where
 fromAnnot cc
  = case cc of
        N.CastWeakenEffect t    -> G.CastWeakenEffect <$> pure t
        N.CastPurify w          -> G.CastPurify       <$> fromAnnot w
        N.CastBox               -> pure G.CastBox
        N.CastRun               -> pure G.CastRun


instance FromAnnot (N.Witness () A.Name) Witness where
 fromAnnot ww
  = case ww of
        N.WVar  _ u             -> G.WVar  <$> pure u
        N.WCon  _ wc            -> G.WCon  <$> fromAnnot wc
        N.WApp  _ w1 w2         -> G.WApp  <$> fromAnnot w1 <*> fromAnnot w2
        N.WType _ t             -> G.WType <$> pure t


instance FromAnnot (N.DaCon A.Name) (N.DaCon A.Name)  where
 fromAnnot dc   = pure dc


instance FromAnnot (N.WiCon A.Name) WiCon where
 fromAnnot ww
  = case ww of
        N.WiConBound u t        -> G.WiConBound <$> pure u <*> pure t


instance FromAnnot (C.Type A.Name) (C.Type A.Name) where
 fromAnnot tt   = pure tt


instance FromAnnot (N.Bind A.Name) (C.Bind A.Name) where
 fromAnnot bb   = pure bb


instance FromAnnot (N.Bound A.Name) (C.Bound A.Name) where
 fromAnnot uu   = pure uu


fromAnnot2 (x, y)
 = (,) <$> fromAnnot x <*> fromAnnot y

fromAnnots xs
 = sequence $ fmap fromAnnot xs

fromAnnotM Nothing      = pure Nothing
fromAnnotM (Just x)     = Just <$> fromAnnot x

