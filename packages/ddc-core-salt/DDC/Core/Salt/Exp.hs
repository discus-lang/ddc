{-# LANGUAGE TypeFamilies #-}

module DDC.Core.Salt.Exp 
        ( module DDC.Core.Exp.Generic
        , FromAnnot (..)
        , ErrorFromAnnot (..)

        , Annot, Bind, Bound, Prim
        , Exp, Abs, Arg, Lets, Alt, Pat, Cast, Witness, WiCon
        , Type)
where
import DDC.Core.Exp.Generic
import qualified DDC.Core.Exp.Generic           as G
import qualified DDC.Core.Salt.Name             as A
import qualified DDC.Core.Exp.Annot             as N
import qualified DDC.Type.Exp                   as C


---------------------------------------------------------------------------------------------------
-- Type synonyms for the Salt fragment.
type instance G.GAnnot A.Name  = ()
type instance G.GBind  A.Name  = C.Bind  A.Name
type instance G.GBound A.Name  = C.Bound A.Name
type instance G.GPrim  A.Name  = A.PrimOp

type Annot      = G.GAnnot   A.Name
type Bind       = G.GBind    A.Name
type Bound      = G.GBound   A.Name
type Prim       = G.GPrim    A.Name
type Exp        = G.GExp     A.Name
type Abs        = G.GAbs     A.Name
type Arg        = G.GArg     A.Name
type Lets       = G.GLets    A.Name
type Alt        = G.GAlt     A.Name
type Pat        = G.GPat     A.Name
type Cast       = G.GCast    A.Name
type Witness    = G.GWitness A.Name
type WiCon      = G.GWiCon   A.Name

type Type       = C.Type    A.Name


---------------------------------------------------------------------------------------------------
-- | Convert annotated version of the Core language to the Salt fragment.
class FromAnnot c1 c2 | c1 -> c2 where
 fromAnnot :: c1 -> Either ErrorFromAnnot c2


-- | Things that can go wrong when converting Salt code.
data ErrorFromAnnot
        -- | Found a type that isn't part of a function application.
        = ErrorFromAnnotFoundNakedType

        -- | Found a witness that isn't part of a function application.
        | ErrorFromAnnotFoundNakedWitness


instance FromAnnot (N.Exp a A.Name) Exp where
 fromAnnot xx
  = case xx of
        N.XVar  _ (C.UPrim (A.NamePrimVal (A.PrimValOp op)) _)
         -> G.XPrim <$> pure op

        N.XVar  _ u
         -> G.XVar  <$> fromAnnot u

        N.XPrim _ _
         -> error "ddc-core-salt: fromAnnot finish me"

        N.XCon  _ c
         -> G.XCon  <$> fromAnnot c

        N.XLAM  _ b x
         -> G.XAbs  <$> (G.ALAM <$> fromAnnot b) <*> fromAnnot x

        N.XLam  _ b x
         -> G.XAbs  <$> (G.ALam <$> fromAnnot b) <*> fromAnnot x

        N.XApp  _ x1 (N.XType _ t) 
         -> G.XApp  <$> fromAnnot x1  <*> (G.RType    <$> fromAnnot t)

        N.XApp  _ x1 (N.XWitness _ w)
         -> G.XApp  <$> fromAnnot x1  <*> (G.RWitness <$> fromAnnot w)    

        N.XApp  _ x1 x2         
         -> G.XApp  <$> fromAnnot x1  <*> (G.RExp     <$> fromAnnot x2)

        N.XLet  _ lts x
         -> G.XLet  <$> fromAnnot lts <*> fromAnnot x

        N.XCase _ x alts
         -> G.XCase <$> fromAnnot x   <*> fromAnnots alts

        N.XCast _ c x
         -> G.XCast <$> fromAnnot c   <*> fromAnnot x

        N.XType{}
         -> Left $ ErrorFromAnnotFoundNakedType

        N.XWitness{}
         -> Left $ ErrorFromAnnotFoundNakedWitness


instance FromAnnot (N.Lets a A.Name) Lets where
 fromAnnot lts
  = case lts of
        N.LLet u x
         -> G.LLet     <$> fromAnnot u <*> fromAnnot x

        N.LRec bxs
         -> G.LRec     <$> (sequence $ fmap fromAnnot2 bxs)

        N.LPrivate rs mt wt     
         -> G.LPrivate <$> fromAnnots rs <*> fromAnnotM mt <*> fromAnnots wt


instance FromAnnot (N.Alt a A.Name) Alt where
 fromAnnot aa
  = case aa of
        N.AAlt w x              -> G.AAlt <$> fromAnnot w <*> fromAnnot x


instance FromAnnot (N.Pat A.Name) Pat where
 fromAnnot pp
  = case pp of
        N.PDefault              -> pure G.PDefault
        N.PData dc bs           -> G.PData <$> pure dc <*> fromAnnots bs


instance FromAnnot (N.Cast a A.Name) Cast where
 fromAnnot cc
  = case cc of
        N.CastWeakenEffect t    -> G.CastWeakenEffect <$> pure t
        N.CastPurify w          -> G.CastPurify       <$> fromAnnot w
        N.CastBox               -> pure G.CastBox
        N.CastRun               -> pure G.CastRun


instance FromAnnot (N.Witness a A.Name) Witness where
 fromAnnot ww
  = case ww of
        N.WVar  _ u             -> G.WVar  <$> pure u
        N.WCon  _ wc            -> G.WCon  <$> fromAnnot wc
        N.WApp  _ w1 w2         -> G.WApp  <$> fromAnnot w1 <*> fromAnnot w2
        N.WType _ t             -> G.WType <$> pure t


instance FromAnnot (N.DaCon A.Name (C.Type A.Name)) (N.DaCon A.Name (C.Type A.Name))  where
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

