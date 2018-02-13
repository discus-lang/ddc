{-# OPTIONS_HADDOCK hide #-}
module DDC.Source.Discus.Convert.Witness
        ( toCoreW
        , toCoreWC)
where
import DDC.Source.Discus.Convert.Type
import DDC.Source.Discus.Convert.Base

import qualified DDC.Source.Discus.Exp                   as S

import qualified DDC.Core.Discus.Prim                    as C
import qualified DDC.Core.Exp.Annot                     as C



toCoreW :: SP -> S.Witness -> ConvertM a (C.Witness SP C.Name)
toCoreW a ww
 = case ww of
        S.WAnnot a' w
         -> toCoreW a' w

        S.WVar  u
         -> C.WVar  <$> pure a <*> toCoreU  u

        S.WCon  wc
         -> C.WCon  <$> pure a <*> toCoreWC wc

        S.WApp  w1 w2
         -> C.WApp  <$> pure a <*> toCoreW a w1 <*> toCoreW a w2

        S.WType t
         -> C.WType <$> pure a <*> toCoreT UniverseSpec t


toCoreWC :: S.WiCon -> ConvertM a (C.WiCon C.Name)
toCoreWC wc
 = case wc of
        S.WiConBound u t
         -> C.WiConBound <$> toCoreU u
                         <*> toCoreT UniverseSpec t

