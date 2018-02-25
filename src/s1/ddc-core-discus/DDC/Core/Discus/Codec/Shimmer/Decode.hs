{-# LANGUAGE OverloadedStrings #-}
module DDC.Core.Discus.Codec.Shimmer.Decode
        (takeName)
where
import qualified DDC.Core.Discus.Prim   as D
import qualified SMR.Core.Exp           as S
import qualified SMR.Prim.Name          as S
import Data.Text                        (Text)


---------------------------------------------------------------------------------------------------
type SExp = S.Exp Text S.Prim


---------------------------------------------------------------------------------------------------
-- | Take the Shimmer encoding of a `Name`.
takeName :: SExp -> Maybe D.Name
takeName _ = Just $ D.NameVar "TODO"
