{-# LANGUAGE TypeFamilies #-}
module DDC.Type.Exp.Flat.Exp 
        ( module DDC.Type.Exp.Generic.Exp
        , Flat(..)
        , Type, TyCon)
where
import DDC.Type.Exp.Generic.Exp
import Data.Text                (Text)

data Flat       
        = Flat
        deriving Show
        

type Type       = GType  Flat
type TyCon      = GTyCon Flat

type instance GTAnnot    Flat   = ()
type instance GTBindVar  Flat   = Text
type instance GTBoundVar Flat   = Text
type instance GTBindCon  Flat   = Text
type instance GTBoundCon Flat   = Text
type instance GTPrim     Flat   = Text

