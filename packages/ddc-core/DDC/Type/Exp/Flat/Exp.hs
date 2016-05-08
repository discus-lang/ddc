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

type instance GAnnot    Flat       = ()
type instance GBindVar  Flat       = Text
type instance GBoundVar Flat       = Text
type instance GBindCon  Flat       = Text
type instance GBoundCon Flat       = Text
type instance GPrim     Flat       = Text

