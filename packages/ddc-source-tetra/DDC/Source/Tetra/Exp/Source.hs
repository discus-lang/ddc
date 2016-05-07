{-# LANGUAGE TypeFamilies #-}
module DDC.Source.Tetra.Exp.Source
        ( Source        (..)

        -- * Binding
        , Bind          (..)
        , Bound         (..)

        -- * Type Abstract Syntax
        , GAnnot, GBind, GBound, GPrim
        , GType         (..)
        , GTyCon        (..)

        -- * Syntactic Sugar
        , pattern TFun
        , pattern TUnit
        , pattern TVoid
        , pattern TForall
        , pattern TExists
        , pattern TPrim

        -- * Annotated types
        , pattern ATCon
        , pattern ATVar
        , pattern ATAbs
        , pattern ATApp)
where
import DDC.Source.Tetra.Prim
import DDC.Type.Exp.Generic.Exp         as T
import DDC.Data.SourcePos
import Data.Text                        (Text)


-- | Type index for Source Tetra Language.
data Source     
        = Source
        deriving Show


-- | Binding occurrence of a variable.
data Bind
        = BNone
        | BAnon
        | BName !Text
        deriving Show


-- | Bound occurrence of a variable.
data Bound 
        = UIx   !Int
        | UName !Text
        deriving Show


type instance T.GAnnot Source  = SourcePos
type instance T.GBind  Source  = Bind
type instance T.GBound Source  = Bound
type instance T.GPrim  Source  = PrimName


pattern ATCon a tc      = TAnnot a (TCon tc)
pattern ATVar a u       = TAnnot a (TVar u)
pattern ATAbs a b t     = TAnnot a (TAbs b t)
pattern ATApp a t1 t2   = TAnnot a (TApp t1 t2)

