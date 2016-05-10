{-# LANGUAGE TypeFamilies #-}
module DDC.Source.Tetra.Exp.Source
        ( Source        (..)

        -- * Binding
        , Bind          (..)
        , Bound         (..)

        -- * Types
        -- ** Abstract Syntax
        , GAnnot
        , GBindVar, GBoundVar
        , GBindCon, GBoundCon
        , GPrim

        -- *** Expressions
        , Type,  GType  (..)

        -- *** Constructors
        , TyCon, GTyCon (..)

        -- ** Syntactic Sugar
        , pattern TFun
        , pattern TUnit
        , pattern TVoid
        , pattern TForall
        , pattern TExists
        , pattern TPrim

        -- ** Annotated types
        , pattern ATCon
        , pattern ATVar
        , pattern ATAbs
        , pattern ATApp
        , pattern ATBot
        , pattern ATKiFun
        , pattern ATDaImpl
        , pattern ATDaFun
        , pattern ATData
        , pattern ATRegion
        , pattern ATEffect

        -- ** Type Constructors
        , TyConPrim      (..)
        , SoCon          (..)
        , KiCon          (..)
        , TwCon          (..)
        , TcCon          (..)
        , PrimTyCon      (..)
        , PrimTyConTetra (..))
where
import DDC.Source.Tetra.Prim
import DDC.Type.Exp.Generic.Exp         as T
import DDC.Type.Exp.TyCon               as T
import DDC.Data.SourcePos
import Data.Text                                (Text)

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

type Type       = GType  Source
type TyCon      = GTyCon Source

type instance T.GAnnot    Source  = SourcePos
type instance T.GBindVar  Source  = Bind
type instance T.GBoundVar Source  = Bound
type instance T.GBindCon  Source  = Text
type instance T.GBoundCon Source  = Text
type instance T.GPrim     Source  = TyConPrim


-------------------------------------------------------------------------------
-- | Primitive type constructors.
data TyConPrim
        -- | Sort constructors.
        = TyConPrimSoCon SoCon

        -- | Kind constructors.
        | TyConPrimKiCon KiCon

        -- | Witness type constructors.
        | TyConPrimTwCon TwCon

        -- | Other type constructors at the spec level.
        | TyConPrimTcCon TcCon

        -- | Machine type constructors.
        | TyConPrimMach  PrimTyCon

        -- | Tetra type constructors.
        | TyConPrimTetra PrimTyConTetra
        deriving Show


-------------------------------------------------------------------------------
-- | Annotated type constructor.
pattern ATCon   a tc    = TAnnot a (TCon tc)

-- | Annotated type variable.
pattern ATVar   a u     = TAnnot a (TVar u)

-- | Annotated type abstraction.
pattern ATAbs   a b t   = TAnnot a (TAbs b t)

-- | Annotated type application.
pattern ATApp   a t1 t2 = TAnnot a (TApp t1 t2)

-- | Annotated bottom type.
pattern ATBot   a k     = TAnnot a (TBot k)

-- | Annotated kind function constructor.
pattern ATKiFun  a      = TAnnot a (TCon (TyConPrim (TyConPrimKiCon KiConFun)))

-- | Annotated implication constructor.
pattern ATDaImpl a      = TAnnot a (TCon (TyConPrim (TyConPrimTwCon TwConImpl)))

-- | Annotated function constructor.
pattern ATDaFun  a      = TAnnot a (TCon (TyConPrim (TyConPrimTcCon TcConFun)))

-- | Annotated effect type constructor.
pattern ATData   a      = TAnnot a (TCon (TyConPrim (TyConPrimKiCon KiConData)))

-- | Annotated effect type constructor.
pattern ATRegion a      = TAnnot a (TCon (TyConPrim (TyConPrimKiCon KiConRegion)))

-- | Annotated effect type constructor.
pattern ATEffect a      = TAnnot a (TCon (TyConPrim (TyConPrimKiCon KiConEffect)))


