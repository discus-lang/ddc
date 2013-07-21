
module DDC.Source.Tetra.Exp.Base
        ( module DDC.Type.Exp

        -- * Expressions
        , Exp           (..)
        , Lets          (..)
        , Alt           (..)
        , Pat           (..)
        , Cast          (..)

        -- * Witnesses
        , Witness       (..)

        -- * Data Constructors
        , DaCon         (..)
        , DaConName     (..)

        -- * Witness Constructors
        , WiCon         (..)
        , WbCon         (..))
where
import DDC.Type.Exp
import DDC.Type.Sum     ()
import Control.DeepSeq
import DDC.Core.Exp     
        ( Witness       (..)
        , WiCon         (..)
        , WbCon         (..)
        , Pat           (..)
        , DaCon         (..)
        , DaConName     (..))


-- | Well-typed expressions have types of kind `Data`.
data Exp a n
        -- | Value variable   or primitive operation.
        = XVar  !a  !(Bound n)

        -- | Data constructor or literal.
        | XCon  !a  !(DaCon n)

        -- | Type abstraction (level-1).
        | XLAM  !a  !(Bind n)   !(Exp a n)

        -- | Value and Witness abstraction (level-0).
        | XLam  !a  !(Bind n)   !(Exp a n)

        -- | Application.
        | XApp  !a  !(Exp a n)  !(Exp a n)

        -- | Possibly recursive bindings.
        | XLet  !a  !(Lets a n) !(Exp a n)

        -- | Case branching.
        | XCase !a  !(Exp a n)  ![Alt a n]

        -- | Type cast.
        | XCast !a  !(Cast a n) !(Exp a n)

        -- | Type can appear as the argument of an application.
        | XType     !(Type n)

        -- | Witness can appear as the argument of an application.
        | XWitness  !(Witness a n)
        deriving (Show, Eq)


-- | Possibly recursive bindings.
data Lets a n
        -- | Possibly recursive binding.
        = LLet    !(Bind n) !(Exp a n)

        -- | Bind a local region variable,
        --   and witnesses to its properties.
        | LLetRegions ![Bind n] ![Bind n]
        deriving (Show, Eq)


-- | Case alternatives.
data Alt a n
        = AAlt !(Pat n) !(Exp a n)
        deriving (Show, Eq)


-- | Type casts.
data Cast a n
        -- | Weaken the effect of an expression.
        --   The given effect is added to the effect
        --   of the body.
        = CastWeakenEffect  !(Effect n)
        
        -- | Purify the effect (action) of an expression.
        | CastPurify !(Witness a n)

        -- | Suspend a computation, 
        --   capturing its effects in the S computation type.
        | CastSuspend 

        -- | Run a computation,
        --   releasing its effects into the environment.
        | CastRun
        deriving (Show, Eq)

        
-- NFData ---------------------------------------------------------------------
instance (NFData a, NFData n) => NFData (Exp a n) where
 rnf xx
  = case xx of
        XVar  a u       -> rnf a `seq` rnf u
        XCon  a dc      -> rnf a `seq` rnf dc
        XLAM  a b x     -> rnf a `seq` rnf b   `seq` rnf x
        XLam  a b x     -> rnf a `seq` rnf b   `seq` rnf x
        XApp  a x1 x2   -> rnf a `seq` rnf x1  `seq` rnf x2
        XLet  a lts x   -> rnf a `seq` rnf lts `seq` rnf x
        XCase a x alts  -> rnf a `seq` rnf x   `seq` rnf alts
        XCast a c x     -> rnf a `seq` rnf c   `seq` rnf x
        XType t         -> rnf t
        XWitness w      -> rnf w


instance (NFData a, NFData n) => NFData (Cast a n) where
 rnf cc
  = case cc of
        CastWeakenEffect e      -> rnf e
        CastPurify w            -> rnf w
        CastSuspend             -> ()
        CastRun                 -> ()


instance (NFData a, NFData n) => NFData (Lets a n) where
 rnf lts
  = case lts of
        LLet b x                -> rnf b `seq` rnf x
        LLetRegions bs1 bs2     -> rnf bs1  `seq` rnf bs2


instance (NFData a, NFData n) => NFData (Alt a n) where
 rnf aa
  = case aa of
        AAlt w x                -> rnf w `seq` rnf x
