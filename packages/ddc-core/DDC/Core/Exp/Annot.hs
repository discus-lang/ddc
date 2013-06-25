
-- | Core language AST that includes an annotation on every node of 
--   an expression.
--
--   This is the default version of the language AST, and should be preferred
--   over the 'Simple' version in most cases. General purpose transformations 
--   that work over this AST should propagate the annotations in some
--   well-defined way.
module DDC.Core.Exp.Annot 
        ( module DDC.Type.Exp

         -- * Expressions
        , Exp           (..)
        , Cast          (..)
        , Lets          (..)
        , Alt           (..)
        , Pat           (..)

          -- * Witnesses
        , Witness       (..)

          -- * Data Constructors
        , DaCon         (..)
        , DaConName     (..)

          -- * Witness Constructors
        , WiCon         (..)
        , WbCon         (..))
where
import DDC.Core.Exp.WiCon
import DDC.Core.Exp.DaCon
import DDC.Core.Exp.Pat
import DDC.Type.Exp
import DDC.Type.Sum             ()
import Control.DeepSeq


-- Values ---------------------------------------------------------------------
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


-- | Type casts.
data Cast a n
        -- | Weaken the effect of an expression.
        --   The given effect is added to the effect
        --   of the body.
        = CastWeakenEffect  !(Effect n)
        
        -- | Weaken the closure of an expression.
        --   The closures of these expressions are added to the closure
        --   of the body.
        | CastWeakenClosure ![Exp a n]

        -- | Purify the effect (action) of an expression.
        | CastPurify !(Witness a n)

        -- | Forget about the closure (sharing) of an expression.
        | CastForget !(Witness a n)
        deriving (Show, Eq)


-- | Possibly recursive bindings.
data Lets a n
        -- | Non-recursive expression binding.
        = LLet    !(Bind n) !(Exp a n)

        -- | Recursive binding of lambda abstractions.
        | LRec    ![(Bind n, Exp a n)]

        -- | Bind a local region variable,
        --   and witnesses to its properties.
        | LLetRegions ![Bind n] ![Bind n]
        
        -- | Holds a region handle during evaluation.
        | LWithRegion !(Bound n)
        deriving (Show, Eq)


-- | Case alternatives.
data Alt a n
        = AAlt !(Pat n) !(Exp a n)
        deriving (Show, Eq)


-- | When a witness exists in the program it guarantees that a
--   certain property of the program is true.
data Witness a n
        -- | Witness variable.
        = WVar  a !(Bound n)
        
        -- | Witness constructor.
        | WCon  a !(WiCon n)
        
        -- | Witness application.
        | WApp  a !(Witness a n) !(Witness a n)

        -- | Joining of witnesses.
        | WJoin a !(Witness a n) !(Witness a n)

        -- | Type can appear as the argument of an application.
        | WType a !(Type n)
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
        CastWeakenClosure xs    -> rnf xs
        CastPurify w            -> rnf w
        CastForget w            -> rnf w


instance (NFData a, NFData n) => NFData (Lets a n) where
 rnf lts
  = case lts of
        LLet b x                -> rnf b `seq` rnf x
        LRec bxs                -> rnf bxs
        LLetRegions bs1 bs2     -> rnf bs1  `seq` rnf bs2
        LWithRegion u           -> rnf u


instance (NFData a, NFData n) => NFData (Alt a n) where
 rnf aa
  = case aa of
        AAlt w x                -> rnf w `seq` rnf x


instance (NFData a, NFData n) => NFData (Witness a n) where
 rnf ww
  = case ww of
        WVar  a u                 -> rnf a `seq` rnf u
        WCon  a c                 -> rnf a `seq` rnf c
        WApp  a w1 w2             -> rnf a `seq` rnf w1 `seq` rnf w2
        WJoin a w1 w2             -> rnf a `seq` rnf w1 `seq` rnf w2
        WType a tt                -> rnf a `seq` rnf tt
