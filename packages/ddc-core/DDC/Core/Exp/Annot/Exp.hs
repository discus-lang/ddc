
-- | Core language AST that includes an annotation on every node of
--   an expression.
--
--   This is the default representation for Disciple Core, and should be preferred
--   over the 'Simple' version of the AST in most cases.
--
--   * Local transformations on this AST should propagate the annotations in a way that
--   would make sense if they were source position identifiers that tracked the provenance
--   of each code snippet. If the specific annotations attached to the AST would not make
--   sense after such a transformation, then the client should erase them to @()@ beforehand
--   using the `reannotate` transform.
--
--   * Global transformations that drastically change the provenance of code snippets should
--     accept an AST with an arbitrary annotation type, but produce one with the annotations
--     set to @()@.
--
module DDC.Core.Exp.Annot.Exp
        ( module DDC.Type.Exp

         -- * Expressions
        , Exp           (..)
        , Param         (..)
        , Prim          (..)
        , Lets          (..)
        , Alt           (..)
        , Pat           (..)
        , Cast          (..)
        , pattern XLam
        , pattern XLAM

          -- * Witnesses
        , Witness       (..)

          -- * Data Constructors
        , DaCon         (..)

          -- * Witness Constructors
        , WiCon         (..))
where
import DDC.Core.Exp.WiCon
import DDC.Core.Exp.DaCon
import DDC.Type.Exp
import DDC.Type.Sum             ()
import Data.Text                (Text)
import Control.DeepSeq


-- Values ---------------------------------------------------------------------
-- | Well-typed expressions have types of kind `Data`.
data Exp a n
        -- | Value variable or fragment specific primitive operation.
        = XVar     !a !(Bound n)

        -- | Primitive operator of the ambient calculus.
        | XPrim    !a !Prim

        -- | Data constructor or literal.
        | XCon     !a !(DaCon n (Type n))

        -- | Function Abstraction.
        | XAbs     !a !(Param n)  !(Exp a n)

        -- | Function Application.
        | XApp     !a !(Exp a n)  !(Exp a n)

        -- | Possibly recursive bindings.
        | XLet     !a !(Lets a n) !(Exp a n)

        -- | Case branching.
        | XCase    !a !(Exp a n)  ![Alt a n]

        -- | Type cast.
        | XCast    !a !(Cast a n) !(Exp a n)

        -- | Type can appear as the argument of an application.
        | XType    !a !(Type n)

        -- | Witness can appear as the argument of an application.
        | XWitness !a !(Witness a n)
        deriving (Show, Eq)


pattern XLam  a b x     = XAbs a (MTerm b) x
pattern XLAM  a b x     = XAbs a (MType b) x


-- | Parameter of an abstraction.
data Param n
        = MTerm         !(Bind n)       -- ^ Term binder.
        | MType         !(Bind n)       -- ^ Type binder.
        | MImplicit     !(Bind n)       -- ^ Implicit term binder.
        deriving (Show, Eq)


-- | Primitive operator of the ambient calculus.
--   These operators can be assigned standard functional types,
--   but might have some special handing in the type checker.
data Prim
        -- | Project a field from a record.
        = PProject Text

        -- | Shuffle fields of a record.
        | PShuffle

        -- | Combine two records into a new one.
        | PCombine
        deriving (Show, Eq)


-- | Possibly recursive bindings.
data Lets a n
        -- | Non-recursive expression binding.
        = LLet     !(Bind n) !(Exp a n)

        -- | Recursive binding of lambda abstractions.
        | LRec     ![(Bind n, Exp a n)]

        -- | Bind a private region variable,
        --   and witnesses to its properties.
        | LPrivate ![Bind n] !(Maybe (Type n)) ![Bind n]
        deriving (Show, Eq)


-- | Case alternatives.
data Alt a n
        = AAlt !(Pat n) !(Exp a n)
        deriving (Show, Eq)


-- | Pattern matching.
data Pat n
        -- | The default pattern always succeeds.
        = PDefault

        -- | Match a data constructor and bind its arguments.
        | PData !(DaCon n (Type n)) ![Bind n]
        deriving (Show, Eq)


-- | Type casts.
data Cast a n
        -- | Weaken the effect of an expression.
        --   The given effect is added to the effect
        --   of the body.
        = CastWeakenEffect  !(Effect n)

        -- | Purify the effect (action) of an expression.
        | CastPurify    !(Witness a n)

        -- | Box up a computation,
        --   capturing its effects in the S computation type.
        | CastBox

        -- | Run a computation,
        --   releasing its effects into the environment.
        | CastRun
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

        -- | Type can appear as the argument of an application.
        | WType a !(Type n)
        deriving (Show, Eq)


-- NFData ---------------------------------------------------------------------
instance (NFData a, NFData n) => NFData (Exp a n) where
 rnf xx
  = case xx of
        XVar  a u       -> rnf a `seq` rnf u
        XPrim a _       -> rnf a 
        XCon  a dc      -> rnf a `seq` rnf dc
        XAbs  a b x     -> rnf a `seq` rnf b   `seq` rnf x
        XApp  a x1 x2   -> rnf a `seq` rnf x1  `seq` rnf x2
        XLet  a lts x   -> rnf a `seq` rnf lts `seq` rnf x
        XCase a x alts  -> rnf a `seq` rnf x   `seq` rnf alts
        XCast a c x     -> rnf a `seq` rnf c   `seq` rnf x
        XType a t       -> rnf a `seq` rnf t
        XWitness a w    -> rnf a `seq` rnf w

instance (NFData n) => NFData (Param n) where
 rnf mm
  = case mm of
        MTerm b                 -> rnf b
        MType b                 -> rnf b
        MImplicit b             -> rnf b


instance (NFData a, NFData n) => NFData (Cast a n) where
 rnf cc
  = case cc of
        CastWeakenEffect e      -> rnf e
        CastPurify w            -> rnf w
        CastBox                 -> ()
        CastRun                 -> ()


instance (NFData a, NFData n) => NFData (Lets a n) where
 rnf lts
  = case lts of
        LLet b x                -> rnf b `seq` rnf x
        LRec bxs                -> rnf bxs
        LPrivate bs1 u2 bs3     -> rnf bs1 `seq` rnf u2 `seq` rnf bs3


instance (NFData a, NFData n) => NFData (Alt a n) where
 rnf aa
  = case aa of
        AAlt w x                -> rnf w `seq` rnf x


instance NFData n => NFData (Pat n) where
 rnf pp
  = case pp of
        PDefault                -> ()
        PData dc bs             -> rnf dc `seq` rnf bs


instance (NFData a, NFData n) => NFData (Witness a n) where
 rnf ww
  = case ww of
        WVar  a u                 -> rnf a `seq` rnf u
        WCon  a c                 -> rnf a `seq` rnf c
        WApp  a w1 w2             -> rnf a `seq` rnf w1 `seq` rnf w2
        WType a tt                -> rnf a `seq` rnf tt
