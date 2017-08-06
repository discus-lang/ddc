{-# OPTIONS_HADDOCK hide #-}
-- | Core language AST that includes an annotation on every node of
--   an expression.
--
--   This is the default representation for Disciple Core, and should be
--   preferred over the 'Simple' version of the AST in most cases.
--
--   * Local transformations on this AST should propagate the annotations in a
--     way that would make sense if they were source position identifiers that
--     tracked the provenance of each code snippet. If the specific annotations
--     attached to the AST would not make sense after such a transformation,
--     then the client should erase them to @()@ beforehand using the
--    `reannotate` transform.
--
--   * Global transformations that drastically change the provenance of code
--     snippets should accept an AST with an arbitrary annotation type, but
--     produce one with the annotations set to @()@.
--
module DDC.Core.Exp.Annot.Exp
        ( module DDC.Type.Exp

         -- * Expressions
        , Exp           (..)
        , ParamSort     (..)
        , ParamMode     (..)
        , Param         (..)
        , Arg           (..)
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


-- | Well-typed expressions have types of kind `Data`.
data Exp a n
        -- | Primitive operator of the ambient calculus.
        = XPrim    !a !Prim

        -- | Data constructor or literal.
        | XCon     !a !(DaCon n (Type n))

        -- | Value variable or fragment specific primitive operation.
        | XVar     !a !(Bound n)

        -- | Function abstraction.
        | XAbs     !a !(Param n)  !(Exp a n)

        -- | Function application.
        | XApp     !a !(Exp a n)  !(Arg a n)

        -- | Possibly recursive bindings.
        | XLet     !a !(Lets a n) !(Exp a n)

        -- | Case branching.
        | XCase    !a !(Exp a n)  ![Alt a n]

        -- | Type cast.
        | XCast    !a !(Cast a n) !(Exp a n)
        deriving (Show, Eq)


pattern XLam  a b x     = XAbs a (MTerm b) x
pattern XLAM  a b x     = XAbs a (MType b) x


-- | Parameter sort.
data ParamSort
        = ParamSortType
        | ParamSortTerm
        | ParamSortWitness
        deriving (Show, Eq)


-- | Parameter mode.
data ParamMode
        = ParamModeExplicit
        | ParamModeImplicit
        | ParamModeElaborate
        deriving (Show, Eq)


-- | Parameter of an abstraction.
data Param n
        = MType         !(Bind n)       -- ^ Type binder.
        | MTerm         !(Bind n)       -- ^ Term binder.
        | MImplicit     !(Bind n)       -- ^ Implicit term binder.
        deriving (Show, Eq)


-- | Argument of an application.
data Arg a n
        = RType         !(Type      n)  -- ^ Type argument.
        | RTerm         !(Exp     a n)  -- ^ Term argument.
        | RWitness      !(Witness a n)  -- ^ Witness argument.
        | RImplicit     !(Arg a n)      -- ^ Indicate an implicit argument.
        deriving (Show, Eq)


-- | Primitive operator of the ambient calculus.
--   These operators can be assigned standard functional types,
--   but might have some special handing in the type checker.
data Prim
        -- | Produce a value by elaboration.
        = PElaborate

        -- | Project a field from a record.
        | PProject Text

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
        = CastWeakenEffect !(Effect n)

        -- | Purify the effect (action) of an expression.
        | CastPurify       !(Witness a n)

        -- | Box up a computation,
        --   capturing its effects in the S computation type.
        | CastBox

        -- | Run a computation,
        --   releasing its effects into the environment.
        | CastRun
        deriving (Show, Eq)


-- | When a witness exists in the program it guarantees that a
--   certain property of the program is true.
--
--   Well-typed witnesses have types of kind `Witness`.
--
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

