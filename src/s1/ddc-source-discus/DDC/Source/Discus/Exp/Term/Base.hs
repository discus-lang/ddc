{-# OPTIONS_HADDOCK hide #-}

-- | Abstract syntax for Discus Source expressions.
module DDC.Source.Discus.Exp.Term.Base
        ( -- * Types
          -- ** Syntax
          GType         (..)
        , GTyCon        (..)

        , pattern TApp2, pattern TApp3
        , pattern TApp4, pattern TApp5

        , pattern TVoid, pattern TUnit
        , pattern TBot,  pattern TUnion
        , pattern TPrim
        , pattern TFunExplicit
        , pattern TFunImplicit

          -- * Terms
          -- ** Syntax
        , GXBindVarMT   (..)
        , GExp          (..)
        , GParam        (..)
        , GArg          (..)
        , GLets         (..)
        , GCaps         (..)
        , GPat          (..)
        , GClause       (..)
        , GGuard        (..)
        , GGuardedExp   (..)
        , GAltMatch     (..)
        , GAltCase      (..)
        , GCast         (..)
        , GWitness      (..)
        , GWiCon        (..)
        , DaCon         (..)
        , DaConBind     (..)
        , DaConBound    (..)
        , ParamSort     (..)

          -- ** Primitives
        , PrimVal       (..)
        , PrimArith     (..)
        , PrimCast      (..)
        , OpVector      (..)
        , OpFun         (..)
        , OpError       (..)
        , PrimLit       (..)
        , Literal       (..)
        , Text

         -- ** Sugar
        , pattern PTrue
        , pattern PFalse

        , module DDC.Source.Discus.Exp.Bind)
where
import DDC.Source.Discus.Exp.Bind
import DDC.Source.Discus.Exp.Term.Prim
import DDC.Source.Discus.Exp.Type.Base

import DDC.Core.Exp             (DaCon   (..))
import DDC.Core.Exp.Literal     (Literal (..))

import Data.Text                (Text)

-------------------------------------------------------------------------------
-- Type functions associated with the language AST.

-- | A possibly typed binding.
data GXBindVarMT a
        = XBindVarMT Bind (Maybe (GType a))


-------------------------------------------------------------------------------
-- | Well-typed expressions have types of kind `Data`.
data GExp a
        ---------------------------------------------------
        -- Core Language Constructs.
        --   These are also in the core language, and after desugaring only
        --   these constructs are used.
        --
        = XAnnot    !a !(GExp a)

        -- | Primitives
        | XPrim     !PrimVal

        -- | Data constructor or literal.
        | XCon      !(DaCon DaConBound (GType a))

        -- | Value variable.
        | XVar      !Bound

        -- | Function abstraction.
        | XAbs      !(GParam a)  !(GExp a)

        -- | Function application.
        | XApp      !(GExp  a)   !(GArg a)

        -- | A non-recursive let-binding.
        | XLet      !(GLets a)   !(GExp a)

        -- | Case branching.
        | XCase     !(GExp  a)   ![GAltCase a]

        -- | Type cast.
        | XCast     !(GCast a)   !(GExp a)


        ---------------------------------------------------
        -- Sugar Constructs.
        --  These constructs are eliminated by the desugarer.
        --
        -- | Some expressions and infix operators that need to be resolved
        --   into proper function applications.
        | XDefix    !a [GArg a]

        -- | Use of a naked infix operator, like in 1 + 2.
        --   INVARIANT: only appears in the list of an XDefix node.
        | XInfixOp  !a String

        -- | Use of an infix operator as a plain variable, like in (+) 1 2.
        --   INVARIANT: only appears in the list of an XDefix node.
        | XInfixVar !a String

        -- | Match expression with default.
        --   Similar to a case expression, except that if an alternative
        --   fails then we try the next one instead of failing.
        --   If none of the alternatives succeeds then the overall value
        --   is the value of the default expression.
        | XMatch    !a ![GAltMatch a] !(GExp a)

        -- | Where expression defines a group of recursive clauses,
        --   and is desugared to a letrec.
        | XWhere    !a !(GExp a) ![GClause a]

        -- | Abstraction which matches its argument against a single pattern.
        | XAbsPat   !a !ParamSort !(GPat a) !(Maybe (GType a)) !(GExp a)

        -- | Lambda abstraction that matches its argument against
        --   the given alternatives.
        | XLamCase  !a ![GAltCase a]


-- | Binding occurrence of a data constructor.
data DaConBind
        = DaConBindName  !Text
        deriving (Eq, Ord, Show)


-- | Bound occurrences of a data constructor.
data DaConBound
        = DaConBoundName !Text
        | DaConBoundLit  !PrimLit
        deriving (Eq, Ord, Show)


-- | Sorts of parameters that we have.
data ParamSort
        = MSType
        | MSTerm
        | MSImplicit
        deriving Show


-- | Parameter for an abstraction.
data GParam a
        -- | Type parameter with optional kind.
        = MType     !Bind     !(Maybe (GType a))

        -- | Term pattern with optional type.
        | MTerm     !(GPat a) !(Maybe (GType a))

        -- | Implicit term pattern with optional type.
        | MImplicit !(GPat a) !(Maybe (GType a))


-- | Argument of an application.
data GArg a
        -- | Type argument.
        = RType     !(GType    a)

        -- | Term argument.
        | RTerm     !(GExp     a)

        -- | Witness argument.
        | RWitness  !(GWitness a)

        -- | Implicit argument.
        | RImplicit !(GArg     a)


-- | Possibly recursive bindings.
--   Whether these are taken as recursive depends on whether they appear
--   in an XLet or XRec group.
data GLets a
        ---------------------------------------------------
        -- Core Language Constructs
        -- | Non-recursive expression binding.
        = LLet      !(GXBindVarMT a) !(GExp a)

        -- | Recursive binding of lambda abstractions.
        | LRec     ![(GXBindVarMT a,   GExp a)]

        -- | Bind a local region variable,
        --   and witnesses to its properties.
        | LPrivate ![Bind] !(GCaps a)

        -- | Extend an existing region with a sub region that has other capabilities.
        | LExtend  ![Bind] !(GType a) !(GCaps a)

        ---------------------------------------------------
        -- Sugar Constructs
        -- | A possibly recursive group of binding clauses.
        --   The flag says if the group is recursive (true) or non-recursive (false).
        --   Multiple clauses in the group may be part of the same function.
        | LGroup   !Bool ![GClause a]


-- | Capability list associate with a 'private' or 'extend' construct.
data GCaps a
        -- | Explicit list of named capabilities.
        = CapsList ![(Bind, GType a)]

        -- | Sugar for '{Read r; Write r; Alloc r}' for each bound region 'r'.
        | CapsMutable

        -- | Sugar for '{Read r; Alloc r}' for each bound region 'r'.
        | CapsConstant


-- | Binding clause
data GClause a
        -- | A separate type signature.
        = SSig   !a !Bind   !(GType a)

        -- | A function binding using pattern matching and guards.
        | SLet   !a !(GXBindVarMT a) ![GParam a] ![GGuardedExp a]


-- | Patterns.
data GPat a
        -- | The default pattern always succeeds.
        = PDefault

        -- | Give a name to the value matched by a pattern.
        | PAt    !Bind !(GPat a)

        -- | The variable pattern always succeeds and binds the value
        --   to the new variable.
        | PVar   !Bind

        -- | Match a data constructor and bind its arguments.
        | PData  !(DaCon DaConBound (GType a)) ![GPat a]


-- | An expression with some guards.
data GGuardedExp a
        = GGuard !(GGuard a) !(GGuardedExp a)
        | GExp   !(GExp   a)


-- | Expression guards.
data GGuard a
        = GPat   !(GPat a) !(GExp a)
        | GPred  !(GExp a)
        | GDefault


-- | Case alternative.
--   If the pattern matches then bind the variables then enter
--   the guarded expression.
data GAltCase a
        = AAltCase   !(GPat a) ![GGuardedExp a]


-- | Match alternative.
--   This is like a case alternative except that the match expression
--   does not give us a head pattern.
data GAltMatch a
        = AAltMatch  !(GGuardedExp a)


-- | Type casts.
data GCast a
        -- | Weaken the effect of an expression.
        --   The given effect is added to the effect
        --   of the body.
        = CastWeakenEffect  !(GType a)

        -- | Box a computation,
        --   capturing its effects in the S computation type.
        | CastBox

        -- | Run a computation,
        --   releasing its effects into the environment.
        | CastRun


-- | Witnesses.
data GWitness a
        -- | Witness annotation
        = WAnnot !a !(GWitness a)

        -- | Witness variable.
        | WVar   !Bound

        -- | Witness constructor.
        | WCon   !(GWiCon a)

        -- | Witness application.
        | WApp   !(GWitness a) !(GWitness a)

        -- | Type can appear as an argument of a witness application.
        | WType  !(GType a)


-- | Witness constructors.
data GWiCon a
        -- | Witness constructors defined in the environment.
        --   In the interpreter we use this to hold runtime capabilities.
        --   The attached type must be closed.
        = WiConBound   !Bound !(GType a)


-- Patterns ---------------------------------------------------------------------------------------
pattern PTrue  = PData (DaConPrim (DaConBoundLit (PrimLitBool True))  TBool) []
pattern PFalse = PData (DaConPrim (DaConBoundLit (PrimLitBool False)) TBool) []


-------------------------------------------------------------------------------
deriving instance Show a => Show (GExp         a)
deriving instance Show a => Show (GLets        a)
deriving instance Show a => Show (GCaps        a)
deriving instance Show a => Show (GClause      a)
deriving instance Show a => Show (GParam       a)
deriving instance Show a => Show (GArg         a)
deriving instance Show a => Show (GAltCase     a)
deriving instance Show a => Show (GAltMatch    a)
deriving instance Show a => Show (GGuardedExp  a)
deriving instance Show a => Show (GGuard       a)
deriving instance Show a => Show (GPat         a)
deriving instance Show a => Show (GCast        a)
deriving instance Show a => Show (GWitness     a)
deriving instance Show a => Show (GWiCon       a)
deriving instance Show a => Show (GXBindVarMT  a)

