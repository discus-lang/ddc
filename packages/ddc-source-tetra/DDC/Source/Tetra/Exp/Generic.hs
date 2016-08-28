{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | Abstract syntax for Tetra Source expressions.
module DDC.Source.Tetra.Exp.Generic
        ( -- * Classes
          HasAnonBind   (..)
        , Anon          (..)

          -- * Types
          -- ** Type Functions
        , GTAnnot
        , GTBindVar,    GTBoundVar
        , GTBindCon,    GTBoundCon
        , GTPrim

          -- ** Syntax
        , GType         (..)
        , GTyCon        (..)

        , pattern TApp2, pattern TApp3
        , pattern TApp4, pattern TApp5

        , pattern TVoid, pattern TUnit
        , pattern TFun
        , pattern TBot,  pattern TUnion
        , pattern TPrim

          -- ** Dictionaries
        , ShowGType

          -- * Terms
          -- ** Type Functions
        , GXAnnot
        , GXBindVar, GXBoundVar
        , GXBindCon, GXBoundCon
        , GXPrim

          -- ** Syntax
        , GXBindVarMT   (..)
        , GExp          (..)
        , GLets         (..)
        , GPat          (..)
        , GClause       (..)
        , GParam        (..)
        , GGuard        (..)
        , GGuardedExp   (..)
        , GAltMatch     (..)
        , GAltCase      (..)
        , GCast         (..)
        , GWitness      (..)
        , GWiCon        (..)
        , DaCon         (..)

          -- * Dictionaries
        , ShowLanguage)
where
import DDC.Type.Exp.Generic.Binding
import DDC.Type.Exp.Generic.Exp
import DDC.Core.Exp                     (DaCon (..))


-------------------------------------------------------------------------------
-- Type functions associated with the language AST.

-- | Yield the type of annotations.
type family GXAnnot    l

-- | Yield the type of binding occurrences of variables.
type family GXBindVar  l

-- | Yield the type of bound occurrences of variables.
type family GXBoundVar l

-- | Yield the type of binding occurrences of constructors.
type family GXBindCon  l

-- | Yield the type of bound occurrences of constructors.
type family GXBoundCon l

-- | Yield the type of primitive operator names.
type family GXPrim     l


class HasAnonBind l where
 isAnon :: l -> GXBindVar l -> Bool


-- | A possibly typed binding.
data GXBindVarMT l 
        = XBindVarMT (GXBindVar l) (Maybe (GType l))


-------------------------------------------------------------------------------
-- | Well-typed expressions have types of kind `Data`.
data GExp l
        ---------------------------------------------------
        -- Core Language Constructs.
        --   These are also in the core language, and after desugaring only
        --   these constructs are used.
        --
        = XAnnot    !(GXAnnot l) !(GExp   l)

        -- | Value variable   or primitive operation.
        | XVar      !(GXBoundVar l)

        -- | Primitive values.
        | XPrim     !(GXPrim  l)

        -- | Data constructor or literal.
        | XCon      !(DaCon (GXBoundCon l) (GType l))

        -- | Type abstraction (level-1).
        | XLAM      !(GXBindVarMT l) !(GExp l)

        -- | Value and Witness abstraction (level-0).
        | XLam      !(GXBindVarMT l) !(GExp l)

        -- | Application.
        | XApp      !(GExp  l)   !(GExp l)

        -- | A non-recursive let-binding.
        | XLet      !(GLets l)   !(GExp l)

        -- | Case branching.
        | XCase     !(GExp  l)   ![GAltCase l]

        -- | Type cast.
        | XCast     !(GCast l)   !(GExp l)

        -- | Type can appear as the argument of an application.
        | XType     !(GType l)

        -- | Witness can appear as the argument of an application.
        | XWitness  !(GWitness l)


        ---------------------------------------------------
        -- Sugar Constructs.
        --  These constructs are eliminated by the desugarer.
        --
        -- | Some expressions and infix operators that need to be resolved
        --   into proper function applications.
        | XDefix    !(GXAnnot l) [GExp l]

        -- | Use of a naked infix operator, like in 1 + 2.
        --   INVARIANT: only appears in the list of an XDefix node.
        | XInfixOp  !(GXAnnot l) String

        -- | Use of an infix operator as a plain variable, like in (+) 1 2.
        --   INVARIANT: only appears in the list of an XDefix node.
        | XInfixVar !(GXAnnot l) String

        -- | Match expression with default.
        --   Similar to a case expression, except that if an alternative
        --   fails then we try the next one instead of failing.
        --   If none of the alternatives succeeds then the overall value
        --   is the value of the default expression.
        | XMatch    !(GXAnnot l) ![GAltMatch l] !(GExp l)

        -- | Where expression defines a group of recursive clauses,
        --   and is desugared to a letrec.
        | XWhere    !(GXAnnot l) !(GExp l) ![GClause l]

        -- | Lambda abstraction which matches its argument against
        --   a single pattern.
        | XLamPat   !(GXAnnot l) !(GPat l) !(Maybe (GType l)) !(GExp l)

        -- | Lambda abstraction that matches its argument against
        --   the given alternatives.
        | XLamCase  !(GXAnnot l) ![GAltCase l]


-- | Possibly recursive bindings.
--   Whether these are taken as recursive depends on whether they appear
--   in an XLet or XLetrec group.
data GLets l
        ---------------------------------------------------
        -- Core Language Constructs
        -- | Non-recursive expression binding.
        = LLet      !(GXBindVarMT l) !(GExp l)

        -- | Recursive binding of lambda abstractions.
        | LRec     ![(GXBindVarMT l,   GExp l)]

        -- | Bind a local region variable,
        --   and witnesses to its properties.
        | LPrivate ![GXBindVar l] !(Maybe (GType l)) ![(GXBindVar l, GType l)]

        ---------------------------------------------------
        -- Sugar Constructs
        -- | A possibly recursive group of binding clauses.
        -- 
        --   Multiple clauses in the group may be part of the same function.
        | LGroup   ![GClause l]


-- | Binding clause
data GClause l
        -- | A separate type signature.
        = SSig   !(GXAnnot l) !(GXBindVar l)   !(GType l)

        -- | A function binding using pattern matching and guards.
        | SLet   !(GXAnnot l) !(GXBindVarMT l) ![GParam l] ![GGuardedExp l]


-- | Parameter for a binding.
data GParam l
        -- | Type parameter with optional kind.
        = MType    !(GXBindVar l) (Maybe (GType l))

        -- | Witness parameter with optional type.
        | MWitness !(GXBindVar l) (Maybe (GType l))

        -- | Value paatter with optional type.
        | MValue   !(GPat l)      (Maybe (GType l))


-- | Patterns.
data GPat l
        -- | The default pattern always succeeds.
        = PDefault

        -- | Give a name to the value matched by a pattern.
        | PAt    !(GXBindVar l) !(GPat l)

        -- | The variable pattern always succeeds and binds the value
        --   to the new variable.
        | PVar   !(GXBindVar l) 

        -- | Match a data constructor and bind its arguments.
        | PData  !(DaCon (GXBoundCon l) (GType l)) ![GPat l]


-- | An expression with some guards.
data GGuardedExp l
        = GGuard !(GGuard l) !(GGuardedExp l)
        | GExp   !(GExp   l)


-- | Expression guards.
data GGuard l
        = GPat   !(GPat l) !(GExp l)
        | GPred  !(GExp l)
        | GDefault


-- | Case alternative.
--   If the pattern matches then bind the variables then enter
--   the guarded expression.
data GAltCase l
        = AAltCase   !(GPat l) ![GGuardedExp l]


-- | Match alternative.
--   This is like a case alternative except that the match expression
--   does not give us a head pattern.
data GAltMatch l
        = AAltMatch  !(GGuardedExp l)


-- | Type casts.
data GCast l
        -- | Weaken the effect of an expression.
        --   The given effect is added to the effect
        --   of the body.
        = CastWeakenEffect  !(GType l)
        
        -- | Purify the effect (action) of an expression.
        | CastPurify !(GWitness l)

        -- | Box a computation, 
        --   capturing its effects in the S computation type.
        | CastBox

        -- | Run a computation,
        --   releasing its effects into the environment.
        | CastRun


-- | Witnesses.
data GWitness l
        -- | Witness annotation
        = WAnnot !(GXAnnot l)  !(GWitness l)

        -- | Witness variable.
        | WVar   !(GXBoundVar l)

        -- | Witness constructor.
        | WCon   !(GWiCon l)

        -- | Witness application.
        | WApp   !(GWitness l) !(GWitness l)

        -- | Type can appear as an argument of a witness application.
        | WType  !(GType l)


-- | Witness constructors.
data GWiCon l
        -- | Witness constructors defined in the environment.
        --   In the interpreter we use this to hold runtime capabilities.
        --   The attached type must be closed.
        = WiConBound   !(GXBoundVar l) !(GType l)


-------------------------------------------------------------------------------
type ShowLanguage l
        = ( Show l
          , ShowGType l
          , Show (GXAnnot    l)
          , Show (GXBindVar l), Show (GXBoundVar l)
          , Show (GXBindCon l), Show (GXBoundCon l)
          , Show (GXPrim l))

deriving instance ShowLanguage l => Show (GExp         l)
deriving instance ShowLanguage l => Show (GLets        l)
deriving instance ShowLanguage l => Show (GClause      l)
deriving instance ShowLanguage l => Show (GParam       l)
deriving instance ShowLanguage l => Show (GAltCase     l)
deriving instance ShowLanguage l => Show (GAltMatch    l)
deriving instance ShowLanguage l => Show (GGuardedExp  l)
deriving instance ShowLanguage l => Show (GGuard       l)
deriving instance ShowLanguage l => Show (GPat         l)
deriving instance ShowLanguage l => Show (GCast        l)
deriving instance ShowLanguage l => Show (GWitness     l)
deriving instance ShowLanguage l => Show (GWiCon       l)
deriving instance ShowLanguage l => Show (GXBindVarMT  l)

