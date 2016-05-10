{-# LANGUAGE TypeFamilies, UndecidableInstances #-}

-- | Abstract syntax for Tetra Source expressions.
module DDC.Source.Tetra.Exp.Generic
        ( -- * Classes
          HasAnonBind   (..)

          -- * Expressions
        , GName
        , GAnnot
        , GBind
        , GBound
        , GPrim
        , GExp          (..)
        , GLets         (..)
        , GAlt          (..)
        , GPat          (..)
        , GClause       (..)
        , GGuardedExp   (..)
        , GGuard        (..)
        , GCast         (..)
        , DaCon         (..)

          -- * Witnesses
        , GWitness      (..)
        , GWiCon        (..)

          -- * Dictionaries
        , ShowLanguage)
where
import DDC.Type.Exp     
import DDC.Type.Sum                     ()
import qualified DDC.Type.Exp           as T

import DDC.Core.Exp
        ( DaCon         (..))


-------------------------------------------------------------------------------
-- | Type functions associated with the language AST.
type family GName  l
type family GAnnot l
type family GBind  l
type family GBound l
type family GPrim  l

class HasAnonBind l where
 isAnon :: l -> GBind l -> Bool


-------------------------------------------------------------------------------
-- | Well-typed expressions have types of kind `Data`.
data GExp l
        ---------------------------------------------------
        -- Core Language Constructs.
        --   These are also in the core language, and after desugaring only
        --   these constructs are used.
        --
        -- | Value variable   or primitive operation.
        = XVar      !(GAnnot l) !(GBound l)

        -- | Primitive values.
        | XPrim     !(GAnnot l) !(GPrim  l)

        -- | Data constructor or literal.
        | XCon      !(GAnnot l) !(DaCon (GName l) (Type (GName l)))

        -- | Type abstraction (level-1).
        | XLAM      !(GAnnot l) !(GBind l) !(GExp l)

        -- | Value and Witness abstraction (level-0).
        | XLam      !(GAnnot l) !(GBind l) !(GExp l)

        -- | Application.
        | XApp      !(GAnnot l) !(GExp  l) !(GExp l)

        -- | A non-recursive let-binding.
        | XLet      !(GAnnot l) !(GLets l) !(GExp l)

        -- | Case branching.
        | XCase     !(GAnnot l) !(GExp  l) ![GAlt l]

        -- | Type cast.
        | XCast     !(GAnnot l) !(GCast l) !(GExp l)

        -- | Type can appear as the argument of an application.
        | XType     !(GAnnot l) !(Type  (GName l))

        -- | Witness can appear as the argument of an application.
        | XWitness  !(GAnnot l) !(GWitness l)


        ---------------------------------------------------
        -- Sugar Constructs.
        --  These constructs are eliminated by the desugarer.
        --
        -- | Some expressions and infix operators that need to be resolved
        --   into proper function applications.
        | XDefix    !(GAnnot l) [GExp l]

        -- | Use of a naked infix operator, like in 1 + 2.
        --   INVARIANT: only appears in the list of an XDefix node.
        | XInfixOp  !(GAnnot l) String

        -- | Use of an infix operator as a plain variable, like in (+) 1 2.
        --   INVARIANT: only appears in the list of an XDefix node.
        | XInfixVar !(GAnnot l) String


-- | Possibly recursive bindings.
--   Whether these are taken as recursive depends on whether they appear
--   in an XLet or XLetrec group.
data GLets l
        ---------------------------------------------------
        -- Core Language Constructs
        -- | Non-recursive expression binding.
        = LLet     !(GBind l) !(GExp l)

        -- | Recursive binding of lambda abstractions.
        | LRec     ![(GBind l, GExp l)]

        -- | Bind a local region variable,
        --   and witnesses to its properties.
        | LPrivate ![GBind l] !(Maybe (Type (GName l))) ![GBind l]

        ---------------------------------------------------
        -- Sugar Constructs
        -- | A possibly recursive group of binding clauses.
        -- 
        --   Multiple clauses in the group may be part of the same function.
        | LGroup   ![GClause l]


-- | Binding clause
data GClause l
        -- | A separate type signature.
        = SSig   !(GAnnot l) !(GBind l) !(Type (GName l))

        -- | A function binding using pattern matching and guards.
        | SLet   !(GAnnot l) !(GBind l) ![GPat l]  ![GGuardedExp l]


-- | Case alternatives.
data GAlt l
        = AAlt   !(GPat l) ![GGuardedExp l]


-- | Patterns.
data GPat l
        -- | The default pattern always succeeds.
        = PDefault

        -- | Match a data constructor and bind its arguments.
        | PData  !(DaCon (GName l) (Type (GName l))) ![GBind l]


-- | An expression with some guards.
data GGuardedExp l
        = GGuard !(GGuard l) !(GGuardedExp l)
        | GExp   !(GExp   l)


-- | Expression guards.
data GGuard l
        = GPat   !(GPat l) !(GExp l)
        | GPred  !(GExp l)
        | GDefault


-- | Type casts.
data GCast l
        -- | Weaken the effect of an expression.
        --   The given effect is added to the effect
        --   of the body.
        = CastWeakenEffect  !(Effect (GName l))
        
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
        = WAnnot !(GAnnot l)  !(GWitness l)

        -- | Witness variable.
        | WVar   !(GBound l)

        -- | Witness constructor.
        | WCon   !(GWiCon l)

        -- | Witness application.
        | WApp   !(GWitness l) !(GWitness l)

        -- | Type can appear as an argument of a witness application.
        | WType  !(T.Type (GName l))


-- | Witness constructors.
data GWiCon l
        -- | Witness constructors defined in the environment.
        --   In the interpreter we use this to hold runtime capabilities.
        --   The attached type must be closed.
        = WiConBound   !(GBound l) !(T.Type (GName l))


-------------------------------------------------------------------------------
type ShowLanguage l
        = ( Show l
          , Show (GName l), Show (GAnnot l)
          , Show (GBind l), Show (GBound l), Show (GPrim l))

deriving instance ShowLanguage l => Show (GExp        l)
deriving instance ShowLanguage l => Show (GLets       l)
deriving instance ShowLanguage l => Show (GClause     l)
deriving instance ShowLanguage l => Show (GAlt        l)
deriving instance ShowLanguage l => Show (GGuardedExp l)
deriving instance ShowLanguage l => Show (GGuard      l)
deriving instance ShowLanguage l => Show (GPat        l)
deriving instance ShowLanguage l => Show (GCast       l)
deriving instance ShowLanguage l => Show (GWitness    l)
deriving instance ShowLanguage l => Show (GWiCon      l)

