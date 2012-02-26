-- | Sea Expressions (rvalues)
module DDC.Sea.Exp
        ( Exp   (..)
        , Lit   (..)
        , Var   (..))
where
import DDC.Sea.Prim


-- Exp ------------------------------------------------------------------------
-- | Expressions
data Exp n
        -- | Literal values.
        = XLit          (Lit n)

        -- | A value variable.
        | XVar          (Var n)

        -- | Invoke a primitive operator.
        | XPrim         Prim [Exp  n]
        deriving (Show, Eq)


-- Lit ------------------------------------------------------------------------
-- | A literal value.
data Lit n
        -- | The null pointer.
        = LNull

        -- | The tag of a data constructor.
        | LTag          n

        -- | A natural number used as an argument to some function or primitive
        --   in the runtime system. These don't hold values defined by the 
        --   actual source program.
        | LNat          Integer

        -- | A literal word of the given width.
        | LWord         Int Integer

        -- | A literal integer of the given width.
        | LInt          Int Integer

        -- | A literal float of the given width.
        | LFloat        Int Double
        deriving (Show, Eq)


-- Var ------------------------------------------------------------------------
-- | The name of something we can take the value of.
data Var n
        -- | A top-level name belonging to the runtime system,
        --   like a heap or stack base pointer.
        = VRts   n

        -- | The name of a top-level supercombinator.
        | VFun   n

        -- | Some automatic variable, managed by the back-end compiler.
        --   Could refer to either a boxed or unboxed value.
        --   Used for parameters to supers, and block local variables.
        | VAuto  n

        -- | A slot of the GC shadow stack.
        --   All pointers to objects in the heap must be on the slot stack
        --   when we do something that might cause a garbage collection.
        | VSlot  Int

        -- | A top-level Constant Applicative Form (CAF).
        --   Pointers to CAF objects are held on the slot stack.
        --   For each CAF there is another pointer that gives us the address
        --   of the slot. To get to the actual object we have to dereference
        --   the CafPtr twice.
        -- 
        --  @
        --                      SLOT(4)
        --   _ddcCAF_someVar -> SLOT(5) -> OBJ
        --                      SLOT(6)
        --  @
        | VCafPtr   n

        -- | The pointer to the CAF object.
        --   This is formed by dereferencing the corresponding CafPtr once only.
        | VCaf      n
        deriving (Show, Eq)



