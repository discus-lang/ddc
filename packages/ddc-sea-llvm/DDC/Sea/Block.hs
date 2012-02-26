
-- | Basic Blocks
module DDC.Sea.Block
        ( Label         (..)
        , Block         (..)
        , Stmt          (..)
        , Assign        (..)
        , Alt           (..)
        , Pat           (..))
where

-- | A code label that can be jumped to.
data Label n
        = Label n
        deriving (Show, Eq)


-- | A basic block.
data Block n
        = Block 
        { -- | Label of the block to be jumped to.
          blockLabel    :: Label n

          -- | Automatic variables.
        , blockAuto     :: [(n, Type n)]

          -- | GC slots which are pushed on the stack when the block is
          --   entered, and popped of when left.
        , blockSlot     :: [Type n] 

          -- | Statements to exectute.
        , blockStmts    :: [Stmt n] }
        deriving (Show, Eq)


-- | Statement.
data Stmt n
        -- | Comment to insert into the output.
        = SComment String

        -- | Evaluate an expression and assign it to somewhere.
        | SAssign  (Assign n) (Exp n)

        -- | Jump to a label.
        | SJump    (Label  n)

        -- | Return from the enclosing function, 
        --   with an optional return value.
        | SReturn  (Maybe (Var n))

        -- | Switch on the given variable.
        | SSwitch  (Var n) [Alt n]
        deriving (Show, Eq)


-- | An assignable.
data Assign n
        -- | Drop the value on the floor.
        = ANone

        -- | Assign value to an automatic variable.
        | AAuto n

        -- | Assign value to a slot.
        | ASlot Int
        deriving (Show, Eq)


-- | A case or switch alternative.
data Alt n
        -- | If all the guards succeed then run the stmts.
        = AAlt     (Pat n) (Block n)
        deriving (Show, Eq)


-- | Patterns.
data Pat n
        -- | The default pattern always matches.
        = PDefault

        -- | A natural number.
        | PNat Integer

        -- | The tag of some boxed data object.
        | PTag n

        -- | The tag of a tunk.
        | PTagThunk

        -- | The tag of an indirection.
        | PTagIndir
        deriving (Show, Eq)

