
module DDC.Sea.Exp.Name
	( Name	(..)
	, varOfName)
where
import DDC.Var
import Data.Function

-- | The name of something we can take the value of.
data Name
	-- | A top-level name belonging to the runtime system,
	--   like a heap or stack base pointer.
	= NRts	 Var

	-- | The name of a top-level supercombinator.
	| NSuper Var

	-- | Some automatic variable, managed by the back-end compiler.
	--   Could refer to either a boxed or unboxed value.
	--   Used for parameters to supers, and block local variables.
	| NAuto  Var

	-- | A slot of the GC shadow stack.
	--   All pointers to objects in the heap must be on the slot stack
	--   when we do something that might cause a garbage collection.
	| NSlot 
		{ -- | The original variable this slot is standing in for.
		  nameSlotVar 	:: Var
		  -- | Number of the slot in the current frame.
		, nameSlotNum	:: Int}

	-- | A top-level Constant Applicative Form (CAF).
	--   Pointers to CAF objects are held on the slot stack.
	--   For each CAF there is another pointer that gives us the address of the slot.
        --   To get to the actual object we have to dereference the CafPtr twice.
        --  @
        --                      SLOT(4)
	--   _ddcCAF_someVar -> SLOT(5) -> OBJ
	--                      SLOT(6)
	--  @
	| NCafPtr   Var

	-- | The pointer to the CAF object.
	--   This is formed by dereferencing the corresponding CafPtr once only.
	| NCaf	    Var
	
	deriving (Show, Eq)


instance Ord Name where
 compare 	= compare `on` varOfName


-- | Get the `Var` of a `Name`.
varOfName :: Name -> Var
varOfName name
 = case name of
	NRts	v	-> v
	NSuper	v	-> v
	NAuto	v	-> v
	NSlot	v _	-> v
	NCafPtr v	-> v
	NCaf	v	-> v

