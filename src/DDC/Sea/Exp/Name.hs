
module DDC.Sea.Exp.Name
	( Name	(..)
	, varOfName)
where
import DDC.Var
import Data.Function

-- | The name of something we can take the value of.
data Name
	-- | Some automatic variable, managed by the back-end compiler.
	--   Could refer to either a boxed or unboxed value.
	--   Used for parameters to supers, and block local "scratch" vars.
	= NAuto  Var

	-- | A slot of the GC shadow stack.
	--   All pointers to objects in the heap must be on the slot stack
	--   when we do something that might cause a garbage collection.
	| NSlot 
		{ -- | The original variable this slot is standing in for.
		  nameSlotVar 	:: Var
		  -- | Number of the slot in the current frame.
		, nameSlotNum	:: Int}

	-- | A top-level Constant Applicative Form (CAF).
	| NCaf   Var

	-- | A top-level supercombinator.
	| NSuper Var

	-- | A top-level name belonging to the runtime system,
	--   like a heap or stack base pointer.
	| NRts	 Var
	deriving (Show, Eq)


instance Ord Name where
 compare 	= compare `on` varOfName


-- | Get the `Var` of a `Name`.
varOfName :: Name -> Var
varOfName name
 = case name of
	NAuto	v	-> v
	NCaf	v	-> v
	NSuper	v	-> v
	NSlot	v _	-> v
	NRts	v	-> v

