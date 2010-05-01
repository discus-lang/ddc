
-- | Kind constructors.
module DDC.Type.KiCon 
	(KiCon(..))
where
import DDC.Var


-- | Kind constructors.
data KiCon
	-- Atomic kinds.
	= KiConValue			-- ^ The kind of data (value) types.
	| KiConRegion			-- ^ The kind of region types.
	| KiConEffect			-- ^ The kind of effect types.
	| KiConClosure			-- ^ The kind of closure types.

	-- | A kind constructor defined in the source program.
	--	These aren't interpreted in any special way by the compiler.
	| KiConVar Var

	-- Constancy of regions.
	| KiConConst			-- ^ Constancy for a single region.
	| KiConDeepConst		-- ^ Constancy for all the regions in a type.

	-- Mutability of regions.
	| KiConMutable			-- ^ Mutability for a single region.
	| KiConDeepMutable		-- ^ Mutability for all the regions in a type.

	-- Region might contain thunks.
	| KiConLazy			-- ^ Thunks may appear a single region.
	| KiConHeadLazy			-- ^ Thunks may appear in the primary (head) region of a type.

	-- Region does not contain thunks.
	| KiConDirect			-- ^ Absence of thunks for a single region.
		
	-- | Given effect is pure.
	| KiConPure
	
	-- | Given closure is empty.
	| KiConEmpty
	deriving (Show, Eq, Ord)
