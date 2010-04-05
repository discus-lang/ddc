
-- | Kind constructors.
module DDC.Type.KiCon 
	(KiCon(..))
where
import DDC.Var
import DDC.Main.Pretty


-- | Kind constructors.
data KiCon
	-- | A Kind constructor defined in the source program.
	--	These aren't interpreted in any special way by the compiler.
	= KiCon Var

	-- Atomic kinds.
	| KiConValue			-- ^ The kind of data (value) types.
	| KiConRegion			-- ^ The kind of region types.
	| KiConEffect			-- ^ The kind of effect types.
	| KiConClosure			-- ^ The kind of closure types.
	
	-- Mutability of regions.
	| KiConMutable			-- ^ Mutability for a single region.
	| KiConDeepMutable		-- ^ Mutability for all the regions in a type.

	-- Constancy of regions.
	| KiConConst			-- ^ Constancy for a single region.
	| KiConDeepConst		-- ^ Constancy for all the regions in a type.

	-- Region might contain thunks.
	| KiConLazy			-- ^ Thunks may appear a single region.
	| KiConHeadLazy			-- ^ Thunks may appear in the primary (head) region of a type.

	-- Region does not contain thunks.
	| KiConDirect			-- ^ Absence of thunks for a single region.
		
	-- | Given effect is pure.
	| KiConPure
	
	-- | Given closure is empty.
	| KiConEmpty
	deriving (Show, Eq)


instance Pretty KiCon PMode where
 ppr con
  = case con of
	KiCon v			-> ppr v
	KiConValue		-> ppr "*"
	KiConRegion		-> ppr "%"
	KiConEffect		-> ppr "!"
	KiConClosure		-> ppr "$"
	KiConMutable		-> ppr "Mutable"
	KiConDeepMutable	-> ppr "DeepMutable"
	KiConConst		-> ppr "Const"
	KiConDeepConst		-> ppr "DeepConst"
	KiConLazy		-> ppr "Lazy"
	KiConHeadLazy		-> ppr "HeadLazy"
	KiConDirect		-> ppr "Direct"
	KiConPure		-> ppr "Pure"
	KiConEmpty		-> ppr "Empty"

