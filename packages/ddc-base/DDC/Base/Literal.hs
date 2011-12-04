
module DDC.Base.Literal
        (Literal(..))
where

-- | A literal value.
--	This stores literal values as we see them in the source program.
--	We need enough numeric precison here to represent any possible
--	value we might get in the source.
data Literal
	= LBool		Bool		-- ^ Boolean.
	| LInteger	Integer		-- ^ An integer.
	| LFloat	Double		-- ^ A floating point number.
	| LChar		Char		-- ^ A character.
	| LString	String		-- ^ A string.
	deriving (Show, Eq)
