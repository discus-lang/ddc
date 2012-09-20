{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | A position in a source file
module DDC.Base.SourcePos
	(SourcePos(..))
where
import DDC.Main.Pretty


-- | A position in a source file
data SourcePos		
	= SourcePos 
		( String	-- path to file
		, Int		-- line number
		, Int)		-- column number
	deriving (Show, Eq)
	

instance Pretty SourcePos PMode where
 ppr (SourcePos (f, l, c))	
	= ppr $ f ++ ":" ++ show l ++ ":" ++ show (c - 1)
