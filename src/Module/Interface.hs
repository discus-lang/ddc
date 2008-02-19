
module Module.Interface
	( Interface (..) )

where

import Shared.Var		(Module)
import Shared.Base
import Source.Exp		as S

-----
-- Interface
--	Holds useful information about an imported module,
--	derived from its .ti interface file.
--
data	Interface =
	Interface
	{ name		:: String

	, filePath	:: FilePath
	, fileDir	:: FilePath
	, fileName	:: String
     
	, imports	:: [Module]		-- Other modules imported by this one.
	, sourceTree	:: [S.Top SourcePos] 
	}

  	deriving (Show)
  
