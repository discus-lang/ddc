
module Module.Interface
(
	Interface (..), 
	initInterface,
)

where

-----
import Debug.Trace

-----
import Util.Pretty
import Util.List
import Util.Monad
import Util.Tuple

-----
import qualified Shared.Var	as Var
import Shared.Var 	(Var, Module)
import Shared.Pretty

import qualified Shared.VarGen	as VarGen
import Shared.VarGen (VarGen)

import qualified Source.Exp		as S
import qualified Source.Pretty		as S
import qualified Source.Slurp		as S

import qualified Source.Lexer		as S
import qualified Source.Parser		as S
import qualified Source.Rename		as S
import Source.RenameM		(Rename(..), RenameM)
import qualified Source.Token		as Token

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
	, sourceTree	:: [S.Top] 
	}

  	deriving (Show)
  
-----
initInterface
	= Interface
	{ name		= ""

	, filePath	= ""
	, fileDir	= ""
	, fileName	= ""
	
	, imports	= []
	, sourceTree	= []
	}


-----
tagInfixTable		= "-- infix table\n"
tagDataDefs		= "-- data defs\n"
tagConstructors		= "-- constructors\n"
tagImportExterns	= "-- import externs\n"
tagBoundTypes		= "-- bound types\n"



