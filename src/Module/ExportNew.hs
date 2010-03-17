{- 	Base this on cabal style format
	Will be easy enough to break into sections after converting to lines
	Use fast 'lines' fn
	suspend parser of each binding type, will only have to parse things we need.
	make renamer so that it only renames types etc, just the stuff we need to be able
		to defer the parsing/ renaming of the other bindings
	will need to load type decls at least.
	load interface files as bytestrings. Only convert bits to char strings as needed.
	
	EXAMPLE ----
	DDC Module Interface File
	ddc-version: 
	module-name: 
	imported-modules:

 
	binding 
 		source-position: 
		

 		source-type:
   			Int -(!e1)> Int
			:- 

 		sea-type:
-}


module Module.ExportNew
	(makeInterface)
where
import Module.Interface
import Module.Scrape
import qualified Data.Set	as Set
import qualified Data.Map	as Map

makeInterface
	:: Scrape
	-> Interface
	
makeInterface
	scrape
 = Interface
	{ intModuleId		= scrapeModuleName scrape
	, intImportedModules	= Set.fromList $ scrapeImported scrape
	, intData		= Map.empty
	, intRegion		= Map.empty
	, intEffect		= Map.empty
	, intClass		= Map.empty
	, intClassDecl		= Map.empty
	, intClassInst		= Map.empty
	, intProjDict		= Map.empty
	, intInfix		= Map.empty
	, intBind		= Map.empty }
	
	