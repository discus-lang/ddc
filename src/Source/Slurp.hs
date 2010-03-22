
module Source.Slurp 
	( slurpFixTable
	, slurpImportModules
	, slurpKinds
	, slurpTopNames )
where
import Util.Data.Maybe
import Util
import Source.Exp
import Type.Util
import Type.Exp
import DDC.Main.Error
import DDC.Var.NameSpace
import Shared.Var		(Var)
import qualified Shared.Var	as Var


-----
stage	= "Source.Slurp"

--- | Slurp the fixity tables from this module.
slurpFixTable 
	:: [Top a] -> [FixDef a]

slurpFixTable	 tops			
	= concat $ map slurpFixTable' tops

slurpFixTable'	 (PInfix sp mode prec syms) 
	= zip syms (repeat (prec, mode))

slurpFixTable'	 _			= []


-- | Slurp out the list of modules imported by this one.
slurpImportModules
	:: Tree	a -> [Var.ModuleId]

slurpImportModules	tops
	= concat 
	$ catMaybes 
	$ map (\x -> 
		case x of 
			PImportModule sp xx 	-> Just xx
			_		 	-> Nothing)
	$ tops


-- | Slurp out the kinds of things defined at top level.
slurpKinds 
	:: Tree a -> [(Var, Kind)]

slurpKinds tree
	= catMaybes
	$ map (\p -> case p of
		PEffect sp v k		-> Just (v, k)
		PData   sp v vs fs	-> Just (v, makeDataKind vs)
		_			-> Nothing)
	$ tree


-- | Slurp out binding occurrences of top level cars.
--	Recover the namespace also, if we can determine it from the Top.
slurpTopNames :: Show a => Top a -> [Var]
slurpTopNames p
 = case p of
	PPragma{}			-> []
	PModule{}			-> []

	PTypeKind	sp v k		
	 -> [v { Var.nameSpace = NameType }]

 	PTypeSynonym 	sp v t		
	 -> [v { Var.nameSpace = NameType }]

	PInfix 		sp im i vs	
	 -> [] 
	
	PImportModule{}			-> []
	PExport{}			-> []

	PForeign sp (OImport mS v t mT)
	 -> [bindSeaName mS v { Var.nameSpace = NameValue }]	

	PForeign sp (OImportUnboxedData s v k)
	 -> [bindSeaName (Just s) v { Var.nameSpace = NameType}]

	PData sp v vs ctors 		
	 -> ( v { Var.nameSpace = NameType }) 
	 :  [ c { Var.nameSpace = NameValue} 
	 	| (c, fs) <- ctors ]
	 
	PEffect	sp v k	
	 -> [v { Var.nameSpace = NameEffect }]

	PRegion sp v	
	 -> [v { Var.nameSpace = NameRegion }]

	PStmt (SSig sp vs t) 
	 -> [v { Var.nameSpace = NameValue } | v <- vs]
	
	PStmt (SBindFun sp v pats alts)
	 -> [v { Var.nameSpace = NameValue }]

	-- classes		
	PClass sp v k	
	 -> [v { Var.nameSpace = NameClass}]

	PClassDict sp vClass ps inh sigs
	 -> vClass : 
	  [ v { Var.nameSpace = NameValue }
	 		| v <- catMap fst sigs ]
		
	PClassInst{}		
	 -> []
		
	-- projections
	PProjDict{}
	 -> []
		
	_	-> panic stage
		$  "slurpTopNames: no match for " % show p

bindSeaName ::	(Maybe String) -> Var -> Var
bindSeaName mS v
 = case mS of
	Nothing		-> v
	Just name	-> v { Var.info = Var.ISeaName name : Var.info v }
	
