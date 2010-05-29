
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
import DDC.Main.Error
import DDC.Type
import DDC.Var

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
	:: Tree	a -> [ModuleId]

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
		PKindSig sp v k		-> Just (v, k)
		PData    sp v vs fs	-> Just (v, makeDataKind vs)
		_			-> Nothing)
	$ tree


-- | Slurp out binding occurrences of top level cars.
--	Recover the namespace also, if we can determine it from the Top.
slurpTopNames :: Show a => Top a -> [Var]
slurpTopNames p
 = case p of
	PPragma{}		-> []
	PModule{}		-> []
	PInfix sp im i vs	-> [] 
	PImportModule{}		-> []
	PExport{}		-> []

	PKindSig sp v k		
	 | resultKind k == kEffect	-> [v { varNameSpace = NameEffect }]
	 | otherwise			-> [v { varNameSpace = NameType }]

 	PTypeSynonym  sp v t		
	 -> [v { varNameSpace = NameType }]

	PForeign sp (OImport mS v t mT)
	 -> [bindSeaName mS v { varNameSpace = NameValue }]	

	PForeign sp (OImportUnboxedData s v k)
	 -> [bindSeaName (Just s) v { varNameSpace = NameType}]

	PData sp v vs ctors 		
	 -> ( v { varNameSpace = NameType }) 
	 :  [ c { varNameSpace = NameValue} 
	 	| (c, fs) <- ctors ]
	
	PRegion sp v	
	 -> [v { varNameSpace = NameRegion }]

	PStmt (SSig sp vs t) 
	 -> [v { varNameSpace = NameValue } | v <- vs]
	
	PStmt (SBindFun sp v pats alts)
	 -> [v { varNameSpace = NameValue }]

	PClass sp v k	
	 -> [v { varNameSpace = NameClass}]

	PClassDict sp vClass ps inh sigs
	 -> vClass : 
	  [ v { varNameSpace = NameValue }
	 		| v <- catMap fst sigs ]
		
	PClassInst{}		-> []
	PProjDict{}		-> []
		
	_	-> panic stage
		$  "slurpTopNames: no match for " % show p

bindSeaName ::	(Maybe String) -> Var -> Var
bindSeaName mS v
 = case mS of
	Nothing		-> v
	Just name	-> v { varInfo = ISeaName name : varInfo v }
	
