
module Source.Slurp 
	( slurpFixTable
	, slurpImportModules
	, slurpKinds
	, slurpTopNames )
where

-----
import Util.Maybe
import Util

-----
import qualified Shared.Var	as Var
import Shared.Var		(NameSpace(..))
import Source.Exp
import Type.Util
import Shared.Error

stage	= "Source.Slurp"

-----
--- slurpFixTable
---	Strip the fixity table from some top level defs.
---
slurpFixTable 
	:: [Top a] -> [FixDef a]

slurpFixTable	 tops			
	= concat $ map slurpFixTable' tops

slurpFixTable'	 (PInfix sp mode prec syms) 
	= zip syms (repeat (prec, mode))

slurpFixTable'	 _			= []


-----
-- slurpImportModules
--	Strips off the list of modules imported by this one.
--
slurpImportModules
	:: Tree	a -> [Var.Module]

slurpImportModules	tops
	= concat 
	$ catMaybes 
	$ map (\x -> 
		case x of 
			PImportModule sp xx 	-> Just xx
			_		 	-> Nothing)
	$ tops


-----
-- slurpKinds
--
slurpKinds 
	:: Tree a -> [(Var, Kind)]

slurpKinds tree
	= catMaybes
	$ map (\p -> case p of
		PEffect sp v k		-> Just (v, k)
		PData   sp v vs fs	-> Just (v, makeDataKind vs)
		_			-> Nothing)
	$ tree
	
makeDataKind vs
 	= foldl (flip KFun) KData 
	$ map (\v -> kindOfSpace (Var.nameSpace v)) 
	$ reverse vs




-----
-- slurpTopNames
--	Slurp out binding occurances for top level vars from this Top.
--	Recover the NameSpace as well, so we can use this fn in the renamer.
--
slurpTopNames :: Show a => Top a -> [Var]
slurpTopNames p
 = case p of
	PPragma{}			-> []

 	PType 		sp v t		-> []
	PInfix 		sp im i vs	-> []

	PImportExtern 	sp v t mt	-> [ v { Var.nameSpace = NameValue }]
	PImportModule{}			-> []

	PForeign sp (OImport (OCCall mS v t))	
	 -> [bindSeaName mS v { Var.nameSpace = NameValue }]	

	PForeign sp (OImport (OExtern mS v t mT))	
	 -> [bindSeaName mS v { Var.nameSpace = NameValue }]	

	PData sp v vs ctors 		
	 -> ( v { Var.nameSpace = NameType }) 
	 :  [ c { Var.nameSpace = NameValue} 
	 	| (c, fs) <- ctors ]
	 
	PEffect	sp v k	
	 -> [v { Var.nameSpace = NameEffect }]

	PRegion sp v	
	 -> [v { Var.nameSpace = NameRegion }]

	PStmt (SSig sp v t) 
	 -> []
	
	PStmt (SBindPats sp v xx x)
	 -> [v { Var.nameSpace = NameValue }]

	-- classes		
	PClass sp v k	
	 -> [v { Var.nameSpace = NameClass}]

	PClassDict sp n ps inh sigs
	 -> [v { Var.nameSpace = NameValue }
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
	
