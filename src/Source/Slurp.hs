
module Source.Slurp 
	( slurpFixTable
	, slurpDataDefs
	, slurpImportModules
	, slurpImportExterns
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
	:: [Top] -> [FixDef]

slurpFixTable	 tops			
	= concat $ map slurpFixTable' tops

slurpFixTable'	 (PInfix mode prec syms) 
	= zip syms (repeat (prec, mode))

slurpFixTable'	 _			= []


-----
-- slurpDataDefs
--	Strips data constructor definitions from a Source.Exp tree.
--
slurpDataDefs 
	:: Tree	-> [DataDef]

slurpDataDefs	 	tops		
	= catMaybes 
	$ map 	(\top ->
		 case top of
	 		PData v vs cs	-> Just (v, vs, cs)
			_		-> Nothing)
	$ tops


-----
-- slurpImportModules
--	Strips off the list of modules imported by this one.
--
slurpImportModules
	:: Tree	-> [Var.Module]

slurpImportModules	tops
	= concat 
	$ catMaybes 
	$ map (\x -> 
		case x of 
			PImportModule xx -> Just xx
			_		 -> Nothing)
	$ tops


-----
-- slurpImportExterns
--	Strips list the list of external functions imported by this module.
--
slurpImportExterns 
	:: Tree	-> [(Var, Type)]

slurpImportExterns	tops
	= concat $ catMaybes 
	$ map	(\top ->
		 case top of
			PImportExtern v t o  	-> Just [(v, t)]
			_		  	-> Nothing)
	$ tops


-----
-- slurpKinds
--
slurpKinds 
	:: Tree	-> [(Var, Kind)]

slurpKinds tree
	= catMaybes
	$ map (\p -> case p of
		PEffect v k	-> Just (v, k)
		PData   v vs fs	-> Just (v, makeDataKind vs)
		_		-> Nothing)
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
slurpTopNames :: Top -> [Var]
slurpTopNames p
 = case p of
	PPragma{}			-> []

 	PType 		sp v t		-> []
	PInfix 		im i vs		-> []

	PImportExtern 	v t mt		-> [ v { Var.nameSpace = NameValue }]
	PImportModule{}			-> []

	PForeign 	(OImport (OCCall mS v t))	
	 -> [bindSeaName mS v { Var.nameSpace = NameValue }]	

	PForeign 	(OImport (OExtern mS v t mT))	
	 -> [bindSeaName mS v { Var.nameSpace = NameValue }]	

	PData		v vs ctors 		
	 -> ( v { Var.nameSpace = NameType }) 
	 :  [ c { Var.nameSpace = NameValue} 
	 	| (c, fs) <- ctors ]
	 
	PEffect		v k	
	 -> [v { Var.nameSpace = NameEffect }]

	PRegion 	v	
	 -> [v { Var.nameSpace = NameRegion }]

	PStmt (SSig sp v t) 
	 -> [] -- [v { Var.nameSpace = NameValue }]
	
	PStmt (SBind sp (Just v) x) 
	 -> [v { Var.nameSpace = NameValue }]
	 	
	PStmt (SBindPats sp v xx x)
	 -> [v { Var.nameSpace = NameValue }]

	-- classes		
	PClass v k	
	 -> [v { Var.nameSpace = NameClass}]

	PClassDict n ps inh sigs
	 -> [v { Var.nameSpace = NameValue }
	 	| v <- catMap fst sigs ]
		
	PClassInst{}		
	 -> []
		
	-- projections
	PProjDict t ss
	 -> []
		
	_	-> panic stage
		$  "slurpTopNames: no match for " % show p

bindSeaName ::	(Maybe String) -> Var -> Var
bindSeaName mS v
 = case mS of
	Nothing		-> v
	Just name	-> v { Var.info = Var.ISeaName name : Var.info v }
	
