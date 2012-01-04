{-# LANGUAGE ParallelListComp #-}

module Main where

import DDC.Test.Var
import DDC.Test.Type
import DDC.Test.Data
import DDC.Main.Pretty
import qualified Data.Map	as Map


dataDefs
 = 	[ createDataDef
		vInt		[r1]
		[ (varV "MkInt", []) ]
	
	, createDataDef 
		(varT "List") 	[r1, a1]
		[ (varV "Nil",  [])
		, (varV "Cons", [a1]) ]
	
	, createDataDef
		(varT "IntFun")	[r1, r2, r3, r4, e1, c1]
		[ (varV "SInt",	[tInt' r1]) 
		, (varV "SFun", [tFun (tInt' r3) (tInt' r4) e1 c1]) ]

	, createDataDef
		(varT "FunOrder2") [r1, a1, a2, a3, c1, e1, c2, e2]
		[ ( varV "FunOrder2"
		  , [ tFun (tFun a1 a2 e1 c1) a3 e2 c2 ] ) ]
	]


dataDefsAnnot
	= annotMaterialInDataDefs 
		Map.empty
		(Map.fromList [(dataDefName def, def) | def <- dataDefs])
	
main 
 =	putStrLn 
		$ pprStrPlain 
		$ vcat 
		$ Map.elems dataDefsAnnot
		

