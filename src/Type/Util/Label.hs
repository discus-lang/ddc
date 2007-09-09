
module Type.Util.Label
(
--	labelFunsT,
--	unlabelFunsT
)

where

import Util

import qualified Shared.Var	as Var
import Shared.Var		(Var, (=~=))

import Type.Exp
import Type.Plate

import Type.Util.Bits
import Type.Util.Pack

{-
-----------------------
-- labelFunsT
-- 
labelFunsT :: Type -> Type
labelFunsT    t
 = let
 	(t2, (_, fs2))
		= runState 
			(transformTM labelT t) 
			(0, [])
		
   in addFetters t2 (reverse fs2)


labelT ::	Type -> State (Int, [Fetter]) Type
labelT	t
 = case t of
	TFun t1 t2 eff clo
	 |  (isENil eff || isEVar eff || isEClass eff)
	  -> do
	 	t1'		<- labelT t1
		t2'		<- labelT t2
		return		$  TFun t1' t2' eff clo

 	TFun t1 t2 eff clo
	 -> do
	 	(code, table) 	<- get
		let names	=  map (\x -> [x]) 		['A' .. 'Z'] 
				++ map (\x -> "A" ++ show x) 	[0..]

		let label	= Var.new (names !! code)
		let table'	= FFunInfo label eff clo : table
		
		put (code + 1, table')

		t1'		<- labelT t1
		t2'		<- labelT t2
		
		return		$ TFunV t1' t2' (Just label)

	_ -> return t


-----------------------
-- unlabelFunsT
--
unlabelFunsT ::	Type -> Type
unlabelFunsT	t
 = case t of	
	TSigExact x	-> TSigExact (unlabelFunsT x)
	TSig x		-> TSig      (unlabelFunsT x)
 	TForall vks x	-> TForall vks (unlabelFunsT x)
	TFetters fs x	
	 -> let (fsInfo, fsOther)
	 		= partition (\f -> isFFunInfo f) fs

		x'	= transformT (rewriteFunT fsInfo) x

	    in	compactFettersT (TFetters fsOther x')
		
	_ -> t

	
rewriteFunT fs t
 = case t of
 	TFunV t1 t2 (Just v)
	 -> let	[(eff, clo)]	
	 		= [(eff, clo)
				| FFunInfo v' eff clo <- fs
				, v =~= v]
				
	    in	TFun t1 t2 eff clo

	_ -> t
-}
