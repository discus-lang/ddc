{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Lint checks for possible problems with the tree structure which aren't caught
--	by the type checker.
--
--	TODO: 	is type checker is supposed to check everything, merge these together.
--		check syntactic soundness of witnesses.
--
module Core.Lint
	( lintTree )

where

import qualified Debug.Trace	as Debug

import qualified Data.Map	as Map
import Data.Map			(Map)

import Util
import Shared.Error
import Core.Exp
import Core.Pretty
import Core.Plate.Walk
import Core.Util
import Core.Util.Slurp

-----
stage	= "Core.Lint"

-----
type Error	= PrettyP
type LintM	= State [Error]

addError :: Error -> LintM ()
addError    err	
 = 	modify (\s -> s ++ [err]) 


addVT :: Map Var Type -> (Var, Type) -> LintM (Map Var Type)
addVT map (v, t)
 = case Map.lookup v map of
 	Nothing	
	 -> 	return $  Map.insert v t map

	Just _	
	 -> 	return	$ Map.insert v t (Map.delete v map)

-----
lintTree :: Tree -> [String]
lintTree tree
 =	map pretty $ execState (lintTreeM tree) []

lintTreeM :: Tree -> LintM ()
lintTreeM tree
 = do
 	-- load the type of top level things into the type map
	let topTypes	= catMap slurpTypesP tree
 	tt		<- foldM addVT Map.empty topTypes 
 
 	-- check each top level thing
	mapM (lintP tt) tree

   	return ()
  


-----
lintP :: Map Var Type -> Top -> LintM ()
lintP 	tt (PBind v x)	= lintX tt x
lintP	tt _		= return ()


-----
lintBoundV :: Map Var Type -> Var -> LintM ()
lintBoundV tt v
 = case Map.lookup v tt of
 	Nothing 
	 ->	addError 	
	 		$ "Variable " % v % " is not in scope.\n"
			
	Just _
	 ->	return ()

-----------------------
--
lintX :: Map Var Type -> Exp -> LintM ()

lintX tt (XLAM v t x)	
 = do	tt'	<- addVT tt (varOfBind v, t)
 	lintX tt' x
	lintT tt' t
	return ()
	
lintX tt (XAPP x t)
 = do	lintX tt x
 	lintT tt t
	
lintX tt (XTet vts x)
 = do	tt'	<- foldM addVT tt vts
 	mapM_ (lintT tt') $ map snd vts
 	lintX tt' x
	
lintX tt (XTau t x)
 = do	lintX tt x
 	lintT tt t
 
lintX tt (XLam v t x eff clo)
 = do	tt'	<- addVT tt (v, t)
 	lintX tt' x
 	lintT tt' t
 	lintT tt' eff
	lintT tt' clo
 
lintX tt (XApp x1 x2  eff)
 = do	lintX tt x1
 	lintX tt x2
	lintT tt eff
	
lintX tt (XDo ss)
 = do	foldM_ lintS tt ss
 
lintX tt (XMatch aa eff)
 = do	return ()
 
lintX tt (XConst c t)
 = do	lintT tt t
 	return ()
 
lintX tt (XVar v)
 = do	lintBoundV tt v
 
lintX tt (XLocal v vs x)
 = do	tt'	<- addVT tt (v, TKind KRegion)
 	lintX tt' x
 
lintX tt (XPrim p xs eff)
 = do	mapM_ (lintX tt) xs
 
lintX tt (XType t)
 = do	lintT tt t


-----
lintS tt (SBind Nothing x)
 = do	lintX tt x
 	return tt
 
lintS tt (SBind (Just v) x)
 = do	let Just xT	= maybeSlurpTypeX x
 	tt'		<- addVT tt (v, xT)
 	lintX tt' x
	return tt'
	

-----------------------
--
lintT :: Map Var Type -> Type -> LintM ()

lintT tt TNil
 =	addError $ prettyp "Found a TNil\n"

lintT tt (TForall v t1 t2)
 = do	tt'	<- addVT tt (varOfBind v, t1)
 	lintT tt' t2
	
lintT tt (TContext t1 t2)
 = do	lintT tt t1
 	lintT tt t2
	
lintT tt (TWhere t1 vts)
 = do	tt'	<- foldM addVT tt vts
 	lintT tt' t1

lintT tt (TApp t1 t2)
 = do	lintT tt t1
 	lintT tt t2
	
lintT tt (TSum k ts)
 = do	mapM_ (lintT tt) ts
 
lintT tt (TMask k t1 t2)
 = do	lintT tt t1
 	lintT tt t2
	

lintT tt (TVar k v)

	-- Types, Regions, Effects, Closures, bound via a Lambda
	-- ie   /\ (a :: *) -> ...

 	| Just (TKind kt) <- vt
	= if k /= kt
		then addError 
			$ "Variable " % v % " has a different kind than the type bound to it.\n"
			% "    var     kind  = " % k 	% "\n"
			% "    binding kind = " % kt 	% "\n"

		else return ()

	-- Effects, Closures, bound via a XTet
	-- ie   let !e1 = !{ ... }
			
	| Just t	<- vt
	, kt		<- kindOfType t
	= if  k /= kt
		then  addError 
			$ "Variable " % v % " has a different kind than the type bound to it.\n"
			% "    var   kind  = " % k 	% "\n"
			% "    type        = " % t 	% "\n"
			% "    type  kind  = " % kt	% "\n"

		else return ()


	-- simply not in scope
	| Nothing	<- vt
	= addError
	  	$ "Variable " % v % " is not in scope.\n"

	where	vt	= Map.lookup v tt


lintT tt (TTop k)
 =	return ()
 
lintT tt (TBot k)
 = 	return ()
		
lintT tt (TData v ts)
 = do	mapM_ (lintT tt) ts
 
lintT tt (TFunEC t1 t2 eff clo)
 = do	lintT tt t1
 	lintT tt t2
	lintT tt eff
	lintT tt clo		
		 	
lintT tt (TFun t1 t2)
 = do	lintT tt t1
 	lintT tt t2
				
lintT tt (TEffect v ts)
 = do	mapM_ (lintT tt) ts
 
 
lintT tt (TFree	v t)
 = 	lintT tt t
 
lintT tt (TTag v)
 = 	return () 

 
lintT tt (TClass v ts)
 =	mapM_ (lintT tt) ts
 
lintT tt (TKind k)
 =	return ()
 
lintT tt (TWild k)
 =	return ()
 
lintT tt t
 = panic stage	$ "lintT: no match for " % show t


 


