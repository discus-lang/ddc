{-# OPTIONS -fwarn-incomplete-patterns #-}

-- | Lint checks for problems with the tree structure which aren't caught by the type checker. 
--
--	These include:
--		* Type variables which are out of scope.
--		* Variables having the wrong kind
--
--	TODO:	* check syntactic soundness of witnesses.
--		* check other top level things besides PBind
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
import Core.ReconKind

-----
stage	= "Core.Lint"

-----
type Error	= PrettyP
type LintM	= State [Error]


-------------------------------------------------------------------------------
-- | A table of type and kind bindings
data Table
	= Table
	{ tableTypes	:: Map Var Type
	, tableKinds	:: Map Var Kind }

tableInit
	= Table
	{ tableTypes	= Map.empty
	, tableKinds	= Map.empty }


-- | Add a lint error to the lint state.
addError :: Error -> LintM ()
addError    err	
 = 	modify (\s -> s ++ [err]) 

-- | Add a type binding to the table.
addVT :: Var -> Type -> Table -> LintM Table
addVT v t table		= return $ table { tableTypes = Map.insert v t (tableTypes table) }

addVTs vts table	= foldM (\tt (v, t) -> addVT v t tt) table vts 

-- | Add a kind binding to the table.
addVK :: Var -> Kind -> Table -> LintM Table
addVK v k table		= return $ table { tableKinds = Map.insert v k (tableKinds table) }



-------------------------------------------------------------------------------
-- | Lint this core tree
--
lintTree 
	:: Tree 	-- the tree to check
	-> [String]	-- errors found

lintTree tree
 =	map pretty $ execState (lintTreeM tree) []


lintTreeM :: Tree -> LintM ()
lintTreeM tree
 = do
 	-- load the type of top level things into the type map
	let topTypes	= catMap slurpTypesP tree
 	tt		<- addVTs topTypes tableInit 
 
 	-- check each top level thing
	_		<- foldM lintP tt tree

   	return ()
  

--------------------------------------------------------------------------------
-- Lint for top level things
--
lintP :: Table -> Top -> LintM Table

lintP 	tt (PBind v x)	
 = do	let Just vT	=  maybeSlurpTypeX x
	tt'		<- addVT v vT tt
 	lintX tt' x
 	return tt'
	

lintP	tt (PClassDict v ts context vts)
 = do	
 	-- v doesn't have this kind, but it'll do for now.
	--	We're not checking kinds of class applications yet.
	
 	tt'	<- addVK v (KClass v ts) tt
	return	tt'


lintP	tt _		
 =	return tt



--------------------------------------------------------------------------------
-- Lint for Expressions
--
lintX :: Table -> Exp -> LintM ()

lintX tt (XLAM v k x)	
 = do	lintK tt k

 	tt'	<- addVK (varOfBind v) k tt
 	lintX tt' x
	return ()
	
lintX tt (XAPP x t)
 = do	lintX tt x
 	lintT tt t
	
lintX tt (XTet vts x)
 = do	tt'	<- addVTs vts tt
 	mapM_ (lintT tt') $ map snd vts
 	lintX tt' x
	
lintX tt (XTau t x)
 = do	lintX tt x
 	lintT tt t
 
lintX tt (XLam v t x eff clo)
 = do	lintT tt t
 	lintT tt eff
	lintT tt clo

	tt'	<- addVT v t tt
 	lintX tt' x
 
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
 
lintX tt (XVar v)
 = do	lintBoundV tt v
 
lintX tt (XLocal v vts x)
 = do	tt2	<- addVK v KRegion tt
 	tt3	<- addVTs vts tt2
 	lintX tt3 x
 
lintX tt (XPrim p xs eff)
 = do	mapM_ (lintX tt) xs
 
lintX tt (XType t)
 = do	lintT tt t


-- | Lint a bound variable.
lintBoundV :: Table -> Var -> LintM ()
lintBoundV tt v
 = case Map.lookup v (tableTypes tt) of
 	Nothing 
	 ->	addError 	
	 		$ "Variable " % v % " is not in scope.\n"
			
	Just _
	 ->	return ()

--------------------------------------------------------------------------------
-- Lint for Statements
--
lintS tt (SBind Nothing x)
 = do	lintX tt x
 	return tt
 
lintS tt (SBind (Just v) x)
 = do	let Just xT	= maybeSlurpTypeX x
 	tt'		<- addVT v xT tt
 	lintX tt' x
	return tt'
	

--------------------------------------------------------------------------------
-- Lint for Types
--
lintT :: Table -> Type -> LintM ()

lintT tt TNil
 =	addError $ prettyp "Found a TNil\n"

lintT tt (TForall v k1 t2)
 = do	lintK tt k1
 	tt'	<- addVK (varOfBind v) k1 tt
 	lintT tt' t2
	
lintT tt (TContext k1 t2)
 = do	lintK tt k1
 	lintT tt t2
	
lintT tt (TWhere t1 vts)
 = do	tt'	<- addVTs vts tt
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

	-- Effects, Closures, bound via a XTet
	-- ie   let !e1 = !{ ... }
	| Just t	<- Map.lookup v (tableTypes tt)
	, kt		<- kindOfType t
	= if  k /= kt
		then  addError 
			$ "Variable " % v % " has a different kind than the type bound to it.\n"
			% "    var   kind  = " % k 	% "\n"
			% "    type        = " % t 	% "\n"
			% "    type  kind  = " % kt	% "\n"

		else return ()

	-- Types, Regions, Effects, Closures, bound via a Lambda
	-- ie   /\ (a :: *) -> ...
 	| Just kt 	<- Map.lookup v (tableKinds tt)
	= if k /= kt
		then addError 
			$ "Variable " % v % " has a different kind than the type bound to it.\n"
			% "    var     kind  = " % k 	% "\n"
			% "    binding kind = " % kt 	% "\n"

		else return ()


	-- simply not in scope
	| otherwise
	= addError
	  	$ "Type variable " % v % " is not in scope.\n"



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

-- witnesses
lintT tt (TClass v ts)
 =	mapM_ (lintT tt) ts
 
lintT tt (TPurify eff wit)
 = do	lintT tt eff
 	lintT tt wit

lintT tt (TPurifyJoin wits)
 = do	mapM_ (lintT tt) wits	 

lintT tt (TWitJoin wits)
 = do	mapM_ (lintT tt) wits

lintT tt (TWild k)
 =	return ()
 

--------------------------------------------------------------------------------
-- Lint for Kinds
--
lintK :: Table -> Kind -> LintM ()
lintK tt kk
 = case kk of
 	KNil	
	 -> addError $ prettyp "lintK: found a KNil"
	
	KData		-> return ()
	KRegion		-> return ()
	KEffect		-> return ()
	KClosure	-> return ()

	KFun k1 k2	
	 -> do	lintK tt k1
	 	lintK tt k2
		
	KClass v ts
	 -> do	mapM_ (lintT tt) ts
	 	
	KWitJoin ks
	 -> do	mapM_ (lintK tt) ks
	

