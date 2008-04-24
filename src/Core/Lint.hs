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
--		* do proper scoping, not just addVT, addVK. 
--			want withVT, withVK
module Core.Lint
	( lintTree )

where

import Util
import Shared.Error
import Shared.Pretty

import Core.Exp
import Core.Pretty
import Core.Util
import Core.Util.Slurp
import Core.ReconKind
import Core.Reconstruct		(reconX_type)

import Type.Util.Bits		(varOfBind)

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Debug.Trace	as Debug
import qualified Shared.Var	as Var
import qualified Shared.VarPrim	as Var

-----
stage	= "Core.Lint"

-----
type Error	= PrettyM PMode
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
addVT v t table		
 = return $ table { tableTypes = Map.insert v t (tableTypes table) }

{- = case Map.lookup v (tableTypes table) of
 	Nothing	-> 
	Just k	-> panic stage $ "addVT: var " % v % " is already bound to " % t % "\n"
-}
	
addVTs vts table	= foldM (\tt (v, t) -> addVT v t tt) table vts 

-- | Add a kind binding to the table.
--	Check for duplicate bindings before we add the new one.
--	This catches problems with regions being declared locally in their own scope.
--
addVK :: Var -> Kind -> Table -> LintM (Maybe Table)
addVK v k table		
 = case Map.lookup v (tableKinds table) of
 	Nothing	
	 -> do	let table'	= table { tableKinds = Map.insert v k (tableKinds table) }
		return	$ Just table'
	
	Just k	
	 -> 	freakout stage 
	 		("addVK: var " % v % " is already bound to " % k % "\n")
			(return Nothing)


-------------------------------------------------------------------------------
-- | Lint this core tree
--
lintTree 
	:: Tree 	-- the tree to check
	-> [String]	-- errors found

lintTree tree
 =	map pprStrPlain $ execState (lintTreeM tree) []


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

lintP	tt (PBind v x)
 	| Var.name v == "main"
	, Var.nameModule v == Var.ModuleAbsolute ["Main"]
	= do
		let vT	= reconX_type "Core.Lint.lintP" x
		tt'	<- addVT v vT tt
		lintX tt' x

		-- When the top level binding is the main function then check that all
		--	its witnesses are available, and that it has the appropriate 
		--	type () -> ()
		lintMainType tt vT
		return tt'
		
	| otherwise
	= do	let vT	=  reconX_type "Core.Lint.lintP" x
		tt'	<- addVT v vT tt
	 	lintX tt' x
	 	return tt'
	
-- TODO: check that witnesses are only of the created region
lintP	tt (PRegion r vts)
 = do	Just tt2	<- addVK r KRegion tt
 	tt3		<- addVTs vts tt2
	return tt3


lintP	tt (PClassDict v ts context vts)
 = do	
 	-- v doesn't have this kind, but it'll do for now.
	--	We're not checking kinds of class applications yet.
	
-- 	Just tt'	<- addVK v (KClass v ts) tt
	return	tt


lintP	tt _		
 =	return tt


-- | Check the type of the main function.
--	
lintMainType :: Table -> Type -> LintM ()
lintMainType table tt
	
	-- All witnesses passed to main need to be available at top level.
 	| TContext k t	<- tt
	, Just ks	<- sequence $ map kindOfType $ Map.elems $ tableTypes table
	= if elem k ks
 		then lintMainType table t
		else do	addError $ "Context of main function " % k % " is not available at top level.\n"
			return ()
	
	-- main must have type () -> ()
	| Just (t1, t2, eff, clo)	<- takeTFun tt
	, Just (v1, _, [])		<- takeTData t1
	, Just (v2, _, [])		<- takeTData t2
	, v1 == Var.primTUnit
	, v2 == Var.primTUnit
	= return ()
	
	| otherwise
	= do	addError
			$ "Main function does not have type () -> ().\n"
			% "    T[main] = " % tt	% "\n"
		return ()
		
	 	
	

--------------------------------------------------------------------------------
-- Lint for Expressions
--
lintX :: Table -> Exp -> LintM ()

lintX tt (XLAM v k x)	
 = do	lintK tt k

 	Just tt'	<- addVK (varOfBind v) k tt
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
 
lintX tt (XMatch aa)
 = do	foldM_ lintA tt aa
 
lintX tt (XLit{})
 = do	return ()
 
lintX tt (XVar v TNil)
 = do	addError $ "Variable " % v % " has no type annotation.\n"
 	lintBoundV tt v
	
 
lintX tt (XVar v t)
 = do	lintBoundV tt v
 
lintX tt (XLocal v vts x)
 = do	Just tt2	<- addVK v KRegion tt
 	tt3		<- addVTs vts tt2
 	lintX tt3 x
 
lintX tt (XPrim p xs)
 = do	mapM_ (lintX tt) xs
 
lintX tt (XType t)
 = do	lintT tt t

lintX tt xx
 = panic stage $ "lintX: no match for " % xx

-- | Lint a bound variable.
lintBoundV :: Table -> Var -> LintM ()
lintBoundV tt v
 = case Map.lookup v (tableTypes tt) of
 	Nothing 
	 ->	addError 	
	 		$ "Variable " % v % " is not in scope.\n"
			
	Just _
	 ->	return ()


-- Lint for Stmts
lintS tt (SBind Nothing x)
 = do	lintX tt x
 	return tt
 
lintS tt (SBind (Just v) x)
 = do	let xT	= reconX_type "Core.Lint.lintS" x
 	tt'	<- addVT v xT tt
 	lintX tt' x
	return tt'
	

-- | Lint for Alts
lintA :: Table -> Alt -> LintM Table
lintA tt (AAlt gs x)
 = do	tt2		<- foldM lintG tt gs
 	lintX tt2 x
	return tt2
	

-- | Lint for Guards
lintG :: Table -> Guard -> LintM Table
lintG tt (GExp w x)
 = do	tt2		<- lintW tt w
 	lintX tt2 x
	return tt2
	
-- | Lint for Patterns
lintW :: Table -> Pat -> LintM Table
lintW tt (WVar v)
 = do	let Just t	=  Map.lookup v (tableTypes tt)
	tt'		<- addVT v t tt
	return tt'

lintW tt (WLit c)
 = do 	return tt
 
lintW tt (WCon v lvt)
 = do	tt'		<- addVTs (map (\(l, v, t) -> (v, t)) lvt) tt
 	return tt'


--------------------------------------------------------------------------------
-- Lint for Types
--

lintT :: Table -> Type -> LintM ()

lintT tt TNil
 =	addError $ ppr "Found a TNil\n"

lintT tt (TForall v k1 t2)
 = do	lintK tt k1
 	Just tt'	<- addVK (varOfBind v) k1 tt
 	lintT tt' t2
	
lintT tt (TContext k1 t2)
 = do	lintK tt k1
 	lintT tt t2
	
lintT tt (TFetters t1 fs)
 = do	tt'	<- addVTs [(v, t) | FWhere (TVar k v) t <- fs] tt
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
	, Just kt	<- kindOfType t
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

lintT tt (TIndex ix)
 = 	return ()

lintT tt (TCon tyCon)
 =	return ()

-- BUGS: ignore the constraint for now
lintT tt (TVarMore k v t)
 =	lintT tt (TVar k v)

lintT tt (TTop k)
 =	return ()
 
lintT tt (TBot k)
 = 	return ()
		
lintT tt (TEffect v ts)
 = do	mapM_ (lintT tt) ts
  
lintT tt (TFree	v t)
 = 	lintT tt t
 
lintT tt (TTag v)
 = 	return () 

-- witnesses
-- lintT tt (TPurify eff wit)
-- = do	lintT tt eff
-- 	lintT tt wit

-- lintT tt (TPurifyJoin wits)
--  = do	mapM_ (lintT tt) wits	 

lintT tt (TWitJoin wits)
 = do	mapM_ (lintT tt) wits

lintT tt (TWild k)
 =	return ()
 
lintT tt _
 =	return ()

--------------------------------------------------------------------------------
-- Lint for Kinds
--

lintK :: Table -> Kind -> LintM ()
lintK tt kk
 = case kk of
 	KNil	
	 -> addError $ ppr "lintK: found a KNil"

	KFun k1 k2	
	 -> do	lintK tt k1
	 	lintK tt k2

	KForall k1 k2
	 -> do	lintK tt k1
	 	lintK tt k2
	
	KValue		-> return ()
	KRegion		-> return ()
	KEffect		-> return ()
	KClosure	-> return ()

	KClass v ts
	 -> do	mapM_ (lintT tt) ts
	 	
	KWitJoin ks
	 -> do	mapM_ (lintK tt) ks
	
	_	-> return ()

