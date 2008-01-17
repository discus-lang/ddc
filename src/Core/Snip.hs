-----------------------
-- Core.Snip
-- | 	Snip out function applications and compound expressions from function arguments.
--	This needs to be run after Core.Block, which wraps the applications of interest in XDos.
--
module Core.Snip
	( snipTree )
where

import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.Var	as Var
import Shared.Var		(Var, VarBind, NameSpace(..))

import qualified Shared.VarUtil	as Var
import Shared.VarUtil		(VarGenM, newVarN)
import Shared.Error

import Core.Exp
import Core.Pretty
import Core.Util
import Core.Plate.Trans

stage	= "Core.Snip"

-----
type SnipM	= VarGenM

-----
snipTree 
	:: Set Var
	-> String	-- string to use for var prefix
	-> Tree 	-- core tree
	-> Tree		-- snipped core tree

snipTree topVars varPrefix tree
 = let	table	= transTableId { transSS = snipStmts topVars  }
	tree'	= evalState
			(mapM (transZM table) tree)
			$ Var.XBind varPrefix 0
   in	tree'
   

-- | Keep running the snipper on these statements until nothing more will snip.
snipStmts ::	Set Var -> [Stmt] -> SnipM [Stmt]
snipStmts	topVars ss
 = do
	ss'	<- snipPass topVars ss
	
	if length ss == length ss'
	 then return ss'
	 else snipStmts topVars ss'


-- | Run one snipper pass on this list of statements.
--	Any new stmts are placed infront of the one they were cut from.
snipPass :: Set Var -> [Stmt] -> SnipM [Stmt]
snipPass topVars stmts
 = case stmts of
 	[]	-> return []
	(s:ss)
	 -> do 	(ssSnipped, s')	<- snipStmt topVars s
		ssRest		<- snipPass topVars ss
		return		$ ssSnipped ++ [s'] ++ ssRest


-- | Snip some bindings out of this stmt.
--	Returns the new stmt, and the bindings which were snipped.
snipStmt ::	Set Var -> Stmt	-> SnipM ([Stmt], Stmt)
snipStmt	topVars xx
 = case xx of
	SBind mV x
	 -> do 	(ss, x')	<- snipX1 topVars Map.empty x
		return	(ss, SBind mV x')


-- | Enter into an expression on the RHS of a stmt.

snipX1 :: Set Var -> Map Var Type -> Exp -> SnipM ([Stmt], Exp)
snipX1	topVars env xx
 = case xx of
	XAnnot n x
	 -> do	(ss, x')	<- snipX1 topVars env x
	 	return	(ss, XAnnot n x')

	XTau t	x
	 -> do	(ss, x')	<- snipX1 topVars env x
	 	return	(ss, XTau t x')


	-- Decend into an XTet, remembering the bindings.
	--	ALSO: might not need these bindings if we snip out the only bound occurance of it.
	XTet vts x		
	 -> do	let env'	= (Map.union (Map.fromList vts) env)
	 	(ss, x')	<- snipX1 topVars env' x
	 	return	(ss, XTet vts x')
		

	-- Can't lift exprs out of the scope of binders,
	--	there might be free variables in them that would become out of scope
	XLAM{}			-> leaveIt xx
	XLam{}			-> leaveIt xx
	XLocal{}		-> leaveIt xx

	-- These are fine.	
	XDo{}			-> leaveIt xx
	XMatch{}		-> leaveIt xx
	XLit{}			-> leaveIt xx
	XVar{}			-> leaveIt xx
	XType t			-> leaveIt xx


	-- Snip compound exprs from the arguments of applications.
	XApp x1 x2 eff		-> snipXLeft topVars (substituteT env xx)

	XAPP x t	
	 -> do	(ss, x')	<- snipX topVars (substituteT env x)
	 	return		(ss, XAPP x' t)
	
	XPrim p xs
	 -> do	(ss, xs') 	<- liftM unzip 
	 			$  mapM (snipXRight topVars) (substituteT env xs)

	 	return (concat ss, XPrim p xs')

	XProject x j
	 -> do	(ss', x')	<- snipXRight topVars (substituteT env x)
	 	return	(ss', XProject x' j)


-- | Snip some stuff from an expression.

snipX topVars xx
 = case xx of
	XLAM{}			-> snipIt xx

	XAPP{}			-> snipXLeft topVars xx
		
	XTet{}			-> snipIt xx

	XTau t x
	 -> do	(ss, x')	<- snipX topVars x
	 	return	(ss, XTau t x')

	XLam{}			-> snipIt xx
	XApp{}			-> snipXLeft topVars xx		

	XDo{}			-> snipIt xx
	XMatch{}		-> snipIt xx

	XLit{}			-> leaveIt xx
	
	-- snip XVars if they're defined at top level
	XVar v t
	 | Set.member v topVars -> snipIt xx
	 | otherwise		-> leaveIt xx
	 
	-- we should never see XLocals as arguments..
	XLocal{}
	 -> panic stage	$ "snipDownX: unexpected XLocal\n"
	 
	XPrim{}			-> snipIt xx
	XProject{}		-> snipIt xx
	
	XType{}			-> leaveIt xx
	

-- | Snip some expressions from the left hand side of a function application.
--	On the left hand side we leave vars alone, and decend into other applications.
--
snipXLeft :: Set Var -> Exp -> SnipM ([Stmt], Exp)
snipXLeft topVars xx
 = case xx of
	XAPP x1@(XVar v t1) t2	-> leaveIt xx

	XAPP x t
	 -> do	(ss, x')	<- snipX topVars x
	 	return	(ss, XAPP x' t)

	XApp x1@(XVar v t1) x2 eff
	 -> do	(ss2, x2')	<- snipXRight  topVars x2
	 	return	(ss2, XApp x1 x2' eff)

	XApp x1 x2 eff
	 -> do	(ss1, x1')	<- snipX    topVars x1
	 	(ss2, x2')	<- snipXRight topVars x2
		return	( ss1 ++ ss2
			, XApp x1' x2' eff)

	_			-> snipX topVars xx


-- | Snip some expressions from the right hand side of a function application.
--	Right hand sides are arguments, snip function applications and anything else that looks good.
--
snipXRight :: Set Var -> Exp -> SnipM ([Stmt], Exp)
snipXRight topVars xx
 = case xx of
 	XApp{}			-> snipIt xx

	-- leave literal values
	XAPP XLit{} (TVar KRegion _)
				-> leaveIt xx

	XAPP x t
	 -> case takeVar x of
	 	Nothing			-> snipIt  xx
		Just v
		 | Set.member v topVars	-> snipIt  xx
		 | otherwise		-> leaveIt xx

	_			-> snipX topVars xx


-- Snip some thing, creating a new statement.
snipIt :: Exp -> SnipM ([Stmt], Exp)
snipIt xx
 = do	b	<- newVarN NameValue
 	return	( [SBind (Just b) xx] 
		, XVar b TNil )

-- Leave some thing alone.
leaveIt :: Exp -> SnipM ([Stmt], Exp)
leaveIt xx	= return ([], xx)


-----
-- Used by snipX/XAPP
--
takeVar :: Exp -> Maybe Var
takeVar	(XAPP x t)	= takeVar x
takeVar (XVar v t)	= Just v
takeVar _		= Nothing



