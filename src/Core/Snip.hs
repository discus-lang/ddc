-----------------------
-- Core.Snip
-- | 	Snip out function applications and compound expressions from function arguments.
--	This needs to be run after Core.Block, which wraps the applications of interest in XDos.
--
module Core.Snip
	( Table(..)
	, snipTree )
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
import Shared.Pretty

import Core.Exp
import qualified Core.Reconstruct	as Recon
import Core.Pretty
import Core.Util
import Core.Plate.Trans
import qualified Debug.Trace	

stage		= "Core.Snip"
debug 		= False
trace s	x 	= if debug then Debug.Trace.trace (pprStrPlain s) x else x

-----
type SnipM	= VarGenM

-- Table -------------------------------------------------------------------------------------------
data Table
	= Table
	{  -- | vars of top level bindings
	   tableTopVars		:: Set Var
	
	   -- | whether to preserve type information of snipped vars
  	   --	  this requires that expressions have locally reconstuctable types
	,  tablePreserveTypes	:: Bool }
	   
-----
snipTree 
	:: Table
	-> String	-- string to use for var prefix
	-> Tree 	-- core tree
	-> Tree		-- snipped core tree

snipTree table varPrefix tree
 = let	transTable	= transTableId { transSS = snipStmts table }
	tree'		= evalState
				(mapM (transZM transTable) tree)
				$ Var.XBind varPrefix 0
   in	tree'
   

-- | Keep running the snipper on these statements until nothing more will snip.
snipStmts ::	Table -> [Stmt] -> SnipM [Stmt]
snipStmts	table ss
 = do
	ss'	<- snipPass table ss
	
	if length ss == length ss'
	 then return ss'
	 else snipStmts table ss'


-- | Run one snipper pass on this list of statements.
--	Any new stmts are placed infront of the one they were cut from.
snipPass :: Table -> [Stmt] -> SnipM [Stmt]
snipPass table stmts
 = case stmts of
 	[]	-> return []
	(s:ss)
	 -> do 	(ssSnipped, s')	<- snipStmt table s
		ssRest		<- snipPass table ss
		return		$ ssSnipped ++ [s'] ++ ssRest


-- | Snip some bindings out of this stmt.
--	Returns the new stmt, and the bindings which were snipped.
snipStmt :: Table -> Stmt -> SnipM ([Stmt], Stmt)
snipStmt table xx
 = case xx of
	SBind mV x
	 -> do 	(ss, x')	<- snipX1 table Map.empty x
		return	(ss, SBind mV x')


-- | Enter into an expression on the RHS of a stmt.

snipX1 :: Table -> Map Var Type -> Exp -> SnipM ([Stmt], Exp)
snipX1	table env xx
 = trace ("snipX1: " % xx)
 $ case xx of
	XAnnot n x
	 -> do	(ss, x')	<- snipX1 table env x
	 	return	(ss, XAnnot n x')

	XTau t	x
	 -> do	(ss, x')	<- snipX1 table env x
	 	return	(ss, XTau t x')


	-- Decend into an XTet, remembering the bindings.
	--	ALSO: might not need these bindings if we snip out the only bound occurance of it.
	XTet vts x		
	 -> do	let env'	= (Map.union (Map.fromList vts) env)
	 	(ss, x')	<- snipX1 table env' x
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
	XApp x1 x2 eff		-> snipXLeft table (substituteT env xx)

	XAPP (XVar v t1) t2	-> leaveIt xx

	XAPP x t	
	 -> do	(ss, x')	<- snipX table (substituteT env x)
	 	return		(ss, XAPP x' t)
	
	XPrim p xs
	 -> do	(ss, xs') 	<- liftM unzip 
	 			$  mapM (snipXRight table) (substituteT env xs)

	 	return (concat ss, XPrim p xs')

	XProject x j
	 -> do	(ss', x')	<- snipXRight table (substituteT env x)
	 	return	(ss', XProject x' j)


-- | Snip some stuff from an expression.

snipX table xx
 = trace ("snipX " % xx)
 $ case xx of
	XLAM{}			-> snipIt table xx

	XAPP{}			-> snipXLeft table xx
		
	XTet{}			-> snipIt table xx

	XTau t x
	 -> do	(ss, x')	<- snipX table x
	 	return	(ss, XTau t x')

	XLam{}			-> snipIt table xx
	XApp{}			-> snipXLeft table xx		

	XDo{}			-> snipIt table xx
	XMatch{}		-> snipIt table xx

	XLit{}			-> leaveIt xx
	
	-- snip XVars if they're defined at top level
	XVar v t
	 | Set.member v (tableTopVars table) 	-> snipIt table xx
	 | otherwise				-> leaveIt xx
	 
	-- we should never see XLocals as arguments..
	XLocal{}
	 -> panic stage	$ "snipDownX: unexpected XLocal\n"
	 
	XPrim{}			-> snipIt table xx
	XProject{}		-> snipIt table xx
	
	XType{}			-> leaveIt xx
	

-- | Snip some expressions from the left hand side of a function application.
--	On the left hand side we leave vars alone, and decend into other applications.
--
snipXLeft :: Table -> Exp -> SnipM ([Stmt], Exp)
snipXLeft table xx
 = case xx of
	XAPP x1@(XVar v t1) t2	-> leaveIt xx

	XAPP x t
	 -> do	(ss, x')	<- snipX table x
	 	return	(ss, XAPP x' t)

	XApp x1@(XVar v t1) x2 eff
	 -> do	(ss2, x2')	<- snipXRight  table x2
	 	return	(ss2, XApp x1 x2' eff)

	XApp x1 x2 eff
	 -> do	(ss1, x1')	<- snipX    table x1
	 	(ss2, x2')	<- snipXRight table x2
		return	( ss1 ++ ss2
			, XApp x1' x2' eff)

	_			-> snipX table xx


-- | Snip some expressions from the right hand side of a function application.
--	Right hand sides are arguments, snip function applications and anything else that looks good.
--
snipXRight :: Table -> Exp -> SnipM ([Stmt], Exp)
snipXRight table xx
 = case xx of
 	XApp{}			
	 -> snipIt table xx

	-- leave literal values
	XAPP XLit{} (TVar KRegion _)
	 -> leaveIt xx

	XAPP x t
	 -> case takeVar x of
	 	Nothing		-> snipIt table xx
		Just v
		 | Set.member v (tableTopVars table)	-> snipIt  table xx
		 | otherwise				-> leaveIt xx

	_ -> snipX table xx


-- Snip some thing, creating a new statement.
--	If tablePreserve is turned on then preserve the type of the new var, 
--	else just add a TNil. In this case we'll need to call Core.Reconstruct
--	to fill in the type annots later on.
--
snipIt :: Table -> Exp -> SnipM ([Stmt], Exp)
snipIt table xx
	| tablePreserveTypes table
	= do	b	<- newVarN NameValue
		let tX	= Recon.reconX_type (stage ++ ".snipIt") xx
	 	return	( [SBind (Just b) xx] 
			, XVar b tX )

	| otherwise
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



