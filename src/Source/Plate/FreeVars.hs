
module Source.FreeVarsE
(
	freeVarsE,
	freeVarsS
)

where

-----
import Util.List
import Util.Tuple
import Util.Maybe
import Source.Exp

-----------------------
-- freeVarsE
--
freeVarsE :: Exp -> [Var]
freeVarsE   e
 = let ?bound = [] 
   in nub $ freeVE e
 

freeVE 	:: (?bound :: [Var])
	-> Exp	-> [Var]

freeVE	e
 = case e of
 	XLet sp ss e
	 -> let ?bound = (catMaybes $ map takeStmtBoundV ss) ++ ?bound
	    in (catMap freeVS ss ++ freeVE e)
	
	XLambda sp v e
	 -> let ?bound = v : ?bound
	    in  freeVE e
	    
	XApp sp e1 e2
	 -> freeVE e1
	 ++ freeVE e2
	 
	XCase sp e1 cs
	 -> freeVE e1
	 ++ catMap freeVA cs
	 
	XConst sp c -> []

	XVar sp v	
	 -> if elem v ?bound 
	 	then []
		else [v]
		
	XDo sp ss
	 -> let ?bound = (catMaybes $ map takeStmtBoundV ss) ++ ?bound
	    in  catMap freeVS ss
	 
	XIfThenElse sp e1 e2 e3
	 -> freeVE e1
	 ++ freeVE e2
	 ++ freeVE e3
	 
	XDefix sp es
	 -> catMap freeVE es
	 
	XAnnot a e
	 -> freeVE e
	 

-----
freeVarsS :: Stmt -> [Var]
freeVarsS   s
 = let ?bound = [] 
   in  nub $ freeVS s


freeVS 	:: (?bound :: [Var])
	-> Stmt -> [Var]
	
freeVS	stmt
 = case stmt of
	SBindPats sp v ps x
	 -> let ?bound	= [v] ++ (catMap freeVE ps) ++ ?bound
	    in	freeVE x

	SBind sp v e
	 -> let ?bound = (maybeToList v) ++ ?bound
	    in  freeVE e
	
-----
freeVA	:: (?bound :: [Var])
	-> Alt -> [Var]

freeVA a
 = case a of
{- 	ACon v es x 
	 -> let ?bound	= ?bound ++ map (\(XVar v) -> v) es
	    in	freeVE x
-}
	ADefault x
	 -> freeVE x












