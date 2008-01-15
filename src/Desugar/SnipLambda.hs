
-- | Effect masking and cleanup of constraints on a function is tied to generalisation.
--	We need to snip out all lambda expressions to their own bindings so the generaliser sees them.

module Desugar.SnipLambda
	( snipLambdaTree )
where

import Util
import Shared.VarGen		hiding (newVarN)

import qualified Shared.Var	as Var
import Shared.Var		(Var, VarBind, NameSpace(..), incVarBind)

import Desugar.Exp
import Desugar.Bits
import Desugar.Plate.Trans

-----
type SnipM a	= State (SnipS a)
type Table a	= TransTable (SnipM a) a a 

data SnipS a
	= SnipS
	{ sVarGen	:: VarGen
	, sBinds	:: [Stmt a] }

newVarN		= makeNewVarN sVarGen (\b s -> s { sVarGen = b })

addBind :: Stmt a	-> SnipM a ()
addBind stmt	= modify $ \s -> s { sBinds =  sBinds s ++ [stmt]}


-----
snipLambdaTree :: String -> Tree a -> Tree a
snipLambdaTree unique tree
 = let	table	= (transTableId return)
 		{ transX	= snipX 
		, transS	= snipS  
		, transA	= snipA }
		
	tree'	= evalState 
			(mapM (snipP table) tree)
			$ SnipS
			{ sVarGen	= Var.XBind ("x" ++ unique) 0
			, sBinds	= [] }
  in	tree'
  
-----
snipP :: Table a -> Top a -> SnipM a (Top a)
snipP table pp
 = case pp of
	PBind nn mV x
	 -> do	x'	<- snipX_slide table x
	 	return 	$ PBind nn mV x'
		
	_ -> transZM table pp
	
-----	
snipS :: Table a -> Stmt a -> SnipM a (Stmt a)
snipS table ss	
 = case ss of
 	SBind nn mV x
	 -> do	xE	<- snipX_slide table x
	 	xD	<- dropBinds xE
	 	return	$ SBind nn mV xD

	_ -> do
		transS_follow table table ss
		

snipX_slide table xx
 = case xx of
 	XLambda v t x
	 -> do	x'	<- snipX_slide table x
	 	return	$ XLambda v t x'
		
	_ -> do
		xF	<- transX_follow table table xx
		xD	<- dropBinds xF
		return	xD

-----
snipX :: Table a -> Exp a -> SnipM a (Exp a)
snipX table xx
 = case xx of
 	XLambda nn v x
	 -> do	vF	<- newVarN NameValue
		x'	<- snipX_slide table x
		let xx'	= XLambda nn v x'

	 	addBind (SBind nn (Just vF) xx')
		return	$ XVar nn vF

	_ -> do
		transX_follow table table xx

-----
snipA :: Table a -> Alt a -> SnipM a (Alt a)
snipA table aa
 = case aa of
 	AAlt nn gs x
	 -> do	gs'	<- mapM (transZM table) gs
		xF	<- snipX_slide table x
		xD	<- dropBinds xF
		
		return	$ AAlt nn gs' xD
		


-----
dropBinds :: Exp a -> SnipM a (Exp a)
dropBinds xx
 = do	binds	<- gets sBinds
	modify (\s -> s { sBinds = []} )
 	case binds of
	 []	-> return xx
	 _	-> dropBinds' binds xx
	 
dropBinds' binds xx
 = case xx of
 	XDo nn ss	
	 ->	return	$ XDo nn (binds ++ ss)

	XLambda nn v x
	 -> do 	x'	<- dropBinds' binds x
	 	return	$ XLambda nn v x'

	_ -> do
		let nn	= getAnnotX xx
		return 	$ XDo nn (binds ++ [SBind nn Nothing xx])

makeNewVarN 
	:: (s -> VarBind) -> (VarBind -> s -> s) 
	-> NameSpace
	-> State s Var
	
makeNewVarN getF modifyF space
 = do	s		<- get
 	let varBind	= getF s
	let varBind'	= incVarBind varBind
	modify (modifyF varBind')
	
	return	$ (Var.new $ pprStr varBind)
		{ Var.bind	= varBind
		, Var.nameSpace	= space }
	
 	
