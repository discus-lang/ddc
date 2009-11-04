-----
-- Sea.Thunk
--	
--	BUGS:	Setting the return value to XNull in expandS is a brutal hack.
--		Better to detect that nothing is ever returned, or init an
--		unused slot.
--

module Sea.Thunk
	(thunkTree)

where

import Shared.Pretty
import Shared.Var		(VarBind)
import qualified Shared.Var	as Var
import qualified Shared.Unique	as Unique

import Sea.Exp
import Sea.Pretty
import Sea.Util
import Sea.Plate.Trans

import qualified Data.Map	as Map
import Data.Map			(Map)

import Util

-----
type	ExM	= State VarBind
initExS		= Var.XBind Unique.seaThunk 0

newVar :: Maybe String -> ExM Var
newVar mName
 = do
 	gen		<- get
	let gen'	= Var.incVarBind gen
	put gen'
	
	let name	= fromMaybe (pprStrPlain gen) mName
	let var		= (Var.new name) { Var.bind = gen }
	
	return var


-----
thunkTree 
	:: Tree () -> Tree ()

thunkTree tree
 	= evalState (mapM expandP tree) initExS


expandP :: Top () -> ExM (Top ())
expandP	xx 
 = case xx of
 	PSuper v aa r ss
	 -> do
		start		<- newVar $ Just (seaVar False v ++ "_start")

		let ?tailCallTargets	
				= [(v, (start, aa))]

		ss1		<- mapM (transformSSM expandSS) ss
		ss2		<- expandSS ss1
		return 	$ PSuper v aa r 
			$ 	[ SBlank
				, SLabel start] ++ ss2
	 
	 
	PCafInit v ss
	  -> do	let ?tailCallTargets	= []
	  	ss1		<- mapM (transformSSM expandSS) ss
	  	ss2		<- expandSS ss1
		return	$ PCafInit v ss2
	 
	_ -> return xx
	
	
-----
expandSS ss
 	= liftM concat
	$ mapM expandS ss
	
expandS	s

	-- tail call 

	-- BRUTAL HACK:
	-- For functions which never return, such as.
	--	loop f = do { f (); y = loop f; };
	--		
	-- the variable 'y' never gets assigned a real value. 
	--
	-- However, the function exit code needs a value to (never) return,
	--	so we'll give it XNull.
	--	
	| SAssign (XVar v _) t x@XTailCall{}	<- s
	= do	callSS	<- expandTailCall x

		return	$ [ SAssign (XVar v t) t XNull ]
			++ callSS

	| SStmt x@XTailCall{}			<- s
	= do	callSS	<- expandTailCall x
		return	callSS
	

	-- curry
	| SAssign (XVar v erikd) t x@(XCurry f superA args) <- s
	= do
		(assSS, x')	<- expandCurry v x

		return	$  [ SAssign (XVar v t) t x' ]
			++ assSS
	
	-- apply / callApp
	| SAssign v t x		<- s
	,    (x =@= XCallApp{})
	  || (x =@= XApply{})
	= do
		(callSS, x')	<- expandAppX x
		return	$  callSS
			++ [ SAssign v t x' ]
			   

	| SStmt x		<- s
	,    (x =@= XCallApp{})
	  || (x =@= XApply{})
	= do
		(callSS, x')	<- expandAppX x

		return	$  callSS 
			++ [ SStmt x' ]


	-- suspend
	| SAssign (XVar v _) t x@(XSuspend{})	<- s
	= do
		(assSS, x')	<- expandSusp v x
		let s'		= SAssign (XVar v t) t x'
		return		$ s' : assSS
		
	| otherwise
	= do	return [s]


expandAppX x
 = case x of
 	XCallApp{}	-> expandCallApp x
	XApply{}	-> expandApply x


-- | Expand out XTailCall primitives
--	Intra-function tail calls are implemented by jumping back to the start of the function.
expandTailCall
		-- possible call targets
	:: (?tailCallTargets 
		:: [ (Var			-- function name
		     , (Var			-- label to jump to
		       , [(Var, Type)]))])	-- function parameters and their types
				

	-> Exp ()				-- the tail call primitive
	-> ExM [Stmt ()]			-- statements to do the call
	
expandTailCall
	x@(XTailCall v args)
 = do
	-- See how we're supposed to call this function.
	let Just (label, params)	
		= lookup v ?tailCallTargets

	let assignParam param@(vP, tP) arg@(XVar vA _)
		-- don't emit (v = v) assignments
		| vP == vA
		= Nothing
		
	    assignParam param@(vP, tP) arg
	    	= Just $ SAssign (XVar vP tP) tP arg
				
 	return	$ 
		-- Overwrite the parameter vars with the new args.
		(catMaybes $ zipWith assignParam params args)

		-- Jump back to the start of the function.
		++ [SGoto label]
		++ [SBlank]
	
-----
expandCurry 
	:: Var 
	-> Exp ()
	-> ExM ([Stmt ()], Exp ())

expandCurry 
	v 
	x@(XCurry f superA args)
 = do
	let allocX	= XAllocThunk f superA (length args)
	let assignSS	= map (\(a, i) -> SAssign (XArg (XVar v TThunk) TThunk i) TObj a)	-- type here is wrong
		  	$ zip args [0..]
		
  	return	( assignSS
		, allocX)

-----
expandSusp 
	:: Var 
	-> Exp ()
	-> ExM ([Stmt ()], Exp ())

expandSusp 
	v 
	x@(XSuspend f args)
 = do
 	let allocX	= XAllocSusp f (length args)
	
	let assignSS	= map (\(a, i) -> SAssign (XArg (XVar v TSusp) TSusp i) TObj a)
			$ zip args [0..]
			
	return	( assignSS
		, allocX)

	
   
-----
expandCallApp 
	:: Exp ()
	-> ExM ([Stmt ()], Exp ())

expandCallApp  
	x@(XCallApp f superA args)
 = do
 	tmp	<- newVar Nothing
	let (callAs, appAs)
			= splitAt superA args

 	let callSS	= [ SAssign (XVar tmp TObj) TObj (XCall  f   callAs) ]

	(appSS, lastX)	<- expandApply (XApply (XVar tmp TObj) appAs)

	return	( callSS ++ appSS 
		, lastX)
	

-----
expandApply 
	:: Exp ()
	-> ExM ([Stmt ()], Exp ())

expandApply
	x@(XApply (XVar v _) xx)
 = do
	(vApp, ss)	<- expandApplyN 4 v xx []

	let initSS	= init ss
	let Just lastS	= takeLast ss
	let lastX	= case lastS of
				SAssign _ _ x	-> x

	return	( initSS
		, lastX)


-----
expandApplyN 
	:: Int 
	-> Var 
	-> [Exp ()] 
	-> [Stmt ()]
	-> ExM (Var, [Stmt ()])

expandApplyN
	maxApp		-- largest apply function to use
	thunkV		-- the var to apply more args to
	args		-- the args to apply
	ssAcc		-- stmt accumulator

	| []	<- args
	= return (thunkV, ssAcc)
	
	| otherwise
	= do
		v		<- newVar Nothing

	 	let (argsHere, argsMore)	
				= splitAt maxApp args
 		
		let ssAcc'
			=  ssAcc
			++ [SAssign (XVar v TObj) TObj (XApply (XVar thunkV TObj) argsHere)]
		
		expandApplyN maxApp v argsMore ssAcc'


