
-- Expand out code to call supercombinators or create partial applications are required.
--
-- TODO: Setting the return value to XNull in expandS is a brutal hack.
--	 Better to detect that nothing is ever returned, or init an
--	 unused slot.
--

module Sea.Thunk
	(thunkTree)
where
import Sea.Plate.Trans
import Util
import DDC.Sea.Pretty
import DDC.Sea.Exp
import DDC.Main.Pretty
import DDC.Var
import qualified Shared.Unique	as Unique


-- | Expand calls in this tree.
thunkTree :: Tree () -> Tree ()
thunkTree tree
 	= evalState (mapM expandP tree) initExS


-- | Expand calls in this top level thing.
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
	 
	 
	PCafInit v t ss
	  -> do	let ?tailCallTargets	= []
	  	ss1		<- mapM (transformSSM expandSS) ss
	  	ss2		<- expandSS ss1
		return	$ PCafInit v t ss2
	 
	_ -> return xx
	

-- | Expand calls in the body of some super
expandSS ss
 	= liftM concat
	$ mapM expandS ss
	
expandS	s
	-- tail calls.
	--   BRUTAL HACK:
	--   For functions which never return, such as.
	--	loop f = do { f (); y = loop f; };
	--		
	--   the variable 'y' never gets assigned a real value. 
	--
	--   However, code to return from a function needs a value to
	--   (never) return so we'll give it an XNull.
	--
	| SAssign (XVar v _) t x@(XPrim (MApp PAppTailCall{}) _) <- s
	= do	callSS	<- expandTailCall x

		return	$ [ SAssign (XVar v t) t XNull ]
			++ callSS

	-- tail calls
	| SStmt x@(XPrim (MApp PAppTailCall{}) _) <- s
	= do	callSS	<- expandTailCall x
		return	callSS
	
	-- curry
	| SAssign (XVar v _) t x@(XPrim (MApp PAppCurry{}) _) <- s
	= do	(assSS, x')	<- expandCurry v x
		return	$  [ SAssign (XVar v t) t x' ]
			++ assSS

	| SStmt x@(XPrim (MApp PAppCurry{}) _) <- s
	=	-- We have a curried function as a statement. Since the curried
		-- function isn't assigned to a variable it can't be used.
		-- Therefore, we can just drop it.
		return []


	-- apply / callApp
	| SAssign v t x@(XPrim (MApp prim) _)	<- s
	,    (prim =@= PAppCallApp{})
	  || (prim =@= PAppApply{})
	= do
		(callSS, x')	<- expandAppX x
		return	$  callSS
			++ [ SAssign v t x' ]
			   
	-- applications
	| SStmt x@(XPrim (MApp prim) _) <- s
	,    (prim =@= PAppCallApp{})
	  || (prim =@= PAppApply{})
	= do
		(callSS, x')	<- expandAppX x
		return	$  callSS 
			++ [ SStmt x' ]

	| otherwise
	= do	return [s]

expandAppX x
 = case x of
 	XPrim (MApp PAppCallApp{}) _	-> expandCallApp x
	XPrim (MApp PAppApply{}) _	-> expandApply x


-- | Expand out XTailCall primitives
--	Intra-function tail calls are implemented by jumping back to the start of the function.
expandTailCall
	-- possible call targets
	:: (?tailCallTargets 
		:: [ (Var			-- function name
		     , (Var			-- label to jump to
		       , [(Var, Type)]))])	-- function parameters and their types
				

	=> Exp ()				-- the tail call primitive
	-> ExM [Stmt ()]			-- statements to do the call
	
expandTailCall x@(XPrim (MApp PAppTailCall) (XVar v _ : args))
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
	

-- | Expand code to build a thunk.
expandCurry 
	:: Var		-- ^ var to bind the thunk to.
	-> Exp () 	-- ^ thunk expresion.
	-> ExM ([Stmt ()], Exp ())

expandCurry v x@(XPrim (MApp (PAppCurry superArity)) (XVar f _  : args))
 = do	let allocX	= XPrim (MAlloc $ PAllocThunk f superArity (length args)) []
	let assignSS	= map (\(a, i) -> SAssign (XArg (XVar v (TPtr TObj)) TObjThunk i) (TPtr TObj) a)
		  	$ zip args [0..]
		
  	return	(assignSS, allocX)


-- | Expand code to do a super call then an application.
expandCallApp 
	:: Exp ()
	-> ExM ([Stmt ()], Exp ())

expandCallApp x@(XPrim (MApp (PAppCallApp superA)) (xFun@(XVar f _) : args))
 = do 	tmp	<- newVar Nothing
	let (callArgs, appArgs) 
			= splitAt superA args

 	let callSS	= [ SAssign 	(XVar tmp (TPtr TObj)) 
					(TPtr TObj) 
					(XPrim (MApp PAppCall) (xFun : callArgs)) ]

	(appSS, lastX)	<- expandApply (XPrim (MApp PAppApply) (XVar tmp (TPtr TObj) : appArgs))

	return	( callSS ++ appSS 
		, lastX)
	

-- | Expand code to do a general application.
expandApply 
	:: Exp ()
	-> ExM ([Stmt ()], Exp ())

expandApply x@(XPrim (MApp PAppApply) (XVar v _ : xx))
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
			++ [SAssign 	(XVar v (TPtr TObj)) 
					(TPtr TObj) 
					(XPrim (MApp PAppApply) (XVar thunkV (TPtr TObj) : argsHere))]
		
		expandApplyN maxApp v argsMore ssAcc'


-- ExM ------------------------------------------------------------------------
type	ExM	= State VarId
initExS		= VarId Unique.seaThunk 0

-- | Create a fresh variable based on this name
newVar 	:: Maybe String 
	-> ExM Var

newVar mName
 = do	gen		<- get
	let gen'	= incVarId gen
	put gen'
	
	let name	= fromMaybe (pprStrPlain gen) mName
	let var		= (varWithName name) { varId = gen }
	
	return var



