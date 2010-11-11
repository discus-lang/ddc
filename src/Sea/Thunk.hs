
-- Expand out code to call supercombinators or create partial applications as required.
-- We expand out code to do tail-calls.
--
-- TODO: Setting the return value to XNull in expandS is a brutal hack.
--	 Better to detect that nothing is ever returned, or init an
--	 unused slot.
--

module Sea.Thunk
	( thunkTree
	, TailTable
	, TailCallable	(..) )
where
import Sea.Plate.Trans
import Util
import DDC.Sea.Pretty
import DDC.Sea.Exp
import DDC.Sea.Compounds
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Var
import qualified Data.Map	as Map
import qualified Shared.Unique	as Unique

stage = "Sea.Thunk"

-- | Maps the var of a tail-callable super to information
--   about how to do the call.
type TailTable
	= Map 	Var 		-- Var of super name that is tail callable.
		TailCallable	-- How to call tail-call the super.

-- | Describes how to tail-call a supercombinator.
data TailCallable
	= TailCallable
	{ -- | Name of the supercombinator.
	  tailCallableSuper	:: Var

	  -- | Label to jump to to tail-call it.
	, tailCallableLabel	:: Var

	  -- | Parameters of the super, with their types.
	, tailCallableParams	:: [(Name, Type)] }


-- | Expand calls in this tree.
thunkTree :: Tree () -> Tree ()
thunkTree tree
	= evalState (mapM expandP tree) initExS


-- | Expand calls in this top level thing.
expandP :: Top () -> ExM (Top ())
expandP	xx
 = case xx of
 	PSuper vSuper params r ss
	 -> do
		lStart	<- newVar $ Just (seaVar False vSuper ++ "_start")

		let params'	= [(NAuto v, t) | (v, t) <- params]
		let table 	= Map.fromList
				[ (vSuper, TailCallable vSuper lStart params')]

		ss1	<- mapM (transformSSM (expandSS table)) ss
		ss2	<- expandSS table ss1
		return 	$ PSuper vSuper params r
			$ [SLabel lStart] ++ ss2

	PCafInit v t ss
	  -> do	let table	= Map.empty
		ss1	<- mapM (transformSSM (expandSS table)) ss
	  	ss2	<- expandSS table ss1
		return	$ PCafInit v t ss2

	_ -> return xx


-- | Expand calls in the body of some super
expandSS
	:: TailTable
	-> [Stmt()]
	-> ExM [Stmt()]

expandSS table ss
 	= liftM concat
	$ mapM (expandS table) ss

expandS	table s
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
	= do	callSS	<- expandTailCall table x

		return	$ [ SAssign (XVar v t) t xNull ]
			++ callSS

	-- tail calls
	| SStmt x@(XPrim (MApp PAppTailCall{}) _) <- s
	= do	callSS	<- expandTailCall table x
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
	:: TailTable		-- ^ Table descibing how to do the call.
	-> Exp ()		-- ^ The tail call primitive
	-> ExM [Stmt ()]	-- ^ Statements to do the call

expandTailCall table x@(XPrim (MApp PAppTailCall) (XVar name _ : args))
 = do
	-- See how we're supposed to call this function.
	let Just (TailCallable _ label params)
		= Map.lookup (varOfName name) table

	let assignParam param@(vP, tP) arg@(XVar nA _)
		-- don't emit (v = v) assignments
		| vP == nA
		= Nothing

	    assignParam param@(vP, tP) arg
	    	= Just $ SAssign (XVar vP tP) tP arg

 	return	$ -- Overwrite the parameter vars with the new args.
		   (catMaybes $ zipWith assignParam params args)

		-- Jump back to the start of the function.
		++ [SGoto label]

-- | Expand code to build a thunk.
expandCurry
	:: Name		-- ^ name to bind the thunk to.
	-> Exp () 	-- ^ thunk expresion.
	-> ExM ( [Stmt ()]
	       , Exp ())

expandCurry nThunk x@(XPrim (MApp (PAppCurry superArity))
			    (XVar (NSuper super) t  : args))

 = do	let allocX	= XPrim (MAlloc $ PAllocThunk super t superArity (length args)) []
	let assignSS	= map (\(a, i) -> SAssign
						(XArgThunk (XVar nThunk tPtrObj) i)
						tPtrObj a)
		  	$ zip args [0..]

  	return	(assignSS, allocX)

expandCurry _ x
	= panic stage $ "expandCurry: no match for " % show x


-- | Expand code to do a super call then an application.
expandCallApp
	:: Exp ()
	-> ExM ([Stmt ()], Exp ())

expandCallApp x@(XPrim (MApp (PAppCallApp superA)) (xFun@(XVar f _) : args))
 = do 	tmp	<- liftM NAuto $ newVar Nothing
	let (callArgs, appArgs)
			= splitAt superA args

 	let callSS	= [ SAssign 	(XVar tmp tPtrObj)
					tPtrObj
					(XPrim (MApp PAppCall) (xFun : callArgs)) ]

	(appSS, lastX)	<- expandApply 	(XPrim 	(MApp PAppApply)
						(XVar tmp tPtrObj : appArgs))

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
	-> Name
	-> [Exp ()]
	-> [Stmt ()]
	-> ExM (Name, [Stmt ()])

expandApplyN
	maxApp		-- largest apply function to use
	nThunk		-- the var to apply more args to
	args		-- the args to apply
	ssAcc		-- stmt accumulator

	| []	<- args
	= return (nThunk, ssAcc)

	| otherwise
	= do
		name		<- liftM NAuto $ newVar Nothing

	 	let (argsHere, argsMore)
				= splitAt maxApp args

		let ssAcc'
			=  ssAcc
			++ [SAssign 	(XVar name tPtrObj)
					tPtrObj
					(XPrim (MApp PAppApply) (XVar nThunk tPtrObj : argsHere))]

		expandApplyN maxApp name argsMore ssAcc'


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



