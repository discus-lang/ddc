
-- Try and uncover compiler errors by generating random programs and compiling them
module Main where

import Bits

import System.Random
import System.Exit
import System.Cmd

import Control.Monad.State
import Util

import Type.Pretty

import Source.Util
import Source.Pretty
import Source.Exp

import Shared.Base
import Shared.Literal
import Shared.VarPrim

import Shared.Var		(NameSpace(..))
import qualified Shared.Var	as Var

-- Config ------------------------------------------------------------------------------------------

-- what scratch dir to use
scratch	= "tools/churn/scratch/"



----------------------------------------------------------------------------------------------------
type Env	= [(Type, Var)]

-- Generator Monad ---------------------------------------------------------------------------------
-- TODO: add deterministic support
type GenM a 	= StateT GenS IO a

data GenS
	= GenS 
	{ stateVarGen	:: Int 
	, stateFuel	:: Int }

genStateZero
	= GenS
	{ stateVarGen	= 0 
	, stateFuel	= 1000 }

evalGen f = evalStateT f genStateZero

-- | burn some fuel
burn n		= modify $ \s -> s { stateFuel = stateFuel s - n }

-- | check how much fuel is left
checkFuel :: GenM Int
checkFuel 	= gets stateFuel


withFuel :: Int -> GenM a -> GenM a
withFuel fuel f
 = do	oldFuel	<- gets stateFuel
 	modify $ \s -> s { stateFuel = fuel }
	x	<- f
	modify $ \s -> s { stateFuel = oldFuel }
	return x

-- | Eval a generator and print the result
eval :: Pretty a => GenM a -> IO ()
eval f 
 = do	x	<- evalGen f
 	putStr $ pprStr x
	putStr $ "\n\n"


-- | Eval and print this generator multiple times
runN 	:: Pretty a 
	=> Int -> GenM a -> IO ()
runN n f 
 = do	xs	<- replicateM n (evalGen f)
 	putStr $ catInt "\n" $ map pprStr xs
	putStr $ "\n\n"

-- runTest ------------------------------------------------------------------------------------------
main 	= churn 0
 
churn :: Int -> IO ()
churn ix
 = do	(prog, code)	<- runTest ix
 	case code of
	 ExitSuccess	-> churn (ix + 1)
	 ExitFailure _	
	  -> do	stashFailure prog
	  	churn (ix + 1)
	  	
-- | Handle compile failure
--	 
stashFailure :: Tree a -> IO ()
stashFailure code
 = do	
 	-- calculate program size
	let codeSize	= sizeTree code
 	putStr $ "! failed at size " ++ show codeSize ++ "\n"

	-- write the offending program to a new file
	let logName	= scratch ++ "test.error-size" ++ show codeSize ++ ".ds"
	writeFile logName (pprProg code)

	return ()
	

-- | Generate a random program and compile it.

runTest :: Int 			-- index of test
	-> IO (Tree a, ExitCode)

runTest ix
 = do	putStr $ "* running test " ++ show ix ++ "\n"
 	prog		<- evalGen $ genProg
	let fileSource	= scratch ++ "test.ds"

 	writeFile fileSource (pprProg prog)

	system	$ "rm -f " ++ scratch ++ "test.dump*"
	code	<- system $ "bin/ddc -i library -c " ++ fileSource
	return (prog, code)

pprProg prog
	= catInt "\n" $ map pprStr prog




----------------------------------------------------------------------------------------------------
genInt :: Int -> Int -> GenM Int
genInt	min max	= liftIO $ getStdRandom (randomR (min, max))

genBool :: GenM Bool
genBool
 = do	r	<- genInt 0 1
 	case r of
		0 	-> return False
		1	-> return True
		

genVar :: NameSpace -> GenM Var
genVar space
 = do	ix	<- gets stateVarGen
 	modify $ \s -> s { stateVarGen = ix + 1 }
	
	let var	= (Var.new ("v" ++ show ix))
			{ Var.nameSpace = space }
	
	return	var
	

-- generate a random type
genType :: GenM Type
genType 
 = do	r_	<- genInt 0 3
	fuel	<- checkFuel
	
	let r	| fuel > 0	= r_
		| otherwise	= 0

--	liftIO 	$ putStr $ "r = " ++ show r ++ "\n"

	let result
		| r == 0	
		= do	burn 1
			return $ TData primTUnit []

		| r <= 1
		= do	burn 1
			return $ TData primTInt  [TWild KRegion]
		
		| r <= 3
		= do	burn 1
		 	t1	<- genType
			t2	<- genType
			return	$ TFun t1 t2 (TWild KEffect) (TWild KClosure)	
			
	result
	
-- Exp ---------------------------------------------------------------------------------------------
-- generate a random expression
genExpT 
	:: Env			-- environment
	-> Type			-- type of expression to generate
	-> GenM (Exp a)

genExpT env tt
 = do	doApp	<- genBool
	fuel	<- checkFuel
 	if (doApp && fuel > 0)
		then genExpT_app  env tt
		else genExpT_base env tt

-- make an expression of this type, and do it by applying 
--	a function in the environment
genExpT_app env tt
 = do	
 	-- try and find a function in the environment that can give us
	--	the result type we're after
	let mTV		= find (\(t, v)	-> resultTypeT t == tt
					&& isFun t)
			$ env
			
	case mTV of
		Nothing			-> genExpT_base env tt
		Just (tFun, vFun)	-> genExpT_call env tt tFun vFun

genExpT_call env tt tFun vFun
 = do	
 	-- make the arguments to the call
 	let tsArgs	= argTypesT tFun
	burn (length tsArgs)
 	xsArgs		<- mapM (genExpT env) tsArgs
	return	$ makeCall (XVar none vFun : xsArgs)

		
genExpT_base env tt
	-- unit
	| TData v []			<- tt
	, v == primTUnit
	= do	burn 1
		return $ XUnit none

	-- literal
	| TData v [TWild KRegion]	<- tt
	, v == primTInt 
	= do	burn 1
		r	<- genInt 0 100
		return	$ XConst none (CConst (LInt r))
		

	-- function
	| TFun t1 t2 _ _		<- tt
	= do	burn 1
		v	<- genVar NameValue
		xBody	<- genExpT [(t1, v)] t2 
		return	$  XLambda none v xBody
		

-- generate a random expression, starting from an empty environment
genExp :: GenM (Exp a)
genExp 
 = do	t	<- genType
 	x	<- genExpT initEnv t
	return	$ x

-- Bind --------------------------------------------------------------------------------------------
genBind :: Env -> Type -> GenM (Var, Stmt a)
genBind env tt
 = do	var	<- genVar NameValue
 	x	<- genExpT env tt

--	liftIO $ putStr $ pprStr $ "bindType = " % tt % "\n"

	return	( var
		, SBindPats none var [] x)

genTopsChain :: Env -> Int -> GenM [Top a]
genTopsChain env 0	
 = do	x	<- genExpT env tInt
	let pr	= XApp none (xVar "print") (XApp none (xVar "show") x)
 	return	$ [PStmt (SBindPats none (varV "main") [XUnit none] pr)]

genTopsChain env n
 = do	tt	<- withFuel 100 $ genType
 	(v, s)	<- genBind env tt
 	rest	<- genTopsChain ((tt, v) : env) (n-1)
	return	$ PStmt s : rest

-- Prog --------------------------------------------------------------------------------------------
genProg :: GenM [Top a]
genProg
 = do	nBinds		<- genInt 5 15
 	binds		<- genTopsChain initEnv nBinds
 	return binds



-- size --------------------------------------------------------------------------------------------

sizeTree tree
 = sum $ map size tree

class Size a where
 size :: a -> Int
 
instance Size (Top a) where
 size pp
  = case pp of
  	PStmt s	-> size s

instance Size (Stmt a) where
 size ss
  = case ss of
  	SBindPats _ v ps x
	  -> 1 + length ps + size x 
	
instance Size (Exp a) where
 size xx 
  = case xx of
	XVar{}		-> 1
	XUnit{}		-> 1
	XConst{}	-> 1
	XApp _ x1 x2  	-> 1 + size x1 + size x2
	XLambda _ v x	-> 1 + size x



