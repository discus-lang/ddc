
-- Try and uncover compiler errors by generating random programs and compiling them
module Main where

import Exp
import Bits
import Base

----
import Source.Util
import Source.Pretty
import Source.Exp

import Type.Exp

import System.Random
import System.Exit
import System.Cmd

import Shared.VarPrim
import Shared.Pretty
import Shared.Var	(NameSpace(..))
import qualified Shared.Var	as Var

import Util

-- Config ------------------------------------------------------------------------------------------

-- what scratch dir to use
scratch	= "tools/churn/scratch/"
expFuel		= 50 :: Int
typeFuel	= 50 :: Int


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
	= catInt "\n" $ map pprStrPlain prog

	

	

-- Bind --------------------------------------------------------------------------------------------
genBind :: Env -> Type -> GenM (Var, Stmt a)
genBind env tt
 = do	var	<- genVar NameValue
 	x	<- withFuel expFuel $ genExpT env tt

--	liftIO $ putStr $ pprStr $ "bindType = " % tt % "\n"

	return	( var
		, SBindPats none var [] x)

genTopsChain :: Env -> Int -> GenM [Top a]
genTopsChain env 0	
 = do	x	<- withFuel expFuel $ genExpT env tInt
	let pr	= XApp none (xVar "print") (XApp none (xVar "show") x)
 	return	$ [PStmt (SBindPats none (varV "main") [WUnit none] pr)]

genTopsChain env n
 = do	tt	<- withFuel typeFuel $ genType True
 	(v, s)	<- genBind env tt
 	rest	<- genTopsChain ((tt, v) : env) (n-1)
	return	$ PStmt s : rest

-- Prog --------------------------------------------------------------------------------------------
genProg :: GenM [Top a]
genProg
 = do	nBinds		<- genInt 5 10
 	binds		<- genTopsChain initEnv nBinds
 	return binds



-- size --------------------------------------------------------------------------------------------

sizeTree tree
 = sum $ map size tree

class Size a where
 size :: a -> Int

instance Size a => Size [a] where
 size xx	= sum $ map size xx
 
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
	XConst{}	-> 1
	XApp _ x1 x2  	-> 1 + size x1 + size x2
	XLambdaPats _ _ x	-> 1 + size x
	XMatch _ aa	-> 1 + size aa
	
instance Size (Alt a) where
 size xx
  = case xx of
  	AAlt _ gs x	-> 1 + size gs + size x
	
instance Size (Guard a) where
 size xx
  = case xx of
  	GExp _ w x	-> 1 + size w + size x
	
instance Size (Pat a) where
 size xx
  = case xx of
  	WVar _ v	-> 1
	WConst{}	-> 1
	WCon {}		-> 1
	WConLabel{}	-> 1
	WAt _ v w	-> 1 + size w
	WWildcard _	-> 1
	
	WUnit _		-> 1
	WTuple _ ps	-> size ps
	WCons _ p1 p2	-> size p1 + size p2
	WList _ ps	-> size ps



