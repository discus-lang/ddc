{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults -cpp #-}

module Llvm.Runtime.Main
	( llvmMainModule )
where

import DDC.Main.Error
import DDC.Var

import LlvmM
import Llvm
import Llvm.Runtime.Alloc
import Llvm.Runtime.Object
import Llvm.Util

import Util.Data.List


stage = "Llvm.Runtime.Main"


llvmMainModule ::
	ModuleId
	-> [ModuleId]
	-> LlvmM ()
llvmMainModule modName importsExp
 = do	argc		<- newUniqueNamedReg "argc" i32
	argv		<- newUniqueNamedReg "argv" $ pLift (pLift i8)
	let params	= [ argc, argv ]
	startFunction

	initRunTime	params
	addComment	"Call init functions of all used modules."

	mapM_		callModInitFns importsExp
	callModInitFns	modName

	addComment	"Call Main_main."
	callMain	modName
	addComment	"Clean up for the runtime."
	runtimeCleanup
	addBlock	[ Return (Just (i32LitVar 0)) ]
	endFunction	(LlvmFunctionDecl "main" External CC_Ccc i32 FixedArgs (map (\v -> (getVarType v, [])) params) Nothing)
				(map getPlainName params)	-- funcArgs
				[]				-- funcAttrs
				Nothing				-- funcSect

--------------------------------------------------------------------------------

initRunTime :: [LlvmVar] -> LlvmM ()
initRunTime params
 = do	let func	= LlvmFunctionDecl "_ddcRuntimeInit"
					External CC_Ccc LMVoid FixedArgs (map (\v -> (getVarType v, [])) params) ptrAlign
	addGlobalFuncDecl func
	addBlock	[ Expr (Call TailCall (funcVarOfDecl func) params []) ]


callModInitFns :: ModuleId -> LlvmM ()
callModInitFns mid
 = do	let func	= LlvmFunctionDecl ("_ddcInitModule_" ++ (modNameOfId mid))
					External CC_Ccc LMVoid FixedArgs [] ptrAlign
	addGlobalFuncDecl func
	addBlock	[ Expr (Call TailCall (funcVarOfDecl func) [] []) ]


runtimeCleanup :: LlvmM ()
runtimeCleanup
 = do	let func	= LlvmFunctionDecl "_ddcRuntimeCleanup"
					External CC_Ccc LMVoid FixedArgs [] ptrAlign
	addGlobalFuncDecl func
	addBlock	[ Expr (Call TailCall (funcVarOfDecl func) [] []) ]


callMain :: ModuleId -> LlvmM ()
callMain modName
 = do	let main	= LlvmFunctionDecl (modNameOfId modName ++ "_main")
					External CC_Ccc pObj FixedArgs [(pObj, [])] ptrAlign
	let thandle	= LlvmFunctionDecl "Control_Exception_topHandle"
					External CC_Ccc pObj FixedArgs [(pObj, [])] ptrAlign
	addGlobalFuncDecl main
	addGlobalFuncDecl thandle

	r1		<- allocThunk (pVarLift $ funcVarOfDecl main) 1 0
	addBlock	[ Expr (Call TailCall (funcVarOfDecl thandle) [r1] []) ]

--------------------------------------------------------------------------------

modNameOfId :: ModuleId -> String
modNameOfId (ModuleId mid)
 =	(catInt "_" mid)

modNameOfId _
 =	panic stage $ "makeInitVar: no match"
