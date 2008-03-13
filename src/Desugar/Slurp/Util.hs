
module Desugar.Slurp.Util
	( makeCtorType
	, traceM

	, newVarN
	, newVarV, newVarVS
	, newTVarD, newTVarDS
	, newTVarR, newTVarRS
	, newTVarE, newTVarES
	, newTVarC, newTVarCS
	, newTVarF, newTVarFS
	, newVarZ
	, bindVtoT
	, lbindVtoT
	, getVtoT

	, addDataDef

	, getGroundType
	, getConstType

	, addDef
	, addError 
	
	, wantTypeV
	, wantTypeVs )

where

-----
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import Util

import qualified Shared.Var	as Var
import qualified Shared.VarUtil	as Var
import Shared.Var		(NameSpace(..), (=^=))
import Shared.VarPrim
import Shared.VarUtil		(prettyPos)
import Shared.Error
import Shared.Literal
import Shared.Pretty
import Shared.Exp

import Type.Exp
import Type.Pretty
import Type.Util
import Type.Error
import Type.Plate

import Constraint.Exp
import Constraint.Bits

import Desugar.Exp
import Desugar.Slurp.State

import Debug.Trace

-----
stage	= "Desugar.Slurp.Util"

-- makeCtorType ------------------------------------------------------------------------------------
--	Make a constructor type out of the corresponding line from a data definition.
--
makeCtorType 
	:: Monad m
	=> (NameSpace -> m Var)		-- newVarN
	-> Var				-- data type name
	-> [Var]			-- data type args
	-> Var				-- constructor name
	-> [DataField a Type]		-- constructor args
	-> m Type

makeCtorType newVarN vData vs name fs
 = do
	-- the primary fields are the ones that can be passed to the constructor function.
	let tsPrimary	= map dType
			$ filter dPrimary fs

	-- make sure there are variables present on all the effect and closure annots 
	--	for fields with function types.
	tsPrimary_elab	<- mapM (transformTM (elabBot newVarN)) tsPrimary

	-- gather up all the vars from the field type.
	let vsFree	= Set.filter (\v -> not $ Var.isCtorName v) 
			$ freeVars tsPrimary_elab

	-- Check for vars in the field type aren't params of the data type.
	--	If they are effects or closures we can force them to be Bot with Pure / Empty fetters.
	let fsField	= catMaybes 
			$ map (checkTypeVar vs) $ Set.toList vsFree

	-- The objType is the type of the constructed object.
 	let objType	= TData vData  
			$ map (\v -> case Var.nameSpace v of
					NameEffect	-> TVar KEffect  v
					NameRegion	-> TVar KRegion  v
					NameClosure	-> TVar KClosure v
					NameType	-> TVar KData    v)
			$ vs

	-- Constructors don't inspect their arguments.
	let ?newVarN	=  newVarN
 	tCtor		<- elaborateCloT 
			$  makeTFunEC (TBot KEffect) (TBot KClosure) (tsPrimary_elab ++ [objType])

	let vks		= map (\v -> (v, defaultKindV v)) 
			$ Var.sortForallVars 
			$ Set.toList (Set.union vsFree (Set.fromList vs))

	let tQuant	= TForall vks (addFetters_front fsField tCtor)

	return 	$ {- trace (pprStrPlain
			$ "makeCtorType\n"
			% "    vs              = " % vs			% "\n"
			% "    tsPrimary       = " % tsPrimary		% "\n"
			% "    tsPrimary_elab  = " % tsPrimary_elab	% "\n"
			% "    vsFree          = " % vsFree             % "\n"
			% "    fsField         = " % fsField		% "\n") -}
			tQuant


-- | Replace bottoms in this type with fresh variables.
elabBot newVarN tt
 = case tt of
 	TBot k	
	 -> do	v	<- newVarN (spaceOfKind k)
	 	return	$ TVar k v

	_ ->	return tt


checkTypeVar vs v
	| elem v vs
	= Nothing
	
	-- effect vars not present in the data type can be made pure
	| Var.nameSpace v == NameEffect
	= Just $ FConstraint primPure  [TVar KEffect v]
	
	-- closure vars not present in the data type can be made empty
	| Var.nameSpace v == NameClosure
	= Just $ FConstraint primEmpty [TVar KClosure v]
	
	| otherwise
	= dieWithUserError
		[ prettyPos v % "\n"
		% "    Variable " % v % " is not present in the data type\n" ]
	



-----------------------
-- Debugging
--
traceM :: String -> CSlurpM ()
traceM	  ss
 	= modify (\s -> s { stateTrace = (stateTrace s) ++ [ss] })
	

-----------------------
-- newVarN / newVarZ
--	Variable creation
--
newVarN :: NameSpace -> CSlurpM Var
newVarN space	= newVarNS space ""

newVarNS ::	NameSpace ->	String -> CSlurpM Var
newVarNS	space		str	
 = do	gen		<- gets stateGen
 	let spaceGen	= fromMaybe 	(panic stage $ "newVarNS: no space gen for " % space % "\n")
					(Map.lookup space gen)

	let postfix	= if str == []
				then []
				else "" ++ str
			
	let var'	= (Var.new (pprStrPlain spaceGen ++ postfix))
			{ Var.bind		= spaceGen 
			, Var.nameSpace		= space }
		
	let spaceGen'	= Var.incVarBind spaceGen
		
	modify (\s -> s { 
		stateGen	= Map.insert space spaceGen' (stateGen s)})
	
	return var'


newVarZ ::	Var	-> CSlurpM Var
newVarZ		var	= newVarN (Var.nameSpace var) 

-----------------------
-- bindVtoT ... 
--	Value Var -> Type
--
bindVtoT ::	Var -> CSlurpM (Maybe Type)
bindVtoT	varV
 = do
	varType		<- gets stateVarType
	
	-- Check that this var isn't already bound.
	let mVarT :: Maybe Var	= Map.lookup varV varType

	case mVarT of
	 Just vBound
	  -> freakout stage 
	  	( "bindVtoT: var '" % varV % "' is already bound\n"
		% "    trying to bind " % show varV 	% "\n\n"
		% "    conflicts with " % show vBound	% "\n\n")
	
		$ return Nothing
		
	 Nothing 
	  -> do
		-- Make the new type var
		varT		<- newVarN NameType 
		let varT'	= varT 	{ Var.name	= Var.name varV
					, Var.info	= [Var.IValueVar varV] }
		
		-- 
		modify (\s -> s { 
			stateVarType	= Map.insert varV varT' (stateVarType s) })
		
		return $ Just (TVar KData varT')


lookupVtoT ::	Var	-> CSlurpM (Maybe Var)
lookupVtoT	varV
 = do
 	varType		<- gets stateVarType
	return		$ Map.lookup varV varType
	

getVtoT ::	Var	-> CSlurpM Var
getVtoT		varV
 = do
 	mVar		<- lookupVtoT varV
	case mVar of
	 Just varT	-> return varT
	 Nothing	-> panic stage 
	 		$ "getVtoT: no type var bound for value var '" % varV % "'.\n"
			% "  bind = " % (show $ Var.bind varV)	% "\n"
			% "  info = " % (show $ Var.info varV)	% "\n"

lbindVtoT ::	Var	-> CSlurpM Type
lbindVtoT	varV
 = do
 	mVar		<- lookupVtoT varV
	case mVar of
	 Just varT	-> return $ TVar KData varT
	 Nothing	
	  -> do	Just v	<- bindVtoT varV
	  	return	v
	 
	
------
-- getGroundType
--	Looks up the variable assoicated with a type name.
--	All ground types must be defined in the source file eg, "data Int;"
--
getGroundType :: String		-> CSlurpM Var
getGroundType	 name
 = do
	dataDefs	<- gets stateDataDefs

	let vsGround	= [v 	| PData _ v _ _	<- Map.elems dataDefs
				, Var.name v == name]
				
	case vsGround of 
	 [v]	-> return v
	 []	-> panic stage 
	 		$ "getGroundType: no definition for " % name % " is in scope.\n"
			% "    If you're not using the std prelude you still need to define\n"
			% "    all the things in the Base module.\n"

	 _	-> panic stage 
	 		$ "getGroundType: multiple definitions of " % name % " in scope.\n"
		

-----
-- getConstType
--	Lookup the type associated with a particular constant.
--
getConstType ::	  Const		-> CSlurpM Type
getConstType	c
 = case c of
 	CConst lit
	 -> do 	var		<- getConstType' lit
		r		<- newVarN NameRegion
		let var'	= var { Var.info = 
					[ Var.IValueLiteral lit ] }
	
		return		$ TData var' [TVar KRegion r]

	-- unboxed string literals have regions annotations
	CConstU lit@(LString{})
	 -> do	var		<- getConstTypeU' lit
		r		<- newVarN NameRegion
		let var'	= var { Var.info = 
					[ Var.IValueLiteral lit ] }
	
		return		$ TData var' [TVar KRegion r]


	-- other literals ie: Ints, Floats, Chars don't have regions
 	CConstU lit
	 -> do 	var		<- getConstTypeU' lit
		r		<- newVarN NameRegion
		let var'	= var { Var.info = 
					[ Var.IValueLiteral lit ] }

		return		$ TData var' []


getConstType' lit
 = case lit of
 	LInt	_ -> getGroundType "Int"
	LChar	_ -> getGroundType "Char"
	LFloat	_ -> getGroundType "Float"
	LString	_ -> getGroundType "String"

getConstTypeU' lit
 = case lit of
 	LInt	_ -> getGroundType "Int32#"
	LFloat	_ -> getGroundType "Float32#"
	LChar	_ -> getGroundType "Char#"
	LString	_ -> getGroundType "String#"



-----
-- addDataDef
--	Add a DataDef to the CSlurp state
--	This function gets called when any top level TData nodes are encountered.
--
addDataDef ::	(Top Annot2)	-> CSlurpM ()
addDataDef	ddef@(PData _ v vs ctors)
 	= modify (\s -> s 
		{ stateDataDefs = Map.insert v ddef (stateDataDefs s) })

addDef ::	Var -> Type -> CSlurpM ()
addDef		v	t
 	= modify (\s -> s 
		{ stateSlurpDefs = Map.insert v t (stateSlurpDefs s)})

-----
addError :: 	Error -> CSlurpM ()
addError err
 = modify (\s -> s { stateErrors = err : stateErrors s })


-----
wantTypeV :: Var -> CSlurpM ()
wantTypeV v
{-	| Var.nameSpace v /= NameType
	= panic stage 
	$ "wantTypeV: variable " % v % " has namespace " % Var.nameSpace v
-}	
	| otherwise
	= modify (\s -> s { stateTypesRequest = Set.insert v (stateTypesRequest s) })
 
wantTypeVs :: [Var] -> CSlurpM ()
wantTypeVs vs
{-	| badVars@(_:_)	<- [ (v, Var.nameSpace v)
				| v <- vs
				, Var.nameSpace v /= NameType ]
	= panic stage
	$ "wantTypeVs: variables have wrong namespace: " % badVars
-}
	| otherwise
	= modify (\s -> s { stateTypesRequest = Set.union (Set.fromList vs) (stateTypesRequest s) })
 


-----------------------
-- Short forms
--

-- value
newVarV		= newVarN  NameValue	
newVarVS	= newVarNS NameValue

-- data
newTVarD	= newVarN  NameType	   >>= \v -> return $ TVar KData v
newTVarDS s	= newVarNS NameType	s  >>= \v -> return $ TVar KData v

-- region
newTVarR	= newVarN  NameRegion	   >>= \v -> return $ TVar KRegion v
newTVarRS s	= newVarNS NameRegion	s  >>= \v -> return $ TVar KRegion v

-- effect
newTVarE	= newVarN  NameEffect	   >>= \v -> return $ TVar KEffect v
newTVarES s	= newVarNS NameEffect	s  >>= \v -> return $ TVar KEffect v

-- closure
newTVarC	= newVarN  NameClosure	   >>= \v -> return $ TVar KClosure v
newTVarCS s	= newVarNS NameClosure	s  >>= \v -> return $ TVar KClosure v

-- fetter
newTVarF	= newVarN  NameClass	   >>= \v -> return $ TVar KFetter v
newTVarFS s	= newVarNS NameClass	s  >>= \v -> return $ TVar KFetter v




