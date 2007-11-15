
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
	, newVarInst

	, bindVtoT
	, lbindVtoT
	, getVtoT

--	, setBindModeV
--	, getBindModeV

	, addDataDef
	, lookupDataDef

	, getGroundType
	, getConstType

	, slurpInstantiate
	
	, addDef
	, getDef
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
import Shared.Var		(NameSpace(..), (=^=))
import Shared.Error
import Shared.Literal
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

-----
stage	= "Desugar.Slurp.Util"

-----------------------
-- makeCtorType
--	Make a constructor type out of the corresponding line
--	from a data definition.
--
makeCtorType ::	(Var,   [Var]) -> (Var, [DataField (Exp Annot2) Type])	-> CSlurpM Type
makeCtorType	(vData, vs)	  (name, fs)
 = do
	let tsPrimary	= map dType
			$ filter dPrimary fs

	-- The objType is the type of the constructed object.
	--
 	let objType	= TData vData  
			$ map (\v -> case Var.nameSpace v of
					NameEffect	-> TVar KEffect    v
					NameRegion	-> TVar KRegion  v
					NameClosure	-> TVar KClosure v
					NameType	-> TVar KData    v)
			$ vs

	-- Constructors don't inspect their arguments.
	-- BUGS: closure information is wrong	
 	let tCtor	= makeTFunEC (TBot KEffect) (TBot KClosure) (tsPrimary ++ [objType])

	let tQuant	= TForall (map (\v -> (v, defaultKindV v)) vs)
			$ tCtor

	return tQuant

{-
defaultCtorRegion ::	Type	-> CSlurpM Type
defaultCtorRegion	t
 = do
	-- allocate a top level region for the top level
	--	type constructor.
	topRegion	<- newVarR

	-- all unnotated component constructors get this region.
	let t'		= transformT  (defaultCtorRegion' topRegion) t
	return t'


defaultCtorRegion' r t
 = case t of
 	TData v ts 	-> TData v [TVar KRegion r] : ts
	_ 		-> t
-}


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
newVarNS		space		str	
 = do
 	(Just spaceGen)	<- liftM (Map.lookup space)
			$  gets stateGen
			
	let postfix	= if str == []
				then []
				else "" ++ str
			
	let var'	= (Var.new (pretty spaceGen ++ postfix))
			{ Var.bind		= spaceGen 
			, Var.nameSpace		= space }
		
	let spaceGen'	= Var.incVarBind spaceGen
		
	modify (\s -> s { 
		stateGen	= Map.insert space spaceGen' (stateGen s)})
	
	return var'


newVarZ ::	Var	-> CSlurpM Var
newVarZ		var	= newVarN (Var.nameSpace var) 

newVarInst ::	Var	-> CSlurpM (Maybe Var)
newVarInst	var	
 = do	var'	<- newVarZ var
 	return	$ Just var'

-----------------------
-- bindVtoT ... 
--	Value Var -> Type
--
bindVtoT ::	Var -> CSlurpM Type
bindVtoT	varV
 = do
	varType		<- gets stateVarType
	
	-- Check that this var isn't already bound.
	let mVarT	= Map.lookup varV varType
	when (isJust mVarT)
	 (panic stage ("bindVtoT: var '" ++ pretty varV ++ "' is already bound\n"))
	 	

	-- Make the new type var
	varT		<- newVarN NameType 
	let varT'	= varT 	{ Var.name	= Var.name varV
				, Var.info	= [Var.IValueVar varV] }
		
	-- 
	modify (\s -> s { 
		stateVarType	= Map.insert varV varT' (stateVarType s) })
		
	return $ TVar KData varT'


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
	 Nothing	-> bindVtoT varV
	 
	
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
	 []	-> panic stage $ "getGroundType: no definition for " % name % " is in scope.\n"
	 _	-> panic stage $ "getGroundType: multiple definitions of " % name % " in scope.\n"
		

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

 	CConstU lit
	 -> do 	var		<- getConstTypeU' lit
		r		<- newVarN NameRegion
		let var'	= var { Var.info = 
					[ Var.IValueLiteral lit ] }
	
		return		$ TData var' [TVar KRegion r]

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

lookupDataDef :: Var		-> CSlurpM (Top Annot2)
lookupDataDef	 v
 = do 	dataDefs	<- gets stateDataDefs
 	let (Just def)	=  Map.lookup v dataDefs
	return def
 	

-----
-- lookupCtor
--	Use the table of DataDefs from the monad state (set up by calls to addDataDef)
--	to lookup the types associated with a particular constructor variable.
--
--	For some data def
--		data List a = Nil | Cons a (List a)
--
--	lookupCtor ('Cons') will return	  Just ('Cons', ['a', 'List a'], 'List', ['a'])
--
{-
lookupCtor 	:: Var	
		-> CSlurpM 
			(Maybe 	( Var					-- constructor name
				, [DataField (Exp Annot2) Type]		-- constructor fields
				, Var					-- constructed type name
				, [Var]))				-- constructed type args
lookupCtor	cName
 = do
	dataDefs	<- gets stateDataDefs
	let rs		= catMap (lookupCtor' cName) dataDefs

	case rs of
	 []		-> return  Nothing
	 [r]		-> returnJ r
	 
lookupCtor'	cName	(PData _ vData vsData ctors)
 = 	[ (cName, cFields, vData, vsData)
 		| CtorDef _ cName' cFields	<- ctors
 		, cName == cName']
-}

-----
-- instantiate
--	Instantiates a type, removing forall nodes.
--
--	PANIC: If any type variables are encountered which have not been bound by a forall.
--	
slurpInstantiate ::	Type	-> CSlurpM Type
slurpInstantiate	t	
 	= instantiateT newVarInst t

-----
-- addDef
--
addDef ::	Var -> Type -> CSlurpM ()
addDef		v	t
 	= modify (\s -> s 
		{ stateSlurpDefs = Map.insert v t (stateSlurpDefs s)})
	
getDef ::	Var -> CSlurpM Type
getDef		v
 = do
 	slurpDefs	<- gets stateSlurpDefs

	case Map.lookup v slurpDefs of
	 Nothing	-> panic stage $ "getDef: no def for " ++ pretty v
	 Just t		-> return t


-----
addError :: 	Error -> CSlurpM ()
addError err
 = modify (\s -> s { stateErrors = err : stateErrors s })


-----
wantTypeV :: Var -> CSlurpM ()
wantTypeV v
	| Var.nameSpace v /= NameType
	= panic stage 
	$ "wantTypeV: variable " % v % " has namespace " % Var.nameSpace v
	
	| otherwise
	= modify (\s -> s { stateTypesRequest = Set.insert v (stateTypesRequest s) })
 
wantTypeVs :: [Var] -> CSlurpM ()
wantTypeVs vs
	| badVars@(_:_)	<- [ (v, Var.nameSpace v)
				| v <- vs
				, Var.nameSpace v /= NameType ]
	= panic stage
	$ "wantTypeVs: variables have wrong namespace: " % badVars
	
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




