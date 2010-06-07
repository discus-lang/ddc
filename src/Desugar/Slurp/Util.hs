
module Desugar.Slurp.Util
	( makeCtorType
	, traceM
	, newVarN
	, newVarV, newVarVS
	, newTVarD, newTVarDS
	, newTVarR, newTVarRS
	, newTVarE, newTVarES
	, newTVarC, newTVarCS
	, newVarZ
	, bindVtoT
	, lbindVtoT
	, getVtoT
	, addDataDef
	, addDef
	, addError 
	, wantTypeV
	, wantTypeVs )
where
import Util
import Shared.VarPrim
import Shared.Exp
import Type.Error
import Desugar.Exp
import Desugar.Slurp.State
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
import DDC.Type.Transform
import DDC.Var
import DDC.Util.FreeVars
import Shared.VarUtil		(prettyPos)
import qualified Shared.VarUtil	as Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set

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
 	let objType	= makeTData vData (makeDataKind vs)
			$ map (\v -> case varNameSpace v of
					NameEffect	-> TVar kEffect  $ UVar v
					NameRegion	-> TVar kRegion  $ UVar v
					NameClosure	-> TVar kClosure $ UVar v
					NameType	-> TVar kValue   $ UVar v)
			$ vs

	-- Constructors don't inspect their arguments.
	let ?newVarN	=  newVarN
 	tCtor		<- liftM toFetterFormT
			$  elaborateCloT_constrainForm
			$  makeTFunsPureEmpty (tsPrimary_elab ++ [objType])

	let vks		= map (\v -> (v, let Just k = defaultKindOfVar v in k)) 
			$ Var.sortForallVars 
			$ Set.toList (Set.union vsFree (Set.fromList vs))

	let tQuant	= makeTForall_back vks (addFetters fsField tCtor)

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
 	TSum k []
	 -> do	let Just nameSpace = spaceOfKind k
		v	<- newVarN nameSpace
	 	return	$ TVar k $ UVar v

	_ ->	return tt


checkTypeVar vs v
	| elem v vs
	= Nothing
	
	-- effect vars not present in the data type can be made pure
	| varNameSpace v == NameEffect
	= Just $ FConstraint primPure  [TVar kEffect $ UVar v]
	
	-- closure vars not present in the data type can be made empty
	| varNameSpace v == NameClosure
	= Just $ FConstraint primEmpty [TVar kClosure $ UVar v]
	
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
 	let spaceGen	= fromMaybe 	(panic stage $ "newVarNS: no space gen for " % show space % "\n")
					(Map.lookup space gen)

	let postfix	= if str == []
				then []
				else "" ++ str
			
	let var'	= (varWithName (pprStrPlain spaceGen ++ postfix))
			{ varId		= spaceGen 
			, varNameSpace		= space }
		
	let spaceGen'	= incVarId spaceGen
		
	modify (\s -> s { 
		stateGen	= Map.insert space spaceGen' (stateGen s)})
	
	return var'


newVarZ ::	Var	-> CSlurpM Var
newVarZ		var	= newVarN (varNameSpace var) 

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
		let varT'	= varT 	{ varName	= varName varV
					, varInfo	= [IValueVar varV] }
		
		-- 
		modify (\s -> s { 
			stateVarType	= Map.insert varV varT' (stateVarType s) })
		
		return $ Just (TVar kValue $ UVar varT')


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
			% "  bind = " % (show $ varId varV)	% "\n"
			% "  info = " % (show $ varInfo varV)	% "\n"

lbindVtoT ::	Var	-> CSlurpM Type
lbindVtoT	varV
 = do
 	mVar		<- lookupVtoT varV
	case mVar of
	 Just varT	-> return $ TVar kValue $ UVar varT
	 Nothing	
	  -> do	Just v	<- bindVtoT varV
	  	return	v
	 
	
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
{-	| varNameSpace v /= NameType
	= panic stage 
	$ "wantTypeV: variable " % v % " has namespace " % varNameSpace v
-}	
	| otherwise
	= modify (\s -> s { stateTypesRequest = Set.insert v (stateTypesRequest s) })
 
wantTypeVs :: [Var] -> CSlurpM ()
wantTypeVs vs
{-	| badVars@(_:_)	<- [ (v, varNameSpace v)
				| v <- vs
				, varNameSpace v /= NameType ]
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
newTVarD	= newVarN  NameType	   >>= \v -> return $ TVar kValue $ UVar v
newTVarDS s	= newVarNS NameType	s  >>= \v -> return $ TVar kValue $ UVar v

-- region
newTVarR	= newVarN  NameRegion	   >>= \v -> return $ TVar kRegion $ UVar v
newTVarRS s	= newVarNS NameRegion	s  >>= \v -> return $ TVar kRegion $ UVar v

-- effect
newTVarE	= newVarN  NameEffect	   >>= \v -> return $ TVar kEffect $ UVar v
newTVarES s	= newVarNS NameEffect	s  >>= \v -> return $ TVar kEffect $ UVar v

-- closure
newTVarC	= newVarN  NameClosure	   >>= \v -> return $ TVar kClosure $ UVar v
newTVarCS s	= newVarNS NameClosure	s  >>= \v -> return $ TVar kClosure $ UVar v

-- fetter
-- newTVarF	= newVarN  NameClass	   >>= \v -> return $ TVar kWitness v
-- newTVarFS s	= newVarNS NameClass	s  >>= \v -> return $ TVar kWitness v




