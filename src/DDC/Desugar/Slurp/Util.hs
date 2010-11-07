{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Utils used by the type constraint slurper.
module DDC.Desugar.Slurp.Util
	( traceM
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
import DDC.Desugar.Slurp.State
import DDC.Solve.Error
import DDC.Desugar.Exp
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type
import DDC.Type.Data
import DDC.Var
import Util
import qualified Data.Map	as Map
import qualified Data.Set	as Set

stage	= "Desugar.Slurp.Util"


-- Debugging --------------------------------------------------------------------------------------
traceM :: String -> CSlurpM ()
traceM	  ss
 	= modify (\s -> s { stateTrace = (stateTrace s) ++ [ss] })
	

-- Variables --------------------------------------------------------------------------------------
-- | Allocate a fresh variable in a given namespace.
newVarN :: NameSpace -> CSlurpM Var
newVarN space	= newVarNS space ""


-- | Allocate a fresh variable named after some string.
newVarNS :: NameSpace -> String -> CSlurpM Var
newVarNS space str	
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


-- | Allocate a fresh variable, in the same namespace as a given one.
newVarZ :: Var -> CSlurpM Var
newVarZ	var 
	= newVarN (varNameSpace var) 


-- | Bind value variables to type variables.
bindVtoT :: Var -> CSlurpM (Maybe Type)
bindVtoT varV
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


-- | Lookup the type variable corresponding to a value variable.
lookupVtoT :: Var -> CSlurpM (Maybe Var)
lookupVtoT varV
 = do 	varType		<- gets stateVarType
	return		$ Map.lookup varV varType
	

-- | Get the type variable corresponding to a value variable.
--   If there is none then `panic`.
getVtoT :: Var -> CSlurpM Var
getVtoT	varV
 = do 	mVar		<- lookupVtoT varV
	case mVar of
	 Just varT	-> return varT
	 Nothing	-> panic stage 
	 		$ "getVtoT: no type var bound for value var '" % varV % "'.\n"
			% "  bind = " % (show $ varId varV)	% "\n"
			% "  info = " % (show $ varInfo varV)	% "\n"


-- | Lazily bind a value variable to at type variable.
--   If there is an existing one then use that, otherwise create a fresh one.
lbindVtoT :: Var -> CSlurpM Type
lbindVtoT varV
 = do
 	mVar		<- lookupVtoT varV
	case mVar of
	 Just varT	-> return $ TVar kValue $ UVar varT
	 Nothing	
	  -> do	Just v	<- bindVtoT varV
	  	return	v
	 
	
-- | Add a DataDef to the slurper state.
--   This function gets called when any top level TData nodes are encountered.
addDataDef :: (Top Annot2) -> CSlurpM ()
addDataDef pp
 = case pp of
	PData _ def
 	 -> modify (\s -> s 
		{ stateDataDefs = Map.insert (dataDefName def) def (stateDataDefs s) })

	_ -> panic stage $ "addDataDef: no match"
		

-- | Add a def to the slurper state.
addDef :: Var -> Type -> CSlurpM ()
addDef v t
 	= modify (\s -> s 
		{ stateSlurpDefs = Map.insert v t (stateSlurpDefs s)})


-- | Add an error the slurper state.
addError :: Error -> CSlurpM ()
addError err
 = modify (\s -> s { stateErrors = err : stateErrors s })


-- | Say that we want this variable in the constraint solution.
--   It's the inferencers job to supply a type for each of these variables.
wantTypeV :: Var -> CSlurpM ()
wantTypeV v
	= modify (\s -> s { stateTypesRequest = Set.insert v (stateTypesRequest s) })

 
-- | Say we want several variables in the constraint solution.
wantTypeVs :: [Var] -> CSlurpM ()
wantTypeVs vs
	= modify (\s -> s { stateTypesRequest = Set.union (Set.fromList vs) (stateTypesRequest s) })
 

-- Wrappers ---------------------------------------------------------------------------------------
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

